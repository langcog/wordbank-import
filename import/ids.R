# Assign Redivis-consistent surrogate IDs: reuse by natural key, else max+1.
# Persists id_registry.rds so re-runs stay stable.

suppressPackageStartupMessages({
  library(tidyverse)
  library(redivis)
})

CORE_TABLES <- c(
  "instruments", "datasets", "children", "administrations",
  "items", "language_exposures", "health_conditions"
)

#' Use Redivis browser OAuth (or cached ~/.redivis credentials), not API tokens.
ensure_redivis_auth <- function() {
  if (nzchar(Sys.getenv("REDIVIS_API_TOKEN", unset = ""))) {
    Sys.unsetenv("REDIVIS_API_TOKEN")
  }
  invisible(TRUE)
}

wordbank_dataset <- function(version = NULL) {
  ensure_redivis_auth()
  ref <- if (is.null(version)) "wordbank" else paste0("wordbank:", version)
  redivis$organization("datapages")$dataset(ref)
}

#' Pull core Redivis tables (skips item_responses).
pull_redivis_core <- function(ds = NULL) {
  if (is.null(ds)) ds <- wordbank_dataset()
  ds$get()
  out <- map(set_names(CORE_TABLES), \(nm) {
    message("pulling ", nm)
    ds$table(nm)$to_tibble()
  })
  out
}

manifest_dataset_keys <- function(manifest_dataset) {
  manifest_dataset |>
    select(dataset_name, dataset_origin_name, language, form)
}

#' Drop manifest datasets from a Redivis core-table snapshot (complete mode).
remove_manifest_from_existing <- function(existing, manifest_dataset) {
  keys <- manifest_dataset_keys(manifest_dataset)
  lang_form <- manifest_dataset |> distinct(language, form)

  admins_remove <- existing$administrations |>
    semi_join(keys, by = c("dataset_name", "language", "form"))
  remove_data_ids <- admins_remove$data_id
  remove_child_ids <- unique(admins_remove$child_id)

  list(
    instruments = existing$instruments |>
      anti_join(lang_form, by = c("language", "form")),
    datasets = existing$datasets |>
      anti_join(keys, by = c("dataset_name", "dataset_origin_name", "language", "form")),
    children = existing$children |>
      filter(!child_id %in% remove_child_ids),
    administrations = existing$administrations |>
      filter(!data_id %in% remove_data_ids),
    items = existing$items |>
      anti_join(lang_form, by = c("language", "form")),
    language_exposures = existing$language_exposures |>
      filter(!data_id %in% remove_data_ids),
    health_conditions = existing$health_conditions
  )
}

#' Compare Redivis snapshot vs fresh ingest for manifest datasets (complete mode).
log_merge_discrepancies <- function(existing, new_parts, manifest_dataset, registry) {
  count_rows <- map_dfr(seq_len(nrow(manifest_dataset)), \(i) {
    k <- manifest_dataset[i, ]
    tibble(
      dataset_name = k$dataset_name,
      language = k$language,
      form = k$form,
      redivis_administrations = existing$administrations |>
        filter(
          dataset_name == k$dataset_name,
          language == k$language,
          form == k$form
        ) |>
        nrow(),
      ingest_administrations = new_parts$administrations |>
        filter(
          dataset_name == k$dataset_name,
          language == k$language,
          form == k$form
        ) |>
        nrow()
    )
  }) |>
    mutate(administration_delta = ingest_administrations - redivis_administrations)

  reg_adm <- registry$administrations |>
    semi_join(manifest_dataset, by = c("dataset_name", "language", "form")) |>
    mutate(
      study_internal_id = as.character(study_internal_id),
      admin_row = as.integer(admin_row)
    )
  ing_adm <- new_parts$administrations |>
    semi_join(manifest_dataset, by = c("dataset_name", "language", "form")) |>
    mutate(
      study_internal_id = as.character(study_internal_id),
      admin_row = as.integer(admin_row)
    ) |>
    distinct(across(all_of(ADMIN_NATURAL_KEY_COLS)))
  registry_only <- reg_adm |>
    anti_join(ing_adm, by = ADMIN_NATURAL_KEY_COLS)
  ingest_only <- ing_adm |>
    anti_join(reg_adm, by = ADMIN_NATURAL_KEY_COLS)

  if (nrow(count_rows)) {
    message("Complete mode administration counts (ingest - Redivis):")
    walk(seq_len(nrow(count_rows)), \(i) {
      r <- count_rows[i, ]
      message(
        "  ", r$dataset_name, " ", r$language, " ", r$form, ": ",
        r$ingest_administrations, " ingest, ", r$redivis_administrations,
        " Redivis (delta ", r$administration_delta, ")"
      )
    })
  }
  if (nrow(registry_only)) {
    warning(
      nrow(registry_only),
      " administration(s) in id_registry but missing from raw ingest",
      call. = FALSE
    )
  }
  if (nrow(ingest_only)) {
    message(nrow(ingest_only), " new administration(s) in raw ingest (not in id_registry)")
  }

  list(
    counts = count_rows,
    registry_only = registry_only,
    ingest_only = ingest_only
  )
}

item_response_slug <- function(language, form) {
  paste(language, form) |>
    str_to_lower() |>
    str_replace_all("[^a-z0-9]+", "_") |>
    str_replace_all("^_|_$", "")
}

empty_registry <- function() {
  list(
    children = tibble(
      dataset_origin_name = character(),
      study_internal_id = character(),
      sex = character(),
      race = character(),
      ethnicity = character(),
      birth_order = integer(),
      caregiver_education = character(),
      child_id = integer()
    ),
    administrations = tibble(
      dataset_name = character(),
      dataset_origin_name = character(),
      language = character(),
      form = character(),
      study_internal_id = character(),
      admin_row = integer(),
      data_id = numeric()
    ),
    instruments = tibble(
      language = character(),
      form = character(),
      instrument_id = integer()
    ),
    datasets = tibble(
      dataset_name = character(),
      dataset_origin_name = character(),
      language = character(),
      form = character(),
      dataset_id = integer()
    )
  )
}

load_registry <- function(path = "id_registry.rds") {
  if (file.exists(path)) readRDS(path) else empty_registry()
}

save_registry <- function(registry, path = "id_registry.rds") {
  saveRDS(registry, path)
  invisible()
}

#' Allocate integer IDs: prefer existing Redivis / registry match, else max+1.
allocate_ids <- function(keys, existing_map, id_col, max_so_far) {
  # keys: tibble of natural key cols + optional existing id from join
  # existing_map: natural key -> id
  keys <- keys |> mutate(.row = row_number())
  joined <- keys |>
    left_join(existing_map, by = setdiff(names(existing_map), id_col))

  need <- joined |> filter(is.na(.data[[id_col]]))
  n_new <- nrow(need)
  if (n_new > 0) {
    new_ids <- as.integer(seq.int(max_so_far + 1L, length.out = n_new))
    joined[[id_col]][is.na(joined[[id_col]])] <- new_ids
    max_so_far <- max(new_ids)
  }
  list(map = joined |> select(-.row), max = max_so_far)
}

#' Merge harmonized tables onto Redivis core tables.
#'
#' @param mode `"append"` keeps datasets already on Redivis and uploads only new
#'   rows; `"complete"` reimports every manifest dataset, logs count/key
#'   discrepancies, and rebuilds manifest rows from raw.
#' @return list with `tables`, `registry`, `new_item_responses`, `upload_deltas`,
#'   `discrepancies`, `mode`, and `datasets_imported`
merge_with_existing <- function(
    existing,
    new_parts,
    registry = empty_registry(),
    mode = c("append", "complete")
) {
  mode <- match.arg(mode)
  manifest_dataset <- new_parts$dataset
  existing_at_start <- existing
  discrepancies <- NULL

  coerce_redivis_types <- function(existing) {
    if (nrow(existing$children)) {
      existing$children <- existing$children |>
        mutate(
          birth_order = harm_int(birth_order),
          gestational_age = harm_int(gestational_age),
          birth_weight = harm_dbl(birth_weight)
        )
    }
    if (nrow(existing$administrations)) {
      existing$administrations <- existing$administrations |>
        mutate(
          date_of_test = harm_chr(date_of_test),
          age = harm_int(age),
          comprehension = harm_int(comprehension),
          production = harm_int(production),
          is_norming = harm_lgl(is_norming),
          in_age_range = harm_lgl(in_age_range)
        )
    }
    if (nrow(existing$language_exposures)) {
      existing$language_exposures <- existing$language_exposures |>
        mutate(
          exposure_percentage = harm_int(exposure_percentage),
          age_of_first_exposure = harm_int(age_of_first_exposure)
        )
    }
    if (nrow(existing$instruments)) {
      existing$instruments <- existing$instruments |>
        mutate(
          age_min = harm_int(age_min),
          age_max = harm_int(age_max),
          has_grammar = harm_lgl(has_grammar),
          unilemma_coverage = harm_dbl(unilemma_coverage)
        )
    }
    existing
  }
  existing <- coerce_redivis_types(existing)

  coerce_keys <- function(df) {
    if ("study_internal_id" %in% names(df)) {
      df$study_internal_id <- as.character(df$study_internal_id)
    }
    if ("admin_row" %in% names(df)) df$admin_row <- as.integer(df$admin_row)
    if ("birth_order" %in% names(df)) df$birth_order <- as.integer(df$birth_order)
    df
  }
  new_parts <- map(new_parts, coerce_keys)
  registry$administrations <- coerce_keys(registry$administrations)
  registry$children <- coerce_keys(registry$children)

  if (mode == "complete") {
    discrepancies <- log_merge_discrepancies(existing, new_parts, manifest_dataset, registry)
    message(
      "Complete mode: replacing ", nrow(manifest_dataset),
      " manifest dataset(s) on Redivis"
    )
    existing <- remove_manifest_from_existing(existing, manifest_dataset)
  }

  # --- instruments ---
  inst_existing <- existing$instruments |>
    select(language, form, instrument_id)
  inst_reg <- bind_rows(registry$instruments, inst_existing) |>
    distinct(language, form, .keep_all = TRUE)
  max_inst <- max(c(0L, inst_reg$instrument_id), na.rm = TRUE)

  new_inst_keys <- new_parts$instrument |> distinct(language, form, .keep_all = TRUE)
  alloc_inst <- allocate_ids(
    new_inst_keys |> select(language, form),
    inst_reg,
    "instrument_id",
    max_inst
  )
  inst_meta <- existing$instruments |>
    select(language, form, age_min, age_max, has_grammar)
  instruments_new <- new_inst_keys |>
    left_join(alloc_inst$map, by = c("language", "form")) |>
    left_join(inst_meta, by = c("language", "form")) |>
    mutate(
      age_min = coalesce(age_min, if_else(form_type == "WG", 8L, 16L)),
      age_max = coalesce(age_max, if_else(form_type == "WG", 36L, 30L)),
      has_grammar = coalesce(has_grammar, FALSE)
    ) |>
    select(
      instrument_id, language, form, form_type,
      age_min, age_max, has_grammar, unilemma_coverage
    )
  instruments <- bind_rows(
    existing$instruments,
    instruments_new |> anti_join(existing$instruments, by = c("language", "form"))
  ) |>
    distinct(language, form, .keep_all = TRUE)
  registry$instruments <- instruments |> select(language, form, instrument_id)

  # --- datasets ---
  ds_existing <- existing$datasets |>
    select(dataset_name, dataset_origin_name, language, form, dataset_id)
  ds_reg <- bind_rows(registry$datasets, ds_existing) |>
    distinct(dataset_name, dataset_origin_name, language, form, .keep_all = TRUE)
  max_ds <- max(c(0L, ds_reg$dataset_id), na.rm = TRUE)

  new_ds_keys <- new_parts$dataset
  if (mode == "append") {
    already <- new_ds_keys |>
      semi_join(ds_existing, by = c("dataset_name", "dataset_origin_name", "language", "form"))
    if (nrow(already)) {
      message(
        "Skipping ", nrow(already), " dataset row(s) already on Redivis."
      )
    }
    new_ds_keys <- new_ds_keys |>
      anti_join(ds_existing, by = c("dataset_name", "dataset_origin_name", "language", "form"))
  }

  alloc_ds <- allocate_ids(
    new_ds_keys |> select(dataset_name, dataset_origin_name, language, form),
    ds_reg,
    "dataset_id",
    max_ds
  )
  datasets_new <- new_ds_keys |>
    left_join(alloc_ds$map, by = c("dataset_name", "dataset_origin_name", "language", "form")) |>
    select(
      dataset_id, dataset_name, dataset_origin_name, contributor, citation,
      license, longitudinal, source, date_format, file_location, norming,
      splitcol, language, form, form_type, n_admins
    )
  datasets <- bind_rows(existing$datasets, datasets_new) |>
    distinct(dataset_name, dataset_origin_name, language, form, .keep_all = TRUE)
  registry$datasets <- datasets |>
    select(dataset_name, dataset_origin_name, language, form, dataset_id)

  keep_keys <- datasets_new |> select(dataset_name, language, form)
  if (nrow(keep_keys) == 0) {
    message(if (mode == "append") "No new datasets to merge." else "No manifest datasets to merge.")
    return(list(
      tables = existing,
      registry = registry,
      new_item_responses = tibble(),
      upload_deltas = NULL,
      discrepancies = discrepancies,
      mode = mode,
      datasets_imported = tibble()
    ))
  }

  new_admins <- new_parts$administrations |>
    semi_join(keep_keys, by = c("dataset_name", "language", "form"))
  child_keys <- new_admins |> distinct(across(all_of(CHILD_KEY_COLS)))
  new_children <- new_parts$children |>
    semi_join(child_keys, by = CHILD_KEY_COLS)
  new_items <- new_parts$items |>
    semi_join(keep_keys |> select(language, form), by = c("language", "form"))
  new_resp <- new_parts$item_responses |>
    semi_join(
      new_admins |> select(dataset_origin_name, study_internal_id, admin_row, language, form),
      by = c("dataset_origin_name", "study_internal_id", "admin_row", "language", "form")
    )
  new_lexp <- new_parts$language_exposures |>
    semi_join(
      new_admins |> select(dataset_origin_name, study_internal_id, admin_row),
      by = c("dataset_origin_name", "study_internal_id", "admin_row")
    )

  # --- children ---
  ch_reg <- registry$children |>
    coerce_keys() |>
    select(any_of(c(CHILD_KEY_COLS, "child_id")))
  max_ch <- max(c(0L, existing$children$child_id, ch_reg$child_id), na.rm = TRUE)

  alloc_ch <- allocate_ids(
    child_keys,
    ch_reg,
    "child_id",
    max_ch
  )
  children_new <- new_children |>
    left_join(alloc_ch$map, by = CHILD_KEY_COLS) |>
    select(
      child_id, study_internal_id, dataset_origin_name, birth_order, caregiver_education,
      ethnicity, race, sex, birth_weight, born_early_or_late,
      gestational_age, zygosity
    ) |>
    distinct(child_id, .keep_all = TRUE)
  children <- bind_rows(existing$children, children_new) |>
    distinct(child_id, .keep_all = TRUE)
  registry$children <- bind_rows(
    ch_reg,
    alloc_ch$map |> select(all_of(c(CHILD_KEY_COLS, "child_id")))
  ) |>
    distinct(across(all_of(CHILD_KEY_COLS)), .keep_all = TRUE)

  # --- administrations ---
  adm_reg <- registry$administrations
  max_data <- max(c(0, existing$administrations$data_id, adm_reg$data_id), na.rm = TRUE)

  adm_keys <- new_admins |> select(all_of(ADMIN_NATURAL_KEY_COLS))
  alloc_adm <- allocate_ids(adm_keys, adm_reg, "data_id", as.integer(max_data))
  alloc_adm$map <- alloc_adm$map |> mutate(data_id = as.numeric(data_id))

  inst_ages <- instruments |>
    select(language, form, age_min, age_max)
  admins_new <- new_admins |>
    left_join(
      alloc_adm$map,
      by = ADMIN_NATURAL_KEY_COLS
    ) |>
    left_join(
      alloc_ch$map,
      by = CHILD_KEY_COLS
    ) |>
    left_join(inst_ages, by = c("language", "form")) |>
    mutate(
      in_age_range = if_else(
        !is.na(age) & !is.na(age_min) & !is.na(age_max),
        age >= age_min & age <= age_max,
        NA
      )
    ) |>
    select(
      data_id, child_id, dataset_name, language, form, age, date_of_test,
      comprehension, production, is_norming, in_age_range
    )

  administrations <- bind_rows(existing$administrations, admins_new) |>
    distinct(data_id, .keep_all = TRUE)
  registry$administrations <- bind_rows(adm_reg, alloc_adm$map) |>
    distinct(across(all_of(ADMIN_NATURAL_KEY_COLS)), .keep_all = TRUE)

  # --- items ---
  items <- bind_rows(existing$items, new_items) |>
    distinct(language, form, item_id, .keep_all = TRUE)

  # --- language exposures ---
  lexp_new <- new_lexp |>
    left_join(
      alloc_adm$map |>
        select(dataset_origin_name, study_internal_id, admin_row, data_id),
      by = c("dataset_origin_name", "study_internal_id", "admin_row")
    ) |>
    select(data_id, language, exposure_percentage, age_of_first_exposure) |>
    mutate(
      exposure_percentage = as.integer(exposure_percentage),
      age_of_first_exposure = as.integer(age_of_first_exposure)
    )
  language_exposures <- bind_rows(existing$language_exposures, lexp_new)

  health_conditions <- existing$health_conditions

  new_item_responses <- new_resp |>
    left_join(
      alloc_adm$map |>
        select(dataset_origin_name, study_internal_id, admin_row, data_id),
      by = c("dataset_origin_name", "study_internal_id", "admin_row")
    ) |>
    left_join(
      instruments |> select(language, form, instrument_id),
      by = c("language", "form")
    ) |>
    select(
      instrument_id, language, form, data_id, item_id, value, produces, understands
    )

  upload_deltas <- if (mode == "append") {
    list(
      instruments = instruments_new |>
        anti_join(existing_at_start$instruments, by = c("language", "form")),
      datasets = datasets_new,
      children = children_new,
      administrations = admins_new,
      items = new_items |>
        anti_join(existing_at_start$items, by = c("language", "form", "item_id")),
      language_exposures = lexp_new
    )
  } else {
    NULL
  }

  list(
    tables = list(
      instruments = instruments,
      datasets = datasets,
      children = children,
      administrations = administrations,
      items = items,
      language_exposures = language_exposures,
      health_conditions = health_conditions
    ),
    registry = registry,
    new_item_responses = new_item_responses,
    upload_deltas = upload_deltas,
    discrepancies = discrepancies,
    mode = mode,
    datasets_imported = datasets_new |>
      select(dataset_name, dataset_origin_name, language, form)
  )
}

#' Upload merged export files to Redivis.
#'
#' @param export_dir Directory containing core-table CSVs and `item_responses/`.
#' @param mode `"append"` appends delta rows; `"complete"` replaces core tables
#'   and manifest instrument item-response files.
upload_to_redivis <- function(
    merged,
    export_dir,
    mode = merged$mode,
    release = FALSE,
    release_notes = NULL
) {
  mode <- match.arg(mode, c("append", "complete"))
  ensure_redivis_auth()
  ds <- wordbank_dataset()
  ds <- ds$create_next_version(if_not_exists = TRUE)

  upload_csv <- function(tname, path, strategy) {
    tb <- ds$table(tname)
    if (!tb$exists()) tb$create()
    tb$update(upload_merge_strategy = strategy)
    tb$upload(basename(path))$create(
      content = path,
      type = "delimited",
      replace_on_conflict = TRUE,
      wait_for_finish = TRUE,
      raise_on_fail = TRUE
    )
  }

  if (mode == "append") {
    stopifnot(!is.null(merged$upload_deltas))
    for (nm in names(merged$upload_deltas)) {
      df <- merged$upload_deltas[[nm]]
      if (nrow(df) == 0) next
      path <- file.path(export_dir, paste0(nm, "_delta.csv"))
      write_csv(df, path, na = "")
      message("appending ", nm, " (", nrow(df), " rows)")
      upload_csv(nm, path, "append")
    }
    resp_dir <- file.path(export_dir, "item_responses")
    resp_files <- list.files(resp_dir, full.names = TRUE)
    for (f in resp_files) {
      message("appending item_responses: ", basename(f))
      upload_csv("item_responses", f, "append")
    }
  } else {
    for (nm in names(merged$tables)) {
      path <- file.path(export_dir, paste0(nm, ".csv"))
      message("replacing ", nm)
      upload_csv(nm, path, "replace")
    }
    resp_dir <- file.path(export_dir, "item_responses")
    resp_files <- list.files(resp_dir, full.names = TRUE)
    for (f in resp_files) {
      message("replacing item_responses: ", basename(f))
      upload_csv("item_responses", f, "replace")
    }
  }

  if (isTRUE(release)) {
    notes <- if (is.null(release_notes)) {
      paste("Wordbank import", mode, Sys.Date())
    } else {
      release_notes
    }
    ds$release(release_notes = notes)
  }
  invisible(ds)
}
