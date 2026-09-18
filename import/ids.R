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

pull_redivis_core <- function(ds = NULL) {
  if (is.null(ds)) ds <- wordbank_dataset()
  ds$get()
  out <- map(set_names(CORE_TABLES), \(nm) {
    message("pulling ", nm)
    ds$table(nm)$to_tibble()
  })
  out
}

#' Compare Redivis snapshot vs fresh ingest for manifest datasets (complete mode).
log_merge_discrepancies <- function(existing, new_parts, manifest_dataset, registry) {
  count_rows <- map(seq_len(nrow(manifest_dataset)), \(i) {
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
    list_rbind() |>
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

ITEM_RESPONSE_EXPORT_COLS <- c(
  "instrument_id", "language", "form", "data_id", "item_id",
  "value", "produces", "understands"
)

assign_item_response_chunk <- function(chunk, adm_map, inst_lookup, aliases = NULL) {
  aliases <- resolve_dataset_aliases(aliases)
  chunk <- chunk |>
    mutate(
      study_internal_id = as.character(study_internal_id),
      admin_row = as.integer(admin_row)
    )
  if (nrow(aliases)) {
    chunk <- translate_dataset_keys(
      chunk,
      aliases,
      "to_manifest",
      origin_substrings = FALSE
    )
  }
  chunk |>
    left_join(adm_map, by = ADMIN_NATURAL_KEY_COLS) |>
    left_join(inst_lookup, by = c("language", "form")) |>
    select(all_of(ITEM_RESPONSE_EXPORT_COLS))
}

#' Write item responses with surrogate IDs, one harmonized source file at a time.
export_item_responses_with_ids <- function(
    sources,
    keep_keys,
    adm_map,
    inst_lookup,
    export_dir,
    replace = FALSE,
    aliases = NULL,
    exclude_data_ids = numeric(),
    unresolved_path = file.path("export", "item_response_unresolved.csv")
) {
  aliases <- resolve_dataset_aliases(aliases)
  stopifnot(
    "path" %in% names(sources),
    all(c("dataset_name", "language", "form") %in% names(sources))
  )
  dir.create(export_dir, recursive = TRUE, showWarnings = FALSE)
  if (isTRUE(replace)) {
    old <- list.files(export_dir, pattern = "\\.csv$", full.names = TRUE)
    if (length(old)) unlink(old)
  }

  adm_map <- adm_map |>
    select(all_of(ADMIN_NATURAL_KEY_COLS), data_id) |>
    filter(!data_id %in% exclude_data_ids)
  inst_lookup <- inst_lookup |> select(language, form, instrument_id)
  sources <- sources |>
    semi_join(keep_keys, by = c("dataset_name", "language", "form"))
  if (nrow(sources) == 0L) {
    return(list(files = character(), n_rows = 0L, n_sources = 0L))
  }

  slug_written <- character()
  written_files <- character()
  total_rows <- 0L
  n_unresolved <- 0L
  unresolved_parts <- list()
  for (i in seq_len(nrow(sources))) {
    src <- sources[i, ]
    if (!file.exists(src$path)) {
      stop("Missing item_responses source: ", src$path)
    }
    chunk <- read_csv(src$path, show_col_types = FALSE)
    chunk <- cast_harmonized_table(chunk, "item_responses")
    joined <- assign_item_response_chunk(chunk, adm_map, inst_lookup, aliases)
    rm(chunk)
    bad <- joined |>
      filter(is.na(data_id) | is.na(instrument_id) | is.na(item_id))
    if (nrow(bad)) {
      n_unresolved <- n_unresolved + nrow(bad)
      unresolved_parts[[length(unresolved_parts) + 1L]] <- bad |>
        mutate(source_file = basename(src$path), .before = 1)
    }
    out <- joined |>
      filter(!is.na(data_id), !is.na(instrument_id), !is.na(item_id))
    rm(joined)
    if (nrow(out) == 0L) {
      rm(out)
      next
    }
    slug <- item_response_slug(src$language, src$form)
    path <- file.path(export_dir, paste0(slug, ".csv"))
    append <- slug %in% slug_written
    write_csv(out, path, na = "", append = append)
    slug_written <- c(slug_written, slug)
    written_files <- c(written_files, path)
    total_rows <- total_rows + nrow(out)
    rm(out)
    if (i %% 10L == 0L) gc(verbose = FALSE)
  }

  if (n_unresolved > 0L) {
    dir.create(dirname(unresolved_path), recursive = TRUE, showWarnings = FALSE)
    write_csv(bind_rows(unresolved_parts), unresolved_path, na = "")
    message(
      "Skipped ", n_unresolved,
      " item response row(s) with unresolved IDs; wrote ",
      unresolved_path
    )
  }

  list(
    files = unique(written_files),
    n_rows = total_rows,
    n_sources = nrow(sources),
    n_unresolved = n_unresolved
  )
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
  invisible(registry)
}

resolve_dataset_aliases <- function(aliases) {
  if (!is.null(aliases)) return(aliases)
  if (exists("load_dataset_aliases", mode = "function")) {
    return(load_dataset_aliases())
  }
  if (exists("empty_aliases", mode = "function")) {
    return(empty_aliases())
  }
  tibble(
    field = character(),
    redivis_value = character(),
    manifest_value = character(),
    notes = character()
  )
}

#' Rebuild id_registry from Redivis core tables + harmonized administration keys.
rebuild_registry_from_redivis <- function(
    existing,
    harm_admins = NULL,
    out_harm = NULL,
    manifest = NULL,
    aliases = NULL
) {
  aliases <- resolve_dataset_aliases(aliases)

  children <- existing$children |>
    mutate(
      study_internal_id = as.character(study_internal_id),
      birth_order = harm_int(birth_order)
    ) |>
    select(all_of(c(CHILD_KEY_COLS, "child_id"))) |>
    alias_normalize_id_map(CHILD_KEY_COLS, aliases, "child_id") |>
    distinct(across(all_of(CHILD_KEY_COLS)), .keep_all = TRUE)

  instruments <- existing$instruments |>
    select(language, form, instrument_id) |>
    alias_normalize_id_map(c("language", "form"), aliases, "instrument_id")

  datasets <- existing$datasets |>
    select(
      dataset_name, dataset_origin_name, language, form, dataset_id
    ) |>
    alias_normalize_id_map(DATASET_GROUP_COLS, aliases, "dataset_id")

  if (is.null(harm_admins)) {
    if (is.null(out_harm) || is.null(manifest)) {
      stop(
        "Pass harm_admins, or out_harm + manifest, to rebuild administration mappings"
      )
    }
    if (!exists("load_harmonized_administrations", mode = "function")) {
      stop("Source import/ingest.R before rebuilding administration registry")
    }
    harm_admins <- load_harmonized_administrations(manifest, out_harm)
  }

  harm_admins <- harm_admins |>
    mutate(
      study_internal_id = as.character(study_internal_id),
      admin_row = as.integer(admin_row),
      date_of_test = harm_chr(date_of_test),
      age = harm_int(age)
    ) |>
    distinct(across(all_of(ADMIN_NATURAL_KEY_COLS)), .keep_all = TRUE)

  redivis_admins <- existing$administrations |>
    mutate(
      date_of_test = harm_chr(date_of_test),
      age = harm_int(age)
    ) |>
    select(
      data_id, child_id, dataset_name, language, form, age, date_of_test
    ) |>
    alias_normalize_id_map(
      c("dataset_name", "language", "form"),
      aliases,
      "data_id"
    )

  harm_linked <- harm_admins |>
    left_join(children, by = CHILD_KEY_COLS) |>
    left_join(
      redivis_admins,
      by = c(
        "child_id", "dataset_name", "language", "form", "age", "date_of_test"
      )
    )

  n_unmatched <- sum(is.na(harm_linked$data_id))
  if (n_unmatched > 0L) {
    warning(
      n_unmatched,
      " harmonized administration(s) did not match a Redivis data_id",
      call. = FALSE
    )
  }

  administrations <- harm_linked |>
    filter(!is.na(data_id)) |>
    select(all_of(ADMIN_NATURAL_KEY_COLS), data_id) |>
    distinct(across(all_of(ADMIN_NATURAL_KEY_COLS)), .keep_all = TRUE)

  list(
    children = children,
    administrations = administrations,
    instruments = instruments,
    datasets = datasets
  )
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
  key_cols <- setdiff(names(existing_map), id_col)
  list(
    map = joined |> select(all_of(c(key_cols, id_col))),
    max = max_so_far
  )
}

#' Merge harmonized tables onto Redivis core tables.
merge_with_existing <- function(
    existing,
    new_parts,
    registry = empty_registry(),
    mode = c("append", "complete"),
    item_response_sources = NULL,
    item_response_export_dir = NULL,
    aliases = NULL
) {
  mode <- match.arg(mode)
  aliases <- resolve_dataset_aliases(aliases)
  manifest_dataset <- new_parts$dataset
  discrepancies <- NULL
  existing_instrument_keys <- existing$instruments |>
    select(language, form) |>
    alias_normalize_id_map(c("language", "form"), aliases)
  existing_item_keys <- existing$items |>
    select(language, form, item_id) |>
    alias_normalize_id_map(c("language", "form"), aliases)

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

  stopifnot(
    "manifest_row" %in% names(new_parts$administrations),
    "manifest_row" %in% names(new_parts$dataset),
    "manifest_row" %in% names(new_parts$instrument)
  )

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
      "Complete mode: rebuilding ", nrow(manifest_dataset),
      " manifest dataset(s); export will not retain other Redivis rows"
    )
  }

  # --- instruments ---
  inst_existing <- existing$instruments |>
    select(language, form, instrument_id)
  inst_reg <- alias_normalize_id_map(
    bind_rows(registry$instruments, inst_existing),
    c("language", "form"),
    aliases,
    "instrument_id"
  )
  max_inst <- max(c(0L, inst_reg$instrument_id), na.rm = TRUE)

  new_inst_keys <- new_parts$instrument |> distinct(language, form, .keep_all = TRUE)
  alloc_inst <- allocate_ids(
    sort_keys_for_allocation(new_inst_keys, "instrument_id"),
    inst_reg,
    "instrument_id",
    max_inst
  )
  instruments_new <- new_inst_keys |>
    left_join(alloc_inst$map, by = c("language", "form")) |>
    select(
      instrument_id, language, form, form_type,
      age_min, age_max, has_grammar, unilemma_coverage
    )

  # --- datasets ---
  ds_existing <- existing$datasets |>
    select(dataset_name, dataset_origin_name, language, form, dataset_id)
  ds_existing_norm <- alias_normalize_id_map(
    ds_existing, DATASET_GROUP_COLS, aliases, "dataset_id"
  )
  ds_reg <- alias_normalize_id_map(
    bind_rows(registry$datasets, ds_existing),
    DATASET_GROUP_COLS,
    aliases,
    "dataset_id"
  )
  max_ds <- max(c(0L, ds_reg$dataset_id), na.rm = TRUE)

  new_ds_keys <- new_parts$dataset
  stopifnot("manifest_row" %in% names(new_ds_keys))
  if (mode == "append") {
    already <- new_ds_keys |>
      semi_join(
        ds_existing_norm,
        by = c("dataset_name", "dataset_origin_name", "language", "form")
      )
    if (nrow(already)) {
      message(
        "Skipping ", nrow(already), " dataset row(s) already on Redivis."
      )
    }
    new_ds_keys <- new_ds_keys |>
      anti_join(
        ds_existing_norm,
        by = c("dataset_name", "dataset_origin_name", "language", "form")
      )
  }

  alloc_ds <- allocate_ids(
    sort_keys_for_allocation(new_ds_keys, "dataset_id"),
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

  keep_keys <- datasets_new |> select(dataset_name, language, form)
  if (nrow(keep_keys) == 0) {
    message(if (mode == "append") "No new datasets to merge." else "No manifest datasets to merge.")
    empty_tables <- map(existing, \(df) df[0, , drop = FALSE])
    return(list(
      tables = empty_tables,
      registry = if (mode == "complete") empty_registry() else registry,
      item_response_export = NULL,
      upload_deltas = NULL,
      discrepancies = discrepancies,
      mode = mode,
      datasets_imported = tibble()
    ))
  }

  new_admins <- new_parts$administrations |>
    semi_join(keep_keys, by = c("dataset_name", "language", "form"))
  child_keys <- new_admins |>
    group_by(across(all_of(CHILD_KEY_COLS))) |>
    summarise(
      manifest_row = min(manifest_row, na.rm = TRUE),
      admin_row = min(admin_row, na.rm = TRUE),
      .groups = "drop"
    )
  new_children <- new_parts$children |>
    semi_join(child_keys, by = CHILD_KEY_COLS)
  new_items <- new_parts$items |>
    semi_join(keep_keys |> select(language, form), by = c("language", "form"))
  new_lexp <- new_parts$language_exposures |>
    semi_join(
      new_admins |>
        select(all_of(ADMIN_NATURAL_KEY_COLS)) |>
        rename(
          instrument_language = language,
          instrument_form = form
        ),
      by = c(
        "dataset_name", "dataset_origin_name", "study_internal_id", "admin_row",
        "instrument_language", "instrument_form"
      )
    )
  new_hc <- if (
    "health_conditions" %in% names(new_parts) && nrow(new_parts$health_conditions) > 0L
  ) {
    new_parts$health_conditions
  } else {
    tibble(
      dataset_origin_name = character(),
      study_internal_id = character(),
      health_condition_name = character()
    )
  }

  # --- children ---
  ch_reg <- registry$children |>
    coerce_keys() |>
    select(any_of(c(CHILD_KEY_COLS, "child_id"))) |>
    alias_normalize_id_map(CHILD_KEY_COLS, aliases, "child_id")
  max_ch <- max(c(0L, existing$children$child_id, ch_reg$child_id), na.rm = TRUE)

  alloc_ch <- allocate_ids(
    sort_keys_for_allocation(child_keys, "child_id"),
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

  hc_new <- new_hc |>
    semi_join(
      new_children |> select(dataset_origin_name, study_internal_id),
      by = c("dataset_origin_name", "study_internal_id")
    ) |>
    left_join(
      children_new |> select(dataset_origin_name, study_internal_id, child_id),
      by = c("dataset_origin_name", "study_internal_id")
    ) |>
    filter(!is.na(child_id)) |>
    select(child_id, health_condition_name) |>
    distinct()

  # --- administrations ---
  adm_reg <- registry$administrations |>
    alias_normalize_id_map(ADMIN_NATURAL_KEY_COLS, aliases, "data_id")
  max_data <- max(c(0, existing$administrations$data_id, adm_reg$data_id), na.rm = TRUE)

  adm_keys <- new_admins |>
    select(all_of(ADMIN_NATURAL_KEY_COLS), manifest_row)
  alloc_adm <- allocate_ids(
    sort_keys_for_allocation(adm_keys, "data_id"),
    adm_reg,
    "data_id",
    as.integer(max_data)
  )
  alloc_adm$map <- alloc_adm$map |> mutate(data_id = as.numeric(data_id))

  inst_ages <- instruments_new |>
    select(language, form, age_min, age_max)
  admins_merged <- new_admins |>
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
    )

  na_cascade <- cascade_na_age_admin_drops(
    admins_merged = admins_merged,
    new_lexp = new_lexp,
    children_new = children_new,
    alloc_adm_map = alloc_adm$map,
    alloc_ch_map = alloc_ch$map,
    health_conditions = hc_new
  )
  admins_merged <- na_cascade$admins_merged
  new_lexp <- na_cascade$new_lexp
  children_new <- na_cascade$children_new
  alloc_adm$map <- na_cascade$alloc_adm_map
  alloc_ch$map <- na_cascade$alloc_ch_map
  hc_new <- na_cascade$health_conditions

  admins_new <- admins_merged |>
    select(
      data_id, child_id, dataset_name, language, form, age, date_of_test,
      comprehension, production, is_norming, in_age_range
    )

  lexp_new <- new_lexp |>
    left_join(
      alloc_adm$map,
      by = join_by(
        dataset_name,
        dataset_origin_name,
        study_internal_id,
        admin_row,
        instrument_language == language,
        instrument_form == form
      )
    ) |>
    select(data_id, language, exposure_percentage, age_of_first_exposure) |>
    mutate(
      exposure_percentage = as.integer(exposure_percentage),
      age_of_first_exposure = as.integer(age_of_first_exposure)
    ) |>
    filter(!data_id %in% na_cascade$drop_data_ids)

  if (mode == "complete") {
    instruments <- instruments_new
    datasets <- datasets_new
    children <- children_new
    administrations <- admins_new
    items <- new_items
    language_exposures <- lexp_new
    registry <- list(
      instruments = instruments |> select(language, form, instrument_id),
      datasets = datasets |>
        select(dataset_name, dataset_origin_name, language, form, dataset_id),
      children = alloc_ch$map |> select(all_of(c(CHILD_KEY_COLS, "child_id"))),
      administrations = alloc_adm$map |>
        select(all_of(c(ADMIN_NATURAL_KEY_COLS, "data_id")))
    )
  } else {
    instruments <- bind_rows(
      existing$instruments,
      instruments_new |> anti_join(existing$instruments, by = c("language", "form"))
    ) |>
      distinct(language, form, .keep_all = TRUE)
    datasets <- bind_rows(existing$datasets, datasets_new) |>
      distinct(dataset_name, dataset_origin_name, language, form, .keep_all = TRUE)
    children <- bind_rows(existing$children, children_new) |>
      distinct(child_id, .keep_all = TRUE)
    administrations <- bind_rows(existing$administrations, admins_new) |>
      distinct(data_id, .keep_all = TRUE)
    items <- bind_rows(existing$items, new_items) |>
      distinct(language, form, item_id, .keep_all = TRUE)
    language_exposures <- bind_rows(existing$language_exposures, lexp_new)
    registry$instruments <- instruments |> select(language, form, instrument_id)
    registry$datasets <- datasets |>
      select(dataset_name, dataset_origin_name, language, form, dataset_id)
    registry$children <- bind_rows(
      ch_reg,
      alloc_ch$map |> select(all_of(c(CHILD_KEY_COLS, "child_id")))
    ) |>
      distinct(across(all_of(CHILD_KEY_COLS)), .keep_all = TRUE)
    registry$administrations <- bind_rows(adm_reg, alloc_adm$map) |>
      distinct(across(all_of(ADMIN_NATURAL_KEY_COLS)), .keep_all = TRUE)
  }

  health_conditions <- if (mode == "complete") {
    hc_new
  } else {
    bind_rows(existing$health_conditions, hc_new) |>
      distinct(child_id, health_condition_name)
  } |>
    semi_join(children, by = "child_id")

  item_response_export <- NULL
  if (!is.null(item_response_export_dir) && !is.null(item_response_sources)) {
    message(
      "Exporting item_responses from ", nrow(item_response_sources),
      " source file(s) (streaming)..."
    )
    item_response_export <- export_item_responses_with_ids(
      sources = item_response_sources,
      keep_keys = datasets_new |> select(dataset_name, language, form),
      adm_map = alloc_adm$map,
      inst_lookup = instruments_new |> select(language, form, instrument_id),
      export_dir = item_response_export_dir,
      replace = mode == "complete",
      aliases = aliases,
      exclude_data_ids = na_cascade$drop_data_ids
    )
    message(
      "Exported ", item_response_export$n_rows, " item response row(s) to ",
      item_response_export_dir
    )
  } else if (!is.null(new_parts$item_responses) && nrow(new_parts$item_responses) > 0L) {
    stop(
      "new_parts$item_responses is in memory; pass item_response_sources instead ",
      "(ingest with out_harm or --from-harmonized)"
    )
  }

  new_parts$item_responses <- NULL
  gc(verbose = FALSE)

  upload_deltas <- if (mode == "append") {
    list(
      instruments = instruments_new |>
        anti_join(existing_instrument_keys, by = c("language", "form")),
      datasets = datasets_new,
      children = children_new,
      administrations = admins_new,
      items = new_items |>
        anti_join(existing_item_keys, by = c("language", "form", "item_id")),
      language_exposures = lexp_new,
      health_conditions = hc_new |>
        anti_join(existing$health_conditions, by = c("child_id", "health_condition_name"))
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
    item_response_export = item_response_export,
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
    tables = c("instruments", "datasets", "children", "administrations",
               "items", "language_exposures", "health_conditions", 
               "item_responses"),
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
    for (nm in intersect(names(merged$upload_deltas), tables)) {
      df <- merged$upload_deltas[[nm]]
      if (nrow(df) == 0) next
      path <- file.path(export_dir, paste0(nm, "_delta.csv"))
      write_csv(df, path, na = "")
      message("appending ", nm, " (", nrow(df), " rows)")
      upload_csv(nm, path, "append")
    }
    if ("item_responses" %in% tables) {
      resp_dir <- file.path(export_dir, "item_responses")
      resp_files <- list.files(resp_dir, full.names = TRUE)
      for (f in resp_files) {
        message("appending item_responses: ", basename(f))
        upload_csv("item_responses", f, "append")
      }
    }
  } else {
    for (nm in intersect(names(merged$tables), tables)) {
      path <- file.path(export_dir, paste0(nm, ".csv"))
      message("replacing ", nm)
      upload_csv(nm, path, "replace")
    }
    if ("item_responses" %in% tables) {
      resp_dir <- file.path(export_dir, "item_responses")
      resp_files <- list.files(resp_dir, full.names = TRUE)
      for (f in resp_files) {
        message("replacing item_responses: ", basename(f))
        upload_csv("item_responses", f, "replace")
      }
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
