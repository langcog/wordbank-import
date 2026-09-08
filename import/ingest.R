# Triplet ingest: raw *_data / *_fields / *_values → harmonized tables (natural keys).

suppressPackageStartupMessages({
  library(tidyverse)
  library(glue)
})

if (!exists("parse_languages", mode = "function")) {
  stop("Source import/helpers.R before import/ingest.R")
}
if (!exists("validate_imports", mode = "function")) {
  stop("Source import/validate.R before import/ingest.R")
}
if (!exists("normalize_demog_wide", mode = "function")) {
  stop("Source import/demographics.R before import/ingest.R")
}

#' Split manifest into one tibble per Redivis dataset (may contain >1 triplet).
split_manifest <- function(manifest) {
  manifest |>
    group_by(across(all_of(DATASET_GROUP_COLS))) |>
    group_split()
}

harmonized_out_dir <- function(meta_rows, out_harm) {
  file.path(
    out_harm,
    meta_rows$instrument_dir[[1]],
    slug_sanitize(meta_rows$dataset_origin_name[[1]])
  )
}

harmonized_group_label <- function(meta_rows) {
  paste(
    meta_rows$dataset_name[[1]], meta_rows$language[[1]], meta_rows$form[[1]],
    sep = " / "
  )
}

#' TRUE when all harmonized tables and triplet_ranges exist on disk.
harmonized_is_complete <- function(out_dir) {
  if (!dir.exists(out_dir)) return(FALSE)
  tables_ok <- all(
    file.exists(file.path(out_dir, paste0(HARMONIZED_DATASET_TABLES, ".csv")))
  )
  ranges_ok <- file.exists(file.path(out_dir, "triplet_ranges.csv"))
  tables_ok && ranges_ok
}

combine_ingest_parts <- function(parts, meta) {
  meta <- meta[1, ]
  list(
    dataset = parts[[1]]$dataset |>
      mutate(
        manifest_row = min(map_int(parts, ~ .x$meta$manifest_row[[1]])),
        n_admins = sum(map_int(parts, ~ nrow(.x$administrations))),
        file_location = str_c(
          unique(map_chr(parts, ~ .x$dataset$file_location[[1]])),
          collapse = ";"
        )
      ),
    children = bind_rows(map(parts, "children")) |>
      distinct(across(all_of(CHILD_KEY_COLS)), .keep_all = TRUE),
    administrations = bind_rows(map(parts, "administrations")),
    language_exposures = bind_rows(map(parts, "language_exposures")),
    item_responses = bind_rows(map(parts, "item_responses")),
    items = parts[[1]]$items
  )
}

combine_ingest_results <- function(results) {
  list(
    new_parts = list(
      items = dedupe_items(map(results, "items") |> list_rbind()),
      dataset = map(results, "dataset") |> list_rbind(),
      children = map(results, "children") |> list_rbind(),
      administrations = map(results, "administrations") |> list_rbind(),
      language_exposures = map(results, "language_exposures") |> list_rbind()
    ),
    triplet_ranges = map(results, "triplet_ranges") |> list_rbind(),
    results = results
  )
}

#' One row per on-disk harmonized `item_responses.csv` (avoids loading into memory).
item_response_sources_from_groups <- function(groups, out_harm) {
  map(groups, \(rows) {
    tibble(
      path = file.path(harmonized_out_dir(rows, out_harm), "item_responses.csv"),
      dataset_name = rows$dataset_name[[1]],
      language = rows$language[[1]],
      form = rows$form[[1]]
    )
  }) |> list_rbind()
}

#' Spill in-memory item responses to temp CSVs when harmonized output is not kept.
item_response_sources_from_results <- function(results, spill_dir) {
  dir.create(spill_dir, recursive = TRUE, showWarnings = FALSE)
  map(results, \(res) {
    rows <- as_tibble(res$meta_rows)
    slug <- slug_sanitize(rows$dataset_origin_name[[1]])
    path <- file.path(
      spill_dir,
      paste0(slug, "_", rows$language[[1]], "_", rows$form[[1]], ".csv")
    )
    write_csv(res$item_responses, path, na = "")
    tibble(
      path = path,
      dataset_name = rows$dataset_name[[1]],
      language = rows$language[[1]],
      form = rows$form[[1]]
    )
  }) |> list_rbind()
}

assert_item_responses <- function(result, label) {
  if (nrow(result$item_responses) == 0L) {
    stop("item_responses empty after ingest: ", label)
  }
  invisible(result)
}

instrument_path <- function(raw_dir, meta) {
  file.path(raw_dir, paste0(instrument_slug(meta$language, meta$form), ".csv"))
}

#' Ingest one *_data / *_fields / *_values triplet.
ingest_triplet <- function(
    raw_root,
    meta,
    categories = NULL,
    admin_row_offset = 0L,
    keep_intermediates = FALSE
) {
  raw_dir <- file.path(raw_root, meta$instrument_dir)
  paths <- triplet_paths(meta, raw_root)
  data_file <- paths$data
  fields_file <- paths$fields
  values_file <- paths$values
  instrument_file <- instrument_path(raw_dir, meta)

  stopifnot(
    file.exists(data_file),
    file.exists(fields_file),
    file.exists(values_file),
    file.exists(instrument_file)
  )

  df_data <- read_raw_data_csv(data_file)
  df_fields <- read_csv(fields_file, show_col_types = FALSE,
                        name_repair = "unique_quiet") |>
    mutate(across(everything(), as.character))
  df_values <- read_csv(values_file, show_col_types = FALSE,
                        name_repair = "unique_quiet") |>
    distinct() |>
    mutate(across(c(type, value, data_value), as.character))

  if ("data_id" %in% names(df_data)) {
    df_data <- df_data |> rename(data_id_ = data_id)
    df_fields <- df_fields |>
      mutate(column = if_else(column == "data_id", "data_id_", column))
  }

  df_long <- df_data |>
    mutate(
      admin_row = row_number() + admin_row_offset,
      across(-admin_row, as.character)
    ) |>
    pivot_longer(-admin_row, names_to = "column", values_to = "data_value") |>
    left_join(df_fields, by = "column") |>
    left_join(df_values, by = c("type", "data_value")) |>
    filter(!is.na(field), field != "") |>
    mutate(
      value = if_else(
        coalesce(group, "") == "item" | field %in% c("condition", "race", "ethnicity", "mom_ed", "sex"),
        value,
        coalesce(value, data_value)
      )
    )

  demog_wide <- df_long |>
    filter(coalesce(group, "") != "item", !str_starts(field, "item_")) |>
    select(admin_row, field, value) |>
    distinct() |>
    group_by(admin_row, field) |>
    summarise(
      value = {
        v <- unique(value[!is.na(value) & value != ""])
        if (length(v) == 0) NA_character_ else str_c(v, collapse = "\n")
      },
      .groups = "drop"
    ) |>
    pivot_wider(names_from = field, values_from = value)

  for (col in c(
    "study_id", "data_age", "sex", "mom_ed", "birth_order",
    "ethnicity", "race", "date_of_test", "date_of_birth", "is_norming", "languages"
  )) {
    if (!col %in% names(demog_wide)) demog_wide[[col]] <- NA_character_
  }

  demog_wide <- normalize_demog_wide(demog_wide)
  if ("study_id" %in% names(demog_wide)) {
    demog_wide <- demog_wide |> rename(study_internal_id = study_id)
  } else {
    demog_wide$study_internal_id <- NA_character_
  }

  language_exposures_nat <- demog_wide |>
    select(admin_row, study_internal_id, languages) |>
    mutate(parsed = map(languages, parse_languages)) |>
    select(study_internal_id, admin_row, parsed) |>
    unnest(parsed) |>
    mutate(
      dataset_name = meta$dataset_name,
      dataset_origin_name = meta$dataset_origin_name,
      instrument_language = meta$language,
      instrument_form = meta$form
    )

  instrument <- read_instrument(instrument_file)
  if (!is.null(categories) && "category" %in% names(instrument)) {
    instrument <- instrument |> left_join(categories, by = "category")
  }
  if (!"lexical_category" %in% names(instrument)) {
    instrument$lexical_category <- NA_character_
  }
  if (!"complexity_category" %in% names(instrument)) {
    instrument$complexity_category <- NA_character_
  }

  item_long <- df_long |>
    filter(coalesce(group, "") == "item" | str_starts(field, "item_")) |>
    select(admin_row, item_id = field, value, item_kind_field = type) |>
    mutate(value = na_if(value, ""))

  pu <- code_produces_understands(
    item_long$value,
    coalesce(
      instrument$item_kind[match(item_long$item_id, instrument$item_id)],
      item_long$item_kind_field
    ),
    meta$form_type
  )

  item_responses_nat <- item_long |>
    left_join(demog_wide |> select(admin_row, study_internal_id), by = "admin_row") |>
    left_join(
      instrument |> select(
        item_id, item_kind, category, item_definition,
        english_gloss, uni_lemma, lexical_category, complexity_category
      ),
      by = "item_id"
    ) |>
    mutate(
      item_kind = coalesce(item_kind, item_kind_field),
      dataset_name = meta$dataset_name,
      dataset_origin_name = meta$dataset_origin_name,
      language = meta$language,
      form = meta$form,
      form_type = meta$form_type,
      produces = pu$produces,
      understands = pu$understands
    ) |>
    select(
      study_internal_id, admin_row, dataset_name, dataset_origin_name,
      language, form, form_type, item_id, item_kind,
      value, produces, understands
    )

  if (nrow(item_responses_nat) == 0L) {
    stop(
      "item_responses empty for triplet ",
      meta$data_file, " (", meta$dataset_origin_name, ")"
    )
  }

  vocab <- item_responses_nat |>
    filter(item_kind == "word") |>
    group_by(admin_row) |>
    summarise(
      production = sum(produces, na.rm = TRUE),
      comprehension = if (identical(as.character(meta$form_type), "WG")) {
        sum(understands, na.rm = TRUE)
      } else {
        NA_real_
      },
      .groups = "drop"
    )

  administrations_nat <- demog_wide |>
    left_join(vocab, by = "admin_row") |>
    mutate(
      study_internal_id = as.character(study_internal_id),
      age = as.integer(data_age),
      sex = as.character(sex),
      caregiver_education = as.character(mom_ed),
      birth_order = as.integer(birth_order),
      ethnicity = as.character(ethnicity),
      race = as.character(race),
      date_of_test = as.character(date_of_test),
      date_of_birth = as.character(date_of_birth),
      is_norming = parse_is_norming(is_norming),
      dataset_name = meta$dataset_name,
      dataset_origin_name = meta$dataset_origin_name,
      language = meta$language,
      form = meta$form,
      form_type = meta$form_type,
      production = as.integer(coalesce(production, 0L)),
      comprehension = if_else(
        form_type == "WG",
        as.integer(coalesce(comprehension, 0L)),
        NA_integer_
      ),
      birth_weight = NA_real_,
      born_early_or_late = NA_character_,
      gestational_age = NA_integer_,
      zygosity = NA_character_,
      manifest_row = as.integer(meta$manifest_row[[1]])
    )

  validate_study_internal_ids(
    administrations_nat,
    dataset_context = meta |>
      select(dataset_name, dataset_origin_name, language, form, data_file),
    stop_on_fail = TRUE
  )

  children_nat <- administrations_nat |>
    select(
      study_internal_id, dataset_origin_name,
      sex, race, ethnicity, birth_order, caregiver_education,
      date_of_birth, birth_weight, born_early_or_late, gestational_age, zygosity
    ) |>
    distinct(across(all_of(CHILD_KEY_COLS)), .keep_all = TRUE)

  items_tbl <- instrument |>
    mutate(
      language = meta$language,
      form = meta$form,
      form_type = meta$form_type
    ) |>
    select(
      item_id, language, form, form_type, item_kind, category,
      item_definition, english_gloss, uni_lemma, lexical_category,
      complexity_category
    )

  out <- list(
    meta = meta,
    dataset = tibble(
      dataset_name = meta$dataset_name,
      dataset_origin_name = meta$dataset_origin_name,
      contributor = as.character(if (is.na(meta$contributor)) NA else meta$contributor),
      citation = as.character(if (is.na(meta$citation)) NA else meta$citation),
      license = as.character(if (is.na(meta$license)) "CC-BY" else meta$license),
      longitudinal = as.logical(if (is.na(meta$longitudinal)) FALSE else meta$longitudinal),
      source = NA_character_,
      date_format = NA_character_,
      file_location = as.character(raw_dir),
      norming = NA_character_,
      splitcol = NA_character_,
      language = meta$language,
      form = meta$form,
      form_type = meta$form_type,
      n_admins = as.numeric(nrow(administrations_nat))
    ),
    children = children_nat,
    administrations = administrations_nat |>
      select(
        study_internal_id, admin_row, manifest_row, date_of_test, age,
        comprehension, production,
        is_norming, dataset_name, dataset_origin_name, language, form, form_type,
        birth_order, caregiver_education, ethnicity, race, sex,
        birth_weight, born_early_or_late, gestational_age, zygosity
      ),
    language_exposures = language_exposures_nat,
    item_responses = item_responses_nat,
    items = items_tbl
  )

  if (isTRUE(keep_intermediates)) {
    out$intermediates <- list(
      paths = c(paths, list(instrument = instrument_file)),
      raw = list(data = df_data, fields = df_fields, values = df_values),
      long = df_long,
      demog_wide = demog_wide,
      item_long = item_long,
      instrument = instrument
    )
  }

  out
}

#' Ingest one or more triplets that belong to the same Redivis dataset row.
#'
#' @param skip_processed If TRUE and `out_harm` is set, load groups whose
#'   harmonized output already exists instead of re-ingesting raw CSVs.
#' @param out_harm Root harmonized output directory (required when
#'   `skip_processed = TRUE`).
ingest_dataset_group <- function(
    meta_rows,
    raw_root,
    categories = NULL,
    validate = TRUE,
    stop_on_fail = TRUE,
    keep_intermediates = FALSE,
    skip_processed = FALSE,
    out_harm = NULL
) {
  meta_rows <- as_tibble(meta_rows)
  if (!"manifest_row" %in% names(meta_rows)) {
    stop("meta_rows must include manifest_row (call attach_manifest_row() on manifest first)")
  }
  label <- harmonized_group_label(meta_rows)

  if (skip_processed) {
    if (is.null(out_harm)) {
      stop("out_harm is required when skip_processed = TRUE")
    }
    out_dir <- harmonized_out_dir(meta_rows, out_harm)
    if (harmonized_is_complete(out_dir)) {
      message("Skipping processed: ", label)
      res <- load_harmonized_group(meta_rows, out_harm)
      res$meta_rows <- meta_rows
      res$skipped <- TRUE
      return(res)
    }
  }

  message(
    "Ingesting ", label,
    if (nrow(meta_rows) > 1) paste0(" (", nrow(meta_rows), " triplets)") else ""
  )

  offset <- 0L
  ranges <- list()
  triplet_parts <- list()
  parts <- map(seq_len(nrow(meta_rows)), \(i) {
    meta <- meta_rows[i, ]
    part <- ingest_triplet(
      raw_root,
      meta,
      categories,
      admin_row_offset = offset,
      keep_intermediates = keep_intermediates
    )
    triplet_parts[[meta$data_file[[1]]]] <<- part
    n_admins <- nrow(part$administrations)
    ranges[[i]] <<- tibble(
      dataset_name = meta$dataset_name,
      dataset_origin_name = meta$dataset_origin_name,
      language = meta$language,
      form = meta$form,
      data_file = meta$data_file,
      instrument_dir = meta$instrument_dir,
      admin_row_min = offset + 1L,
      admin_row_max = offset + n_admins
    )
    offset <<- offset + n_admins
    part
  })

  out <- combine_ingest_parts(parts, meta_rows)
  out$triplet_ranges <- bind_rows(ranges)
  out$meta_rows <- meta_rows
  out$triplets <- triplet_parts
  assert_item_responses(out, label)

  canon <- tryCatch(
    canonicalize_administration_demographics(
      out$administrations,
      meta_rows |> select(dataset_name, dataset_origin_name, language, form),
      stop_on_fail = stop_on_fail
    ),
    demographic_conflict_error = function(e) {
      if (nrow(e$mismatches) > 0L) write_demog_mismatches(e$mismatches)
      stop(e)
    }
  )
  out$administrations <- canon$administrations
  out$demog_mismatches <- canon$mismatches

  canon_demo <- out$administrations |>
    distinct(dataset_origin_name, study_internal_id, across(all_of(CHILD_DEMO_COLS)))
  child_extra_cols <- setdiff(
    names(out$children),
    c("dataset_origin_name", "study_internal_id", CHILD_DEMO_COLS)
  )
  out$children <- out$children |>
    distinct(dataset_origin_name, study_internal_id, .keep_all = TRUE) |>
    select(dataset_origin_name, study_internal_id, all_of(child_extra_cols)) |>
    left_join(canon_demo, by = c("dataset_origin_name", "study_internal_id")) |>
    distinct(across(all_of(CHILD_KEY_COLS)), .keep_all = TRUE)

  if (validate) {
    out$validation <- validate_imports(
      meta_rows,
      out,
      raw_root,
      out$triplet_ranges,
      stop_on_fail = stop_on_fail
    )
    out$demographic_validation <- validate_demographics(
      out$administrations,
      out$children,
      stop_on_fail = stop_on_fail
    )
  }

  out
}

write_harmonized <- function(result, out_dir) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  iwalk(result[HARMONIZED_DATASET_TABLES], \(df, nm) {
    write_csv(df, file.path(out_dir, paste0(nm, ".csv")), na = "")
  })
  if (!is.null(result$triplet_ranges)) {
    write_csv(
      result$triplet_ranges,
      file.path(out_dir, "triplet_ranges.csv"),
      na = ""
    )
  }
  invisible(out_dir)
}

write_harmonized_items <- function(items, language, form, out_harm) {
  out_dir <- file.path(out_harm, "_instruments")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  path <- harmonized_items_path(language, form, out_harm)
  write_csv(dedupe_items(items), path, na = "")
  invisible(path)
}

write_harmonized_instrument_table <- function(manifest, items, out_harm) {
  out_dir <- file.path(out_harm, "_instruments")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  path <- harmonized_instrument_path(out_harm)
  write_csv(build_instrument_table(manifest, items), path, na = "")
  invisible(path)
}

read_harmonized_items <- function(language, form, out_harm) {
  path <- harmonized_items_path(language, form, out_harm)
  if (!file.exists(path)) {
    stop("Missing harmonized items for ", language, " ", form, ": ", path)
  }
  cast_harmonized_table(read_csv(path, show_col_types = FALSE), "items")
}

read_harmonized <- function(
    out_dir,
    language = NULL,
    form = NULL,
    out_harm = NULL,
    load_item_responses = TRUE
) {
  stopifnot(dir.exists(out_dir))
  table_names <- if (isTRUE(load_item_responses)) {
    HARMONIZED_DATASET_TABLES
  } else {
    setdiff(HARMONIZED_DATASET_TABLES, "item_responses")
  }
  tables <- map(set_names(table_names), \(nm) {
    path <- file.path(out_dir, paste0(nm, ".csv"))
    if (!file.exists(path)) {
      stop("Missing harmonized table: ", path)
    }
    cast_harmonized_table(read_csv(path, show_col_types = FALSE), nm)
  })
  if (!isTRUE(load_item_responses)) {
    tables$item_responses <- NULL
  }
  range_path <- file.path(out_dir, "triplet_ranges.csv")
  tables$triplet_ranges <- if (file.exists(range_path)) {
    cast_harmonized_table(read_csv(range_path, show_col_types = FALSE), "triplet_ranges")
  } else {
    tibble(
      dataset_name = character(),
      dataset_origin_name = character(),
      language = character(),
      form = character(),
      data_file = character(),
      instrument_dir = character(),
      admin_row_min = integer(),
      admin_row_max = integer()
    )
  }
  if (!is.null(language) && !is.null(form) && !is.null(out_harm)) {
    tables$items <- read_harmonized_items(language, form, out_harm)
  }
  tables
}

#' Load one harmonized dataset group from disk (output of `write_harmonized()`).
load_harmonized_group <- function(meta_rows, out_harm) {
  meta_rows <- as_tibble(meta_rows)
  stopifnot("manifest_row" %in% names(meta_rows))
  out_dir <- harmonized_out_dir(meta_rows, out_harm)
  tables <- read_harmonized(
    out_dir,
    language = meta_rows$language[[1]],
    form = meta_rows$form[[1]],
    out_harm = out_harm,
    load_item_responses = FALSE
  )
  stopifnot(
    "manifest_row" %in% names(tables$administrations),
    "manifest_row" %in% names(tables$dataset)
  )
  tables
}

#' Load all harmonized dataset groups for a manifest without re-ingesting raw CSVs.
load_ingested_manifest <- function(manifest, out_harm) {
  stopifnot("manifest_row" %in% names(manifest))
  groups <- split_manifest(manifest)
  results <- map(groups, \(rows) load_harmonized_group(rows, out_harm))
  out <- combine_ingest_results(results)
  out$item_response_sources <- item_response_sources_from_groups(groups, out_harm)
  out$new_parts$items <- dedupe_items(out$new_parts$items)
  out$new_parts$instrument <- build_instrument_table(manifest, out$new_parts$items)
  c(out, list(manifest = manifest))
}

#' Ingest every dataset group in the manifest; return combined parts + ranges.
#' Each group is validated against its raw triplets immediately after ingest.
#'
#' @param skip_processed Skip groups that already have complete harmonized output
#'   under `out_harm` (requires `out_harm`).
ingest_all_manifest <- function(
    manifest,
    raw_root,
    categories,
    out_harm = NULL,
    validate = TRUE,
    stop_on_fail = TRUE,
    keep_intermediates = FALSE,
    skip_processed = FALSE
) {
  if (skip_processed && is.null(out_harm)) {
    stop("out_harm is required when skip_processed = TRUE")
  }

  stopifnot("manifest_row" %in% names(manifest))
  groups <- split_manifest(manifest)
  results <- map(groups, \(rows) {
    res <- ingest_dataset_group(
      rows,
      raw_root,
      categories,
      validate = validate,
      stop_on_fail = stop_on_fail,
      keep_intermediates = keep_intermediates,
      skip_processed = skip_processed,
      out_harm = out_harm
    )
    if (!is.null(out_harm) && !isTRUE(res$skipped)) {
      write_harmonized(res, harmonized_out_dir(rows, out_harm))
      write_harmonized_items(
        res$items,
        rows$language[[1]],
        rows$form[[1]],
        out_harm
      )
    }
    res
  })

  out <- combine_ingest_results(results)
  if (!is.null(out_harm)) {
    out$item_response_sources <- item_response_sources_from_groups(groups, out_harm)
  } else {
    spill_dir <- file.path(tempdir(), "wordbank_item_responses")
    out$item_response_sources <- item_response_sources_from_results(results, spill_dir)
  }
  results <- map(results, \(res) {
    res$item_responses <- NULL
    res$triplets <- NULL
    res$intermediates <- NULL
    res
  })
  if (!is.null(out_harm)) {
    write_harmonized_instrument_table(manifest, out$new_parts$items, out_harm)
  }
  out$new_parts$items <- dedupe_items(out$new_parts$items)
  out$new_parts$instrument <- build_instrument_table(manifest, out$new_parts$items)
  out$manifest <- manifest
  out$demog_mismatches <- map(results, \(r) {
    if (is.null(r$demog_mismatches)) empty_demog_mismatches() else r$demog_mismatches
  }) |> list_rbind()
  if (nrow(out$demog_mismatches) > 0L) {
    path <- write_demog_mismatches(out$demog_mismatches)
    message(
      "Wrote ", nrow(out$demog_mismatches), " demographic mismatch row(s) to ", path
    )
  }
  out$n_skipped <- sum(map_lgl(results, ~ isTRUE(.x$skipped)))
  out$n_ingested <- length(results) - out$n_skipped
  if (out$n_skipped > 0L) {
    message(
      "Skipped ", out$n_skipped, " processed group(s); ingested ",
      out$n_ingested, " new group(s)"
    )
  }
  out
}

#' Load harmonized administration rows for every manifest dataset group.
load_harmonized_administrations <- function(manifest, out_harm) {
  stopifnot("manifest_row" %in% names(manifest))
  groups <- split_manifest(manifest)
  map(groups, \(rows) {
    path <- file.path(harmonized_out_dir(rows, out_harm), "administrations.csv")
    if (!file.exists(path)) {
      stop("Missing harmonized administrations: ", path)
    }
    cast_harmonized_table(
      read_csv(path, show_col_types = FALSE),
      "administrations"
    )
  }) |> list_rbind()
}
