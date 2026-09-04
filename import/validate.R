# Validation asserts for merged Wordbank Redivis tables and raw triplet fidelity
# (mirrors wordbank_import_manual/testing.R, comparing harmonized ingest to raw CSVs).
#
# Requires import/helpers.R to be sourced first (for DATASET_GROUP_COLS).

suppressPackageStartupMessages({
  library(tidyverse)
  library(assertthat)
  library(glue)
})

if (!exists("DATASET_GROUP_COLS", inherits = FALSE)) {
  stop("Source import/helpers.R before import/validate.R")
}

#' Raw CSV column holding child age (from *_fields.csv data_age row).
raw_age_column <- function(fields_path) {
  fields <- read_csv(fields_path, show_col_types = FALSE, name_repair = "unique_quiet")
  col <- fields |>
    filter(field == "data_age") |>
    pull(column)
  if (length(col) != 1L || is.na(col) || col == "") {
    stop("Could not find data_age column in ", fields_path)
  }
  col[[1]]
}

default_measure <- function(form_type) {
  if (identical(as.character(form_type), "WG")) "understands" else "produces"
}

triplet_label <- function(meta) {
  glue(
    "{meta$language} {meta$form} / {meta$dataset_name} ({meta$dataset_origin_name}; {meta$data_file})"
  )
}

#' Extract failing rows from one `validate_triplet()` result.
validation_failures <- function(res) {
  out <- list(label = res$label, paths = res$paths)
  if (isTRUE(res$admin$row_diff)) {
    out$row_count <- tibble(
      harm_n = res$admin$n_harm,
      raw_n = res$admin$n_raw
    )
  }
  if (res$admin$n_diff > 0) {
    out$age_bins <- res$admin$combined |> filter(diff)
  }
  if (!is.null(res$inst) && res$inst$n_diff > 0) {
    out$items <- res$inst$combined |> filter(diff)
  }
  if (length(out) <= 2L) return(NULL)
  out
}

#' Summarize failures from `validate_imports()` output.
summarize_validation_failures <- function(results) {
  compact(map(results, validation_failures))
}

#' Subset harmonized tables to one manifest triplet (handles combined multi-triplet groups).
slice_triplet <- function(new_parts, meta, triplet_ranges = NULL, manifest = NULL) {
  admins <- new_parts$administrations |>
    filter(
      dataset_name == meta$dataset_name,
      dataset_origin_name == meta$dataset_origin_name,
      language == meta$language,
      form == meta$form
    )

  group_n <- if (!is.null(manifest)) {
    manifest |>
      filter(
        dataset_name == meta$dataset_name,
        dataset_origin_name == meta$dataset_origin_name,
        language == meta$language,
        form == meta$form
      ) |>
      nrow()
  } else {
    1L
  }

  range <- NULL
  if (!is.null(triplet_ranges) && nrow(triplet_ranges) > 0) {
    range <- triplet_ranges |>
      filter(
        dataset_name == meta$dataset_name,
        dataset_origin_name == meta$dataset_origin_name,
        language == meta$language,
        form == meta$form,
        data_file == meta$data_file
      )
  }

  if (group_n > 1L) {
    if (is.null(range) || nrow(range) != 1L) {
      stop(
        "triplet_ranges required for combined dataset ",
        meta$dataset_name, " / ", meta$dataset_origin_name,
        " (", meta$data_file, ")"
      )
    }
    admins <- admins |>
      filter(
        admin_row >= range$admin_row_min,
        admin_row <= range$admin_row_max
      )
  }

  responses <- new_parts$item_responses |>
    semi_join(
      admins |> distinct(dataset_origin_name, study_internal_id, admin_row),
      by = c("dataset_origin_name", "study_internal_id", "admin_row")
    )

  list(administrations = admins, item_responses = responses, triplet_range = range)
}

#' Compare harmonized administration ages to raw CSV age counts.
validate_admin_raw <- function(administrations, raw_loc, raw_age_var) {
  suppressMessages(raw_df <- read_raw_data_csv(raw_loc))

  n_raw <- nrow(raw_df)
  n_harm <- nrow(administrations)
  row_diff <- n_harm != n_raw

  harm_age <- table(administrations$age) |>
    as_tibble(.name_repair = ~c("age", "n"))

  raw_vals <- suppressWarnings(as.numeric(raw_df[[raw_age_var]]) |> na_if(0))
  raw_age <- table(floor(raw_vals)) |>
    as_tibble(.name_repair = ~c("age", "n"))

  combined <- harm_age |>
    rename(harm_n = n) |>
    full_join(raw_age |> rename(raw_n = n), by = "age") |>
    mutate(
      harm_n = coalesce(harm_n, 0L),
      raw_n = coalesce(raw_n, 0L),
      diff = harm_n != raw_n
    )

  list(
    combined = combined,
    n_diff = sum(combined$diff, na.rm = TRUE) + if (row_diff) 1L else 0L,
    n_raw = n_raw,
    n_harm = n_harm,
    row_diff = row_diff
  )
}

#' Compare harmonized word-item sums to raw CSV (testing.R validate_inst logic).
#' Applies *_values.csv mapping when present (same as ingest).
validate_inst_raw <- function(item_responses, measure, fields_loc, raw_loc, values_loc = NULL) {
  suppressMessages(fields <- read_csv(fields_loc, show_col_types = FALSE, name_repair = "unique_quiet"))
  suppressMessages(raw_df <- read_raw_data_csv(raw_loc))
  if (is.null(values_loc)) {
    values_loc <- str_replace(raw_loc, "_data\\.csv$", "_values.csv")
  }

  word_fields <- fields |> filter(type == "word")
  word_cols <- word_fields$column
  word_cols <- word_cols[!is.na(word_cols) & word_cols %in% names(raw_df)]

  harm_id <- item_responses |>
    filter(item_id %in% word_fields$field) |>
    group_by(item_id) |>
    summarise(harm_sum = sum(value == measure, na.rm = TRUE), .groups = "drop")

  if (length(word_cols) == 0L) {
    return(list(
      combined = harm_id |> mutate(raw_sum = NA_integer_, diff = NA),
      n_diff = NA_integer_,
      skipped = TRUE,
      reason = "no word columns in raw data matching fields"
    ))
  }

  raw_long <- raw_df |>
    mutate(across(all_of(word_cols), as.character)) |>
    pivot_longer(cols = all_of(word_cols), names_to = "column", values_to = "data_value") |>
    mutate(data_value = as.character(data_value)) |>
    left_join(
      fields |> select(column, item_id = field, type_ = type),
      by = "column"
    )

  if (file.exists(values_loc)) {
    suppressMessages(
      values <- read_csv(values_loc, show_col_types = FALSE) |>
        distinct() |>
        mutate(across(c(type, value, data_value), as.character))
    )
    raw_long <- raw_long |>
      left_join(values, by = join_by(type_ == type, data_value))
    raw_id <- raw_long |>
      filter(!is.na(item_id)) |>
      group_by(item_id) |>
      summarise(raw_sum = sum(value == measure, na.rm = TRUE), .groups = "drop")
  } else {
    raw_id <- raw_long |>
      filter(!is.na(item_id)) |>
      group_by(item_id) |>
      summarise(
        raw_sum = sum(data_value == "1" | data_value == measure, na.rm = TRUE),
        .groups = "drop"
      )
  }

  combined <- harm_id |>
    full_join(raw_id, by = "item_id") |>
    mutate(
      harm_sum = coalesce(harm_sum, 0L),
      raw_sum = coalesce(raw_sum, 0L),
      diff = harm_sum != raw_sum
    ) |>
    arrange(item_id)

  list(combined = combined, n_diff = sum(combined$diff, na.rm = TRUE))
}

#' Validate one manifest triplet against its raw CSV files.
validate_triplet <- function(meta, new_parts, raw_root, triplet_ranges = NULL, manifest = NULL) {
  paths <- triplet_paths(meta, raw_root)
  raw_loc <- paths$data
  fields_loc <- paths$fields
  raw_age_var <- raw_age_column(fields_loc)
  measure <- default_measure(meta$form_type)

  sliced <- slice_triplet(new_parts, meta, triplet_ranges, manifest)
  label <- triplet_label(meta)

  if (nrow(sliced$item_responses) == 0L) {
    stop("item_responses empty in ", label)
  }

  message(glue("...Validating {meta$data_file}"))

  admin_res <- validate_admin_raw(sliced$administrations, raw_loc, raw_age_var)
  inst_res <- validate_inst_raw(
    sliced$item_responses, measure, fields_loc, raw_loc
  )

  list(
    label = label,
    meta = meta,
    paths = paths,
    measure = measure,
    sliced = sliced,
    admin = admin_res,
    inst = inst_res
  )
}

validation_error <- function(msg, res) {
  structure(
    list(message = msg, call = sys.call(-1), validation = res),
    class = c("validation_error", "error", "condition")
  )
}

#' Run admin + instrument validation for each manifest row before export.
validate_imports <- function(
    manifest,
    new_parts,
    raw_root,
    triplet_ranges = NULL,
    stop_on_fail = TRUE
) {
  results <- map(seq_len(nrow(manifest)), \(i) {
    meta <- manifest[i, ]
    res <- validate_triplet(meta, new_parts, raw_root, triplet_ranges, manifest)

    if (isTRUE(res$admin$row_diff)) {
      msg <- glue(
        "Admin row count mismatch in {res$label}: ",
        "{res$admin$n_harm} harmonized vs {res$admin$n_raw} raw"
      )
      if (stop_on_fail) stop(validation_error(msg, res)) else warning(msg, call. = FALSE)
    }
    if (res$admin$n_diff > 0) {
      msg <- glue(
        "Admin age mismatch in {res$label}: ",
        "{res$admin$n_diff} age bin(s)"
      )
      if (stop_on_fail) stop(validation_error(msg, res)) else warning(msg, call. = FALSE)
    }
    if (!is.null(res$inst) && !isTRUE(res$inst$skipped) && res$inst$n_diff > 0) {
      msg <- glue(
        "Instrument item mismatch in {res$label}: ",
        "{res$inst$n_diff} item(s)"
      )
      if (stop_on_fail) stop(validation_error(msg, res)) else warning(msg, call. = FALSE)
    }

    res
  })

  # message("...Raw validation OK: ", length(results), " triplet(s)")
  results
}

validate_merged <- function(tables, new_item_responses = NULL) {
  instruments <- tables$instruments
  datasets <- tables$datasets
  children <- tables$children
  administrations <- tables$administrations
  items <- tables$items
  language_exposures <- tables$language_exposures
  health_conditions <- tables$health_conditions

  assert_that(n_distinct(instruments$instrument_id) == nrow(instruments))
  assert_that(n_distinct(datasets$dataset_id) == nrow(datasets))
  assert_that(n_distinct(children$child_id) == nrow(children))
  assert_that(n_distinct(administrations$data_id) == nrow(administrations))

  assert_that(
    nrow(administrations |> anti_join(children, by = "child_id")) == 0,
    msg = "administrations.child_id must resolve to children"
  )
  assert_that(
    nrow(health_conditions |> anti_join(children, by = "child_id")) == 0,
    msg = "health_conditions.child_id must resolve to children"
  )
  assert_that(
    nrow(language_exposures |> anti_join(administrations, by = "data_id")) == 0,
    msg = "language_exposures.data_id must resolve to administrations"
  )

  dup_admins <- administrations |>
    count(data_id) |>
    filter(n > 1)
  assert_that(
    nrow(dup_admins) == 0,
    msg = "duplicate data_id after merge"
  )

  na_admins <- administrations |>
    filter(if_any(c("data_id", "child_id", "age", "language", "form", "dataset_name"), is.na))
  assert_that(nrow(na_admins) == 0, msg = "required admin fields have NA")

  na_children <- children |> filter(if_any(c("child_id", "dataset_origin_name"), is.na))
  assert_that(nrow(na_children) == 0, msg = "required child fields have NA")

  if (!is.null(new_item_responses) && nrow(new_item_responses) > 0) {
    assert_that(
      nrow(new_item_responses |> anti_join(administrations, by = "data_id")) == 0,
      msg = "item_responses.data_id must resolve"
    )
    assert_that(
      nrow(new_item_responses |>
             anti_join(instruments, by = c("language", "form", "instrument_id"))) == 0,
      msg = "item_responses.instrument_id must resolve"
    )
    assert_that(
      nrow(new_item_responses |> filter(is.na(data_id) | is.na(item_id) | is.na(instrument_id))) == 0
    )
  }

  message("validation OK: ",
          nrow(administrations), " admins, ",
          nrow(children), " children, ",
          nrow(instruments), " instruments, ",
          nrow(datasets), " datasets")
  invisible()
}
