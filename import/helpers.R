# Shared helpers for Wordbank → Redivis import.

suppressPackageStartupMessages({
  library(tidyverse)
})

is_blank_cell <- function(x) {
  is.na(x) | str_trim(as.character(x)) == ""
}

#' Drop rows where every column is blank or NA.
drop_all_na_rows <- function(df) {
  if (nrow(df) == 0L || ncol(df) == 0L) return(df)
  df |>
    filter(!if_all(everything(), is_blank_cell))
}

#' Read a raw `*_data.csv`, treating blank strings as NA and dropping empty rows.
read_raw_data_csv <- function(path, ...) {
  read_csv(
    path,
    show_col_types = FALSE,
    na = c("", "NA", "N/A"),
    guess_max = 3000,
    name_repair = "unique_quiet",
    ...
  ) |>
    drop_all_na_rows()
}

DATASET_GROUP_COLS <- c(
  "dataset_name", "dataset_origin_name", "language", "form"
)

#' Child-group demographic fields from raw `*_fields.csv` (group = child).
CHILD_DEMO_COLS <- c(
  "sex", "race", "ethnicity", "birth_order", "caregiver_education"
)

#' Natural key for child_id assignment (matches Redivis).
CHILD_KEY_COLS <- c("dataset_origin_name", "study_internal_id", CHILD_DEMO_COLS)

ADMIN_NATURAL_KEY_COLS <- c(
  "dataset_name", "dataset_origin_name", "language", "form",
  "study_internal_id", "admin_row"
)

HARMONIZED_TABLES <- c(
  "instrument", "dataset", "children", "administrations",
  "language_exposures", "item_responses", "items"
)

HARMONIZED_DATASET_TABLES <- setdiff(
  HARMONIZED_TABLES,
  c("instrument", "items")
)

HARMONIZED_INSTRUMENT_TABLES <- c("instrument", "items")

#' Filename slug for one instrument (matches raw `[Lang_Form].csv` naming).
instrument_slug <- function(language, form) {
  lang <- replace_values(
    language,
    "American Sign Language" ~ "ASL",
    "British Sign Language" ~ "BSL"
  )
  frm <- replace_values(form, "Oxford CDI" ~ "Oxford")
  glue("[{str_replace_all(lang, '[ ()]', '')}_{str_replace_all(frm, '[ ()]', '')}]")
}

#' Path to harmonized items CSV for one language + form.
harmonized_items_path <- function(language, form, out_harm) {
  file.path(out_harm, "_instruments", paste0(instrument_slug(language, form), ".csv"))
}

#' Path to the shared harmonized instrument registry CSV.
harmonized_instrument_path <- function(out_harm) {
  file.path(out_harm, "_instruments", "instrument.csv")
}

#' One instrument row per distinct language + form in the manifest.
instrument_table_from_manifest <- function(manifest) {
  manifest |>
    distinct(language, form, form_type) |>
    mutate(unilemma_coverage = NA_real_) |>
    select(language, form, form_type, unilemma_coverage)
}

#' Collapse items to one row set per language + form.
dedupe_items <- function(items) {
  items |> distinct(language, form, item_id, .keep_all = TRUE)
}

#' Paths to raw triplet CSVs for one manifest row.
triplet_paths <- function(meta, raw_root) {
  raw_loc <- file.path(raw_root, meta$instrument_dir, meta$data_file)
  list(
    data = raw_loc,
    fields = str_replace(raw_loc, "_data\\.csv$", "_fields.csv"),
    values = str_replace(raw_loc, "_data\\.csv$", "_values.csv")
  )
}

#' Parse language-exposure cells of the form "Lang;pct;AoFE".
parse_languages <- function(x) {
  empty <- tibble(
    language = character(),
    exposure_percentage = numeric(),
    age_of_first_exposure = numeric()
  )
  if (is.null(x) || length(x) == 0 || all(is.na(x)) || all(x == "")) return(empty)

  lines <- x[!is.na(x) & x != ""] |>
    str_split("\\n") |>
    unlist() |>
    str_trim()
  lines <- lines[lines != ""]
  if (length(lines) == 0) return(empty)

  read_delim(
    I(str_c(lines, collapse = "\n")),
    delim = ";",
    col_names = c("language", "exposure_percentage", "age_of_first_exposure"),
    show_col_types = FALSE,
    col_types = cols(
      language = col_character(),
      exposure_percentage = col_double(),
      age_of_first_exposure = col_double()
    )
  ) |>
    filter(!is.na(language), language != "") |> 
    mutate(exposure_percentage = exposure_percentage |> 
             replace_when(exposure_percentage < 0 ~ NA,
                          exposure_percentage > 100 ~ NA))
}

#' Map produces / understands from a response value (wordbankr conventions).
#' Empty / NA word responses map to FALSE (not NA).
code_produces_understands <- function(value, item_kind, form_type) {
  is_word <- item_kind == "word"
  empty <- is.na(value) | value == ""
  tibble(
    produces = case_when(
      !is_word ~ NA,
      empty ~ FALSE,
      TRUE ~ value == "produces"
    ),
    understands = case_when(
      !(form_type == "WG" & is_word) ~ NA,
      empty ~ FALSE,
      TRUE ~ value %in% c("understands", "produces")
    )
  )
}

#' Read an instrument definition CSV.
read_instrument <- function(instrument_path) {
  df <- read_csv(instrument_path, show_col_types = FALSE)
  if ("itemID" %in% names(df)) df <- df |> rename(item_id = itemID)
  if ("definition" %in% names(df)) df <- df |> rename(item_definition = definition)
  if ("gloss" %in% names(df)) df <- df |> rename(english_gloss = gloss)
  if ("type" %in% names(df)) df <- df |> rename(item_kind = type)

  df |>
    mutate(across(any_of(c(
      "item_id", "item_kind", "category", "item_definition",
      "english_gloss", "uni_lemma", "complexity_category"
    )), as.character)) |>
    select(any_of(c(
      "item_id", "item_kind", "category", "item_definition",
      "english_gloss", "uni_lemma", "complexity_category"
    )))
}

parse_is_norming <- function(x) {
  case_when(
    is.na(x) | x == "" ~ FALSE,
    tolower(as.character(x)) %in% c("true", "t", "1", "yes") ~ TRUE,
    tolower(as.character(x)) %in% c("false", "f", "0", "no") ~ FALSE,
    TRUE ~ FALSE
  )
}

harm_chr <- function(x) {
  if (inherits(x, c("Date", "POSIXt"))) {
    x <- format(as.Date(x), "%Y-%m-%d")
  }
  out <- as.character(x)
  out[!is.na(out) & out == ""] <- NA_character_
  out
}

harm_int <- function(x) {
  suppressWarnings(as.integer(x))
}

harm_dbl <- function(x) {
  suppressWarnings(as.double(x))
}

harm_lgl <- function(x) {
  if (is.logical(x)) return(x)
  raw <- harm_chr(x)
  case_when(
    is.na(raw) ~ NA,
    tolower(raw) %in% c("true", "t", "1", "yes") ~ TRUE,
    tolower(raw) %in% c("false", "f", "0", "no") ~ FALSE,
    TRUE ~ NA
  )
}

cast_cols <- function(df, chr = character(), int = character(), dbl = character(), lgl = character()) {
  if (length(chr)) {
    df <- df |> mutate(across(any_of(chr), harm_chr))
  }
  if (length(int)) {
    df <- df |> mutate(across(any_of(int), harm_int))
  }
  if (length(dbl)) {
    df <- df |> mutate(across(any_of(dbl), harm_dbl))
  }
  if (length(lgl)) {
    df <- df |> mutate(across(any_of(lgl), harm_lgl))
  }
  df
}

#' Restore column types after reading harmonized CSVs (readr guesses vary by file).
cast_harmonized_table <- function(df, table) {
  switch(
    table,
    dataset = cast_cols(
      df,
      chr = c(
        "dataset_name", "dataset_origin_name", "contributor", "citation", "license",
        "source", "date_format", "file_location", "norming", "splitcol",
        "language", "form", "form_type"
      ),
      dbl = "n_admins",
      lgl = "longitudinal"
    ),
    children = cast_cols(
      df,
      chr = c(
        "study_internal_id", "dataset_origin_name", "caregiver_education", "ethnicity",
        "race", "sex", "date_of_birth", "born_early_or_late", "zygosity"
      ),
      int = c("birth_order", "gestational_age"),
      dbl = "birth_weight"
    ),
    administrations = cast_cols(
      df,
      chr = c(
        "study_internal_id", "date_of_test", "dataset_name", "dataset_origin_name",
        "language", "form", "form_type", "caregiver_education", "ethnicity",
        "race", "sex", "born_early_or_late", "zygosity"
      ),
      int = c("admin_row", "age", "comprehension", "production", "birth_order", "gestational_age"),
      dbl = "birth_weight",
      lgl = "is_norming"
    ),
    language_exposures = cast_cols(
      df,
      chr = c("study_internal_id", "language", "dataset_name", "dataset_origin_name"),
      int = "admin_row",
      dbl = c("exposure_percentage", "age_of_first_exposure")
    ),
    item_responses = cast_cols(
      df,
      chr = c(
        "study_internal_id", "dataset_name", "dataset_origin_name", "language", "form",
        "form_type", "item_id", "item_kind", "value"
      ),
      int = "admin_row",
      lgl = c("produces", "understands")
    ),
    items = cast_cols(
      df,
      chr = c(
        "item_id", "language", "form", "form_type", "item_kind", "category",
        "item_definition", "english_gloss", "uni_lemma", "lexical_category",
        "complexity_category"
      )
    ),
    triplet_ranges = cast_cols(
      df,
      chr = c(
        "dataset_name", "dataset_origin_name", "language", "form", "data_file",
        "instrument_dir"
      ),
      int = c("admin_row_min", "admin_row_max")
    ),
    instrument = cast_cols(
      df,
      chr = c("language", "form", "form_type"),
      dbl = "unilemma_coverage"
    ),
    df
  )
}
