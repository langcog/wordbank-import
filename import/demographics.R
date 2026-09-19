# Normalize and validate administration / child demographic fields.

suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
})

CAREGIVER_EDUCATION_ALLOWED <- c(
  "None", "Primary", "Some Secondary", "Secondary",
  "Some College", "College", "Some Graduate", "Graduate"
)
MOM_ED_ALLOWED <- CAREGIVER_EDUCATION_ALLOWED

RACE_ALLOWED <- c("Asian", "Black", "White", "Other/Mixed")
SEX_ALLOWED <- c("Male", "Female", "Other")
ETHNICITY_ALLOWED <- c("Hispanic", "Non-Hispanic")

BIRTH_ORDER_ALLOWED <- c(
  "First", "Second", "Third", "Fourth", "Fifth", "Sixth",
  "Seventh", "Eighth", "Ninth", "Tenth", "Eleventh", "Twelfth"
)

DATE_YMD_RE <- "^[0-9]{4}-[0-9]{2}-[0-9]{2}$"

normalize_mom_ed <- function(x) {
  raw <- str_squish(as.character(x))
  empty <- is.na(raw) | raw == ""
  out <- raw
  out[empty] <- NA_character_

  num <- suppressWarnings(as.integer(raw))
  ok_num <- !empty & !is.na(num) & num >= 1L & num <= 8L
  out[ok_num] <- CAREGIVER_EDUCATION_ALLOWED[num[ok_num]]

  key <- str_to_lower(raw)
  labels_lower <- str_to_lower(CAREGIVER_EDUCATION_ALLOWED)
  ok_label <- !empty & !ok_num & !is.na(match(key, labels_lower))
  out[ok_label] <- CAREGIVER_EDUCATION_ALLOWED[match(key[ok_label], labels_lower)]

  out
}

normalize_sex <- function(x) {
  raw <- str_trim(as.character(x))
  empty <- is.na(raw) | raw == ""
  key <- str_to_upper(raw)
  case_when(
    empty ~ NA_character_,
    key %in% c("M", "MALE") ~ "Male",
    key %in% c("F", "FEMALE") ~ "Female",
    key %in% c("O", "OTHER") ~ "Other",
    raw %in% SEX_ALLOWED ~ raw,
    TRUE ~ raw
  )
}

normalize_race <- function(x) {
  raw <- str_squish(as.character(x))
  empty <- is.na(raw) | raw == ""
  key <- str_to_upper(raw)
  other_mixed <- c("O", "OTHER", "MIXED", "OTHER/MIXED", "MIXED/OTHER")
  case_when(
    empty ~ NA_character_,
    key %in% c("A", "ASIAN") ~ "Asian",
    key %in% c("B", "BLACK") ~ "Black",
    key %in% c("W", "WHITE") ~ "White",
    key %in% other_mixed ~ "Other/Mixed",
    raw %in% RACE_ALLOWED ~ raw,
    TRUE ~ raw
  )
}

normalize_ethnicity <- function(x) {
  raw <- str_trim(as.character(x))
  empty <- is.na(raw) | raw == ""
  key <- str_to_upper(raw)
  case_when(
    empty ~ NA_character_,
    key %in% c("H", "HISPANIC") ~ "Hispanic",
    key %in% c("N", "NONHISPANIC", "NON-HISPANIC") ~ "Non-Hispanic",
    raw %in% ETHNICITY_ALLOWED ~ raw,
    TRUE ~ raw
  )
}

normalize_birth_order <- function(x) {
  raw <- str_trim(as.character(x))
  empty <- is.na(raw) | raw == ""
  out <- raw
  out[empty] <- NA_character_

  num <- suppressWarnings(as.integer(raw))
  n_labels <- length(BIRTH_ORDER_ALLOWED)
  ok_zero <- !empty & !is.na(num) & num == 0L
  out[ok_zero] <- NA_character_
  ok_num <- !empty & !is.na(num) & num >= 1L & num <= n_labels
  out[ok_num] <- BIRTH_ORDER_ALLOWED[num[ok_num]]

  labels_lower <- str_to_lower(BIRTH_ORDER_ALLOWED)
  key <- str_to_lower(raw)
  ok_label <- !empty & !ok_num & !is.na(match(key, labels_lower))
  out[ok_label] <- BIRTH_ORDER_ALLOWED[match(key[ok_label], labels_lower)]

  ok_raw <- !empty & raw %in% BIRTH_ORDER_ALLOWED
  out[ok_raw] <- raw[ok_raw]

  out
}

LB_TO_KG <- 0.45359237
GRAMS_TO_KG <- 1 / 1000

#' Parse birth weight strings / numbers to kg (Wordbank / Redivis convention).
birth_weight_to_kg <- function(x) {
  raw <- str_trim(as.character(x))
  empty <- is.na(raw) | raw == ""
  out <- rep(NA_real_, length(raw))
  if (all(empty)) return(out)

  grams <- str_match(raw, "(?i)^\\s*([0-9]+(?:\\.[0-9]+)?)\\s*g(?:ram)?s?\\s*$")[, 2]
  kg <- str_match(raw, "(?i)^\\s*([0-9]+(?:\\.[0-9]+)?)\\s*k(?:ilo)?g?\\s*$")[, 2]
  lbs_oz <- str_match(
    raw,
    "(?i)^\\s*([0-9]+(?:\\.[0-9]+)?)\\s*lbs?\\s*([0-9]+)?\\s*oz?\\s*$"
  )
  lbs_only <- str_match(raw, "(?i)^\\s*([0-9]+(?:\\.[0-9]+)?)\\s*lbs?\\s*$")[, 2]
  num <- suppressWarnings(as.numeric(raw))

  has_grams <- !empty & !is.na(grams)
  out[has_grams] <- as.numeric(grams[has_grams]) * GRAMS_TO_KG

  has_kg <- !empty & is.na(out) & !is.na(kg)
  out[has_kg] <- as.numeric(kg[has_kg])

  has_lbs_oz <- !empty & is.na(out) & !is.na(lbs_oz[, 2])
  if (any(has_lbs_oz)) {
    lbs <- as.numeric(lbs_oz[has_lbs_oz, 2])
    oz <- suppressWarnings(as.numeric(lbs_oz[has_lbs_oz, 3]))
    oz[is.na(oz)] <- 0
    out[has_lbs_oz] <- (lbs + oz / 16) * LB_TO_KG
  }

  has_lbs_only <- !empty & is.na(out) & !is.na(lbs_only)
  out[has_lbs_only] <- as.numeric(lbs_only[has_lbs_only]) * LB_TO_KG

  has_num <- !empty & is.na(out) & !is.na(num)
  if (any(has_num)) {
    n <- num[has_num]
    k <- rep(NA_real_, length(n))
    k[n >= 500] <- n[n >= 500] * GRAMS_TO_KG
    as_kg <- !is.na(n) & n > 0 & n < 500 & n <= 5.5
    k[as_kg] <- n[as_kg]
    as_lb <- !is.na(n) & n > 5.5 & n <= 20
    k[as_lb] <- n[as_lb] * LB_TO_KG
    out[has_num] <- k
  }

  out
}

normalize_born_early_or_late <- function(x) {
  raw <- str_squish(as.character(x))
  empty <- is.na(raw) | raw == ""
  out <- str_to_title(raw)
  out[empty] <- NA_character_
  out
}

normalize_gestational_age <- function(x) {
  raw <- str_trim(as.character(x))
  empty <- is.na(raw) | raw == ""
  num <- suppressWarnings(as.integer(as.numeric(raw)))
  out <- rep(NA_integer_, length(raw))
  ok <- !empty & !is.na(num) & num > 0L & num <= 45L
  out[ok] <- num[ok]
  out
}

normalize_zygosity <- function(x) {
  raw <- str_trim(as.character(x))
  empty <- is.na(raw) | raw == ""
  key <- str_to_upper(raw)
  case_when(
    empty ~ NA_character_,
    key %in% c("M", "MZ", "MONOZYGOTIC") ~ "MZ",
    key %in% c("D", "DZ", "DIZYGOTIC") ~ "DZ",
    raw %in% c("MZ", "DZ") ~ raw,
    TRUE ~ raw
  )
}

#' Apply demographic decoders to harmonized / export column names.
normalize_demographic_columns <- function(df) {
  if ("sex" %in% names(df)) df$sex <- normalize_sex(df$sex)
  if ("race" %in% names(df)) df$race <- normalize_race(df$race)
  if ("ethnicity" %in% names(df)) df$ethnicity <- normalize_ethnicity(df$ethnicity)
  if ("caregiver_education" %in% names(df)) {
    df$caregiver_education <- normalize_mom_ed(df$caregiver_education)
  }
  if ("birth_order" %in% names(df)) {
    df$birth_order <- normalize_birth_order(df$birth_order)
  }
  if ("birth_weight" %in% names(df)) {
    df$birth_weight <- birth_weight_to_kg(df$birth_weight)
  }
  if ("born_early_or_late" %in% names(df)) {
    df$born_early_or_late <- normalize_born_early_or_late(df$born_early_or_late)
  }
  if ("gestational_age" %in% names(df)) {
    df$gestational_age <- normalize_gestational_age(df$gestational_age)
  }
  if ("zygosity" %in% names(df)) {
    df$zygosity <- normalize_zygosity(df$zygosity)
  }
  df
}

normalize_positive_int <- function(x, allow_floor = FALSE) {
  raw <- as.character(x)
  empty <- is.na(raw) | str_trim(raw) == ""
  out <- rep(NA_integer_, length(raw))
  num <- suppressWarnings(as.numeric(raw))
  if (allow_floor) {
    ok <- !empty & !is.na(num) & num > 0
    out[ok] <- floor(num[ok])
  } else {
    ok <- !empty & !is.na(num) & num > 0 & num == floor(num)
    out[ok] <- as.integer(num[ok])
  }
  out
}

normalize_age <- function(x) {
  normalize_positive_int(x, allow_floor = TRUE)
}

normalize_date_ymd <- function(x) {
  raw <- as.character(x)
  out <- rep(NA_character_, length(raw))
  empty <- is.na(raw) | str_trim(raw) == ""
  out[empty] <- NA_character_
  if (!any(!empty)) return(out)

  idx <- which(!empty)
  for (i in idx) {
    v <- str_trim(raw[i])
    parsed <- as.POSIXct(NA)
    if (grepl("^\\d+(\\.\\d+)?$", v)) {
      n <- as.numeric(v)
      if (n > 1000 && n < 100000) {
        parsed <- as.POSIXct(as.Date(n, origin = "1899-12-30"), tz = "UTC")
      }
    }
    if (is.na(parsed)) {
      parsed <- suppressWarnings(parse_date_time(
        v,
        orders = c("Ymd", "ymd", "mdy", "dmy", "Y-m-d", "m/d/Y", "d/m/Y", "dbY", "bdY"),
        quiet = TRUE,
        tz = "UTC"
      ))
    }
    if (!is.na(parsed)) {
      out[i] <- format(as.Date(parsed), "%Y-%m-%d")
    }
  }
  out
}

#' Apply demographic normalization to wide demog columns from ingest.
normalize_demog_wide <- function(demog_wide) {
  if ("data_age" %in% names(demog_wide)) {
    demog_wide$data_age <- normalize_age(demog_wide$data_age)
  }
  if ("birth_order" %in% names(demog_wide)) {
    demog_wide$birth_order <- normalize_birth_order(demog_wide$birth_order)
  }
  if ("sex" %in% names(demog_wide)) {
    demog_wide$sex <- normalize_sex(demog_wide$sex)
  }
  if ("mom_ed" %in% names(demog_wide)) {
    demog_wide$mom_ed <- normalize_mom_ed(demog_wide$mom_ed)
  }
  if ("race" %in% names(demog_wide)) {
    demog_wide$race <- normalize_race(demog_wide$race)
  }
  if ("ethnicity" %in% names(demog_wide)) {
    demog_wide$ethnicity <- normalize_ethnicity(demog_wide$ethnicity)
  }
  if ("date_of_test" %in% names(demog_wide)) {
    demog_wide$date_of_test <- normalize_date_ymd(demog_wide$date_of_test)
  }
  if ("date_of_birth" %in% names(demog_wide)) {
    demog_wide$date_of_birth <- normalize_date_ymd(demog_wide$date_of_birth)
  }
  if ("birth_weight" %in% names(demog_wide)) {
    demog_wide$birth_weight <- birth_weight_to_kg(demog_wide$birth_weight)
  }
  if ("born_early_or_late" %in% names(demog_wide)) {
    demog_wide$born_early_or_late <- normalize_born_early_or_late(
      demog_wide$born_early_or_late
    )
  }
  if ("gestational_age" %in% names(demog_wide)) {
    demog_wide$gestational_age <- normalize_gestational_age(demog_wide$gestational_age)
  }
  if ("zygosity" %in% names(demog_wide)) {
    demog_wide$zygosity <- normalize_zygosity(demog_wide$zygosity)
  }
  demog_wide
}

is_valid_date_ymd <- function(x) {
  is.na(x) | (str_detect(x, DATE_YMD_RE) & !is.na(suppressWarnings(ymd(x, quiet = TRUE))))
}

demographic_issues <- function(
    df,
    id_cols = c("admin_row", "study_internal_id"),
    fields = c(
      "age", "birth_order", "date_of_test", "date_of_birth",
      "race", "ethnicity", "sex", "caregiver_education"
    )
) {
  id_cols <- intersect(id_cols, names(df))
  present <- intersect(fields, names(df))
  if (length(present) == 0L) return(tibble())

  map(present, \(field) {
    vals <- df[[field]]
    empty <- is.na(vals) | str_trim(as.character(vals)) == ""
    bad <- rep(FALSE, length(vals))

    bad <- bad | (!empty & field == "age" & (
      is.na(suppressWarnings(as.integer(vals))) |
        suppressWarnings(as.integer(vals)) < 1L
    ))
    bad <- bad | (!empty & field == "birth_order" & !(vals %in% BIRTH_ORDER_ALLOWED))

    bad <- bad | (!empty & field %in% c("date_of_test", "date_of_birth") & !is_valid_date_ymd(vals))

    bad <- bad | (!empty & field == "race" & !(vals %in% RACE_ALLOWED))
    bad <- bad | (!empty & field == "ethnicity" & !(vals %in% ETHNICITY_ALLOWED))
    bad <- bad | (!empty & field == "sex" & !(vals %in% SEX_ALLOWED))
    bad <- bad | (!empty & field == "caregiver_education" & !(vals %in% CAREGIVER_EDUCATION_ALLOWED))

    if (!any(bad)) return(tibble())

    df |>
      filter(bad) |>
      mutate(
        across(all_of(id_cols)),
        field = field,
        value = as.character(vals[bad]),
        reason = case_when(
          field == "age" ~ "must be a positive integer or blank",
          field == "birth_order" ~ "must be a valid birth-order label or blank",
          field %in% c("date_of_test", "date_of_birth") ~ "must be yyyy-mm-dd or blank",
          field == "race" ~ "must be one of Asian, Black, White, Other/Mixed or blank",
          field == "ethnicity" ~ "must be Hispanic, Non-Hispanic, or blank",
          field == "sex" ~ "must be one of Male, Female, Other or blank",
          field == "caregiver_education" ~ "must be a valid education level or blank",
          TRUE ~ "invalid value"
        )
      )
  }) |> list_rbind()
}

STRICT_CHILD_DEMO_COLS <- c("sex", "race", "ethnicity", "birth_order")

is_empty_demog <- function(x) {
  is.na(x) | str_trim(as.character(x)) == ""
}

empty_demog_mismatches <- function() {
  tibble(
    dataset_name = character(),
    dataset_origin_name = character(),
    language = character(),
    form = character(),
    study_internal_id = character(),
    field = character(),
    severity = character(),
    observed_values = character(),
    canonical_value = character(),
    admin_rows = character(),
    message = character()
  )
}

caregiver_education_rank <- function(x) {
  match(x, CAREGIVER_EDUCATION_ALLOWED)
}

caregiver_education_max <- function(values) {
  values <- unique(values[!is_empty_demog(values)])
  if (length(values) == 0L) return(NA_character_)
  values[which.max(caregiver_education_rank(values))]
}

format_demog_value <- function(x) {
  if (is_empty_demog(x)) return(NA_character_)
  as.character(x)
}

#' Collapse inconsistent child demographics across longitudinal visits.
#'
#' Rules:
#' 1. NA vs filled -> use filled (silent).
#' 2. Conflicting caregiver_education -> take max rank; warn and log.
#' 3. Conflicting sex, race, ethnicity, or birth_order -> error and log.
#'
#' @return List with `administrations`, `children`, and `mismatches`.
canonicalize_administration_demographics <- function(
    administrations,
    dataset_context,
    demo_cols = c("sex", "race", "ethnicity", "birth_order", "caregiver_education"),
    stop_on_fail = TRUE
) {
  ctx <- dataset_context[1, ]

  if (nrow(administrations) == 0L) {
    return(list(
      administrations = administrations,
      mismatches = empty_demog_mismatches()
    ))
  }

  strict_cols <- intersect(STRICT_CHILD_DEMO_COLS, demo_cols)
  mismatch_rows <- list()
  canonical <- list()

  study_groups <- administrations |>
    group_by(dataset_origin_name, study_internal_id) |>
    group_split()

  for (grp in study_groups) {
    sid <- as.character(grp$study_internal_id[[1]])
    origin <- grp$dataset_origin_name[[1]]
    key <- paste(origin, sid, sep = "\t")
    canonical[[key]] <- list()

    for (field in demo_cols) {
      vals <- grp[[field]]
      formatted <- map_chr(vals, format_demog_value)
      non_empty <- formatted[!is_empty_demog(formatted)]
      distinct <- unique(non_empty)
      admin_rows <- paste(sort(unique(grp$admin_row)), collapse = ",")

      if (field %in% strict_cols) {
        if (length(distinct) > 1L) {
          mismatch_rows[[length(mismatch_rows) + 1L]] <- tibble(
            dataset_name = ctx$dataset_name,
            dataset_origin_name = origin,
            language = ctx$language,
            form = ctx$form,
            study_internal_id = sid,
            field = field,
            severity = "error",
            observed_values = paste(distinct, collapse = ";"),
            canonical_value = NA_character_,
            admin_rows = admin_rows,
            message = paste0(
              "Conflicting ", field, " for study_internal_id ", sid,
              "; manual fix required"
            )
          )
          canonical[[key]][[field]] <- NA_character_
        } else {
          canonical[[key]][[field]] <- if (length(distinct)) distinct[[1]] else NA_character_
        }
      } else if (field == "caregiver_education") {
        if (length(distinct) > 1L) {
          chosen <- caregiver_education_max(distinct)
          mismatch_rows[[length(mismatch_rows) + 1L]] <- tibble(
            dataset_name = ctx$dataset_name,
            dataset_origin_name = origin,
            language = ctx$language,
            form = ctx$form,
            study_internal_id = sid,
            field = field,
            severity = "warning",
            observed_values = paste(distinct, collapse = ";"),
            canonical_value = chosen,
            admin_rows = admin_rows,
            message = paste0(
              "Multiple caregiver_education values; using max (", chosen, ")"
            )
          )
          canonical[[key]][[field]] <- chosen
        } else {
          canonical[[key]][[field]] <- if (length(distinct)) distinct[[1]] else NA_character_
        }
      }
    }
  }

  mismatches <- if (length(mismatch_rows)) bind_rows(mismatch_rows) else empty_demog_mismatches()

  if (any(mismatches$severity == "error") && stop_on_fail) {
    n_err <- sum(mismatches$severity == "error")
    preview <- mismatches |>
      filter(severity == "error") |>
      distinct(study_internal_id, field) |>
      head(5)
    stop(structure(
      list(
        message = paste0(
          "Demographic conflicts in ", n_err, " field(s) (see demog_mismatch.csv). ",
          "Examples: ",
          paste0(
            preview$study_internal_id, " (", preview$field, ")",
            collapse = "; "
          )
        ),
        mismatches = mismatches
      ),
      class = c("demographic_conflict_error", "error", "condition")
    ))
  }

  error_keys <- mismatches |>
    filter(severity == "error") |>
    mutate(key = paste(dataset_origin_name, study_internal_id, sep = "\t")) |>
    pull(key) |>
    unique()

  out_admins <- administrations
  for (key in names(canonical)) {
    if (key %in% error_keys) next
    parts <- str_split(key, "\t", n = 2)[[1]]
    idx <- out_admins$dataset_origin_name == parts[[1]] &
      out_admins$study_internal_id == parts[[2]]
    canon <- canonical[[key]]
    for (field in demo_cols) {
      val <- canon[[field]]
      out_admins[[field]][idx] <- if (is_empty_demog(val)) {
        NA_character_
      } else {
        val
      }
    }
  }

  warnings <- mismatches |> filter(severity == "warning")
  if (nrow(warnings) > 0L) {
    walk(seq_len(nrow(warnings)), \(i) {
      w <- warnings[i, ]
      message(
        "Demographic warning: ", w$dataset_origin_name,
        " study ", w$study_internal_id, " — ", w$message
      )
    })
  }

  list(
    administrations = out_admins,
    mismatches = mismatches
  )
}

#' Write demographic mismatch log from ingest.
write_demog_mismatches <- function(mismatches, path = "export/demog_mismatch.csv") {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  write_csv(mismatches, path, na = "")
  invisible(path)
}

#' Fail when administrations lack a study_internal_id.
validate_study_internal_ids <- function(
    administrations,
    dataset_context = NULL,
    stop_on_fail = TRUE
) {
  if (nrow(administrations) == 0L || !"study_internal_id" %in% names(administrations)) {
    return(invisible(tibble()))
  }

  bad <- administrations |>
    mutate(study_internal_id = as.character(study_internal_id)) |>
    filter(is.na(study_internal_id) | str_trim(study_internal_id) == "")

  if (nrow(bad) == 0L) return(invisible(tibble()))

  issues <- bad |>
    distinct(admin_row, .keep_all = TRUE) |>
    mutate(
      admin_row,
      study_internal_id = as.character(study_internal_id),
      field = "study_internal_id",
      reason = "must be present",
      .keep = "none"
    )

  if (!stop_on_fail) return(issues)

  ctx <- if (!is.null(dataset_context) && nrow(dataset_context) > 0L) {
    paste0(
      dataset_context$dataset_origin_name[[1]],
      if ("data_file" %in% names(dataset_context) && !is.na(dataset_context$data_file[[1]])) {
        paste0(" (", dataset_context$data_file[[1]], ")")
      } else {
        ""
      }
    )
  } else {
    "dataset"
  }

  stop(paste0(
    "Missing study_internal_id in ", nrow(issues),
    " administration(s) from ", ctx, ". admin_row: ",
    paste(head(issues$admin_row, 10), collapse = ", "),
    if (nrow(issues) > 10L) " ..." else ""
  ))
}

#' Validate normalized demographic fields; returns issue rows.
validate_demographics <- function(administrations, children = NULL, stop_on_fail = TRUE) {
  study_id_issues <- validate_study_internal_ids(
    administrations,
    stop_on_fail = stop_on_fail
  )

  admin_issues <- demographic_issues(
    administrations,
    id_cols = c("admin_row", "study_internal_id", "dataset_origin_name"),
    fields = c(
      "age", "birth_order", "date_of_test",
      "race", "ethnicity", "sex", "caregiver_education"
    )
  )

  child_issues <- if (!is.null(children) && nrow(children) > 0) {
    demographic_issues(
      children,
      id_cols = c("study_internal_id", "dataset_origin_name", CHILD_DEMO_COLS),
      fields = c(
        "birth_order", "date_of_birth",
        "race", "ethnicity", "sex", "caregiver_education"
      )
    )
  } else {
    tibble()
  }

  issues <- bind_rows(study_id_issues, admin_issues, child_issues)
  if (nrow(issues) > 0L && stop_on_fail) {
    preview <- issues |>
      count(field, reason, sort = TRUE) |>
      head(5)
    msg <- paste0(
      "Demographic validation failed: ", nrow(issues), " invalid value(s). ",
      "Top issues: ",
      paste0(preview$field, " (", preview$n, ")", collapse = "; ")
    )
    stop(structure(
      list(message = msg, issues = issues),
      class = c("demographic_validation_error", "error", "condition")
    ))
  }
  issues
}
