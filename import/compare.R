# Compare manifest / harmonized ingest to live Redivis datasets before merge.
# Supports field-level rename aliases (see dataset_aliases.csv).

suppressPackageStartupMessages({
  library(tidyverse)
})

DATASET_KEY_COLS <- c(
  "dataset_name", "dataset_origin_name", "language", "form"
)

ALIAS_FIELDS <- DATASET_KEY_COLS

empty_aliases <- function() {
  tibble(
    field = character(),
    redivis_value = character(),
    manifest_value = character(),
    notes = character()
  )
}

#' Load field-level rename aliases (`field`, `redivis_value`, `manifest_value`).
load_dataset_aliases <- function(path = "dataset_aliases.csv") {
  if (!file.exists(path)) return(empty_aliases())
  read_csv(path, show_col_types = FALSE) |>
    filter(!is.na(field), field != "") |>
    mutate(across(c(field, redivis_value, manifest_value, notes), as.character))
}

alias_map <- function(aliases, field, direction = c("to_redivis", "to_manifest")) {
  direction <- match.arg(direction)
  sub <- aliases |> filter(field == !!field)
  if (nrow(sub) == 0) return(character())
  if (direction == "to_redivis") {
    set_names(sub$redivis_value, sub$manifest_value)
  } else {
    set_names(sub$manifest_value, sub$redivis_value)
  }
}

#' Map manifest key values to Redivis names (or the reverse).
translate_dataset_keys <- function(df, aliases, direction = c("to_redivis", "to_manifest")) {
  direction <- match.arg(direction)
  out <- df
  for (fld in intersect(ALIAS_FIELDS, names(out))) {
    mp <- alias_map(aliases, fld, direction)
    if (length(mp) == 0) next
    vals <- out[[fld]]
    hit <- !is.na(vals) & vals %in% names(mp)
    if (any(hit)) vals[hit] <- unname(mp[vals[hit]])
    out[[fld]] <- vals
  }
  if ("dataset_origin_name" %in% names(out)) {
    out$dataset_origin_name <- translate_origin_name(
      out$dataset_origin_name, aliases, direction
    )
  }
  out
}

translate_origin_name <- function(origin, aliases, direction) {
  if (length(origin) == 0L || nrow(aliases) == 0L) return(origin)
  out <- origin
  rows <- aliases |> filter(field %in% ALIAS_FIELDS)
  if (direction == "to_redivis") {
    from <- rows$manifest_value
    to <- rows$redivis_value
  } else {
    from <- rows$redivis_value
    to <- rows$manifest_value
  }
  for (i in seq_len(nrow(rows))) {
    if (is.na(from[i]) || from[i] == "" || is.na(to[i]) || to[i] == "") next
    out <- str_replace_all(out, fixed(from[i]), to[i])
  }
  out
}

dataset_keys <- function(df) {
  df |>
    select(any_of(DATASET_KEY_COLS)) |>
    distinct()
}

alias_field_summary <- function(mdn, mdo, ml, mf, rdn, rdo, rl, rf) {
  changed <- c(
    if (!identical(mdn, rdn)) paste0("dataset_name: ", rdn, " -> ", mdn),
    if (!identical(mdo, rdo)) paste0("dataset_origin_name: ", rdo, " -> ", mdo),
    if (!identical(ml, rl)) paste0("language: ", rl, " -> ", ml),
    if (!identical(mf, rf)) paste0("form: ", rf, " -> ", mf)
  )
  str_c(changed, collapse = "; ")
}

#' Match manifest dataset rows to Redivis dataset rows (exact, then alias).
match_manifest_to_redivis <- function(manifest_dataset, redivis_datasets, aliases = empty_aliases()) {
  manifest_keys <- dataset_keys(manifest_dataset) |>
    mutate(manifest_row = row_number())
  redivis_keys <- dataset_keys(redivis_datasets) |>
    mutate(redivis_row = row_number())

  exact <- manifest_keys |>
    inner_join(redivis_keys, by = DATASET_KEY_COLS) |>
    transmute(
      manifest_row,
      redivis_row,
      match_type = "exact",
      dataset_name_manifest = dataset_name,
      dataset_origin_name_manifest = dataset_origin_name,
      language_manifest = language,
      form_manifest = form,
      dataset_name_redivis = dataset_name,
      dataset_origin_name_redivis = dataset_origin_name,
      language_redivis = language,
      form_redivis = form,
      alias_fields = NA_character_
    )

  remaining <- manifest_keys |>
    anti_join(exact, by = "manifest_row")

  alias_matches <- if (nrow(remaining) == 0L || nrow(aliases) == 0L) {
    tibble(
      manifest_row = integer(), redivis_row = integer(), match_type = character(),
      dataset_name_manifest = character(), dataset_origin_name_manifest = character(),
      language_manifest = character(), form_manifest = character(),
      dataset_name_redivis = character(), dataset_origin_name_redivis = character(),
      language_redivis = character(), form_redivis = character(),
      alias_fields = character()
    )
  } else {
    map_dfr(seq_len(nrow(remaining)), \(i) {
      m <- remaining[i, ]
      lookup <- translate_dataset_keys(m, aliases, "to_redivis")
      hit <- redivis_keys |>
        inner_join(lookup, by = DATASET_KEY_COLS)
      if (nrow(hit) == 0L) return(NULL)
      if (nrow(hit) > 1L) {
        warning(
          "Multiple Redivis matches for manifest row ", m$manifest_row,
          call. = FALSE
        )
      }
      hit <- hit[1, ]
      tibble(
        manifest_row = m$manifest_row,
        redivis_row = hit$redivis_row,
        match_type = "alias",
        dataset_name_manifest = m$dataset_name,
        dataset_origin_name_manifest = m$dataset_origin_name,
        language_manifest = m$language,
        form_manifest = m$form,
        dataset_name_redivis = hit$dataset_name,
        dataset_origin_name_redivis = hit$dataset_origin_name,
        language_redivis = hit$language,
        form_redivis = hit$form,
        alias_fields = alias_field_summary(
          m$dataset_name, m$dataset_origin_name, m$language, m$form,
          hit$dataset_name, hit$dataset_origin_name, hit$language, hit$form
        )
      )
    })
  }

  matches <- bind_rows(exact, alias_matches)

  unmatched_manifest <- manifest_keys |>
    anti_join(matches, by = "manifest_row")

  manifest_lang_form <- manifest_keys |> distinct(language, form)
  redivis_scope <- redivis_keys |>
    inner_join(manifest_lang_form, by = c("language", "form"), relationship = "many-to-many")

  unmatched_redivis <- redivis_scope |>
    anti_join(matches, by = "redivis_row")

  list(
    matches = matches,
    unmatched_manifest = unmatched_manifest,
    unmatched_redivis = unmatched_redivis
  )
}

#' Redivis administrations omit `dataset_origin_name`; attach it from children.
enrich_redivis_administrations <- function(existing) {
  admins <- existing$administrations
  if ("dataset_origin_name" %in% names(admins)) {
    return(admins)
  }
  if (is.null(existing$children) || !"child_id" %in% names(admins)) {
    return(admins)
  }
  admins |>
    left_join(
      existing$children |> distinct(child_id, dataset_origin_name),
      by = "child_id"
    )
}

filter_administrations <- function(admins, keys) {
  out <- admins |>
    filter(
      dataset_name == keys$dataset_name,
      language == keys$language,
      form == keys$form
    )
  if ("dataset_origin_name" %in% names(out) && !is.na(keys$dataset_origin_name)) {
    out <- out |> filter(dataset_origin_name == keys$dataset_origin_name)
  }
  out
}

score_distribution <- function(admins, col) {
  vals <- suppressWarnings(as.integer(admins[[col]]))
  vals <- vals[!is.na(vals)]
  if (length(vals) == 0L) {
    return(tibble(score = integer(), n = integer()))
  }
  tibble(score = vals) |>
    count(score, name = "n") |>
    arrange(score)
}

score_distribution_diff <- function(ingest_admins, redivis_admins, col) {
  ing <- score_distribution(ingest_admins, col)
  red <- score_distribution(redivis_admins, col)
  if (nrow(ing) == 0L && nrow(red) == 0L) {
    return(list(
      applicable = FALSE,
      equivalent = NA,
      max_abs_delta = 0L,
      n_diff_scores = 0L,
      table = tibble(
        score = integer(), n_ingest = integer(), n_redivis = integer(), delta = integer()
      )
    ))
  }
  table <- full_join(ing, red, by = "score", suffix = c("_ingest", "_redivis")) |>
    mutate(
      n_ingest = replace_na(n_ingest, 0L),
      n_redivis = replace_na(n_redivis, 0L),
      delta = n_ingest - n_redivis
    ) |>
    arrange(score)
  list(
    applicable = TRUE,
    equivalent = all(table$delta == 0L),
    max_abs_delta = max(abs(table$delta)),
    n_diff_scores = sum(table$delta != 0L),
    table = table
  )
}

compare_vocab_distributions <- function(ingest_admins, redivis_admins, form_type = NULL) {
  prod <- score_distribution_diff(ingest_admins, redivis_admins, "production")
  comp <- if (!is.null(form_type) && form_type == "WS") {
    list(
      applicable = FALSE,
      equivalent = NA,
      max_abs_delta = 0L,
      n_diff_scores = 0L,
      table = tibble(
        score = integer(), n_ingest = integer(), n_redivis = integer(), delta = integer()
      )
    )
  } else {
    score_distribution_diff(ingest_admins, redivis_admins, "comprehension")
  }
  list(production = prod, comprehension = comp)
}

form_type_for_match <- function(new_parts, m) {
  hit <- new_parts$dataset |>
    filter(
      dataset_name == m$dataset_name_manifest,
      dataset_origin_name == m$dataset_origin_name_manifest,
      language == m$language_manifest,
      form == m$form_manifest
    ) |>
    pull(form_type)
  if (length(hit)) hit[[1]] else NA_character_
}

compare_matched_vocab_distributions <- function(existing, new_parts, matches) {
  if (nrow(matches) == 0L) {
    return(list(
      summary = tibble(),
      diffs = tibble()
    ))
  }

  summary <- map_dfr(seq_len(nrow(matches)), \(i) {
    m <- matches[i, ]
    ingest_keys <- m |>
      transmute(
        dataset_name = dataset_name_manifest,
        dataset_origin_name = dataset_origin_name_manifest,
        language = language_manifest,
        form = form_manifest
      )
    redivis_keys <- m |>
      transmute(
        dataset_name = dataset_name_redivis,
        dataset_origin_name = dataset_origin_name_redivis,
        language = language_redivis,
        form = form_redivis
      )
    ingest_admins <- filter_administrations(new_parts$administrations, ingest_keys)
    redivis_admins <- filter_administrations(existing$administrations, redivis_keys)
    dist <- compare_vocab_distributions(
      ingest_admins, redivis_admins, form_type = form_type_for_match(new_parts, m)
    )
    tibble(
      production_equivalent = dist$production$equivalent,
      production_max_abs_delta = dist$production$max_abs_delta,
      production_n_diff_scores = dist$production$n_diff_scores,
      comprehension_equivalent = dist$comprehension$equivalent,
      comprehension_max_abs_delta = dist$comprehension$max_abs_delta,
      comprehension_n_diff_scores = dist$comprehension$n_diff_scores
    )
  })

  diffs <- map_dfr(seq_len(nrow(matches)), \(i) {
    m <- matches[i, ]
    ingest_keys <- m |>
      transmute(
        dataset_name = dataset_name_manifest,
        dataset_origin_name = dataset_origin_name_manifest,
        language = language_manifest,
        form = form_manifest
      )
    redivis_keys <- m |>
      transmute(
        dataset_name = dataset_name_redivis,
        dataset_origin_name = dataset_origin_name_redivis,
        language = language_redivis,
        form = form_redivis
      )
    ingest_admins <- filter_administrations(new_parts$administrations, ingest_keys)
    redivis_admins <- filter_administrations(existing$administrations, redivis_keys)
    dist <- compare_vocab_distributions(
      ingest_admins, redivis_admins, form_type = form_type_for_match(new_parts, m)
    )

    bind_rows(
      if (dist$production$applicable) {
        dist$production$table |>
          mutate(score_type = "production")
      },
      if (dist$comprehension$applicable) {
        dist$comprehension$table |>
          mutate(score_type = "comprehension")
      }
    ) |>
      filter(delta != 0L) |>
      mutate(
        dataset_name_manifest = m$dataset_name_manifest,
        dataset_origin_name_manifest = m$dataset_origin_name_manifest,
        language_manifest = m$language_manifest,
        form_manifest = m$form_manifest,
        dataset_name_redivis = m$dataset_name_redivis,
        dataset_origin_name_redivis = m$dataset_origin_name_redivis,
        language_redivis = m$language_redivis,
        form_redivis = m$form_redivis,
        match_type = m$match_type,
        alias_fields = m$alias_fields
      )
  })

  list(summary = summary, diffs = diffs)
}

count_manifest_metrics <- function(new_parts, keys) {
  map_dfr(seq_len(nrow(keys)), \(i) {
    k <- keys[i, ]
    admins <- filter_administrations(new_parts$administrations, k)
    n_admins_val <- new_parts$dataset |>
      filter(
        dataset_name == k$dataset_name,
        dataset_origin_name == k$dataset_origin_name,
        language == k$language,
        form == k$form
      ) |>
      pull(n_admins)
    tibble(
      ingest_administrations = nrow(admins),
      ingest_children = admins |> distinct(across(all_of(CHILD_KEY_COLS))) |> nrow(),
      ingest_n_admins = if (length(n_admins_val)) n_admins_val[[1]] else NA_real_
    )
  })
}

count_redivis_metrics <- function(existing, keys) {
  map_dfr(seq_len(nrow(keys)), \(i) {
    k <- keys[i, ]
    admins <- filter_administrations(existing$administrations, k)
    children <- existing$children |>
      filter(child_id %in% admins$child_id)
    n_admins_val <- existing$datasets |>
      filter(
        dataset_name == k$dataset_name,
        dataset_origin_name == k$dataset_origin_name,
        language == k$language,
        form == k$form
      ) |>
      pull(n_admins)
    tibble(
      redivis_administrations = nrow(admins),
      redivis_children = n_distinct(children$child_id),
      redivis_n_admins = if (length(n_admins_val)) n_admins_val[[1]] else NA_real_
    )
  })
}

#' Compare manifest ingest to Redivis before merge.
compare_manifest_to_redivis <- function(
    existing,
    new_parts,
    aliases = empty_aliases(),
    manifest_dataset = new_parts$dataset
) {
  existing$administrations <- enrich_redivis_administrations(existing)

  matching <- match_manifest_to_redivis(
    manifest_dataset,
    existing$datasets,
    aliases
  )

  comparison <- tibble()
  score_distribution_diffs <- tibble()

  if (nrow(matching$matches) > 0L) {
    manifest_for_metrics <- matching$matches |>
      transmute(
        dataset_name = dataset_name_manifest,
        dataset_origin_name = dataset_origin_name_manifest,
        language = language_manifest,
        form = form_manifest
      )
    redivis_for_metrics <- matching$matches |>
      transmute(
        dataset_name = dataset_name_redivis,
        dataset_origin_name = dataset_origin_name_redivis,
        language = language_redivis,
        form = form_redivis
      )

    vocab <- compare_matched_vocab_distributions(
      existing, new_parts, matching$matches
    )

    comparison <- matching$matches |>
      bind_cols(count_manifest_metrics(new_parts, manifest_for_metrics)) |>
      bind_cols(count_redivis_metrics(existing, redivis_for_metrics)) |>
      bind_cols(vocab$summary) |>
      mutate(
        administration_delta = ingest_administrations - redivis_administrations,
        children_delta = ingest_children - redivis_children,
        n_admins_delta = ingest_n_admins - redivis_n_admins,
        equivalent = administration_delta == 0L &
          children_delta == 0L &
          (is.na(n_admins_delta) | n_admins_delta == 0) &
          (is.na(production_equivalent) | production_equivalent) &
          (is.na(comprehension_equivalent) | comprehension_equivalent)
      )

    score_distribution_diffs <- vocab$diffs
  }

  list(
    comparison = comparison,
    score_distribution_diffs = score_distribution_diffs,
    unmatched_manifest = matching$unmatched_manifest,
    unmatched_redivis = matching$unmatched_redivis,
    aliases = aliases
  )
}

format_alias_rename <- function(row) {
  if (is.na(row$alias_fields) || row$alias_fields == "") {
    return(paste0(
      row$dataset_name_manifest, " / ", row$language_manifest, " ", row$form_manifest
    ))
  }
  paste0(
    row$dataset_name_manifest, " / ", row$language_manifest, " ", row$form_manifest,
    "  [", row$alias_fields, "]"
  )
}

#' Print a human-readable pre-merge equivalence report.
print_dataset_equivalence <- function(report) {
  cmp <- report$comparison
  if (nrow(cmp) > 0L) {
    message("Dataset equivalence (ingest vs Redivis):")
    walk(seq_len(nrow(cmp)), \(i) {
      r <- cmp[i, ]
      label <- format_alias_rename(r)
      status <- if (isTRUE(r$equivalent)) "OK" else "MISMATCH"
      if (!isTRUE(r$equivalent)) {
        score_bits <- c(
          if (!is.na(r$administration_delta) && r$administration_delta != 0) {
            paste0("admins delta ", r$administration_delta)
          },
          if (!is.na(r$children_delta) && r$children_delta != 0) {
            paste0("children delta ", r$children_delta)
          },
          if (isFALSE(r$production_equivalent)) {
            paste0(
              "production distribution differs (",
              r$production_n_diff_scores, " score value(s), max delta ",
              r$production_max_abs_delta, ")"
            )
          },
          if (isFALSE(r$comprehension_equivalent)) {
            paste0(
              "comprehension distribution differs (",
              r$comprehension_n_diff_scores, " score value(s), max delta ",
              r$comprehension_max_abs_delta, ")"
            )
          }
        )
        detail <- paste(score_bits, collapse = "; ")
      } else {
        detail <- paste0(
          "admins ", r$ingest_administrations,
          if (isTRUE(r$production_equivalent)) "; production OK" else "",
          if (isTRUE(r$comprehension_equivalent)) "; comprehension OK" else ""
        )
      }
      message(
        "  ", status, ": ", label,
        " — ", detail
      )
      if (identical(r$match_type, "alias")) {
        message("    matched via alias (Redivis: ", r$dataset_name_redivis,
                " / ", r$language_redivis, " ", r$form_redivis, ")")
      }
    })
  }

  if (nrow(report$unmatched_manifest) > 0L) {
    message(
      "Manifest datasets with no Redivis match (", nrow(report$unmatched_manifest), "):"
    )
    walk(seq_len(nrow(report$unmatched_manifest)), \(i) {
      r <- report$unmatched_manifest[i, ]
      message(
        "  NEW? ", r$dataset_name, " / ", r$language, " ", r$form,
        " (", r$dataset_origin_name, ")"
      )
    })
  }

  if (nrow(report$unmatched_redivis) > 0L) {
    message(
      "Redivis datasets in manifest language/form scope with no ingest match (",
      nrow(report$unmatched_redivis), "):"
    )
    walk(seq_len(nrow(report$unmatched_redivis)), \(i) {
      r <- report$unmatched_redivis[i, ]
      message(
        "  OLD? ", r$dataset_name, " / ", r$language, " ", r$form,
        " (", r$dataset_origin_name, ")"
      )
    })
  }

  invisible(report)
}

write_dataset_equivalence_report <- function(report, out_dir) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  paths <- list()
  if (nrow(report$comparison) > 0L) {
    paths$comparison <- file.path(out_dir, "dataset_equivalence.csv")
    write_csv(report$comparison, paths$comparison, na = "")
  }
  if (!is.null(report$score_distribution_diffs) && nrow(report$score_distribution_diffs) > 0L) {
    paths$score_distribution_diffs <- file.path(out_dir, "score_distribution_diffs.csv")
    write_csv(report$score_distribution_diffs, paths$score_distribution_diffs, na = "")
  }
  if (nrow(report$unmatched_manifest) > 0L) {
    paths$unmatched_manifest <- file.path(out_dir, "unmatched_manifest.csv")
    write_csv(report$unmatched_manifest, paths$unmatched_manifest, na = "")
  }
  if (nrow(report$unmatched_redivis) > 0L) {
    paths$unmatched_redivis <- file.path(out_dir, "unmatched_redivis.csv")
    write_csv(report$unmatched_redivis, paths$unmatched_redivis, na = "")
  }
  paths
}
