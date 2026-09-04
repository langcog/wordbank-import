#!/usr/bin/env Rscript
# Run ingest + merge + export for datasets.csv.
#
# Usage:
#   Rscript run_merge.R [append|complete] [--upload] [--from-harmonized] [--skip-processed]
#
# append            — pull Redivis, merge only datasets not yet on Redivis, append upload
# complete          — reimport all manifest raw_data, log discrepancies, replace on Redivis
# --from-harmonized  — load harmonized_data/ instead of re-ingesting raw CSVs
# --skip-processed  — during ingest, load groups already present in harmonized_data/

args <- commandArgs(trailingOnly = TRUE)
mode <- "append"
upload <- FALSE
from_harmonized <- FALSE
skip_processed <- FALSE
for (a in args) {
  if (a %in% c("append", "complete")) mode <- a
  if (a == "--upload") upload <- TRUE
  if (a == "--from-harmonized") from_harmonized <- TRUE
  if (a == "--skip-processed") skip_processed <- TRUE
}

suppressMessages({
  library(tidyverse)
  library(here)
})

setwd(here())
source(here("import", "helpers.R"))
source(here("import", "demographics.R"))
source(here("import", "validate.R"))
source(here("import", "ingest.R"))
source(here("import", "compare.R"))
source(here("import", "ids.R"))

RAW <- here("raw_data")
OUT_HARM <- here("harmonized_data")
OUT_EXPORT <- here("export")
dir.create(OUT_HARM, showWarnings = FALSE)
dir.create(OUT_EXPORT, showWarnings = FALSE)
dir.create(file.path(OUT_EXPORT, "item_responses"), recursive = TRUE, showWarnings = FALSE)

message("Mode: ", mode,
        if (from_harmonized) " (from harmonized)" else "",
        if (skip_processed) " (skip processed)" else "")

manifest <- read_csv(here("datasets.csv"), show_col_types = FALSE)
categories <- read_csv(
  here("categories.csv"),
  col_names = c("category", "lexical_class", "lexical_category"),
  show_col_types = FALSE
) |>
  distinct()

if (from_harmonized) {
  message("Loading harmonized tables from ", OUT_HARM)
  ingested <- load_ingested_manifest(manifest, OUT_HARM)
} else {
  ingested <- ingest_all_manifest(
    manifest,
    RAW,
    categories,
    out_harm = OUT_HARM,
    skip_processed = skip_processed
  )
}
new_parts <- ingested$new_parts
triplet_ranges <- ingested$triplet_ranges
iwalk(new_parts, \(df, nm) message(nm, ": ", nrow(df), " rows"))

message("Pulling Redivis core tables...")
existing <- pull_redivis_core()

dataset_aliases <- load_dataset_aliases(here("dataset_aliases.csv"))
equivalence <- compare_manifest_to_redivis(existing, new_parts, dataset_aliases)
print_dataset_equivalence(equivalence)
write_dataset_equivalence_report(equivalence, file.path(OUT_EXPORT, "equivalence"))

registry <- load_registry(here("id_registry.rds"))
merged <- if (from_harmonized) {
  merge_from_harmonized(manifest, OUT_HARM, existing, registry, mode = mode)
} else {
  merge_with_existing(existing, new_parts, registry, mode = mode)
}
save_registry(merged$registry, here("id_registry.rds"))
validate_merged(merged$tables, merged$new_item_responses)

iwalk(merged$tables, \(df, nm) {
  path <- file.path(OUT_EXPORT, paste0(nm, ".csv"))
  write_csv(df, path, na = "")
  message("wrote ", path, " (", nrow(df), " rows)")
})

if (!is.null(merged$upload_deltas)) {
  delta_dir <- file.path(OUT_EXPORT, "deltas")
  dir.create(delta_dir, showWarnings = FALSE)
  iwalk(merged$upload_deltas, \(df, nm) {
    if (nrow(df) == 0) return()
    path <- file.path(delta_dir, paste0(nm, ".csv"))
    write_csv(df, path, na = "")
    message("wrote delta ", path, " (", nrow(df), " rows)")
  })
}

if (nrow(merged$new_item_responses) > 0) {
  merged$new_item_responses |>
    mutate(slug = item_response_slug(language, form)) |>
    group_split(slug) |>
    walk(\(df) {
      slug <- df$slug[[1]]
      path <- file.path(OUT_EXPORT, "item_responses", paste0(slug, ".csv"))
      write_csv(df |> select(-slug), path, na = "")
      message("wrote ", path, " (", nrow(df), " rows)")
    })
}

if (upload) {
  message("Uploading to Redivis (", mode, " strategy)...")
  upload_to_redivis(merged, OUT_EXPORT, mode = mode)
}

message("Done.")
