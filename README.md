Wordbank import
======

[Wordbank](http://wordbank.stanford.edu/) is an open database of children's vocabulary development, archiving data from the MacArthur-Bates Communicative Development Inventories (CDIs) contributed by researchers around the world.
This repository contains code for importing data into the Wordbank format as expected by the [Redivis](https://stanford.redivis.com/datasets/627v-9ewzpdvz0) database. 

## Structure

Scripts for import are contained in `import`.
Raw data files are contained in `raw_data`, with one folder per instrument (language x form); each instrument folder contains one instrument file `[<Lang>_<Form>].csv` and one triplet of `_data`, `_fields`, and `_values` CSV files per dataset. 

The root directory contains the following files:

- `categories.csv`: This records all the vocabulary categories and their lexical classes.
- `datasets.csv`: This is the main manifest that is trawled during ingestion; it contains one row per dataset.
- `merge_and_export.Rmd`: Rerunning this file reingests, harmonises, and exports the data.
- `run_merge.R`: This is an alternative method for `merge_and_export.Rmd` in a non-interactive setting.
