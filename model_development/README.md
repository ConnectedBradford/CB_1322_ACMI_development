# ACMI

## Overview

AntiCholinergic Medication Index (ACMI)

Project to develop an Anticholinergic Medication Index (ACMI). More info about the project at:

<https://www.hdruk.ac.uk/projects/better-care-northern-partnership-development-of-a-learning-system-to-optimise-anticholinergic-medication-prescribing-for-older-people-living-with-frailty/>

This folder contains the code to build the ACMI from the Connected Bradford data.

## File structure

- R
  - analysis
    - final_analysis.R
  - data_prep
    - acmi.R
    - build_sql_dose.R
    - build_sql_drugs.R
    - build_sql_efi.R
    - build_sql_excl_PRNs.R
    - build_sql.R
    - build_sql_bri_only.R
  - qc
    - qc_processed.R
    - qc_raw_data.R
- lists
  - selected_snomed_codes_delirium.csv
  - selected_snomed_codes_falls.csv
- stata
  - Do_file_for_AC_meds_.do
- utils
  - build_SNOMED_to_AC_lookup
- README.md

## Code

This code is written to work with the Connected Bradford data stored in Google BigQuery. It doesn't use any of the BigQuery-specific syntax, so it should just be table paths that need to be changed to be reused. It's split into parts to try to keep it manageable.

The data_prep part is a series of R functions which themselves just build SQL strings which get passed to BigQuery. It's done this way so we can pass variables into the functions to change the source data and output location. Tables are built iteratively in BigQuery, ending up with over 30 by the time the final data table is built.

The qc scripts allow for checking of duplicate data, NULLs etc in the BigQuery data.

The analysis code reads in the final data tables from BigQuery into R and fits it's models there.

The Stata code builds the XLSX file which is imported into BigQuery.

## Lists

The list of anticholinergic medications to consider is defined in the `AC_medication_codes.csv` this includes a long list (20,000+) of anticholinergic medications consisting of every brand and dosage. Each medication in this list is mapped to a top level anticholinergic, e.g. *alprazolam 250microgram tablets* is mapped to *alprozam*. Similarly, *xanax 250microgram tablets* is mapped to *alprozam*. This enables the selection of all preparations of a given anticholinergic medication later on in the code. This list was manually curated from the BNF and other sources.

The list of anticholinergics medications used in the development of ACMI is defined in `selected_ac_medications.csv`. This is a subset of the full list of anticholinergic medications in `AC_medication_codes.csv`.

These AC medication code files are not included in this repo and are available on request.

The lists of SNOMED codes for delirium and falls are defined in `selected_snomed_codes_delirium.csv` and `selected_snomed_codes_falls.csv` respectively.

The list of eFI_deficits is in eFI_deficits.csv note that this is not included in this repo and must be obtained from elsewhere. These can be obtained from the author of https://doi.org/10.1093/ageing/afw039

## Running the code

### Prerequisites

#### AC_medication_codes

`AC_medication_codes` needs to exist as a table in BigQuery before running the code. This can be imported via the web interface. Note that all of the items in the `selected_ac_medications.csv` file must exist in the `AC_medication_codes` BigQuery table - this should not be a problem if the files in the lists directory were used.

#### eFI_deficits

The eFI deficits need to exist as a table in BigQuery (called eFI_SNOMED_2) before running the code. This can be got from <https://github.com/HDRUK-North/ACMI-eFI-codes> and imported via the web interface.

Additionally, the efi_deficits.csv file needs to be uploaded alongside the rest of the lists into RStudio.

### Data structure

The data is set up like:

| Database names       | Description                                                                                              |
|----------------------|----------------------------------------------------------------------------------------------------------|
| `cdm_source_dataset` | The common data model source data. This is a set of tables with read only permissions. |
| `fdm_source_dataset` | The flexible data model source data. This is a set of tables with read only permissions. |
| `target_dataset`     | The current user's space (database) to store data (tables), where all the intermediate steps are stored. |

The target_table_prefix is used to add a prefix to the derived table names. This means you can run the code to generate all the data with e.g. v1_table_A, v1_table_B etc, then run the code again to generate v2_table_A, v2_table_B etc. In this way you can do development without having to delete previous versions of the data.

### Data prep

To do the data prep just edit the config settings at the start of acmi.R, then source the file. Depending on how big the underlying data is it could take hours to run. It will give feedback on the process as it runs.

Since most of the code is just about generating SQL statements, you can run the functions directly in R and they will print out the SQL it intends to run. This can be useful for debugging, or if you want to manually run the command in the BigQuery web interface.

### QC

Both the QC scripts are self contained so can be edited to refer to the required datasets and run.

### Analysis

The analysis code is in the R folder. This can be run once the data prep has been done.
