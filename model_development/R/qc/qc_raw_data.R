# Some QC scripts to check the raw data


project <- "yhcr-prd-phm-bia-core"
# cdm_source_dataset <- "CY_CDM_V1_KB_Subset"
cdm_source_dataset <- "CY_CDM_V1_50k_Random"

library(bigrquery)

# The two lists of tables. Note that there are several empty tables
# in the first list that could probably be ignored.

# CY_CDM_V1_50k_Random
tables_to_check_50k_Random <- c(
  "care_site",
  "cdm_source",
  "concept",
  "concept_ancestor",
  "concept_class",
  "concept_relationship",
  "concept_synonym",
  "condition_occurrence",
  "cyorks_about",
  "cyorks_cdm_source",
  "device_exposure",
  "domain",
  "dose_era",
  "drug_era",
  "drug_exposure",
  "drug_strength",
  "fact_relationship",
  "location",
  "measurement",
  "metadata",
  "note",
  "note_nlp",
  "observation",
  "observation_period",
  "payer_plan_period",
  "person",
  "procedure_occurrence",
  "provider",
  "relationship",
  "source_to_concept_map",
  "specimen",
  "survey_conduct",
  "tmp_cohort",
  "visit_detail",
  "visit_occurrence",
  "vocabulary"
)

# CY_CDM_V1_KB_Subset
tables_to_check_KB_Subset <- c(
  "care_site",
  "concept",
  "concept_ancestor",
  "concept_class",
  "concept_relationship",
  "concept_synonym",
  "condition_occurrence",
  "device_exposure",
  "domain",
  "dose_era",
  "drug_era",
  "drug_exposure",
  "drug_strength",
  "fact_relationship",
  "location",
  "measurement",
  "observation",
  "observation_period",
  "person",
  "procedure_occurrence",
  "relationship",
  "tmp_cohort",
  "visit_occurrence",
  "vocabulary"
)

# Pick one of the sets of tables
tables_to_check <- tables_to_check_50k_Random


################################################################################
# Checking for duplicates in the tables. Just do a GROUP BY on all columns and
# a COUNT(*) for each.
################################################################################
check_distinct <- function(this_dataset_name, this_table_name) {

  # Handle for this dataset
  dsh <- bq_dataset(project, this_dataset_name)

  # Get list of columns for this_table_name
  tbh <- bq_table(dsh, this_table_name)
  my_fields <- bq_table_fields(tbh)
  cols <- lapply(my_fields,'[[',"name")

  sql <- str_c(
    "SELECT *, COUNT(*) AS COUNT
    FROM `",project,".",this_dataset_name,".",this_table_name,"`
    GROUP BY ",paste0(cols, collapse = ", "),"
    HAVING COUNT > 1
    ORDER BY COUNT DESC;"
  )

  # tidy the sql
  sql = gsub("\\s+", " ", sql)
  print(sql)

  # Run the query
  tb <- bq_dataset_query(dsh, sql)
  print(paste0(this_table_name, " duplicates: ", bq_table_nrow(tb)))
  return(bq_table_nrow(tb))
}

# Run the above over the list of tables to check
# Build a list of duplicates for each table
duplicates <-list()
for (this_table in tables_to_check) {
  print(this_table)
  duplicates[[this_table]] <- check_distinct(cdm_source_dataset, this_table)
}

str(duplicates)


