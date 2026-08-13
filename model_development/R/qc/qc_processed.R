# Some QC functions to run manually

project <- "yhcr-prd-phm-bia-core"
cdm_source_dataset <- "CY_CDM_V1_KB_Subset"
target_dataset <- "CY_MYSPACE_OB"
target_table_prefix <- "v2_"



library(bigrquery)


################################################################################
# Check the concept codes are valid (according to dates defined). Should be zero.
################################################################################
check_valid_concept_codes <- function() {

  sql <- str_c(
    "SELECT *
    FROM `",project,".",target_dataset,".",target_table_prefix,"Delirium_Condition` AS del
    LEFT JOIN `",project,".",cdm_source_dataset,".concept` AS con
    ON del.concept_code = CAST(con.concept_id AS STRING)
    WHERE
    condition_start_date < valid_start_date OR
    condition_start_date > valid_end_date
    LIMIT 1000;"
  )

  return(sql)
}

check_valid_concept_codes()


################################################################################
# Checking for NULLs in the tables. Outputs a table of how many NULLs in each
# column in the table.
################################################################################
check_nulls <- function(this_dataset_name, this_table_name) {

  # Handle for this dataset
  dsh <- bq_dataset(project, this_dataset_name)

  # Get list of columns for this_table_name
  tbh <- bq_table(dsh, this_table_name)
  my_fields <- bq_table_fields(tbh)
  cols <- lapply(my_fields,'[[',"name")

  print(cols)

  all_nulls <- data_frame()
  for (this_col in cols) {

    sql <- str_c(
      "SELECT COUNT(*) AS num_null
      FROM `",project,".",this_dataset_name,".",this_table_name,"`
      WHERE ",this_col," IS NULL;"
    )

    # Run the query
    tb <- bq_dataset_query(dsh, sql)

    # Get the data
    temp <- bq_table_download(tb)
    this_nulls <- as.data.frame(temp[1,1])

    # Add it to the data frome
    all_nulls <- rbind(all_nulls, data.frame(cols=this_col, this_nulls))

    print(paste0(this_col, " nulls: ", this_nulls))
  }

  return(all_nulls)
}


temp <- check_nulls(target_dataset,"full_v1_eFI_All")
print(temp)

