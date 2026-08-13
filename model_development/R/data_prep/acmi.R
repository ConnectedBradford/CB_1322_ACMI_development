# ACMI stuff
# install.packages("bigrquery")
# install.packages("tidyverse")   = Needed for API calls
library(tidyverse)

################################################################################
# Build all the SQL tables in Google Big Query to enable the ACMI analysis.
# Can run individual lines of code, or source the whole file to run. If sourcing
# and it fails then run rlang::last_error() to see what it failed on.
# The target_table_prefix will make new tables with the table prefix, so you can
# run the code multiple times without it overwriting any existing tables.
# Existing tables are deleted if they have the same prefix/table name.
################################################################################

# 18/1/23

# Kate Best - https://orcid.org/0000-0002-4663-7141
# Olly Butters - https://orcid.org/0000-0003-0354-8461

################################################################################
# PREREQUISITIES:
# User supplied tables in GBQ
# AC_Drugs table imported into GBQ from AC_medication_codes.csv
# eFI_SNOMED_2 table imported into GBQ
#
# User uploaded files in lists folder.
# selected_snomed_codes_delirium.csv  <- One column of numbers with a header
# selected_snomed_codes_falls.csv     <- One column of numbers with a header
# selected_ac_medications.csv         <- One column of words, no header. 
#                                        NOTE: this list must be a subset of whats in AC_Drugs table.
# eFI_deficits.csv                    <- One column of words, no header.

################################################################################
# Database settings
project <- "yhcr-prd-phm-bia-core"
cdm_source_dataset <- "CY_CDM_V1_KB_Subset"
# cdm_source_dataset <- "CY_CDM_V1_50k_Random"
fdm_source_dataset <- "CY_FDM_V1"
target_dataset <- "CY_MYSPACE_KB"
target_table_prefix <- "Bford_only_"                # e.g. 50k_ or v2_ or left blank
# Dates - format YYYY-MM-DD
project_start_date <- "2019-01-01"
project_end_date <- "2019-12-31"
################################################################################


################################################################################
# Load the lists of drugs and codes from file
# Get list of AC meds from file
ac_medications <- read.csv("~/acmi/lists/selected_ac_medications.csv", header = FALSE, sep = ",")
ac_medications <- unlist(ac_medications)
print(paste0(length(ac_medications), " ac medications loaded."))

# Get list of SNOMED codes for delirium from file
selected_snomed_codes_delirium <- read.csv("~/acmi/lists/selected_snomed_codes_delirium.csv", header = TRUE, sep = ",")
selected_snomed_codes_delirium <- unlist(selected_snomed_codes_delirium)
print(paste0(length(selected_snomed_codes_delirium), " snomed codes for delirium loaded."))

# Get list of SNOMED codes for falls from file
selected_snomed_codes_falls_v2 <- read.csv("~/acmi/lists/selected_snomed_codes_falls_v2.csv", header = TRUE, sep = ",")
selected_snomed_codes_falls_v2 <- unlist(selected_snomed_codes_falls_v2)
print(paste0(length(selected_snomed_codes_falls_v2), " snomed codes for falls loaded."))

#Get list of eFI deficits from file- can't get this to work in the code and not sure why- so 
eFI_deficits <- read.csv("~/acmi/lists/eFI_deficits.csv", header = FALSE, sep = ",")
eFI_deficits <- unlist(eFI_deficits)
print(paste0(length(eFI_deficits), " eFI_deficits loaded."))

################################################################################

library(bigrquery)
# First bq_* call should start a tidyverse authorisation where you'll get a key
# which you have to paste into the console below.

# Show project (should only be yhcr-prd-phm-bia-core)
bq_projects()

# List the datasets in the project. Results like: yhcr-prd-phm-bia-core.CY_MYSPACE_OB
bq_project_datasets(project)

# Make a dataset handle where all the new tables will be saved
dbh <- bq_dataset(project, target_dataset)

# All the SQL is stored as functions in separate files
source("~/acmi/R/data_prep/build_sql_bri_only.R")
source("~/acmi/R/data_prep/build_sql_drugs.R")
source("~/acmi/R/data_prep/build_sql_efi.R")
source("~/acmi/R/data_prep/build_sql_excl_PRNs.R")

# Can run any of the SQL functions to see what the generated SQL looks like, e.g.:
care_site()
visit_info()
delirium_condition()

################################################################################
# Delete the tables (example below)
# The tables are manually listed as you will only want to delete the
# auto-generated ones and not everything in your space.
################################################################################


# Can pick just one or two tables instead of all of them by doing:
# tables_to_delete <-c("AC_Drugs_2")

# Add the prefix to all of the tables
tables_to_delete <- paste0(target_table_prefix, tables_to_delete)

typeof(tables_to_delete)

for(this_table in tables_to_delete) {
  if (bq_table_exists(bq_table(dbh, this_table))) {
    print(paste0("Deleting table: ", this_table))
    bq_table_delete(bq_table(dbh, this_table))
  }
}

################################################################################
# Run the SQL queries. These wait until finished before moving on.
# build_sql_bri_only
################################################################################
# Get distinct care_sites.
tb <- bq_dataset_query(dbh, care_site())
print(paste0("care_site nrows: ", bq_table_nrow(tb)))

#Make person table for Bradford only (excl Airedale and any other area e.g. Leeds, Harrogate, Newcastle)
#Keep in those with missing LAD
tb <- bq_dataset_query(dbh, airedale_person_table())
print(paste0("airedale_person_table nrows: ", bq_table_nrow(tb)))

tb <- bq_dataset_query(dbh, bradford_airedale_person_table())
print(paste0("bradford_airedale_person_table: ", bq_table_nrow(tb)))

tb <- bq_dataset_query(dbh, bradford_person_table())
print(paste0("bradford_person_table: ", bq_table_nrow(tb)))

# Get over 65s who have no death recorded
tb <- bq_dataset_query(dbh, person_table_1())
print(paste0("person_table_1 nrows: ", bq_table_nrow(tb)))


#Get table of last visit occurrence
tb <- bq_dataset_query(dbh, last_visit())
print(paste0("last_visit nrows: ", bq_table_nrow(tb)))

#Remake person table excluding those with no visit in last 2 years prior to study start date
tb <- bq_dataset_query(dbh, person_table())
print(paste0("person_table nrows: ", bq_table_nrow(tb)))

#Remove >99
tb <- bq_dataset_query(dbh,person_table_delete_99())
print(paste0("person_table nrows: ", bq_table_nrow(tb)))

# Add site and type of site to every visit occurrence
# NOTE - THERE ARE NULL VALUES IN HERE
tb <- bq_dataset_query(dbh, visit_info())
print(paste0("visit_info nrows: ", bq_table_nrow(tb)))

################################################################################
# Delirium

# Everyone with delirium in hospital >65.

tb <- bq_dataset_query(dbh, delirium_condition())
print(paste0("delirium_condition nrows: ", bq_table_nrow(tb)))

# person_id, first delirium, 1?
tb <- bq_dataset_query(dbh, logistic_d())
print(paste0("logistic_d nrows: ", bq_table_nrow(tb)))

################################################################################
# Falls

# falls in hospital over 65s
tb <- bq_dataset_query(dbh, falls_condition())
print(paste0("falls_conditon nrows: ", bq_table_nrow(tb)))

# Falls procedures in hospital over 65s
tb <- bq_dataset_query(dbh, falls_procedure())
print(paste0("falls_procedure nrows: ", bq_table_nrow(tb)))

# Join falls_condition and falls_procedure.
tb <- bq_dataset_query(dbh, falls())
print(paste0("falls nrows: ", bq_table_nrow(tb)))

# person_id, first fall, 1?
#tb <- bq_dataset_query(dbh, logistic_f())
#print(paste0("logistic_f nrows: ", bq_table_nrow(tb)))

################################################################################
# Drugs (build_sql_drug)
################################################################################
tb <- bq_dataset_query(dbh, exposure_table())
print(paste0("exposure table nrows: ", bq_table_nrow(tb)))

tb <- bq_dataset_query(dbh, rxnorm_map())
print(paste0("rxnorm_map nrows: ", bq_table_nrow(tb)))

tb <- bq_dataset_query(dbh, ac_drugs())
print(paste0("ac_drugs nrows: ", bq_table_nrow(tb)))

tb <- bq_dataset_query(dbh, ac_exposure_table_snomed())
print(paste0("ac_exposure_table_snomed nrows: ", bq_table_nrow(tb)))

#tb <- bq_dataset_query(dbh, Drug_Amount_2())
#tb <- bq_dataset_query(dbh,Drug_Amount_2_STRING())
#tb <- bq_dataset_query(dbh,Drug_Amount_Total())

tb <- bq_dataset_query(dbh,Drug_Form_1())
tb <- bq_dataset_query(dbh,Drug_Form_2())
tb <- bq_dataset_query(dbh,Drug_Form_3())
tb <- bq_dataset_query(dbh,Drug_Form_4())
tb <- bq_dataset_query(dbh,Drug_Form_5())
tb <- bq_dataset_query(dbh,Drug_Form_6())
tb <- bq_dataset_query(dbh,Drug_Form_7())
tb <- bq_dataset_query(dbh,Drug_Form_8())
tb <- bq_dataset_query(dbh,Drug_Form_9())
tb <- bq_dataset_query(dbh,Drug_Form_10())
tb <- bq_dataset_query(dbh,Drug_Form_11())
tb <- bq_dataset_query(dbh,Daily_Drug_Amount())
tb<-bq_dataset_query(dbh,Daily_Form_Drop())

#Note this code takes a little while to run
for (this_drug in ac_medications) {
  tb <- bq_dataset_query(dbh, update_this_drug_dose(this_drug))
  print(paste0(this_drug, " doses updated"))
}

##############################
# eFI (build_sql_efi)
##############################
tb <- bq_dataset_query(dbh, efi_condition())
print(paste0("efi_condition nrows: ", bq_table_nrow(tb)))

tb <- bq_dataset_query(dbh, efi_observation())
print(paste0("efi_observation nrows: ", bq_table_nrow(tb)))

tb <- bq_dataset_query(dbh, efi_procedure())
print(paste0("efi_procedure nrows: ", bq_table_nrow(tb)))

tb <- bq_dataset_query(dbh, efi_measurement())
print(paste0("efi_measurement nrows: ", bq_table_nrow(tb)))

tb <- bq_dataset_query(dbh, efi_all())
print(paste0("efi_all nrows: ", bq_table_nrow(tb)))

tb <- bq_dataset_query(dbh,efi_all_primary()) 
print(paste0("efi_all_primary nrows: ", bq_table_nrow(tb)))

tb <- bq_dataset_query(dbh, efi_logistic())
print(paste0("efi_logistic nrows: ", bq_table_nrow(tb)))

tb <- bq_dataset_query(dbh, efi_logistic_collapsed())
print(paste0("efi_logistic_collapsed nrows: ", bq_table_nrow(tb)))

## This switches to SQL from build_dql_bri_only ###
tb <- bq_dataset_query(dbh, staging_months_create_table())
print(paste0("staging_months_create_table nrows: ", bq_table_nrow(tb)))

tb <- bq_dataset_query(dbh, staging_months_populate_table())
print(paste0("staging_months_populate_table nrows: ", bq_table_nrow(tb)))

tb <- bq_dataset_query(dbh, staging_monthdrug())
print(paste0("staging_month_drug nrows: ", bq_table_nrow(tb)))

tb <- bq_dataset_query(dbh, staging_people())
print(paste0("staging_people nrows: ", bq_table_nrow(tb)))

tb <- bq_dataset_query(dbh, staging_peopledrugs())
print(paste0("staging_peopledrugs nrows: ", bq_table_nrow(tb)))

tb <- bq_dataset_query(dbh, staging_month_efi())
print(paste0("staging_month_efi nrows: ", bq_table_nrow(tb)))

# tb <- bq_dataset_query(dbh, staging_month_efi_update())
# print(paste0("staging_month_efi_update nrows: ", bq_table_nrow(tb)))

tb <- bq_dataset_query(dbh, staging_people_efi())
print(paste0("staging_people_efi nrows: ", bq_table_nrow(tb)))

tb <- bq_dataset_query(dbh, staging_delirium())
print(paste0("staging_delirium nrows: ", bq_table_nrow(tb)))

tb <- bq_dataset_query(dbh, staging_falls())
print(paste0("staging_falls nrows: ", bq_table_nrow(tb)))

tb <- bq_dataset_query(dbh, cox_df())
print(paste0("cox_df nrows: ", bq_table_nrow(tb)))

tb <- bq_dataset_query(dbh, cox_df_update())
tb <- bq_dataset_query(dbh, cox_df_update_2())
tb <- bq_dataset_query(dbh, cox_df_update_3())

########################
#Run Excl PRNs code 
#build_sql_excl_PRNs 
########################
# All the SQL is stored as functions in separate file
tb <- bq_dataset_query(dbh, staging_monthdrug_PRN())
print(paste0("staging_monthdrug_PRN nrows: ", bq_table_nrow(tb)))

tb <- bq_dataset_query(dbh, staging_people_PRN())
print(paste0("staging_people_PRN nrows: ", bq_table_nrow(tb)))

tb <- bq_dataset_query(dbh, staging_peopledrugs_PRN())
print(paste0("staging_peopledrugs_PRN nrows: ", bq_table_nrow(tb)))

tb <- bq_dataset_query(dbh, cox_df_PRN())
print(paste0("cox_df_PRN nrows: ", bq_table_nrow(tb)))

tb <- bq_dataset_query(dbh, cox_df_update_PRN())
print(paste0("cox_df_update_PRN nrows: ", bq_table_nrow(tb)))

tb <- bq_dataset_query(dbh, cox_df_update_2_PRN())
print(paste0("cox_df_update_2_PRN nrows: ", bq_table_nrow(tb)))

tb <- bq_dataset_query(dbh, cox_df_update_3_PRN())
print(paste0("cox_df_update_3_PRN nrows: ", bq_table_nrow(tb)))

###############################
#Should be left with 3 analysis sets
#Analysis set excluding PRNs was the one used for the paper
################################
Cox_D_F_Update
Cox_D_F_Update_PRN

