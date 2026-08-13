library(tidyverse)

care_site <- function() {

  ##############################################################################
  # the care_site table
  ##############################################################################
  sql <- str_c(
    "CREATE TABLE `",project,".",target_dataset,".",target_table_prefix,"care_site_distinct`
    AS
    SELECT DISTINCT
      care_site_id,
      care_site_name,
      place_of_service_concept_id,
      location_id,
      care_site_source_value,
      place_of_service_source_value
    FROM
      `", project,".",cdm_source_dataset,".care_site`"
  )
  return(sql)
}


person_table <- function() {
  ##############################################################################
  # person_table
  #
  # Get everyone over 65 as of the specified date.
  # OB:
  # - OK.
  ##############################################################################
  sql <- str_c(
    "CREATE TABLE `",project,".",target_dataset,".",target_table_prefix,"person_table`
    AS
    SELECT
      person_id,
      gender_concept_id,
      date_diff(cast('",project_start_date,"' AS DATE),cast(birth_datetime AS DATE),YEAR) AS AgeJan19,
      year_of_birth,
      death_datetime,
      cast('",project_start_date,"' AS DATE) AS DateOfStudy
    FROM
      `", project,".",cdm_source_dataset,".person`
    WHERE
      (date_diff(cast('",project_start_date,"' AS date),cast(birth_datetime AS date),YEAR) > 64 AND death_datetime IS NULL) OR
      (date_diff(cast('",project_start_date,"' AS date),cast(birth_datetime AS date),YEAR) > 64 AND death_datetime > (cast('",project_start_date,"' AS date)));"
  )
  return(sql)
}

visit_info <- function() {

  ##############################################################################
  # Visit_Info
  #
  # Make table that links together visit occurrence id, care_site_id, place_of_service_concept_id/name
  # This table can be linked to condition/ procedure/ observation tables to identify if the events occurred as a hospital admission
  # Events=51591124 or 49677385 distinct (same as visit_occurrence table)
  #
  # OB:
  # - visit_occurrence needed vo.prefix and dropped co.condition_occurrence
  # - Get some NULLs in this
  # - The vo->cs join is 1:n, but the column selection and DISTICT collapses this to 1:1.
  # - Can confirm with:
  # SELECT * FROM `yhcr-prd-phm-bia-core.CY_MYSPACE_OB.care_site_distinct` WHERE care_site_id IN (
  # SELECT care_site_id FROM `yhcr-prd-phm-bia-core.CY_MYSPACE_OB.care_site_distinct`
  # GROUP BY care_site_id HAVING COUNT(care_site_id) > 1) ORDER BY care_site_id;
  #KB multiple care site source values for some care sites. so happy with distinct
  # NOTE: 7 visit occurences with no care site info
  ##############################################################################
  sql <- str_c(
    "CREATE TABLE `",project,".",target_dataset,".",target_table_prefix,"Vist_Info`
    AS
    SELECT DISTINCT
      vo.visit_occurrence_id,
      vo.care_site_id,
      cs.place_of_service_concept_id,
      c.concept_name AS place_of_service_concept_name,
      cs.care_site_name
    FROM
      `", project,".",cdm_source_dataset,".visit_occurrence` AS vo
    LEFT JOIN
      `", project,".",target_dataset,".",target_table_prefix,"care_site_distinct` AS cs
      ON vo.care_site_id=cs.care_site_id
    LEFT JOIN
      `", project,".",cdm_source_dataset,".concept` AS c
      ON cs.place_of_service_concept_id= c.concept_id"
  )
  return(sql)
}


delirium_condition <- function() {

  ##############################################################################
  # Delirium Condition Table
  #
  # Create table that restricts to hospital admission for delirium in over 65's in study period
  # n=11160
  #
  # OB:
  # - Moved list of SNOMED codes to a separate table (selected_snomed_codes)
  # - Added column of 1's in main CREATE TABLE statement
  # - Need to look at if start/end dates for concepts overlap with time period.
  # - Can check snomed codes with something like:
  # SELECT * FROM `yhcr-prd-phm-bia-core.CY_CDM_V1_50k_Random.concept` WHERE concept_id IN (SELECT snomed_code FROM yhcr-prd-phm-bia-core.CY_MYSPACE_OB.selected_snomed_codes) LIMIT 1000
  # Dropped: c.concept_id IN (SELECT snomed_code FROM ",project,".",target_dataset,".selected_snomed_codes_delirium)
  ##############################################################################

  sql <- str_c(
    "CREATE TABLE `",project,".",target_dataset,".",target_table_prefix,"Delirium_Condition`
    AS
    SELECT DISTINCT
      p.person_id,
      v.condition_occurrence_id,
      v.condition_concept_id,
      c.concept_name AS Condition_concept_name,
      v.condition_source_value,
      c.concept_code,
      v.condition_start_date,
      p.AgeJan19,
      cv.care_site_id,
      cv.care_site_name,
      1 AS Delirium
    FROM
      `",project,".",cdm_source_dataset,".condition_occurrence` AS v
    Left JOIN
      `",project,".",cdm_source_dataset,".concept` AS c
      On v.condition_concept_id = c.concept_id
    Left JOIN
      `", project,".",target_dataset,".",target_table_prefix,"person_table` AS p
      ON v.person_id=p.person_id
    LEFT JOIN
      `", project,".",target_dataset,".",target_table_prefix,"Vist_Info` AS cv
      ON cv.visit_occurrence_id=v.visit_occurrence_id
    WHERE
      c.concept_id IN (", paste0(selected_snomed_codes_delirium, collapse = ", "),")
      AND
      v.condition_start_date BETWEEN DATE(\"",project_start_date,"\") AND DATE(\"",project_end_date,"\")
      AND
      cv.place_of_service_concept_id=4263714;"

  )
  return(sql)
}

# delirium_procedure <- function() {
#   ##############################################################################
#   # Delirium Procedure & Observation Table
#   #
#   # No results found- Delirium all solely recorded in condition table
#   # OB - not used?
#   ##############################################################################
#   sql <- str_c(
#     "CREATE TABLE `",project,".",target_dataset,".",target_table_prefix,"Delirium_Procedure`
#     AS
#     WITH SelectedSnomedCodes AS
#     (SELECT 4296251 AS SnomedCode)
#     SELECT DISTINCT
#       p.person_id,
#       v.procedure_occurrence_id AS condition_occurrence_id,
#       v.procedure_concept_id AS condition_concept_id,
#       c.concept_name AS Condition_concept_name,
#       v.procedure_source_value AS condition_source_value,
#       c.concept_code,
#       v.procedure_date AS condition_start_date,
#       p.AgeJan19,
#       cv.care_site_id,
#       cv.care_site_name
#     FROM
#       `",project,".",cdm_source_dataset,".procedure_occurrence` AS v
#     Left JOIN
#       `",project,".",cdm_source_dataset,".concept` AS c
#       On v.procedure_concept_id = c.concept_id
#     Left JOIN
#       `",project,".",target_dataset,".",target_table_prefix,"person_table` AS p
#       On v.person_id = p.person_id
#     LEFT JOIN
#       `",project,".",target_dataset,".",target_table_prefix,"Vist_Info` AS cv
#       On cv.visit_occurrence_id = v.visit_occurrence_id
#     WHERE
#       c.concept_id IN ( SELECT SnomedCode FROM SelectedSnomedCodes )
#       AND
#         v.procedure_date BETWEEN DATE(\"",project_start_date,"\") AND DATE(\"",project_end_date,"\")
#       AND
#         place_of_service_concept_id=4263714;"
#   )
# }

logistic_d <- function () {
  #Should be 3807 people with a hospital admission for delirium
  sql <- str_c(
    "CREATE TABLE `",project,".",target_dataset,".",target_table_prefix,"Logistic_D`
    AS
    SELECT
      d.person_id,
      min(d.condition_start_date) AS Delirium_date,
      max(d.Delirium) AS Delirium
    FROM
      `",project,".",target_dataset,".",target_table_prefix,"Delirium_Condition` AS d
    GROUP BY
      d.person_id;"
  )
  return(sql)
}

# Falls



falls_condition <- function() {
  ##############################################################################
  # Falls Condition
  #
  # Create table the restricts to hospital admission for falls/ fracture in over 65's in study period
  # n=32,751
  # Note that raw codes used as at this point I didn’t know how to upload from Excel at the time!
  #
  # OB:
  # - Added SNOMED codes to a table (selected_snomed_codes_falls)
  # - NOTE: concept_code is not unique - there are multiple vocabulary_id in the table.
  #   Might need to add a qualifier below.
  # SELECT * FROM `yhcr-prd-phm-bia-core.CY_CDM_V1_50k_Random.concept` WHERE concept_code IN (SELECT snomed_code FROM yhcr-prd-phm-bia-core.CY_MYSPACE_OB.selected_snomed_codes_falls) AND vocabulary_id = "SNOMED" ORDER BY concept_code
  # dropped: #c.concept_code IN (SELECT snomed_code FROM ",project,".",target_dataset,".selected_snomed_codes_falls)
  ##############################################################################
  sql <- str_c(
    "CREATE TABLE `",project,".",target_dataset,".",target_table_prefix,"Falls_Condition`
    AS
    SELECT DISTINCT
      p.person_id,
      v.condition_occurrence_id,
      v.condition_concept_id,
      c.concept_name AS Condition_concept_name,
      v.condition_source_value,
      c.concept_code,
      v.condition_start_date,
      p.AgeJan19,
      v.visit_occurrence_id,
      cv.care_site_id,
      cv.care_site_name
    FROM
      `",project,".",cdm_source_dataset,".condition_occurrence` AS v
    Left JOIN
      `",project,".",cdm_source_dataset,".concept` AS c
      ON v.condition_concept_id = c.concept_id
    Left JOIN
      `",project,".",target_dataset,".",target_table_prefix,"person_table` AS p
      ON v.person_id = p.person_id
    LEFT JOIN
      `",project,".",target_dataset,".",target_table_prefix,"Vist_Info` AS cv
      ON cv.visit_occurrence_id = v.visit_occurrence_id
    WHERE
      c.concept_code IN (\"",paste0(selected_snomed_codes_falls, collapse = "\", \""),"\")
    AND
      c.vocabulary_id = 'SNOMED'
    AND
      v.condition_start_date BETWEEN DATE(\"",project_start_date,"\") AND DATE(\"",project_end_date,"\")
    AND
      cv.place_of_service_concept_id=4263714;"
  )

  # tidy the sql
  sql = gsub("\\s+", " ", sql)
  return(sql)
}


falls_procedure <- function() {
  ##############################################################################
  #Falls: Procedure
  #
  #Note changed variable names to condition (instead of procedure) so that can easily merge with Falls Condition later
  #n=814
  # c.concept_code IN (SELECT snomed_code FROM ",project,".",target_dataset,".selected_snomed_codes_falls)
  ##############################################################################
  sql <- str_c(
    "CREATE TABLE `",project,".",target_dataset,".",target_table_prefix,"Falls_Procedure`
    AS
    SELECT DISTINCT
      p.person_id,
      p.procedure_occurrence_id AS condition_occurrence_id,
      p.procedure_concept_id AS condition_concept_id,
      c.concept_name AS Condition_concept_name,
      p.procedure_source_value AS condition_source_value,
      c.concept_code,
      p.procedure_date AS condition_start_date,
      z.AgeJan19,
      p.visit_occurrence_id,
      cv.care_site_id,
      cv.care_site_name
    FROM
      `",project,".",cdm_source_dataset,".procedure_occurrence` AS p
    Left JOIN
      `",project,".",cdm_source_dataset,".concept` AS c
      ON p.procedure_concept_id = c.concept_id
    Left JOIN
      `",project,".",target_dataset,".",target_table_prefix,"person_table` AS z
      ON p.person_id = z.person_id
    LEFT JOIN
      `",project,".",target_dataset,".",target_table_prefix,"Vist_Info` AS cv
      ON cv.visit_occurrence_id = p.visit_occurrence_id
    WHERE
      c.concept_code IN (\"",paste0(selected_snomed_codes_falls, collapse = "\", \""),"\")
    AND
      c.vocabulary_id = 'SNOMED'
    AND
      p.procedure_date BETWEEN DATE(\"",project_start_date,"\") AND DATE(\"",project_end_date,"\")
    AND
      cv.place_of_service_concept_id=4263714;"
  )

  # tidy the sql
  sql = gsub("\\s+", " ", sql)
  return(sql)
}


falls <- function() {
  ###############################
  #Union falls condition and procedures#
  ###############################
  #Add the falls identified from the conditions table to the falls identified in the procedures table
  #n=33565 falls
  sql <- str_c(
    "CREATE TABLE `",project,".",target_dataset,".",target_table_prefix,"Falls` AS
    SELECT 1 AS Falls, *
      FROM `",project,".",target_dataset,".",target_table_prefix,"Falls_Condition`
    UNION DISTINCT
    SELECT 1 AS Falls, *
      FROM `",project,".",target_dataset,".",target_table_prefix,"Falls_Procedure`;"
  )
  return(sql)
}




##############################################
# FORMATTING FOR COX (MONTHS).
##############################################
#CREATE TABLE OF MONTHS#
########################
staging_months_create_table <- function() {
  sql <- str_c(
    "CREATE TABLE `",project,".",target_dataset,".",target_table_prefix,"Staging_Months`
    (Month DATE);"
  )
  return(sql)
}



staging_months_populate_table <- function() {

  dates <- seq(as.Date(project_start_date), as.Date(project_end_date), by="month")

  sql <- str_c(
    "INSERT INTO `",project,".",target_dataset,".",target_table_prefix,"Staging_Months`
    VALUES ",
    paste0("(\"", dates, "\")", collapse=","),";"
  )
  return(sql)
}


staging_monthdrug <- function() {
  ##############################################################################
  # MONTH DRUG 
  #
  #Need to know which months the drugs were taken in (can’t just use start month as some drug prescribed over >1 month)
  #Note this table contains exposed, but not unexposed to ANY AC meds
  ##############################################################################
  
  sql <- str_c(
    "CREATE TABLE `",project,".",target_dataset,".",target_table_prefix,"Staging_MonthDrug`
    AS
    SELECT DISTINCT
      ac.ID, ",
      paste0("ac.",ac_medications, collapse = ", "),",
      PRN, ",
      paste0("ac.Dose_",ac_medications, collapse = ", "),",
      M.Month
    FROM
      `",project,".",target_dataset,".",target_table_prefix,"AC_Exposure_Table_SNOMED` AS ac
    JOIN
      `",project,".",target_dataset,".",target_table_prefix,"Staging_Months` AS M
    ON
      ac.DrugStartMonth <= M.Month 
    AND
      ac.DrugEndMonth >= M.Month;"
  )
  return(sql)
  #Note currently one row per drug not per person
  #But multiple rows for same prescription that occurred over multiple months
}


staging_people <- function() {
  ##############################################################################
  ####Staging People ##########
  #Make 12 rows per person- representing each of the 12 months of the study period
  #Remove months after a person dies
  #Join this to the Drug exposures tables created above
  #Table should now include those with AC meds and those without any (exposed and unexposed)
  #Note currently still an additional row if multiple drugs prescribed in same month
  ##############################################################################
  sql <- str_c(
    "CREATE TABLE `",project,".",target_dataset,".",target_table_prefix,"Staging_People`
    AS
    SELECT
      P.person_ID,
      P.gender_concept_id,
      P.year_of_birth,
      P.death_datetime,
      P.AgeJan19,
      M.Month, ",
    paste0("COALESCE(ac.",ac_medications, ", 0) AS ", ac_medications, collapse = ", "),",
    ac.PRN
    FROM
      `",project,".",target_dataset,".",target_table_prefix,"person_table` AS P
    Join
      `",project,".",target_dataset,".",target_table_prefix,"Staging_Months` AS M
      ON COALESCE(P.death_datetime, cast(\"2100-01-01\" AS date)) >= M.Month
    left Join
      `",project,".",target_dataset,".",target_table_prefix,"Staging_MonthDrug` AS ac
      On P.person_ID = ac.ID
      And ac.Month = M.Month
    Order by P.person_ID asc
    , M.Month asc;"
  )
  return(sql)
}

staging_peopledrugs <- function() {
  ##############################################################################
  # Now collapse above
  # Note max shouldn’t change anything for gender, YOB, death, Age because these
  # should be the same where there are multiple rows per month
  ##############################################################################
  sql <- str_c(
    "CREATE TABLE `",project,".",target_dataset,".",target_table_prefix,"Staging_PeopleDrugs`
    AS
    SELECT
      ac.person_ID,
      max(ac.gender_concept_id) AS gender_concept_id,
      max(ac.year_of_birth) AS year_of_birth,
      max(ac.death_datetime) AS death_datetime,
      max(ac.AgeJan19) AS AgeJan19,
      ac.Month, ",
    paste0("COALESCE(MAX(ac.",ac_medications, "), 0) AS ", ac_medications, collapse = ", "),",
    FROM
      `",project,".",target_dataset,".",target_table_prefix,"Staging_People` AS ac
    GROUP BY
      ac.person_ID,
      ac.Month
    ORDER BY
      ac.person_ID asc,
      ac.Month asc;"
  )
  return(sql)
}

staging_month_efi <- function() {
  #Now need to link in info on outcomes and eFI to Staging_PeopleDrugs table
  
  #######################
  #Link in eFI deficits
  #######################

  sql <- str_c(
    "CREATE TABLE `",project,".",target_dataset,".",target_table_prefix,"Staging_Month_eFI` as
    SELECT Distinct
      efi.person_id,
      efi.start_date,   ",
      paste0("efi.",eFI_deficits, collapse = ", "), ",
      M.Month
    FROM
      `",project,".",target_dataset,".",target_table_prefix,"eFI_logistic` as efi
    JOIN
      `",project,".",target_dataset,".",target_table_prefix,"Staging_Months` as M
      On efi.start_date <= M.Month ;"
  )
  return(sql)
}


staging_people_efi <- function () {
  sql <- str_c(
    "CREATE TABLE `",project,".",target_dataset,".",target_table_prefix,"Staging_People_eFI` as
    SELECT
      P.person_id,
      max(P.gender_concept_id) as gender_concept_id,
      max(P.year_of_birth) as year_of_birth,
      max(P.death_datetime) as death_datetime,
      max(P.AgeJan19) as AgeJan19, ",
      paste0("COALESCE(max(efi.",eFI_deficits, "), 0) AS ", eFI_deficits, collapse = ", "),  ",
      M.Month
    FROM
      `",project,".",target_dataset,".",target_table_prefix,"person_table` as P
    Join
      `",project,".",target_dataset,".",target_table_prefix,"Staging_Months` as M
      on COALESCE(P.death_datetime, cast(\"2100-01-01\" as date)) >= M.Month
    left Join
      `",project,".",target_dataset,".",target_table_prefix,"Staging_Month_eFI` as efi
      ON P.person_ID = efi.person_id
      AND efi.Month = M.Month
    Group by
      P.person_id, M.Month
    Order by
      P.person_id asc, M.Month asc;"
  )
  return(sql)
}

staging_delirium <- function () {
  ######Delirium#########
  #Make month of delirium & select distinct so we only record one delirium episode per month
  
  sql <- str_c(
    "CREATE TABLE `",project,".",target_dataset,".",target_table_prefix,"Staging_Delirium` AS
  SELECT distinct
    person_id as PersonID,
    Delirium,
    DATE_TRUNC(CAST(condition_start_date as date), Month) as DeliriumStartMonth
  FROM
    `",project,".",target_dataset,".",target_table_prefix,"Delirium_Condition`;"
  )
  return(sql)
}

staging_falls <- function() {
  ######Falls#########
  
  #Make month of Falls removing those with multiple episodes per month
  sql <- str_c(
    "CREATE TABLE `",project,".",target_dataset,".",target_table_prefix,"Staging_Falls` AS
    SELECT distinct
      person_id as PersonID,
      Falls,
      DATE_TRUNC(CAST(condition_start_date as date), Month) as FallStartMonth
    FROM `",project,".",target_dataset,".",target_table_prefix,"Falls`;"
  )
  return(sql)
}

cox_df <- function () {
  ####Link Falls, Delirium and eFI to Staging PeopleDrugs
  #Include those within same month of exposure
  #NOTE this could be imperfect as those with exposure on last day of month don't have a long exposure window...
  #  Hope that it averages out!
  
  sql <- str_c(
    "CREATE TABLE `",project,".",target_dataset,".",target_table_prefix,"Cox_DF` as
    SELECT
      ac.person_ID,
      ac.Month,
      ac.gender_concept_id,
      ac.year_of_birth,
      ac.AgeJan19 , ",
    paste0("ac.",ac_medications, collapse = ", "), ",
      ac.death_datetime,     ",
    paste0("D.",eFI_deficits, collapse = ", "), ",
      COALESCE( B.Delirium , 0 ) as Delirium,
      COALESCE( C.Falls , 0 ) as Falls_Outcome, 
      0 as D_F
    FROM
      `",project,".",target_dataset,".",target_table_prefix,"Staging_PeopleDrugs` AS ac
    LEFT JOIN
      `",project,".",target_dataset,".",target_table_prefix,"Staging_Delirium` AS B
      ON ac.person_ID = B.PersonID
      AND ac.Month = B.DeliriumStartMonth
    LEFT JOIN
      `",project,".",target_dataset,".",target_table_prefix,"Staging_Falls` AS C
      ON ac.person_ID = C.PersonID
      AND ac.Month = C.FallStartMonth
    LEFT JOIN
      `",project,".",target_dataset,".",target_table_prefix,"Staging_People_eFI` AS D
      ON ac.person_ID=D.person_ID
      AND ac.Month = D.Month
    ORDER BY
      ac.person_ID asc,
      ac.Month asc;"
  )
  return(sql)
}

cox_df_update <- function () {
  ####Make composite variable for falls and delirium
  
  sql <- str_c(
    "UPDATE `",project,".",target_dataset,".",target_table_prefix,"Cox_DF`
SET D_F= 1 where Falls_Outcome=1 or Delirium=1;"
)
return(sql)
}

#Make new data that has min month of event for each person (then can drop rows within person where an event already occurred)
cox_df_update_2 <- function () {
  sql <- str_c(
    "CREATE TABLE `",project,".",target_dataset,".",target_table_prefix,"minD_F` as
SELECT 
 person_ID,
  min(Month) as Min_Month 
 FROM `",project,".",target_dataset,".",target_table_prefix,"Cox_DF` 
 where D_F=1
 group by  person_ID;"
  )
  return(sql)
}

cox_df_update_3 <- function () {
  sql <- str_c(
    "CREATE TABLE `",project,".",target_dataset,".",target_table_prefix,"Cox_D_F_Update` as
SELECT 
L.person_ID, 
L.Month,
L.gender_concept_id,
L.year_of_birth, 
L.AgeJan19, ",
paste0("L.",ac_medications, collapse = ", "), ",
L.death_datetime, ",
paste0("L.",eFI_deficits, collapse = ", "), ",
L.Delirium, 
L.Falls_Outcome, 
L.D_F
  FROM `",project,".",target_dataset,".",target_table_prefix,"Cox_DF` as L
Left join `",project,".",target_dataset,".",target_table_prefix,"minD_F` as R
on L.person_ID=R.person_ID
Where L.Month <= R.Min_Month
Or R.Min_Month is null
order by L.person_ID, L.Month;"
  )
  return(sql)
}