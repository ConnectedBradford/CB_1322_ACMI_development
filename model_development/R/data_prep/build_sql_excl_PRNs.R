#In this code, we need to eliminate drugs tagged as PRNs from the AC_Exposure_Table and then rebuild the following:
#StagingMonthDrug, StagingPeople, Staging_People_Drugs, Cox_DF, Cox_DF_update

# Also produce eliminate PRNs from the data containing dose aswell 

library(tidyverse)


staging_monthdrug_PRN <- function() {
  ###############
  #MONTH DRUG #
  ###############
  # Recreate Staging_MonthDrug table but this time only include drugs that are NOT tagged as PRNs
  # Need to know which months the drugs were taken in (can’t just use start month as some drug prescribed over >1 month)
  # Note this table contains exposed not unexposed to ANY AC meds
  
  sql <- str_c(
    "CREATE TABLE `",project,".",target_dataset,".",target_table_prefix,"Staging_MonthDrug_PRN`
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
      ac.DrugEndMonth >= M.Month
    WHERE 
      ac.PRN=0 ;"
  )
  return(sql)
  #Note currently one row per drug not per person
  #But multiple rows for same prescription that occurred over multiple months
}



staging_people_PRN <- function() {
  ##############################################################################
  ####Staging People ##########
  #Make 12 rows per person- representing each of the 12 months of the study period
  # Remove months after a person dies
  # Join this to the Drug exposures tables created above
  # Table should now include those with AC meds and those without any (exposed and unexposed)
  # Note currently still an additional row if multiple drugs prescribed in same month
  ##############################################################################
  sql <- str_c(
    "CREATE TABLE `",project,".",target_dataset,".",target_table_prefix,"Staging_People_PRN`
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
      `",project,".",target_dataset,".",target_table_prefix,"Staging_MonthDrug_PRN` as ac
      ON 
        P.person_ID = ac.ID
      AND 
        ac.Month = M.Month
    ORDER BY 
      P.person_ID asc, 
      M.Month asc;"
  )
  return(sql)
}

staging_peopledrugs_PRN <- function() {
  ##############################################################################
  # Now collapse above
  # Note max shouldn’t change anything for gender, YOB, death, Age because these
  # should be the same where there are multiple rows per month
  ##############################################################################
  sql <- str_c(
    "CREATE TABLE `",project,".",target_dataset,".",target_table_prefix,"Staging_PeopleDrugs_PRN`
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
      `",project,".",target_dataset,".",target_table_prefix,"Staging_People_PRN` AS ac
    GROUP BY
      ac.person_ID,
      ac.Month
    ORDER BY
      ac.person_ID asc,
      ac.Month asc;"
  )
  return(sql)
}

cox_df_PRN <- function () {
  ####Link Falls, Delirium and eFI to Staging PeopleDrugs
  #Include those within same month of exposure
  #NOTE this could be imperfect as those with exposure on last day of month don't have a long exposure window...
  #  Hope that it averages out!
  
  sql <- str_c(
    "CREATE TABLE `",project,".",target_dataset,".",target_table_prefix,"Cox_DF_PRN` as
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
      `",project,".",target_dataset,".",target_table_prefix,"Staging_PeopleDrugs_PRN` AS ac
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

cox_df_update_PRN <- function () {
  ####Make composite variable for falls and delirium
  
  sql <- str_c(
    "UPDATE `",project,".",target_dataset,".",target_table_prefix,"Cox_DF_PRN`
SET D_F= 1 where Falls_Outcome=1 or Delirium=1;"
    )
  return(sql)
}

#Make new data that has min month of event for each person (then can drop rows within person where an event already occurred)
cox_df_update_2_PRN <- function () {
  sql <- str_c(
    "CREATE TABLE `",project,".",target_dataset,".",target_table_prefix,"minD_F_PRN` as
    SELECT 
      person_ID,
      min(Month) as Min_Month 
    FROM 
      `",project,".",target_dataset,".",target_table_prefix,"Cox_DF_PRN` 
    WHERE
      D_F=1
    GROUP BY
      person_ID;"
  )
  return(sql)
}

cox_df_update_3_PRN <- function () {
  sql <- str_c(
    "CREATE TABLE `",project,".",target_dataset,".",target_table_prefix,"Cox_D_F_Update_PRN` as
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
FROM
      `",project,".",target_dataset,".",target_table_prefix,"Cox_DF_PRN` as L
    LEFT JOIN 
      `",project,".",target_dataset,".",target_table_prefix,"minD_F_PRN` as R
    ON 
      L.person_ID=R.person_ID
    WHERE
      L.Month <= R.Min_Month
      OR 
      R.Min_Month is null
    ORDER BY 
      L.person_ID, L.Month;"
  )
  return(sql)
}
