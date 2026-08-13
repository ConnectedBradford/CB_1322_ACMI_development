#In this code, we will incorporate doses from the AC_Exposure_Table and then rebuild the following:
#StagingMonthDrug, StagingPeople, Staging_People_Drugs, Cox_DF, Cox_DF_update

staging_monthdrug_Dose <- function() {
  ##############################################################################
  # MONTH DRUG 
  # Need to know which months the drugs were taken in (can’t just use start month as some drug prescribed over >1 month)
  # Note this table contains exposed not unexposed to ANY AC meds
  # INNER JOIN.
  ##############################################################################

  sql <- str_c(
    "CREATE TABLE `",project,".",target_dataset,".",target_table_prefix,"Staging_MonthDrug_Dose`
    AS
    SELECT DISTINCT
      ac.ID, ",
      paste0("ac.Dose_",ac_medications, collapse = ", "),",
      ac.Daily_Dose_mg,
      ac.PRN,
      M.Month
    FROM
      `",project,".",target_dataset,".",target_table_prefix,"AC_Exposure_Table_SNOMED` AS ac
    JOIN
      `",project,".",target_dataset,".",target_table_prefix,"Staging_Months` AS M
    ON
      ac.DrugStartMonth <= M.Month AND
      ac.DrugEndMonth >= M.Month;"
  )
  return(sql)
}


staging_people_Dose <- function() {
  ##############################################################################
  # Staging People 
  # Make 12 rows per person- representing each of the 12 months of the study period
  # Remove months after a person dies
  # Join this to the Drug exposures tables 
  # Table should now include those with AC meds and those without any (exposed and unexposed)
  # Note currently still an additional row if multiple drugs prescribed in same month
  ##############################################################################
  sql <- str_c(
    "CREATE TABLE `",project,".",target_dataset,".",target_table_prefix,"Staging_People_Dose`AS
    SELECT
      P.person_ID,
      P.gender_concept_id,
      P.year_of_birth,
      P.death_datetime,
      P.AgeJan19,
      M.Month, ",
      paste0("COALESCE(ac.Dose_",ac_medications, ", 0) AS Dose_", ac_medications, collapse = ", "),",   
      ac.PRN
    FROM
      `",project,".",target_dataset,".",target_table_prefix,"person_table` AS P
    Join
      `",project,".",target_dataset,".",target_table_prefix,"Staging_Months` AS M
    ON 
      COALESCE(P.death_datetime, cast(\"2100-01-01\" as date)) >= M.Month
    LEFT JOIN
      `",project,".",target_dataset,".",target_table_prefix,"Staging_MonthDrug_Dose` as ac
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

staging_peopledrugs_Dose <- function() {
  ##############################################################################
  # Now collapse above
  # Note max shouldn’t change anything for gender, YOB, death, Age because these
  # should be the same where there are multiple rows per month
  ##############################################################################
  sql <- str_c(
    "CREATE TABLE `",project,".",target_dataset,".",target_table_prefix,"Staging_PeopleDrugs_Dose`
    AS
    SELECT
      ac.person_ID,
      max(ac.gender_concept_id) as gender_concept_id,
      max(ac.year_of_birth) as year_of_birth,
      max(ac.death_datetime) as death_datetime,
      max(ac.AgeJan19) as AgeJan19,
      ac.Month, ",
    paste0("COALESCE(MAX(ac.Dose_",ac_medications, "), 0) AS Dose_", ac_medications, collapse = ", "),",
  FROM
  `",project,".",target_dataset,".",target_table_prefix,"Staging_People_Dose` AS ac
  GROUP BY
  ac.person_ID,
  ac.Month
  ORDER BY
  ac.person_ID asc,
  ac.Month asc;"
  )
  return(sql)
}

cox_df_Dose <- function () {
  ##############################################################################
  # Link Falls, Delirium and eFI to Staging PeopleDrugs
  ##############################################################################
  
  sql <- str_c(
    "CREATE TABLE `",project,".",target_dataset,".",target_table_prefix,"Cox_DF_Dose` as
    SELECT
      ac.person_ID,
      ac.Month,
      ac.gender_concept_id,
      ac.year_of_birth,
      ac.death_datetime,
      ac.AgeJan19 , ",
      paste0("ac.Dose_",ac_medications, collapse = ", "), ",",
      paste0("D.",eFI_deficits, collapse = ", "), ",
      COALESCE( B.Delirium , 0 ) as Delirium,
      COALESCE( C.Falls , 0 ) as Falls_Outcome,
      0 as D_F
    FROM
      `",project,".",target_dataset,".",target_table_prefix,"Staging_PeopleDrugs_Dose` AS ac
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

cox_df_update_Dose <- function () {
  ##############################################################################
  # Make composite variable for falls and delirium
  ##############################################################################

  sql <- str_c(
    "UPDATE `",project,".",target_dataset,".",target_table_prefix,"Cox_DF_Dose`
    SET D_F = 1 
    WHERE 
      Falls_Outcome=1 
      OR 
      Delirium=1;"
  )
  return(sql)
}


cox_df_update_2_Dose <- function () {
  ##############################################################################
  # Make new data that has min month of event for each person (then can drop rows within person where an event already occurred)
  ##############################################################################

  sql <- str_c(
    "CREATE TABLE `",project,".",target_dataset,".",target_table_prefix,"minD_F_Dose` as
    SELECT 
      person_ID,
      min(Month) as Min_Month 
    FROM 
      `",project,".",target_dataset,".",target_table_prefix,"Cox_DF_Dose` 
    WHERE D_F=1
    GROUP BY person_ID;"
  )
  return(sql)
}

cox_df_update_3_Dose <- function () {

  sql <- str_c(
    "CREATE TABLE `",project,".",target_dataset,".",target_table_prefix,"Cox_D_F_Update_Dose` as
    SELECT 
      L.person_ID, 
      L.Month,
      L.gender_concept_id,
      L.year_of_birth, 
      L.death_datetime, 
      L.AgeJan19, ",
      paste0("L.Dose_", ac_medications, collapse = ", "), ",",
      paste0("L.",eFI_deficits, collapse = ", "), ",
      L.Falls_Outcome, 
      L.D_F
    FROM 
      `",project,".",target_dataset,".",target_table_prefix,"Cox_DF_Dose` as L
    LEFT JOIN 
      `",project,".",target_dataset,".",target_table_prefix,"minD_F_Dose` as R
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