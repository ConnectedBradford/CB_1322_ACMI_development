

exposure_table <- function() {
  ##############################################################################
  #EXPOSURE TABLE
  #link together select variables from drug_exposure, concept table, and limit to drugs prescribed in study period.
  # Note this table is any medication and is not yet restricted to AC medications only
  # limit to drug exposures 120 days or less to get rid of obviously incorrect exposure windows
  ##############################################################################
  sql <- str_c(
    "CREATE TABLE `",project,".",target_dataset,".",target_table_prefix,"Exposure_Table`
    AS
    SELECT DISTINCT
      d.person_id AS ID,
      d.visit_occurrence_id,
      d.drug_exposure_id,
      d.drug_concept_id,
      c.concept_name AS Drug_Concept_Name,
      d.drug_source_value,
      c.domain_id AS Drug_Domain_ID,
      c.vocabulary_id AS Drug_Vocabulary_ID,
      c.concept_code AS Drug_Concept_Code,
      d.drug_exposure_start_date,
      d.drug_exposure_end_date,
      DATE_DIFF( d.drug_exposure_end_date, d.drug_exposure_start_date, DAY ) AS Drug_Exposure_Period,
      d.sig,
      d.lot_number,
      CAST(d.drug_source_value AS INT64) AS drug_source_value_2,
      v.care_site_id,
      v.place_of_service_concept_id,
      v.place_of_service_concept_name,
      v.care_site_name
    FROM
      `",project,".",cdm_source_dataset,".drug_exposure` AS d
    LEFT JOIN
      `",project,".",cdm_source_dataset,".concept` AS c
      On d.drug_concept_id = c.concept_id
    LEFT JOIN
      `",project,".",target_dataset,".",target_table_prefix,"Vist_Info` AS v
      On v.visit_occurrence_id = d.visit_occurrence_id
    WHERE
      #d.drug_exposure_end_date BETWEEN DATE(\"",project_start_date,"\") AND DATE(\"",project_end_date,"\")
    (d.drug_exposure_end_date > DATE(\"",project_start_date,"\")and d.drug_exposure_start_date < DATE(\"",project_end_date,"\"))
    AND DATE_DIFF( d.drug_exposure_end_date, d.drug_exposure_start_date, DAY ) <=120 ;"
  )
  return(sql)
}


rxnorm_map <- function() {
  ##############################################################################
  # Mapping SNOMED to RxNorm ############
  # Make a table that links RxNorm codes to SNOMED codes
  # Note that prescriptions are coded as SNOMED/dm+d as their source values, but these are mapped to RxNorm in Connected #Bradford as the concept id’s
  # will use this table later to make sure I have all the RxNorm Codes for our AC meds that are in SNOMED/ dm+d
  ##############################################################################
  sql <- str_c(
    "CREATE TABLE `",project,".",target_dataset,".",target_table_prefix,"RxNorm_map`
    AS
    SELECT
      snomed.concept_code AS snomed_concept_code,
      snomed.concept_id AS snomed_omop_concept_id,
      snomed.concept_name AS snomed_concept_name,
      rxnorm.concept_code AS rxnorm_concept_code,
      rxnorm.concept_id AS rxnorm_omop_concept_id,
      rxnorm.concept_name AS rxnorm_concept_name
    FROM
      `",project,".",cdm_source_dataset,".concept` AS snomed
    inner join
      `",project,".",cdm_source_dataset,".concept_relationship` AS map
      ON snomed.concept_id = map.concept_id_1 AND map.relationship_id = 'Maps to' AND map.invalid_reason IS NULL
    inner join
      `",project,".",cdm_source_dataset,".concept` AS rxnorm
      ON map.concept_id_2 = rxnorm.concept_id
    WHERE
      rxnorm.domain_id = 'Drug'
    AND
      (snomed.vocabulary_id = 'SNOMED' OR snomed.vocabulary_id = 'dm+d' );"
  )
  return(sql)
}

ac_drugs <- function() {
  ##############################################################################
  # UPLOAD AC drug codes & link to RxNorm codes
  # upload ‘AC_Drugs’ then make new table with SNOMED codes as strings and integers (for merging)
  # Also link in the RxNorm concept codes for each AC Drug from RxNorm map above
  # Note:the paste0 builds ac.Alprozalam, ac.Alvernine, etc
  ##############################################################################
  sql <- str_c(
    "CREATE TABLE `",project,".",target_dataset,".",target_table_prefix,"AC_Drugs_2`
    AS
    SELECT DISTINCT
      CAST(ac.SNOMEDCode AS INT64) AS Code_2,
      CAST(ac.SNOMEDCode AS STRING) AS Code_3,
      ac.DMDProductDescription,
      ac.Drug,
      ac.VMPVMPPAMPAMPP, ",
      paste0("ac.",ac_medications, collapse = ", "),",
      ac.Dose1,
      ac.Dose2,
      ac.Dose3,
      ac.Dose4,
      r.rxnorm_concept_code,
      r.rxnorm_omop_concept_id,
      r.rxnorm_concept_name
    FROM
      `",project,".",target_dataset,".AC_Drugs_v2` AS ac
    LEFT JOIN
      `",project,".",target_dataset,".",target_table_prefix,"RxNorm_map` AS r
      ON r.snomed_concept_code=CAST(ac.SNOMEDCode AS STRING);"
  )
  return(sql)
}



ac_exposure_table_snomed <- function() {
  #############################################
  # AC Exposure table (only AC meds) with doses
  # Make dose variable for each med to be filled in later#
  # If statement doing:
  # IF lot_number looks like number_1 packs(s) of number_2
  # THEN Drug_Amount_Total = number_1 (AS INT64) * number_2 (AS INT64)
  # ELSE Drug_Amount_Total = number_1
  # NOTE: Dose not used in ACMI manuscript, but building foundations for future work
  ############################################
  sql <- str_c(
    "CREATE TABLE `",project,".",target_dataset,".",target_table_prefix,"AC_Exposure_Table_SNOMED`
    AS
    SELECT DISTINCT
      d.ID,
      d.visit_occurrence_id,
      d.drug_exposure_id,
      ac.Code_3,
      ac.Drug,
      ac.DMDProductDescription, ",
      paste0("ac.",ac_medications, collapse = ", "), ",
      ac.Dose1,
      ac.Dose2,
      ac.Dose3,
      ac.Dose4, ",
      paste0("0 AS Dose_",ac_medications, collapse = ", "), ",
      d.drug_concept_id,
      d.Drug_Concept_Name,
      d.drug_source_value,
      d.Drug_Domain_ID,
      d.Drug_Vocabulary_ID,
      d.Drug_Concept_Code,
      d.drug_exposure_start_date,
      d.drug_exposure_end_date,
      d.Drug_Exposure_Period,
      d.sig,
      d.lot_number,
      d.drug_source_value_2,
      d.care_site_id,
      d.place_of_service_concept_id,
      d.place_of_service_concept_name,
      d.care_site_name,
      DATE_TRUNC(CAST(d.drug_exposure_start_date AS date), Month) AS DrugStartMonth,
      DATE_TRUNC(CAST(d.drug_exposure_end_date   AS date), Month) AS DrugEndMonth ,
      cast(REGEXP_EXTRACT(d.lot_number,  r\"(\\d+)\") AS int64) AS Drug_Amount_1,
      cast(REGEXP_EXTRACT(d.lot_number, r\"pack[s]? of (\\d+)\") AS string) AS Drug_Amount_2,
      IF(REGEXP_CONTAINS(lot_number, r\"\\d+ pack[s]? of \\d+\"),
        CAST(REGEXP_EXTRACT(d.lot_number, r\"(\\d+) pack[s]? of \\d+\") AS INT64) *
        CAST(REGEXP_EXTRACT(d.lot_number, r\"\\d+ pack[s]? of (\\d+)\") AS INT64),
        CAST(REGEXP_EXTRACT(d.lot_number,  r\"(\\d+)\") AS int64)) AS Drug_Amount_Total,
      cast(NULL AS float64) AS Daily_Drug_Amount,
      cast(NULL AS string) AS Drug_Form,
      cast(NULL AS int64) AS Daily_Dose_mg,
      0 AS PRN
    FROM
      `",project,".",target_dataset,".",target_table_prefix,"Exposure_Table` AS d
    INNER JOIN
    `",project,".",target_dataset,".",target_table_prefix,"AC_Drugs_2` AS ac
      ON d.drug_source_value=ac.Code_3;"
  )
  return(sql)

}



########## Drug form updates ####################
# Map the Drug_Form to something if it is NULL

#CAPSULE OR TABLET
Drug_Form_1 <- function() {
  sql <- str_c(
    "UPDATE `",project,".",target_dataset,".",target_table_prefix,"AC_Exposure_Table_SNOMED`
    SET Drug_Form = 'Capsule or tablet'
    WHERE
        (lot_number like '%cap%' or lot_number like '%tab%' or lot_number like '%Tab%' )
    AND
        Drug_Form IS NULL;"
  )
  return(sql)
}

#PATCHES/ PLASTERS
Drug_Form_2 <- function() {
  sql <- str_c(
    "UPDATE `",project,".",target_dataset,".",target_table_prefix,"AC_Exposure_Table_SNOMED`
    SET Drug_Form = 'Patch/ plaster'
    WHERE 
      (lot_number like '%patch%'  or lot_number like '%plaster%')
    AND 
      Drug_Form IS NULL;"
  )
  return(sql)
}
#LOZENGE
Drug_Form_3 <- function() {
  sql <- str_c(
    "UPDATE `",project,".",target_dataset,".",target_table_prefix,"AC_Exposure_Table_SNOMED`
    set Drug_Form = 'Lozenge'
WHERE lot_number like '%lozenge%' AND  Drug_Form IS NULL
;"
  )
  return(sql)
}
#CREAM (to be excluded!)
Drug_Form_4 <- function() {
  sql <- str_c(
    "UPDATE `",project,".",target_dataset,".",target_table_prefix,"AC_Exposure_Table_SNOMED`
    set Drug_Form = 'Cream'
WHERE (lot_number like '%gram%'  or lot_number like '%tube%') AND Drug_Form IS NULL
;"
  )
  return(sql)
}
#ENEMA
Drug_Form_5 <- function() {
  sql <- str_c(
    "UPDATE `",project,".",target_dataset,".",target_table_prefix,"AC_Exposure_Table_SNOMED`
    set Drug_Form = 'Enema'
WHERE lot_number like '%enema%' AND Drug_Form IS NULL
;"
  )
  return(sql)
}
#ORAL SOLUTION
Drug_Form_6 <- function() {
  sql <- str_c(
    "UPDATE `",project,".",target_dataset,".",target_table_prefix,"AC_Exposure_Table_SNOMED`
    set Drug_Form = 'Oral solution'
WHERE (lot_number like '%ml%' or lot_number like '%millil%' ) AND Drug_Form IS NULL
;"
  )
  return(sql)
}
#SACHET
Drug_Form_7 <- function() {
  sql <- str_c(
    "UPDATE `",project,".",target_dataset,".",target_table_prefix,"AC_Exposure_Table_SNOMED`
    set Drug_Form = 'Sachet'
WHERE lot_number like '%sachet%' AND Drug_Form IS NULL
;"
  )
  return(sql)
}
#VIAL/ INJECTION/ AMPOULE
Drug_Form_8 <- function() {
  sql <- str_c(
    "UPDATE `",project,".",target_dataset,".",target_table_prefix,"AC_Exposure_Table_SNOMED`
    set Drug_Form = 'Vial/ injection/ ampoule'
WHERE (lot_number like '%vial%'  or lot_number like '%ampoule%' or lot_number like '%injection%'  or lot_number like '%syringe%') AND Drug_Form IS NULL
;"
  )
  return(sql)
}
#INHALER/ SPRAY
Drug_Form_9 <- function() {
  sql <- str_c(
    "UPDATE `",project,".",target_dataset,".",target_table_prefix,"AC_Exposure_Table_SNOMED`
set Drug_Form = 'Inhaler/ spray/ drops'
WHERE (lot_number like '%dose%' or lot_number like '%inhaler%'  or lot_number like '%spray%' ) AND Drug_Form IS NULL
;"
  )
  return(sql)
}
#SUPPOSITORY
  Drug_Form_10 <- function() {
    sql <- str_c(
      "UPDATE `",project,".",target_dataset,".",target_table_prefix,"AC_Exposure_Table_SNOMED`
     set Drug_Form = 'Suppository'
WHERE lot_number like '%supposit%' AND Drug_Form IS NULL
;"
    )
    return(sql)
  }


#NOT GIVEN
Drug_Form_11 <- function() {
  sql <- str_c(
    "UPDATE `",project,".",target_dataset,".",target_table_prefix,"AC_Exposure_Table_SNOMED`
    SET Drug_Form = 'Not given'
    WHERE 
      (
        lot_number like '%?%' 
        Or lot_number like '%none%' 
        or lot_number like '%short%' 
        or lot_number='0' 
        or lot_number like '%course%' 
        or lot_number like '%Course%' 
        or lot_number like '%unknown%' 
        or lot_number IS NULL
      ) 
    AND Drug_Form IS NULL;"
  )
  return(sql)
}

######Daily_Drug_Amount (for drugs/ capsules- others to follow!) ###########
Daily_Drug_Amount <- function() {
  sql <- str_c(
    "UPDATE `",project,".",target_dataset,".",target_table_prefix,"AC_Exposure_Table_SNOMED`
    Set
        Daily_Drug_Amount =  Drug_Amount_Total / Drug_Exposure_Period
    WHERE
        (Drug_Form = 'Capsule or tablet' or Drug_Form = 'Inhaler/ spray/ drops' or Drug_Form = 'Patch/ plaster') AND Drug_Exposure_Period>0;"
  )
  return(sql)
}


######Daily_Dose_mg ##############
#exclude suppositories, sachets, enema and lozenges from dose analysis as fairly infrequent and not accurate

Daily_Drug_mg <- function() {
  sql <- str_c(
    "UPDATE `",project,".",target_dataset,".",target_table_prefix,"AC_Exposure_Table_SNOMED`
     Set Daily_Dose_mg = safe_cast(Daily_Drug_Amount AS int64) * Dose1
WHERE Drug_Form = 'Capsule or tablet' or Drug_Form=  'Patch/ plaster' or Drug_Form='Inhaler/ spray/ drops'
;"
  )
  return(sql)
}


#DROP drug_Form =cream as creams are excluded
Daily_Form_Drop <- function() {
  sql <- str_c(
    "DELETE FROM `",project,".",target_dataset,".",target_table_prefix,"AC_Exposure_Table_SNOMED`
    WHERE Drug_Form = 'Cream';"
  )
  return(sql)
}

#######Tag the PRN meds ############
PRN <- function() {
  sql <- str_c(
    "UPDATE `",project,".",target_dataset,".",target_table_prefix,"AC_Exposure_Table_SNOMED`Set PRN= 1
WHERE sig like '%need%' or sig like '%Need%' or sig like '%NEED%'
or sig like '%require%' or sig like '%Require%' or sig like '%REQUIRE%'
or sig like '%prn%' or sig like '%Prn%' or sig like '%PRN%'
or sig like '%NECCESSARY%' or sig like '%Neccessary%' or sig like '%neccessary%'
;"
  )
  return(sql)
}


######Update individual ac med dose variables ##########
update_this_drug_dose <- function(this_drug) {
  sql <- str_c(
    "UPDATE
       `",project,".",target_dataset,".",target_table_prefix,"AC_Exposure_Table_SNOMED`
     SET
       Dose_",this_drug," = Daily_Dose_mg
     WHERE
       ",this_drug," = 1;"
  )

  #Dose_",this_drug," = Daily_Dose_mg

  # tidy the sql
  sql = gsub("\\s+", " ", sql)
  return(sql)
}







