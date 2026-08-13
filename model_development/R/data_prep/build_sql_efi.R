####################################################################################
# eFI deficits                                                                     #
# Extract separately from Condition, Procedure, Measurement, Observation table     #
# Combine to make eFI_All table                                                    #
# Combine with person table and restrict time of diagnosis in eFI_logistic         #
# Collapse eFI_logistic to make one row per person with binary deficit variables   # 
# Only primary care deficits                                                       #
####################################################################################

# Extract from condition table those with relevant eFI codes. Exclude those falling outside 5 year time window where appropriate. 
# Haven't yet excluded based on age window though as not linked to person table yet
# Note final join is an inner join so we only have those with an eFI code.
efi_condition <- function() {
  
  #Condition
  sql <- str_c(
    "CREATE TABLE `",project,".",target_dataset,".",target_table_prefix,"eFI_Condition` as
    SELECT distinct
      a.person_id as ID,
      a.condition_concept_id as concept_id,
      a.condition_start_date as start_date,
      a.condition_source_value as source_value,
      a.visit_occurrence_id,
      b.concept_code,
      c.Deficit,
      c.CTV3Description,
      c.SCT_CONCEPTID , ",
      paste0("c.",eFI_deficits, collapse = ", "), ",
      c.TimeConstraint,
      c.TimeConstraint2,
      c.Rule
    FROM
      `",project,".",cdm_source_dataset,".condition_occurrence` as a
    LEFT JOIN
      `",project,".",cdm_source_dataset,".concept` as b
      On a.condition_concept_id = b.concept_id
    LEFT JOIN
      `",project,".",cdm_source_dataset,".person` as p
      On a.person_id=p.person_id
    INNER JOIN
      `",project,".",target_dataset,".eFI_SNOMED_codes_v2` as c
      On b.concept_code = CAST(c.SCT_CONCEPTID AS STRING)
    WHERE
      a.condition_start_date  <= '",project_start_date,"'
      AND
      (
        (c.TimeConstraint is Null) 
        OR
        (c.TimeConstraint = '5 years' AND   a.condition_start_date > '2014-01-01') 
        OR
        (c.TimeConstraint = 'Age >18' AND (date_diff(cast(a.condition_start_date AS date),cast(p.birth_datetime AS date),YEAR) > 18))
      )  ;"
  )
  # tidy the sql
  sql = gsub("\\s+", " ", sql)
  return(sql)
}

efi_observation <- function() {
  #Observation
  sql <- str_c(
    "CREATE TABLE `",project,".",target_dataset,".",target_table_prefix,"eFI_Observation` as
    SELECT distinct
      a.person_id as ID,
      a.observation_concept_id as concept_id,
      a.observation_date as start_date,
      a.observation_source_value as source_value,
      a.visit_occurrence_id,
      b.concept_code,
      c.Deficit,
      c.CTV3Description,
      c.SCT_CONCEPTID, ",
      paste0("c.",eFI_deficits, collapse = ", "), ",
      c.TimeConstraint,
      c.TimeConstraint2,
      c.Rule
    FROM
      `",project,".",cdm_source_dataset,".observation` as a
    LEFT JOIN
      `",project,".",cdm_source_dataset,".concept` as b
      ON a.observation_concept_id = b.concept_id
    LEFT JOIN
     `",project,".",cdm_source_dataset,".person` as p
      ON a.person_id=p.person_id
    INNER JOIN
      `",project,".",target_dataset,".eFI_SNOMED_codes_v2` as c
      On b.concept_code = CAST(c.SCT_CONCEPTID AS STRING)
    WHERE
      a.observation_date  <= '",project_start_date,"'
      AND
      (
        (c.TimeConstraint is Null) 
        OR
        (c.TimeConstraint = '5 years' AND a.observation_date > '2014-01-01')
        OR
        (c.TimeConstraint = 'Age >18' AND (date_diff(cast(a.observation_date AS date),cast(p.birth_datetime AS date),YEAR) > 18))
      );"
  )
  return(sql)
}

efi_procedure <- function() {
  #Procedure
  sql <- str_c(
    "CREATE TABLE `",project,".",target_dataset,".",target_table_prefix,"eFI_Procedure` as
    SELECT distinct
      a.person_id as ID,
      a.procedure_concept_id as concept_id,
      a.procedure_date as start_date,
      a.procedure_source_value as source_value,
      a.visit_occurrence_id,
      b.concept_code,
      c.Deficit,
      c.CTV3Description,
      c.SCT_CONCEPTID, ",
      paste0("c.",eFI_deficits, collapse = ", "), ",
      c.TimeConstraint,
      c.TimeConstraint2,
      c.Rule
    FROM
      `",project,".",cdm_source_dataset,".procedure_occurrence` as a
    LEFT JOIN
      `",project,".",cdm_source_dataset,".concept` as b
      On a.procedure_concept_id = b.concept_id
    LEFT JOIN
     `",project,".",cdm_source_dataset,".person` as p
      On a.person_id=p.person_id
    INNER JOIN
      `",project,".",target_dataset,".eFI_SNOMED_codes_v2` as c
      On b.concept_code = CAST(c.SCT_CONCEPTID AS STRING)
    WHERE
      a.procedure_date  <= '",project_start_date,"'
      AND
      (
        (c.TimeConstraint is Null) 
        OR
        (c.TimeConstraint = '5 years' AND a.procedure_date > '2014-01-01') 
        OR
        (c.TimeConstraint = 'Age >18' AND (date_diff(cast(a.procedure_date AS date),cast(p.birth_datetime AS date),YEAR) > 18))
      );"
  )
  return(sql)
}


efi_measurement <- function() {
  #Procedure
  sql <- str_c(
    "CREATE TABLE `",project,".",target_dataset,".",target_table_prefix,"eFI_Measurement` as
    SELECT distinct
      a.person_id as ID,
      a.measurement_concept_id as concept_id,
      a.measurement_date as start_date,
      a.measurement_source_value as source_value,
      a.visit_occurrence_id,
      b.concept_code,
      c.Deficit,
      c.CTV3Description,
      c.SCT_CONCEPTID, ",
      paste0("c.",eFI_deficits, collapse = ", "), ",
      c.TimeConstraint,
      c.TimeConstraint2,
      c.Rule
    FROM
      `",project,".",cdm_source_dataset,".measurement` as a
    LEFT JOIN
      `",project,".",cdm_source_dataset,".concept` as b
      ON a.measurement_concept_id = b.concept_id
    LEFT JOIN
     `",project,".",cdm_source_dataset,".person` as p
      ON a.person_id=p.person_id
    LEFT JOIN
      `",project,".",target_dataset,".eFI_SNOMED_codes_v2` as c
      ON b.concept_code = CAST(c.SCT_CONCEPTID AS STRING)
    WHERE
      a.measurement_date  <= '",project_start_date,"'
      AND 
      (
        (c.Rule='<=18' AND a.value_as_number<=18) OR
        (c.Rule='M:13-18, F:11.5-16.5 Normal' AND a.value_as_number<13) OR
        (c.Rule='M:13-18, F:11.5-16.5 Normal' AND a.value_as_number>25 AND a.value_as_number<130) OR
        (c.Rule='>0'    AND a.value_as_number>0 )    OR
        (c.Rule='>=85'  AND a.value_as_number>=85)   OR
        (c.Rule='>=135' AND a.value_as_number>=135 ) OR
        (c.Rule='>=8'   AND a.value_as_number>=8)    OR
        (c.Rule='<-2.5' AND a.value_as_number<-2.5)  OR
        (c.Rule='<0.95' AND a.value_as_number<0.95)  OR
        (c.Rule='>=1'   AND a.value_as_number>=1)    OR
        (c.Rule='<=60'  AND a.value_as_number<=60)  OR
        (c.Rule='>=1'   AND a.value_as_number>=1 )   OR
        (c.Rule='<150mg/24hr'     AND a.value_as_number <150 ) OR
        (c.Rule='<20mg/24hr'      AND a.value_as_number <20  ) OR
        (c.Rule='<3'              AND a.value_as_number <3  ) OR
        (c.Rule='<50'             AND a.value_as_number <50  ) OR
        (c.Rule='0.35-5.5 Normal' AND (a.value_as_number <0.35 OR a.value_as_number >5.5))
      );"
  )
  return(sql)
}

efi_all <- function() {
  sql <- str_c(
    "CREATE TABLE `",project,".",target_dataset,".",target_table_prefix,"eFI_All` as
    SELECT
      *
    FROM
      `",project,".",target_dataset,".",target_table_prefix,"eFI_Procedure`
    UNION DISTINCT
    SELECT
      *
    FROM
      `",project,".",target_dataset,".",target_table_prefix,"eFI_Condition`
     UNION DISTINCT
    SELECT
      *
    FROM
      `",project,".",target_dataset,".",target_table_prefix,"eFI_Measurement`;"
  )
  return(sql)
}

#Link in visit table to above then update to drop all of those in secondary care (only want primary)
efi_all_primary<- function() {
  sql <- str_c(
    "CREATE TABLE `",project,".",target_dataset,".",target_table_prefix,"eFI_All_primary` as 
    SELECT  a.ID,
      a.concept_id,
      a.start_date,
      a.source_value,
      a.visit_occurrence_id,
      a.concept_code,
      a.Deficit,
      a.CTV3Description,
      a.SCT_CONCEPTID, ",
    paste0("a.",eFI_deficits, collapse = ", "), ",
      a.TimeConstraint,
      a.TimeConstraint2,
      a.Rule,
      cv.place_of_service_concept_id
   FROM   `",project,".",target_dataset,".",target_table_prefix,"eFI_All` as a
    LEFT JOIN
      `", project,".",target_dataset,".",target_table_prefix,"Vist_Info` as cv
      ON cv.visit_occurrence_id=a.visit_occurrence_id 
    WHERE cv.place_of_service_concept_id !=4263714;"
  )
  return(sql)
}


efi_logistic <- function() {
    sql <- str_c(
    "CREATE TABLE `",project,".",target_dataset,".",target_table_prefix,"eFI_logistic` as
    SELECT distinct
      c.concept_id,
      c.start_date,
      c.source_value,
      c.concept_code,
      c.Deficit,
      c.visit_occurrence_id,
      c.CTV3Description,
      c.SCT_CONCEPTID, ",
      paste0("c.",eFI_deficits, collapse = ", "), ",
      c.TimeConstraint,
      c.TimeConstraint2,
      p.person_id
    FROM
      `",project,".",target_dataset,".",target_table_prefix,"person_table` as p
    LEFT JOIN
      `",project,".",target_dataset,".",target_table_prefix,"eFI_All_primary` as c
    ON c.ID= p.person_id;"
  )
  return(sql)
}


efi_logistic_collapsed <- function () {
  sql <- str_c(
    #NOTE The Falls outcome is later renamed Falls_Outcome
    
    "CREATE TABLE `",project,".",target_dataset,".",target_table_prefix,"eFI_logistic_collapsed` as
    SELECT 
      c.person_id, ",
      paste0("COALESCE(max(c.",eFI_deficits, "), 0) AS ", eFI_deficits, collapse = ", "),",
    FROM
      `",project,".",target_dataset,".",target_table_prefix,"eFI_logistic` as c
    GROUP BY
      c.person_id;"
  )
  return(sql)
}