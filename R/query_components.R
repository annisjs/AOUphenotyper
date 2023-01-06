cpt_query <- function(dataset,cpt_codes)
{
  cpt_terms <- paste('c.CONCEPT_CODE LIKE ',"'",cpt_codes,"'",collapse=' OR ',sep="")
  query <- str_glue("
    SELECT DISTINCT p.person_id,c.CONCEPT_CODE AS cpt_code,p.PROCEDURE_DATE AS entry_date
    FROM
        {dataset}.concept c,
        {dataset}.procedure_occurrence p
        WHERE
        c.VOCABULARY_ID like 'CPT4' AND
        c.CONCEPT_ID = p.PROCEDURE_SOURCE_CONCEPT_ID AND
        ({cpt_terms})
    ")
  download_data(query)
}

high_bp_query <- function(dataset)
{
  dia_concept_query <- str_glue("
    SELECT c.concept_id
    FROM {dataset}.concept c
    WHERE lower(c.concept_name) LIKE '%diastolic blood pressure%'
    ")
  dia_codes <- download_data(dia_concept_query)$concept_id
  dia_codes <- paste(dia_codes,sep="",collapse=",")
  sys_concept_query <- str_glue("
    SELECT c.concept_id
    FROM {dataset}.concept c
    WHERE lower(c.concept_name) LIKE '%systolic blood pressure%'
    ")
  sys_codes <- download_data(sys_concept_query)$concept_id
  sys_codes <- paste(sys_codes,sep="",collapse=",")

  bp_query <- str_glue("
    WITH diatb AS (SELECT
        person_id, measurement_datetime, value_as_number AS dia
        FROM `{dataset}.measurement` m
    WHERE
        m.measurement_concept_id IN ({dia_codes})),
    systb AS (SELECT
        person_id, measurement_datetime, value_as_number AS sys
        FROM `{dataset}.measurement` m
    WHERE
        m.measurement_concept_id IN ({sys_codes}))
    SELECT d.person_id, MIN(CAST(d.measurement_datetime AS DATE)) AS measurement_date
    FROM
    diatb d
    INNER JOIN systb s
    ON (d.person_id = s.person_id)
    WHERE
    d.measurement_datetime = s.measurement_datetime
    AND sys >= 140
    AND dia >= 90
    GROUP BY d.person_id
    ")
  result <- download_data(bp_query)
  return(result)
}

icd9_query <- function(dataset,icd9_codes)
{
  icd9_terms <- paste('co.CONDITION_SOURCE_VALUE LIKE ',"'",icd9_codes,"'",collapse=' OR ',sep="")
  query <-str_glue("
    SELECT DISTINCT co.person_id, co.condition_start_date,co.condition_source_value
    FROM
        {dataset}.condition_occurrence co
        INNER JOIN
        {dataset}.concept c
        ON (co.condition_source_concept_id = c.concept_id)
    WHERE
        c.VOCABULARY_ID LIKE 'ICD9CM' AND
        ({icd9_terms})
    ")
  download_data(query)
}

icd10_query <- function(dataset,icd10_codes)
{
  icd10_terms <- paste('co.CONDITION_SOURCE_VALUE LIKE ',"'",icd10_codes,"'",collapse=' OR ',sep="")
  query <-  str_glue("
        SELECT DISTINCT co.person_id,co.condition_start_date,co.condition_source_value
        FROM
        {dataset}.condition_occurrence co
        INNER JOIN
        {dataset}.concept c
        ON (co.condition_source_concept_id = c.concept_id)
    WHERE
        c.VOCABULARY_ID LIKE 'ICD10CM' AND
        ({icd10_terms})
    ")
  download_data(query)
}

lab_query_concept_id <- function(dataset,labs)
{
  labs <- paste0("'",paste(labs,sep="",collapse="','"),"'")
  query <- str_glue("
        SELECT person_id, measurement_date, value_as_number
        FROM `{dataset}.measurement`
        WHERE
            measurement_source_value IN ({labs})
        ")
  download_data(query)
}

lab_query <- function(dataset,labs)
{
  lab_terms <- paste('c.concept_name LIKE ',"'",labs,"'",collapse=' OR ',sep="")
  query <- str_glue("
        SELECT person_id, measurement_date, value_as_number
        FROM `{dataset}.measurement` m
        INNER JOIN `{dataset}.concept` c ON (m.measurement_concept_id = c.concept_id)
        WHERE
        ({lab_terms})
        ")
  download_data(query)
}

med_query <- function(dataset,meds)
{
  med_terms <- paste('lower(c.concept_name) LIKE ',"'%",meds,"%'",collapse=' OR ',sep="")
  query <- str_glue("
       SELECT DISTINCT d.person_id,d.drug_exposure_start_date
        FROM
        {dataset}.drug_exposure d
        INNER JOIN
        {dataset}.concept c
        ON (d.drug_concept_id = c.concept_id)
        WHERE
        {med_terms}
    ")
  download_data(query)
}

med_query_min_date <- function(dataset,meds)
{
  med_terms <- paste('lower(c.concept_name) LIKE ',"'%",meds,"%'",collapse=' OR ',sep="")
  query <- str_glue("
       SELECT DISTINCT d.person_id,MIN(d.drug_exposure_start_date) min_drug_date
        FROM
        {dataset}.drug_exposure d
        INNER JOIN
        {dataset}.concept c
        ON (d.drug_concept_id = c.concept_id)
        WHERE
        {med_terms}
        GROUP BY d.person_id
    ")
  download_data(query)
}

inpatient_icd_query <- function(dataset,codes)
{
  code_clause <- paste('co.CONDITION_SOURCE_VALUE LIKE ',"'",codes,"'",collapse=' OR ',sep="")
  query <- str_glue(
    "SELECT co.person_id,co.condition_start_date,co.condition_source_value
        FROM
            `{dataset}.condition_occurrence` co
            LEFT JOIN
            `{dataset}.concept` c
            ON (co.condition_source_concept_id = c.concept_id)
            LEFT JOIN
            `{dataset}.visit_occurrence` v
            ON (co.visit_occurrence_id = v.visit_occurrence_id)
        WHERE
            c.vocabulary_id LIKE 'ICD%' AND
            ({code_clause}) AND
            (co.condition_type_concept_id IN (38000200,38000201,
            38000202,38000203,38000204,38000205,38000214,
            38000206,38000207,38000208,38000209,38000210,
            38000211,38000212,38000213) OR
            v.visit_concept_id = 9201)
        ")
  download_data(query)
}

outpatient_icd_query <- function(dataset,codes)
{
  code_clause <- paste('co.CONDITION_SOURCE_VALUE LIKE ',"'",codes,"'",collapse=' OR ',sep="")
  query <- str_glue(
    "SELECT co.person_id,co.condition_start_date,co.condition_source_value
        FROM
            `{dataset}.condition_occurrence` co
            LEFT JOIN
            `{dataset}.concept` c
            ON (co.condition_source_concept_id = c.concept_id)
            LEFT JOIN
            `{dataset}.visit_occurrence` v
            ON (co.visit_occurrence_id = v.visit_occurrence_id)
        WHERE
            c.vocabulary_id LIKE 'ICD%' AND
            ({code_clause}) AND
            (co.condition_type_concept_id IN (38000230,38000231,
            38000232,38000233,38000234,38000235,38000236,
            38000237,38000238,38000239,38000240,38000241,
            38000242,38000243,38000244) OR
            v.visit_concept_id = 9202)
        ")
  download_data(query)
}
