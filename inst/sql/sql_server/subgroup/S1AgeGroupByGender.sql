DELETE FROM @target_database_schema.@target_cohort_table where cohort_definition_id between 10 and 150;

with ages as (
  --GENERAL AGE GROUPS
  SELECT 2 as age_id, 0 as age_low, 5 as age_high
  UNION ALL
  SELECT 3 as age_id, 6 as age_low, 17 as age_high
  UNION ALL
  SELECT 4 as age_id, 18 as age_low, 34 as age_high
  UNION ALL
  SELECT 5 as age_id, 35 as age_low, 54 as age_high
  UNION ALL
  SELECT 6 as age_id, 55 as age_low, 64 as age_high
  UNION ALL
  SELECT 7 as age_id, 65 as age_low, 74 as age_high
  UNION ALL
  SELECT 8 as age_id, 75 as age_low, 84 as age_high
  UNION ALL
  SELECT 9 as age_id, 85 as age_low, 114 as age_high

  UNION ALL

    --PEDIATRIC AGE GROUPS
   SELECT 10 as age_id, 0 as age_low, 4 as age_high
   UNION ALL
   SELECT 11 as age_id, 5 as age_low, 11 as age_high
   UNION ALL
   SELECT 12 as age_id, 12 as age_low, 17 as age_high

),
genders as (
  SELECT 1 as gender_id, 8532 as gender_concept_id, 'Female' as gender_name
  UNION
  SELECT 2 as gender_id, 8507 as gender_concept_id, 'Male' as gender_name
)
SELECT ages.age_id*10+genders.gender_id as subgroup_id, age_low, age_high, gender_concept_id, gender_name
  INTO #subgroups
  FROM ages, genders
;

INSERT INTO @target_database_schema.@target_ref_table (subgroup_cohort_definition_id, subgroup_name)
SELECT subgroup_id, 'Persons aged ' + cast(age_low as varchar) + ' to ' + cast(age_high as varchar) + ' with gender = ' + gender_name
FROM #subgroups;



INSERT INTO @target_database_schema.@target_cohort_table (cohort_definition_id, subject_id, cohort_start_date, cohort_end_date)
SELECT s1.subgroup_id AS cohort_definition_id,
  op1.person_id AS subject_id,
    CASE WHEN YEAR(op1.observation_period_start_date) - p1.year_of_birth >= s1.age_low
    THEN op1.observation_period_start_date
    ELSE DATEFROMPARTS(p1.year_of_birth + s1.age_low,1,1) END
  AS cohort_start_date,
    CASE WHEN YEAR(op1.observation_period_end_date) - p1.year_of_birth <= s1.age_high
    THEN op1.observation_period_end_date
    ELSE DATEFROMPARTS(p1.year_of_birth + s1.age_high,12,31) END
  AS cohort_end_date
FROM @cdm_database_schema.observation_period op1
  INNER JOIN @cdm_database_schema.person p1
  ON op1.person_id = p1.person_id
INNER JOIN #subgroups s1
  ON DATEFROMPARTS(p1.year_of_birth + s1.age_low,1,1) <= op1.observation_period_end_date
  AND DATEFROMPARTS(p1.year_of_birth + s1.age_high,12,31) >= op1.observation_period_start_date
  AND p1.gender_concept_id = s1.gender_concept_id
;


drop table #subgroups;

