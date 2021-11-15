insert into @cohort_database_schema.@ref_table (
 target_cohort_definition_id, 
 target_name
)
SELECT 
  @target_cohort_definition_id,
  '@target_name'
;
