IF OBJECT_ID('@cohort_database_schema.@target_ref_table', 'U') IS NOT NULL
	DROP TABLE @cohort_database_schema.@target_ref_table;

CREATE TABLE @cohort_database_schema.@target_ref_table (
	target_cohort_definition_id INT,
	target_name VARCHAR(255)
	);


IF OBJECT_ID('@cohort_database_schema.@outcome_ref_table', 'U') IS NOT NULL
	DROP TABLE @cohort_database_schema.@outcome_ref_table;

CREATE TABLE @cohort_database_schema.@outcome_ref_table (
	outcome_id bigint,
  outcome_cohort_definition_id bigint,
  outcome_name varchar(255),
  clean_window int,
  primary_time_at_risk_start_offset bigint,
  primary_time_at_risk_start_index int,  /*0 - cohort_start, 1- cohort_end*/
  primary_time_at_risk_end_offset bigint,
  primary_time_at_risk_end_index int,  /*0 - cohort_start, 1- cohort_end*/
  excluded_cohort_definition_id bigint
	);


IF OBJECT_ID('@cohort_database_schema.@subgroup_ref_table', 'U') IS NOT NULL
	DROP TABLE @cohort_database_schema.@subgroup_ref_table;

CREATE TABLE @cohort_database_schema.@subgroup_ref_table (
	subgroup_cohort_definition_id bigint,
  subgroup_name varchar(255)
	);


IF OBJECT_ID('@cohort_database_schema.@time_at_risk_table', 'U') IS NOT NULL
	DROP TABLE @cohort_database_schema.@time_at_risk_table;

CREATE TABLE @cohort_database_schema.@time_at_risk_table
(
  time_at_risk_id bigint,
  time_at_risk_start_offset bigint,
  time_at_risk_start_index int,  /*0 - cohort_start, 1- cohort_end*/
  time_at_risk_end_offset bigint,
  time_at_risk_end_index int  /*0 - cohort_start, 1- cohort_end*/
)
;
