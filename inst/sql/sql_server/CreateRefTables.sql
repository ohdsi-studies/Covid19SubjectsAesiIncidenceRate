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


IF OBJECT_ID('@cohort_database_schema.@summary_table', 'U') IS NOT NULL
	DROP TABLE @cohort_database_schema.@summary_table;

CREATE TABLE @cohort_database_schema.@summary_table
(
      database_name varchar(255),
       target_cohort_definition_id bigint,
       target_name varchar(255),
       time_at_risk_id bigint,
       time_at_risk_start_offset bigint,
       time_at_risk_start_index bigint,
       time_at_risk_end_offset bigint,
       time_at_risk_end_index bigint,
       subgroup_cohort_definition_id bigint,
       subgroup_name varchar(255),
       outcome_id bigint,
       outcome_cohort_definition_id bigint,
       outcome_name varchar(255),
       clean_window bigint,
       num_persons_pre_exclude bigint,
       num_persons_at_risk bigint,
       person_years_pre_exclude bigint,
       person_years bigint,
       num_persons_w_outcome_pre_exclude bigint,
       num_persons_w_outcome bigint,
       num_outcomes_pre_exclude bigint,
       num_outcomes bigint,
       incidence_proportion_p100p float,
       incidence_rate_p100py float
 );
