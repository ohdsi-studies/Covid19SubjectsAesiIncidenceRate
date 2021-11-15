select 
  database_name,
  target_cohort_definition_id,
  target_name,
  time_at_risk_id,
  time_at_risk_start_offset,
  time_at_risk_start_index,
  time_at_risk_end_offset,
  time_at_risk_end_index,
  subgroup_cohort_definition_id,
  subgroup_name,
  outcome_id,
  outcome_cohort_definition_id,
  outcome_name,
  clean_window,
  num_persons_pre_exclude,
  num_persons_at_risk,
  person_years_pre_exclude,
  person_years,
  num_persons_w_outcome_pre_exclude,
  num_persons_w_outcome,
  num_outcomes_pre_exclude,
  num_outcomes,
  incidence_proportion_p100p,
  incidence_rate_p100py
from @cohort_database_schema.@summary_table
;


