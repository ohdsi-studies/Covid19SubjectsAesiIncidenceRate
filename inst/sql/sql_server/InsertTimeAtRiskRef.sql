insert into @cohort_database_schema.@ref_table (
  time_at_risk_id, 
  time_at_risk_start_offset, 
  time_at_risk_start_index, 
  time_at_risk_end_offset, 
  time_at_risk_end_index
)
SELECT 
  @time_at_risk_id,
  @time_at_risk_start_offset,
  @time_at_risk_start_index, 
  @time_at_risk_end_offset,
  @time_at_risk_end_index
;
