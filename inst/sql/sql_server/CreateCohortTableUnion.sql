IF OBJECT_ID('@cohort_database_schema.@cohort_table', 'U') IS NOT NULL
	DROP TABLE @cohort_database_schema.@cohort_table;

CREATE TABLE @cohort_database_schema.@cohort_table (
	cohort_definition_id BIGINT,
	subject_id BIGINT,
	cohort_start_date DATE,
	cohort_end_date DATE
	);

INSERT INTO @cohort_database_schema.@cohort_table
SELECT * FROM @cohort_database_schema.@outcomeCohortTable
UNION ALL
SELECT * FROM @cohort_database_schema.@targetCohortTable;
