
finance_datasets <- dbGetQuery(con, "
  SELECT *,
         YEAR(Effective_Snapshot_Date) - 1 AS YEAR
  FROM NHS_Payments_To_GP_Practices.Figures1
  WHERE YEAR(Effective_Snapshot_Date) >= 2017
")

# Aggregating data by PRACTICE_CODE and Effective_Snapshot_Date before pivoting
finance_datasets_aggregated <- finance_datasets %>%
  group_by(Practice_Code, Effective_Snapshot_Date, Metric) %>%
  summarise(Metric_Value = sum(Metric_Value), .groups = "drop")  # You can change the aggregation method

# Now pivot the data
finance_datasets_timeseries <- finance_datasets_aggregated %>%
  pivot_wider(
    names_from = Metric, 
    values_from = Metric_Value
  )

