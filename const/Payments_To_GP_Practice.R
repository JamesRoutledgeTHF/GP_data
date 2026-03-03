
library(tidyverse)
library(dplyr)

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

#no longer need the code below here, leaving in as legacy
# finance_datasets_regression <- dbGetQuery(con, "
#   SELECT *,
#          YEAR(Effective_Snapshot_Date) - 1 AS YEAR
#   FROM NHS_Payments_To_GP_Practices.Figures1
#   WHERE YEAR(Effective_Snapshot_Date) >= 2024
# ")
# 
# relevant_metrics <- c("Average Number of Registered Patients")
# 
# # Filter the dataset to only check values for the relevant metrics
# filtered_for_metrics <- finance_datasets_regression %>%
#   filter(Metric %in% relevant_metrics) %>%
#   group_by(YEAR, Practice_Code) %>%
#   filter(any(Metric_Value < 1000)) %>%
#   ungroup()
# 
# # Now, exclude those practice codes (based on the YEAR and Practice_Code) that have any value < 1000
# finance_datasets_regression <- finance_datasets_regression %>%
#   anti_join(filtered_for_metrics, by = c("YEAR", "Practice_Code"))
# 
# outliers_005pct <- finance_datasets_regression %>%
#   filter(Metric %in% relevant_metrics) %>%
#   group_by(YEAR) %>%
#   mutate(
#     p005 = quantile(Metric_Value, 0.005, na.rm = TRUE),
#     p995 = quantile(Metric_Value, 0.995, na.rm = TRUE)
#   ) %>%
#   ungroup() %>%
#   filter(Metric_Value <= p005 | Metric_Value >= p995) %>%
#   distinct(YEAR, Practice_Code)
# 
# finance_datasets_regression <- finance_datasets_regression %>%
#   anti_join(outliers_005pct, by = c("YEAR", "Practice_Code"))
# 
# 
# finance_imd_wide_regression <- finance_datasets_regression %>%
#   pivot_wider(
#     id_cols = c(
#       YEAR, Practice_Code, Contract_Type, Dispensing_Practice,
#       Practice_Type, Practice_Rurality
#     ),
#     names_from  = Metric,
#     values_from = Metric_Value
#   ) %>%
#   left_join(
#     prac_imd,
#     by = c(
#       "YEAR" = "YEAR",
#       "Practice_Code" = "PRACTICE_CODE"
#     )
#   )%>% filter(!is.na(IMD_QUINTILE)) %>%
#   mutate(IMD_QUINTILE = as.factor(IMD_QUINTILE))
# 
# financial <- financial %>%
#   mutate(YEAR = as.integer(substr(Period, 1, 4)))
# 
# finance_imd_wide_regression <- finance_imd_wide_regression %>%
#   left_join(
#     financial,
#     by = c(
#       "YEAR" = "YEAR",
#       "Practice_Code" = "GP_Practice_Code"
#     )
#   )
