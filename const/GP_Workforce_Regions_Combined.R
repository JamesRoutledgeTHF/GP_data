library(tidyverse)

gp <- read_csv("GP_Workforce_Combined.csv")

# Calculate averages by Region + Staff Role
gp_summary <- gp %>%
  filter(
    STAFF_ROLE %in% c("GP Partners", "Salaried GPs"),
    !is.na(COMM_REGION_NAME),
    !is.na(STAFF_ROLE),
    !is.na(YEAR),
    !is.na(Month),
    !is.na(FTE),
    !is.na(UNIQUE_IDENTIFIER)
  ) %>%
  # Monthly totals by Region + Role
  group_by(COMM_REGION_NAME, STAFF_ROLE, YEAR, Month) %>%
  summarise(
    total_fte = sum(FTE),
    total_headcount = n_distinct(UNIQUE_IDENTIFIER),
    .groups = "drop"
  ) %>%
  # Average monthly totals across all months
  group_by(COMM_REGION_NAME, STAFF_ROLE) %>%
  summarise(
    avg_fte = mean(total_fte),
    avg_headcount = mean(total_headcount),
    .groups = "drop"
  ) %>%
  arrange(COMM_REGION_NAME, STAFF_ROLE)

gp_summary