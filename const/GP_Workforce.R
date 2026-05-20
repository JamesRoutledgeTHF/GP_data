library(DBI)
library(dplyr)
library(lubridate)
library(writexl)

#query GP headcount and FTE overall
query_gp_roles <- "
WITH monthly AS (
    SELECT 
        CASE 
            WHEN MONTH(Effective_Snapshot_Date) >= 4 
                THEN CONCAT(YEAR(Effective_Snapshot_Date), '/', RIGHT(YEAR(Effective_Snapshot_Date) + 1, 2))
            ELSE 
                CONCAT(YEAR(Effective_Snapshot_Date) - 1, '/', RIGHT(YEAR(Effective_Snapshot_Date), 2))
        END AS Financial_Year,

        Effective_Snapshot_Date,
        Staff_Role,
        SUM(FTE) AS Total_FTE,
        COUNT(DISTINCT Unique_Identifier) AS Headcount

    FROM NHS_Workforce.GP_Level_Census_Data1

    WHERE Staff_Role IN ('GP Partners', 'Salaried GPs')

    GROUP BY 
        CASE 
            WHEN MONTH(Effective_Snapshot_Date) >= 4 
                THEN CONCAT(YEAR(Effective_Snapshot_Date), '/', RIGHT(YEAR(Effective_Snapshot_Date) + 1, 2))
            ELSE 
                CONCAT(YEAR(Effective_Snapshot_Date) - 1, '/', RIGHT(YEAR(Effective_Snapshot_Date), 2))
        END,
        Effective_Snapshot_Date,
        Staff_Role
)

SELECT 
    Financial_Year,
    Staff_Role,
    AVG(Total_FTE) AS Avg_FTE,
    AVG(Headcount) AS Avg_Headcount

FROM monthly

GROUP BY 
    Financial_Year,
    Staff_Role

ORDER BY 
    Financial_Year,
    Staff_Role
"
gp_role_summary <- dbGetQuery(con, query_gp_roles)

#age and gender breakdown for FTE and Headcount
query_gp_gender_age <- "
SELECT 
    Effective_Snapshot_Date,
    Staff_Role,
    Gender,
    Age_Band,
    SUM(FTE) AS Total_FTE,
    COUNT(DISTINCT Unique_Identifier) AS Headcount
FROM NHS_Workforce.GP_Level_Census_Data1
WHERE Staff_Role IN ('GP Partners', 'Salaried GPs')
GROUP BY Effective_Snapshot_Date, Staff_Role, Gender, Age_Band
ORDER BY Effective_Snapshot_Date, Staff_Role, Gender, Age_Band
"

gp_gender_age <- dbGetQuery(con, query_gp_gender_age)

gp_gender_age <- gp_gender_age %>%
  mutate(Effective_Snapshot_Date = as.Date(Effective_Snapshot_Date))

gp_gender_age <- gp_gender_age %>%
  mutate(
    Age_Band = case_when(
      Age_Band %in% c("Under 30", "30-34", "35-39") ~ "Under 40",
      Age_Band %in% c("40-44", "45-49") ~ "40-49",
      Age_Band %in% c("50-54", "55-59") ~ "50-59",
      Age_Band %in% c("60-64", "65 and over") ~ "60 and over",
      TRUE ~ Age_Band
    )
  )

gp_gender_age <- gp_gender_age %>%
  mutate(
    Year = year(Effective_Snapshot_Date),
    Month = month(Effective_Snapshot_Date),
    Financial_Year = ifelse(Month >= 4, Year, Year - 1),
    Financial_Year = paste0(Financial_Year, "/", substr(Financial_Year + 1, 3, 4))
  )

gp_financial_summary_age_gender <- gp_gender_age %>%
  group_by(Financial_Year, Staff_Role, Gender, Age_Band) %>%
  summarise(
    Avg_FTE = mean(Total_FTE, na.rm = TRUE),
    Avg_Headcount = mean(Headcount, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  arrange(Financial_Year, Staff_Role, Gender, Age_Band)

#write_xlsx(gp_financial_summary_age_gender, path = "GP_Gender_Age_Summary.xlsx")

#for Contract FTE and Workforce
query_practice_gp <- "
WITH monthly AS (
    SELECT
        Practice_Code,
        Effective_Snapshot_Date,

        - Contractor FTE
        SUM(CASE 
                WHEN Detailed_Staff_Role IN ('Partner/Provider', 'Senior Partner')
                     AND Measure = 'FTE'
                THEN Measure_Value ELSE 0
            END) AS Contractor_FTE,

        - Contractor Headcount
        SUM(CASE 
                WHEN Detailed_Staff_Role IN ('Partner/Provider', 'Senior Partner')
                     AND Measure = 'Headcount'
                THEN Measure_Value ELSE 0
            END) AS Contractor_Headcount,

        - Salaried FTE
        SUM(CASE 
                WHEN Detailed_Staff_Role IN ('Salaried By Other', 'Salaried By Practice')
                     AND Measure = 'FTE'
                THEN Measure_Value ELSE 0
            END) AS Salaried_FTE,

        - Salaried Headcount
        SUM(CASE 
                WHEN Detailed_Staff_Role IN ('Salaried By Other', 'Salaried By Practice')
                     AND Measure = 'Headcount'
                THEN Measure_Value ELSE 0
            END) AS Salaried_Headcount

    FROM NHS_Workforce.Practice_Level_Census_Data_High_Level1

    WHERE 
        Staff_Group = 'GP'
        AND Effective_Snapshot_Date BETWEEN '2023-04-01' AND '2024-03-31'

    GROUP BY 
        Practice_Code,
        Effective_Snapshot_Date
)

SELECT
    Practice_Code,

    AVG(Contractor_FTE) AS Avg_Contractor_FTE,
    AVG(Contractor_Headcount) AS Avg_Contractor_Headcount,
    AVG(Salaried_FTE) AS Avg_Salaried_FTE,
    AVG(Salaried_Headcount) AS Avg_Salaried_Headcount

FROM monthly

GROUP BY 
    Practice_Code

ORDER BY 
    Practice_Code
"
gp_practice_summary <- dbGetQuery(con, query_practice_gp)

finance_datasets_2024 <- finance_datasets %>%
  filter(
    YEAR == 2024,
    Metric == "Average Number of Registered Patients"
  ) %>%
  distinct(Practice_Code, .keep_all = TRUE)

n_finance <- finance_datasets_2024 %>%
  summarise(n = n_distinct(Practice_Code))

gp_practice_final <- gp_practice_summary %>%
  left_join(finance_datasets_2024, by = "Practice_Code")

gp_practice_final_clean <- gp_practice_final %>%
  filter(!is.na(Contract_Type))


contract_summary <- gp_practice_final_clean %>%
  group_by(Contract_Type) %>%
  summarise(
    Total_Contractor_FTE = sum(Avg_Contractor_FTE, na.rm = TRUE),
    Total_Contractor_Headcount = sum(Avg_Contractor_Headcount, na.rm = TRUE),
    Total_Salaried_FTE = sum(Avg_Salaried_FTE, na.rm = TRUE),
    Total_Salaried_Headcount = sum(Avg_Salaried_Headcount, na.rm = TRUE)
  ) %>%
  ungroup()

listsize_summary <- gp_practice_final_clean %>%
  mutate(
    Patient_Band = case_when(
      Metric_Value < 5000 ~ "<4,999",
      Metric_Value >= 5000 & Metric_Value <= 9999 ~ "5,000 to 9,999",
      Metric_Value >= 10000 & Metric_Value <= 14999 ~ "10,000 to 14,999",
      Metric_Value >= 15000 & Metric_Value <= 19999 ~ "15,000 to 19,999",
      Metric_Value >= 20000 ~ "20,000+",
      TRUE ~ NA_character_
    )
  ) %>%
  group_by(Patient_Band, Dispensing_Practice) %>%
  summarise(
    Total_Contractor_FTE = sum(Avg_Contractor_FTE, na.rm = TRUE),
    Total_Contractor_Headcount = sum(Avg_Contractor_Headcount, na.rm = TRUE),
    Total_Salaried_FTE = sum(Avg_Salaried_FTE, na.rm = TRUE),
    Total_Salaried_Headcount = sum(Avg_Salaried_Headcount, na.rm = TRUE)
  ) %>%
  ungroup()

disp_rurality_summary <- gp_practice_final_clean %>%
  group_by(Practice_Rurality, Dispensing_Practice) %>%
  summarise(
    Total_Contractor_FTE = sum(Avg_Contractor_FTE, na.rm = TRUE),
    Total_Contractor_Headcount = sum(Avg_Contractor_Headcount, na.rm = TRUE),
    Total_Salaried_FTE = sum(Avg_Salaried_FTE, na.rm = TRUE),
    Total_Salaried_Headcount = sum(Avg_Salaried_Headcount, na.rm = TRUE)
  ) %>%
  ungroup()

#write_xlsx(listsize_summary, "listsize_summary.xlsx")



#Work in Progress...
#work out gps pr 10,000 patients
# Region 
payments_region <- payments2425 %>%
  select(
    Practice.Code,
    NHS.England..Region..Name
  ) %>%
  distinct()

# Join GP workforce data to region
gp_region <- gp_practice_final %>%
  left_join(
    payments_region,
    by = c("Practice_Code" = "Practice.Code")
  ) %>%
  
  rename(
    REGION = NHS.England..Region..Name
  ) %>%
  
  filter(
    !is.na(REGION),
    !is.na(Metric_Value)   
  )

# Regional summary
gp_region_summary <- gp_region %>%
  group_by(REGION) %>%
  summarise(
    TOTAL_GP_PARTNERS = sum(Avg_Contractor_Headcount, na.rm = TRUE),
    TOTAL_REGISTERED_PATIENTS = sum(Metric_Value, na.rm = TRUE),
    GP_PARTNERS_PER_10000 = (
      TOTAL_GP_PARTNERS /
        TOTAL_REGISTERED_PATIENTS
    ) * 10000,
    N_PRACTICES = n_distinct(Practice_Code),
    .groups = "drop"
  ) %>%
  arrange(desc(GP_PARTNERS_PER_10000))

gp_region_summary

#for the regional headcount & workforce 
gp_region_headcount <- gp_practice_final_clean %>%
  left_join(
    payments_region,
    by = c("Practice_Code" = "Practice.Code")
  ) %>%
  rename(
    REGION = NHS.England..Region..Name
  ) %>%
  filter(
    !is.na(REGION),
    !is.na(Metric_Value)   
  )

#if we want to work out regional using the practice level data
gp_region_summary <- gp_region_headcount %>%
  group_by(REGION) %>%
  summarise(
    Total_Contractor_FTE = sum(Avg_Contractor_FTE, na.rm = TRUE),
    Total_Contractor_Headcount = sum(Avg_Contractor_Headcount, na.rm = TRUE),
    Total_Salaried_FTE = sum(Avg_Salaried_FTE, na.rm = TRUE),
    Total_Salaried_Headcount = sum(Avg_Salaried_Headcount, na.rm = TRUE)
  ) %>%
  ungroup()
