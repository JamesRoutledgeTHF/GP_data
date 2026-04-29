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

#query GP headcount and FTE by region
query_gp_roles_2 <- "
WITH snapshot_level AS (
  SELECT 
    Effective_Snapshot_Date,
    Commissioner_Code,
    Staff_Role,
    SUM(FTE) AS Total_FTE,
    COUNT(DISTINCT Unique_Identifier) AS Headcount
  FROM NHS_Workforce.GP_Level_Census_Data1
  WHERE Staff_Role IN ('GP Partners', 'Salaried GPs')
    AND Effective_Snapshot_Date BETWEEN '2023-04-01' AND '2024-03-31'
  GROUP BY 
    Effective_Snapshot_Date,
    Commissioner_Code,
    Staff_Role
)

SELECT 
  Commissioner_Code,
  Staff_Role,
  AVG(Total_FTE) AS Yearly_Avg_FTE,
  AVG(Headcount) AS Yearly_Avg_Headcount
FROM snapshot_level
GROUP BY 
  Commissioner_Code,
  Staff_Role
ORDER BY 
  Commissioner_Code,
  Staff_Role;
"

#gp_role_all_2 <- dbGetQuery(con, query_gp_roles_2)
#write_xlsx(gp_role_all_2, path = "GP_Financial_Region_2023_24.xlsx")

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

write_xlsx(gp_financial_summary_age_gender, path = "GP_Gender_Age_Summary.xlsx")

#for Contract FTE and Workforce
query_practice_gp <- "
WITH monthly AS (
    SELECT
        Practice_Code,
        Effective_Snapshot_Date,

        -- Contractor FTE
        SUM(CASE 
                WHEN Detailed_Staff_Role IN ('Partner/Provider', 'Senior Partner')
                     AND Measure = 'FTE'
                THEN Measure_Value ELSE 0
            END) AS Contractor_FTE,

        -- Contractor Headcount
        SUM(CASE 
                WHEN Detailed_Staff_Role IN ('Partner/Provider', 'Senior Partner')
                     AND Measure = 'Headcount'
                THEN Measure_Value ELSE 0
            END) AS Contractor_Headcount,

        -- Salaried FTE
        SUM(CASE 
                WHEN Detailed_Staff_Role IN ('Salaried By Other', 'Salaried By Practice')
                     AND Measure = 'FTE'
                THEN Measure_Value ELSE 0
            END) AS Salaried_FTE,

        -- Salaried Headcount
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
  filter(YEAR == 2024) %>%
  distinct(Practice_Code, .keep_all = TRUE)

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