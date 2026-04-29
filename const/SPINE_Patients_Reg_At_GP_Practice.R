library(DBI)
library(dplyr)
library(stringr)
library(tidyr)
library(readr)
library(writexl)

#function to get GP practice level data 
get_gp_stats <- function(con, period = c("monthly", "yearly", "financial")){
  
  period <- match.arg(period)
  
  date_group <- switch(
    period,
    monthly = "[Effective_Snapshot_Date]",
    yearly = "YEAR([Effective_Snapshot_Date])",
    financial = "
    CASE
      WHEN MONTH([Effective_Snapshot_Date]) >= 4
      THEN CONCAT(YEAR([Effective_Snapshot_Date]),
      '/',
      RIGHT(CAST(YEAR([Effective_Snapshot_Date]) + 1 AS VARCHAR(4)),2)
      ) ELSE
      CONCAT(
      YEAR([Effective_Snapshot_Date]) - 1,
      '/',
      RIGHT(CAST(YEAR([Effective_Snapshot_Date]) AS VARCHAR(4)), 2)
      )
      END
    "
  )
  
  query <- sprintf("
WITH monthly AS(
SELECT  
[GP_Practice_Code], 
%s AS Period,
SUM([Size]) AS Total_Size,
SUM(CASE WHEN CASE WHEN [Age] = '95+' THEN 95 ELSE CAST([Age] AS INT) END >= 65 THEN [Size] ELSE 0 END) AS Over65_Count,
SUM(CASE WHEN [SEX] = 'Male' THEN [Size] ELSE 0 END) AS Male_Count
FROM 
[Demography].[No_Of_Patients_Regd_At_GP_Practice_Single_Age1]
GROUP BY 
[GP_Practice_Code],
%s,
[Effective_Snapshot_Date]
)

SELECT 
[GP_Practice_Code],
Period,
AVG(Total_Size) AS Avg_Size,
100.0 * SUM(Over65_Count) / NULLIF(SUM(Total_Size), 0) AS Pct_Over_65,
100.0 * SUM(Male_Count) / NULLIF(SUM(Total_Size), 0) AS Pct_Male

FROM monthly
GROUP BY
[GP_Practice_Code],
Period
ORDER BY 
[GP_Practice_Code],
Period;
", date_group,date_group)

dbGetQuery(con, query)
}

#yearly <- get_gp_stats(con, "yearly")
financial <- get_gp_stats(con, "financial")
#monthly <- get_gp_stats(con, "monthly")

# Load the IMD SQL data 
imd_sql <- dbGetQuery(con, "
  SELECT
    LSOA_CODE,
    IMD_Score AS IMD_SCORE,
    IMD_Decile AS IMD_DECILE,
    Health_Deprivation_And_Disability_Score AS IMD_HEALTH
    FROM Demography.Domains_Of_Deprivation_By_LSOA1
    WHERE YEAR(Effective_Snapshot_Date) = 2019
")

imd <- imd_sql %>%
  mutate(
    IMD_QUINTILE = case_when(IMD_DECILE %in% c(1:2) ~ 1,
                             IMD_DECILE %in% c(3:4) ~ 2,
                             IMD_DECILE %in% c(5:6) ~ 3,
                             IMD_DECILE %in% c(7:8) ~ 4,
                             IMD_DECILE %in% c(9:10) ~ 5),
    LSOA_DATE = 2011) %>%
  select(LSOA_CODE, IMD_SCORE, IMD_HEALTH, IMD_QUINTILE, LSOA_DATE)


# Load the LSOA mapping data
mapping <- read_csv("data/LSOA_2011_to_LSOA_2021_Lookup_E_W.csv", name_repair = make.names)

# Generate weighted mapping
weighted_mapping <- mapping %>%
  left_join(
    mapping %>%
      filter(CHGIND %in% c("M","X")) %>%
      group_by(LSOA21CD) %>%
      summarise(WEIGHT = 1 / n(), .groups = "drop"),
    by = "LSOA21CD"
  ) %>%
  mutate(WEIGHT = if_else(CHGIND %in% c("S","U"), 1, WEIGHT))

# Join weighted mapping to imd (2011) via LSOA11CD -> LSOA_CODE
weighted_imd <- weighted_mapping %>%
  left_join(
    select(imd, LSOA_CODE, IMD_SCORE, IMD_HEALTH),
    by = c("LSOA11CD" = "LSOA_CODE")
  ) %>%
  mutate(
    weighted_imd = IMD_SCORE * WEIGHT,
    weighted_imd_health = coalesce(IMD_HEALTH, 0) * WEIGHT
  ) %>%
  group_by(LSOA21CD) %>%
  summarise(
    IMD_SCORE = sum(weighted_imd, na.rm = TRUE),
    IMD_HEALTH = sum(weighted_imd_health, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    IMD_QUINTILE = ntile(-IMD_SCORE, 5),  # Rank the inverse of IMD_SCORE to get lower scores in higher quintiles
    LSOA_DATE = 2021
  ) %>%
  rename(LSOA_CODE = LSOA21CD)

# Combine the 2011 and weighted 2021 rows
imd_2011_2021 <- bind_rows( imd,weighted_imd
)

imd_2011_2021 <- imd_2011_2021 %>%
  filter(LSOA_DATE == 2021)

#LOAD ONS LSOA data
ons_raw <- dbGetQuery(con, "
  SELECT
    Area_Code AS LSOA_CODE,
    Sex,
    Age,
    YEAR(Effective_Snapshot_Date) AS YEAR,
    Effective_Snapshot_Date,
    Size AS POP
  FROM Demography.ONS_Population_Estimates_For_LSOAs_By_Year_Of_Age1
  WHERE YEAR(Effective_Snapshot_Date) >= 2017
")

# Group by LSOA_CODE and Effective_Snapshot_Date, then sum the POP
ons_grouped <- ons_raw %>%
  group_by(LSOA_CODE, Effective_Snapshot_Date) %>%
  summarise(
    Total_POP = sum(POP, na.rm = TRUE), 
    .groups = "drop" 
  )

# Clean the data
ons_clean <- ons_raw %>%
  mutate(
    Age = as.character(Age),
    Age = str_replace(Age, "\\+", ""),       
    Age = na_if(Age, ""),                    
    Age = suppressWarnings(as.integer(Age)),  
    
    YEAR = lubridate::year(as.Date(Effective_Snapshot_Date)),
    
    # Assign LSOA_DATE based on year (2011 for pre-2021, 2021 otherwise)
    LSOA_DATE = if_else(YEAR <= 2020, 2011L, 2021L),
    
    SEX = case_when(
      str_to_upper(Sex) %in% c("M", "MALE", "MAL", "MAL.", "MALES") ~ "MALE",
      str_to_upper(Sex) %in% c("F", "FEMALE", "FEM", "FEMALES") ~ "FEMALE",
      TRUE ~ as.character(Sex)
    )
  ) %>%
  select(LSOA_CODE, YEAR, LSOA_DATE, SEX, Age, POP)

# Group by age categories
ons_agegroup <- ons_clean %>%
  mutate(
    AGE = as.numeric(Age),
    AGE = case_when(
      AGE %in% 0:4   ~ "0_4",
      AGE %in% 5:14  ~ "5_14",
      AGE %in% 15:44 ~ "15_44",
      AGE %in% 45:64 ~ "45_64",
      AGE %in% 65:74 ~ "65_74",
      AGE %in% 75:84 ~ "75_84",
      AGE >= 85      ~ "85_PLUS",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(AGE)) %>% 
  group_by(LSOA_CODE, AGE, YEAR, SEX) %>% 
  summarise(TOTAL_POP = sum(POP, na.rm = TRUE), .groups = "drop")


# Load the attribution datasets
lsoa_attributions_df <- dbGetQuery(con, "
  SELECT
    GP_Practice_Code AS PRACTICE_CODE,
    LSOA_Code AS LSOA_CODE,
    Sex as SEX,
    Effective_Snapshot_Date,
    YEAR(Effective_Snapshot_Date) AS YEAR,
    Size AS GP_POP
  FROM Demography.No_Of_Patients_Regd_At_GP_Practice_LSOA_Level1
  WHERE YEAR(Effective_Snapshot_Date) >= 2017 AND MONTH(Effective_Snapshot_Date) = 10
")

# Prepare the lsoa_prac_pc dataset with the necessary transformations
lsoa_prac_pc <- lsoa_attributions_df %>%
  select(YEAR, PRACTICE_CODE, LSOA_CODE, SEX, GP_POP) %>%
  # Summing GP_POP across all sexes for each PRACTICE_CODE and YEAR
  group_by(PRACTICE_CODE, YEAR) %>%
  mutate(total_pat_in_practice = sum(GP_POP, na.rm = TRUE),
         pc = GP_POP / total_pat_in_practice) %>%
  ungroup() %>%
  select(YEAR, PRACTICE_CODE, LSOA_CODE, GP_POP, total_pat_in_practice, pc) %>%
  # Create LSOA_DATE based on YEAR
  mutate(
    LSOA_DATE = case_when(
      YEAR < 2021 ~ 2021,
      YEAR >= 2020 ~ 2021
    )
  )

# Assign IMD at the practice level using the attributions dataset
prac_imd <- lsoa_prac_pc %>% 
  inner_join(imd_2011_2021, by = c("LSOA_CODE", "LSOA_DATE")) %>% 
  inner_join(ons_agegroup, by = c("LSOA_CODE", "YEAR")) %>%
  group_by(YEAR, PRACTICE_CODE) %>%
  summarise(
    IMD_SCORE_PROP = sum(pc * TOTAL_POP * IMD_SCORE, na.rm = TRUE),
    total_weighted_pop = sum(pc * TOTAL_POP, na.rm = TRUE),
    total_pat_in_practice = first(total_pat_in_practice)
  ) %>% 
  mutate(
    IMD_SCORE = IMD_SCORE_PROP / total_weighted_pop
  ) %>%
  ungroup() %>%
  select(YEAR, PRACTICE_CODE, IMD_SCORE, total_weighted_pop, total_pat_in_practice)

# Compute cumulative population and IMD deciles/quintiles
prac_imd <- prac_imd %>% 
  group_by(YEAR) %>%
  arrange(YEAR, -IMD_SCORE) %>%  # Sort within each year by IMD_SCORE
  mutate(
    CUM_POP = cumsum(total_weighted_pop),
    PROP = CUM_POP / max(CUM_POP),
    IMD_DECILE = cut(PROP, breaks = 10, labels = FALSE, include.lowest = TRUE),
    IMD_QUINTILE = ntile(PROP, 5)
  ) %>%
  ungroup() %>%
  select(YEAR, PRACTICE_CODE, IMD_SCORE, IMD_QUINTILE, total_weighted_pop, total_pat_in_practice)

#write_xlsx(financial, path = "Financial_Data.xlsx")
#write_xlsx(prac_imd, path = "Prac_IMD.xlsx")

#Legacy
# summary_patients_per_year <- prac_imd %>%
#   group_by(YEAR) %>%
#   summarise(
#     TOTAL_PATIENTS = sum(total_pat_in_practice, na.rm = TRUE),
#     .groups = "drop"
#   )
# 
# summary_quintile_counts <- prac_imd %>%
#   group_by(YEAR, IMD_QUINTILE) %>%
#   summarise(
#     N_PRACTICES = n_distinct(PRACTICE_CODE),
#     .groups = "drop"
#   ) %>%
#   arrange(YEAR, IMD_QUINTILE)

# Over65_quintile <- financial %>%
#   filter(Period == "2023/24") %>%
#   rename(PRACTICE_CODE = GP_Practice_Code) %>%   
#   mutate(
#     YEAR = 2023,
#     Over65_quintile = ntile(Pct_Over_65, 5)
#   )


