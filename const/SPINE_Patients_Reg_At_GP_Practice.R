library(DBI)

get_gp_stats <- function(con, period = c("monthly", "yearly", "financial")){
  
  period <- match.arg(period)
  
  date_group <- switch(
    period,
    monthly = "[Effective_Snapshop_Date]",
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

m <- get_gp_stats(con, "yearly")

library(tidyverse)
library(DBI)
library(readr)

# --- 1. Load IMD data from SQL ----------------------------------------------

imd <- dbGetQuery(con, "
    SELECT 
        LSOA_CODE,
        IMD_Score AS IMD_SCORE,
        IMD_Decile AS IMD_DECILE,
        NULL AS IMD_HEALTH   -- placeholder unless you have the health domain field
    FROM Demography.Index_Of_Multiple_Deprivation_By_LSOA1
") %>% 
  mutate(
    IMD_QUINTILE = case_when(
      IMD_DECILE %in% 1:2 ~ 1,
      IMD_DECILE %in% 3:4 ~ 2,
      IMD_DECILE %in% 5:6 ~ 3,
      IMD_DECILE %in% 7:8 ~ 4,
      IMD_DECILE %in% 9:10 ~ 5
    ),
    LSOA_DATE = 2011
  ) %>%
  select(LSOA_CODE, IMD_SCORE, IMD_HEALTH, IMD_QUINTILE, LSOA_DATE)

# --- 2. Load LSOA 2011 → 2021 lookup from Excel ------------------------------

mapping <- read_csv("data/LSOA_2011_to_LSOA_2021_Lookup_E_W.csv", 
                    name_repair = make.names)

weighted_mapping <- mapping %>%
  left_join(
    mapping %>% 
      filter(CHGIND %in% c("M","X")) %>% 
      group_by(LSOA21CD) %>% 
      summarise(WEIGHT = 1/n())
  ) %>%
  mutate(WEIGHT = if_else(CHGIND %in% c("S","U"), 1, WEIGHT))

# --- 3. Weighted IMD (convert 2011 IMD to 2021) -------------------------------

weighted_imd <- weighted_mapping %>%
  left_join(select(imd, LSOA_CODE, IMD_HEALTH, IMD_SCORE),
            by = c("LSOA11CD" = "LSOA_CODE")) %>%
  mutate(
    weighted_imd = IMD_SCORE * WEIGHT,
    weighted_imd_health = IMD_HEALTH * WEIGHT
  ) %>%
  group_by(LSOA21CD) %>%
  summarise(
    IMD_SCORE = sum(weighted_imd, na.rm = TRUE),
    IMD_HEALTH = sum(weighted_imd_health, na.rm = TRUE)
  ) %>%
  mutate(
    IMD_QUINTILE = ntile(-IMD_SCORE, 5),
    LSOA_DATE = 2021
  ) %>%
  rename(LSOA_CODE = LSOA21CD)

imd_2011_2021 <- bind_rows(imd, weighted_imd)

# --- 4. Load ONS Population from SQL ------------------------------------------

ons_raw <- dbGetQuery(con, "
    SELECT 
        Area_Code AS LSOA_CODE,
        Sex,
        CASE 
            WHEN Age = '90+' THEN 90
            ELSE CAST(Age AS INT)
        END AS Age,
        YEAR(Effective_Snapshot_Date) AS YEAR,
        Size AS POP
    FROM Demography.ONS_Population_Estimates_For_LSOAs_By_Year_Of_Age1
")

# --- 5. Age grouping & summarisation ------------------------------------------

ons_df <- ons_raw %>%
  mutate(
    AGE_Group = case_when(
      Age %in% 0:4   ~ '0_4',
      Age %in% 5:14  ~ '5_14',
      Age %in% 15:44 ~ '15_44',
      Age %in% 45:64 ~ '45_64',
      Age %in% 65:74 ~ '65_74',
      Age %in% 75:84 ~ '75_84',
      Age >= 85      ~ '85_PLUS'
    ),
    SEX = case_when(
      Sex == 'Male'   ~ 'MALE',
      Sex == 'Female' ~ 'FEMALE',
      TRUE ~ as.character(Sex)
    )
  ) %>%
  group_by(LSOA_CODE, YEAR, SEX, AGE_Group) %>%
  summarise(Total_POP = sum(POP), .groups = "drop")

# --- 6. Pivot age/sex groups & join IMD ---------------------------------------

ons_pivot <- ons_df %>%
  pivot_wider(
    names_from = c(SEX, AGE_Group),
    values_from = Total_POP,
    values_fill = 0
  ) %>%
  left_join(imd_2011_2021, by="LSOA_CODE")

# --- 7. Apply Carr-Hill adjustment --------------------------------------------

adj_pop_df <- ons_pivot %>%
  rowwise() %>%
  mutate(
    TOTAL_POP = sum(c_across(starts_with("MALE_")), 
                    c_across(starts_with("FEMALE_")), na.rm=TRUE),
    
    ADJUSTED_POP =
      (2.354*MALE_0_4    + 1.000*MALE_5_14  + 0.913*MALE_15_44 + 1.373*MALE_45_64 +
         2.531*MALE_65_74  + 3.254*MALE_75_84 + 3.193*MALE_85_PLUS +
         2.241*FEMALE_0_4  + 1.030*FEMALE_5_14 + 1.885*FEMALE_15_44 +
         2.115*FEMALE_45_64 + 2.820*FEMALE_65_74 + 3.301*FEMALE_75_84 +
         3.090*FEMALE_85_PLUS) *
      (1.054 ^ IMD_HEALTH)
  )

# --- 8. Normalise adjusted populations ----------------------------------------

adj_pop_norm <- adj_pop_df %>%
  group_by(YEAR) %>%
  mutate(
    AF = sum(TOTAL_POP, na.rm=TRUE) / sum(ADJUSTED_POP, na.rm=TRUE),
    NEED_ADJ_POP = ADJUSTED_POP * AF
  ) %>%
  ungroup() %>%
  select(YEAR, LSOA_CODE, NEED_ADJ_POP, TOTAL_POP)

