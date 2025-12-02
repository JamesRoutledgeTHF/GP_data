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


# -----------------------
# 1. Load IMD from SQL
# -----------------------
# Note: your SQL table lacks IMD_HEALTH; we set it NULL -> later coalesce to 0
imd_sql <- dbGetQuery(con, "
  SELECT
    LSOA_CODE,
    IMD_Score   AS IMD_SCORE
  FROM Demography.Index_Of_Multiple_Deprivation_By_LSOA1
")

imd <- imd_sql %>%
  mutate(
    IMD_QUINTILE = case_when(
      IMD_DECILE %in% 1:2 ~ 1,
      IMD_DECILE %in% 3:4 ~ 2,
      IMD_DECILE %in% 5:6 ~ 3,
      IMD_DECILE %in% 7:8 ~ 4,
      IMD_DECILE %in% 9:10 ~ 5,
      TRUE ~ NA_real_
    ),
    LSOA_DATE = 2011
  ) %>%
  select(LSOA_CODE, IMD_SCORE, IMD_HEALTH, IMD_QUINTILE, LSOA_DATE)

# -----------------------
# 2. Load LSOA mapping (Excel) and build weighted_mapping
# -----------------------
mapping <- read_csv("data/LSOA_2011_to_LSOA_2021_Lookup_E_W.csv", name_repair = make.names)

weighted_mapping <- mapping %>%
  left_join(
    mapping %>%
      filter(CHGIND %in% c("M","X")) %>%
      group_by(LSOA21CD) %>%
      summarise(WEIGHT = 1 / n(), .groups = "drop"),
    by = "LSOA21CD"
  ) %>%
  mutate(WEIGHT = if_else(CHGIND %in% c("S","U"), 1, WEIGHT))

# -----------------------
# 3. Convert IMD 2011 to 2021 using weighted mapping
# -----------------------
# join weighted mapping to imd (2011) via LSOA11CD -> LSOA_CODE
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
    IMD_QUINTILE = ntile(-IMD_SCORE, 5),
    LSOA_DATE = 2021
  ) %>%
  rename(LSOA_CODE = LSOA21CD)

# combine 2011 rows and weighted 2021 rows to make imd_2011_2021
imd_2011_2021 <- bind_rows(
  imd %>% mutate(IMD_HEALTH = coalesce(IMD_HEALTH, 0)),
  weighted_imd %>% select(LSOA_CODE, IMD_SCORE, IMD_HEALTH, IMD_QUINTILE, LSOA_DATE)
)

# -----------------------
# 4. Pull ONS pop from SQL (only years >= 2017)
# -----------------------
ons_raw <- dbGetQuery(con, "
  SELECT
    Area_Code AS LSOA_CODE,
    Sex,
    Age,
    YEAR(Effective_Snapshot_Date) AS YEAR,
    Size AS POP
  FROM Demography.ONS_Population_Estimates_For_LSOAs_By_Year_Of_Age1
  WHERE YEAR(Effective_Snapshot_Date) >= 2017
")

# -----------------------
# 5. Clean ONS ages & sex, derive YEAR and LSOA_DATE (2011 vs 2021)
# -----------------------
ons_clean <- ons_raw %>%
  mutate(
    # clean Age: handle '90+' (or other trailing +), then numeric
    Age = as.character(Age),
    Age = str_replace(Age, "\\+", ""),        # "90+" -> "90"
    Age = na_if(Age, ""),                     # empty -> NA
    Age = suppressWarnings(as.integer(Age)),  # non-numeric -> NA
    
    # YEAR from Effective_Snapshot_Date
    YEAR = lubridate::year(as.Date(Effective_Snapshot_Date)),
    
    # Determine which LSOA scheme this observation refers to (option A)
    LSOA_DATE = if_else(YEAR <= 2020, 2011L, 2021L),
    
    # Robust Sex mapping: handle common variants
    SEX = case_when(
      str_to_upper(Sex) %in% c("M", "MALE", "MAL", "MAL.", "Males", "MALES") ~ "MALE",
      str_to_upper(Sex) %in% c("F", "FEMALE", "FEM", "FEMALES") ~ "FEMALE",
      TRUE ~ as.character(Sex)
    )
  ) %>%
  select(LSOA_CODE, YEAR, LSOA_DATE, SEX, Age, POP)

# -----------------------
# 6. Aggregate into age groups (per LSOA_CODE, YEAR, SEX)
# -----------------------
ons_agegroup <- ons_clean %>%
  mutate(
    AGE = case_when(
      !is.na(Age) & Age >= 0 & Age <= 4   ~ "0_4",
      !is.na(Age) & Age >= 5 & Age <= 14  ~ "5_14",
      !is.na(Age) & Age >= 15 & Age <= 44 ~ "15_44",
      !is.na(Age) & Age >= 45 & Age <= 64 ~ "45_64",
      !is.na(Age) & Age >= 65 & Age <= 74 ~ "65_74",
      !is.na(Age) & Age >= 75 & Age <= 84 ~ "75_84",
      !is.na(Age) & Age >= 85             ~ "85_PLUS",
      TRUE ~ NA_character_
    )
  ) %>%
  filter(!is.na(AGE)) %>%  # drop weird/missing ages if any
  group_by(LSOA_CODE, YEAR, LSOA_DATE, SEX, AGE) %>%
  summarise(Total_POP = sum(POP, na.rm = TRUE), .groups = "drop") %>%
  distinct()

# -----------------------
# 7. Pivot to wide (SEX_AGE columns) and join IMD (matching LSOA_DATE to YEAR mapping)
# -----------------------
ons_pivot <- ons_agegroup %>%
  pivot_wider(
    names_from = c(SEX, AGE),
    values_from = Total_POP,
    values_fill = 0,
    names_sep = "_"
  )

# Ensure all expected columns exist; define helper to safely extract or 0
get_col_safe <- function(df, nm) if (nm %in% names(df)) df[[nm]] else 0

# Join with imd_2011_2021 using both LSOA_CODE and LSOA_DATE (so 2017-20 join to 2011 rows)
ons_with_imd <- ons_pivot %>%
  left_join(imd_2011_2021, by = c("LSOA_CODE", "LSOA_DATE"))

# Coalesce missing IMD_HEALTH to 0 (so Carr-Hill multiplier doesn't produce NA)
ons_with_imd <- ons_with_imd %>% mutate(IMD_HEALTH = coalesce(IMD_HEALTH, 0))

# -----------------------
# 8. Apply Carr-Hill adjustment per LSOA x YEAR (reproduce exactly)
# -----------------------
# Ensure we have column names matching the expected pattern like MALE_0_4, FEMALE_85_PLUS etc.
# We'll compute totals by pulling columns by name (using get_col_safe)
adj_pop_df <- ons_with_imd %>%
  rowwise() %>%
  mutate(
    MALE_0_4     = get_col_safe(cur_data_all(), "MALE_0_4"),
    MALE_5_14    = get_col_safe(cur_data_all(), "MALE_5_14"),
    MALE_15_44   = get_col_safe(cur_data_all(), "MALE_15_44"),
    MALE_45_64   = get_col_safe(cur_data_all(), "MALE_45_64"),
    MALE_65_74   = get_col_safe(cur_data_all(), "MALE_65_74"),
    MALE_75_84   = get_col_safe(cur_data_all(), "MALE_75_84"),
    MALE_85_PLUS = get_col_safe(cur_data_all(), "MALE_85_PLUS"),
    
    FEMALE_0_4     = get_col_safe(cur_data_all(), "FEMALE_0_4"),
    FEMALE_5_14    = get_col_safe(cur_data_all(), "FEMALE_5_14"),
    FEMALE_15_44   = get_col_safe(cur_data_all(), "FEMALE_15_44"),
    FEMALE_45_64   = get_col_safe(cur_data_all(), "FEMALE_45_64"),
    FEMALE_65_74   = get_col_safe(cur_data_all(), "FEMALE_65_74"),
    FEMALE_75_84   = get_col_safe(cur_data_all(), "FEMALE_75_84"),
    FEMALE_85_PLUS = get_col_safe(cur_data_all(), "FEMALE_85_PLUS"),
    
    TOTAL_POP = sum(
      MALE_0_4, MALE_5_14, MALE_15_44, MALE_45_64, MALE_65_74, MALE_75_84, MALE_85_PLUS,
      FEMALE_0_4, FEMALE_5_14, FEMALE_15_44, FEMALE_45_64, FEMALE_65_74, FEMALE_75_84, FEMALE_85_PLUS,
      na.rm = TRUE
    ),
    
    # Carr-Hill weighted adjusted pop (same coefficients you supplied)
    ADJUSTED_POP = (
      2.354*MALE_0_4    + 1.000*MALE_5_14  + 0.913*MALE_15_44 + 1.373*MALE_45_64 +
        2.531*MALE_65_74  + 3.254*MALE_75_84 + 3.193*MALE_85_PLUS +
        2.241*FEMALE_0_4  + 1.030*FEMALE_5_14 + 1.885*FEMALE_15_44 +
        2.115*FEMALE_45_64 + 2.820*FEMALE_65_74 + 3.301*FEMALE_75_84 +
        3.090*FEMALE_85_PLUS
    ) * (1.054 ^ IMD_HEALTH)
  ) %>%
  ungroup()

# -----------------------
# 9. Year-by-year normalisation (so adjusted sums match raw totals) — reproduces adj_pop_norm
# -----------------------
# Compute AF per year, then apply back to each LSOA row
year_factors <- adj_pop_df %>%
  group_by(YEAR) %>%
  summarise(
    TOTAL = sum(TOTAL_POP, na.rm = TRUE),
    ADJ   = sum(ADJUSTED_POP, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(AF = if_else(ADJ == 0, 1, TOTAL / ADJ))

adj_pop_norm <- adj_pop_df %>%
  left_join(year_factors %>% select(YEAR, AF), by = "YEAR") %>%
  mutate(
    NEED_ADJ_POP = ADJUSTED_POP * AF
  ) %>%
  select(YEAR, LSOA_CODE, NEED_ADJ_POP, TOTAL_POP) %>%
  arrange(YEAR, LSOA_CODE)

# -----------------------
# 10. Final output
# -----------------------
# adj_pop_norm is the dataframe matching your original pipeline:
# columns: YEAR, LSOA_CODE, NEED_ADJ_POP, TOTAL_POP
adj_pop_norm