#calculate average patinet list for financial year from patients registered at a GP practice dataset 

#use this to calcualte average yearly registered users list but only using quarterly registered patients 
avg_patients_list <- dbGetQuery(con, "
 WITH yearly_periods AS (
  SELECT  
    [GP_Practice_Code],
    CASE
      WHEN MONTH([Effective_Snapshot_Date]) >= 4 THEN CAST(YEAR([Effective_Snapshot_Date]) AS VARCHAR)   
      ELSE CAST(YEAR([Effective_Snapshot_Date]) - 1 AS VARCHAR)  
    END AS Financial_Year_Start,
    CASE
      WHEN MONTH([Effective_Snapshot_Date]) = 1 THEN CAST(YEAR([Effective_Snapshot_Date]) AS VARCHAR) + '-01-01'
      WHEN MONTH([Effective_Snapshot_Date]) = 4 THEN CAST(YEAR([Effective_Snapshot_Date]) AS VARCHAR) + '-04-01'
      WHEN MONTH([Effective_Snapshot_Date]) = 7 THEN CAST(YEAR([Effective_Snapshot_Date]) AS VARCHAR) + '-07-01'
      WHEN MONTH([Effective_Snapshot_Date]) = 10 THEN CAST(YEAR([Effective_Snapshot_Date]) AS VARCHAR) + '-10-01'
      ELSE NULL
    END AS Period,
    [Size]
  FROM 
    [Demography].[No_Of_Patients_Regd_At_GP_Practice_Single_Age1]
  WHERE
    MONTH([Effective_Snapshot_Date]) IN (1, 4, 7, 10) 
),
total_size_per_financial_year AS (
  SELECT
    CAST(Financial_Year_Start AS VARCHAR) + '/' + RIGHT(CAST(CAST(Financial_Year_Start AS INT) + 1 AS VARCHAR), 2) AS Financial_Year,
    SUM([Size]) AS Total_Size
  FROM
    yearly_periods
  WHERE Period IS NOT NULL
  GROUP BY 
    Financial_Year_Start
)
SELECT
  Financial_Year,
  Total_Size / 4.0 AS Avg_Size_Per_Financial_Year
FROM 
  total_size_per_financial_year
ORDER BY 
  Financial_Year;

")