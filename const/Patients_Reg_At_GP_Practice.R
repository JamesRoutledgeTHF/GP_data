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
