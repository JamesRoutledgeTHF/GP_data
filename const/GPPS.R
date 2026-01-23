

GPPS_Datasets <- dbGetQuery(con, "
  SELECT *,
         YEAR(Effective_Snapshot_Date) AS YEAR
  FROM GP_Patient_Survey.Practice_Level_Weighted1
  WHERE YEAR(Effective_Snapshot_Date) >= 2023 AND Field_Name = 'Q32_12pct'
")