


GP_Workforce <- dbGetQuery(con, "
  SELECT
  Detailed_Staff_Role,
    Effective_Snapshot_Date,
    SUM(Measure_Value) AS Total_Measure_Value
  FROM NHS_Workforce.Practice_Level_Census_Data_High_Level1
  WHERE YEAR(Effective_Snapshot_Date) >= 2019 
    AND Staff_Group = 'GP'
    AND Measure = 'FTE'
  GROUP BY Effective_Snapshot_Date, Detailed_Staff_Role
")
