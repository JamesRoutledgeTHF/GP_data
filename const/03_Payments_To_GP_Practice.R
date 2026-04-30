
library(tidyverse)
library(dplyr)

finance_datasets <- dbGetQuery(con, "
  SELECT *,
         YEAR(Effective_Snapshot_Date) - 1 AS YEAR
  FROM NHS_Payments_To_GP_Practices.Figures1
  WHERE YEAR(Effective_Snapshot_Date) >= 2017
")

