library(odbc)
library(DBI)

con <- DBI::dbConnect(
  odbc::odbc(),
  Driver = "ODBC Driver 17 for SQL Server",
  Server = "factsanddimensions.database.windows.net",
  Database = "Health_Foundation_UserDB",
  UID = "", #insert username
  PWD = "", #insert password
  Encrypt = "yes"
  
)

