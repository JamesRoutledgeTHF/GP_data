library(odbc)
library(DBI)

con <- DBI::dbConnect(
  odbc::odbc(),
  Driver = "ODBC Driver 17 for SQL Server",
  Server = "factsanddimensions.database.windows.net",
  Database = "Health_Foundation_UserDB",
  UID = "Health_Foundation", #insert username
  PWD = "2933Werthers91334Orion", #insert password
  Encrypt = "yes"
  
)

