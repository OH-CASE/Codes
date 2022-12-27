

#' I don't want to pollute the environment...
cancer_con <- function(db = "NeoCASE_Rshiny") {
  library(odbc)
  library(DBI)
  dbConnect(
    odbc(),
    Driver = "SQL Server",
    Server = "icbcledneo01.sre.case.edu",
    Database = db,
    Prefix = 'dbo',
    Port = 1433
    #, rows_at_time = 1
  )
}