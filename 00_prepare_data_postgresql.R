library(DBI)
library(RPostgres)

con <- dbConnect(
  RPostgres::Postgres(), # Or RPostgreSQL::PostgreSQL()
  dbname = "guardiaguas",
  host = "200.132.11.22",
  port = 1305, # Or your specific port
  user = "guardia",
  password = "guardia_2025"
)
