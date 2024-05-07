library(dplyr)
library(duckdb)
library(nycflights13)

drv <- duckdb()
con <- dbConnect(drv)

# copy the df to duckdb
copy_to(con, flights)
copy_to(con, weather)

# list the tables in the db

dbListTables(con)


# make a connection object to it
flights <- tbl(con, "flights")
weather <- tbl(con, "weather")
