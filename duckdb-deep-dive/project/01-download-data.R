library(dplyr)
library(duckdb)
library(lubridate)

# create the duckdb database
con <- dbConnect(duckdb("data/dc-bike-share.duckdb"))

# function to get and download months of the data
# Data source: https://s3.amazonaws.com/capitalbikeshare-data/index.html
write_trips <- function(con, mon) {
  tmp <- tempdir()
  url <- glue::glue("https://s3.amazonaws.com/capitalbikeshare-data/2023{mon}-capitalbikeshare-tripdata.zip")
  download_fp <- file.path(tmp, "data.zip")
  curl::curl_download(url, download_fp)
  res <- unzip(download_fp, exdir = tmp)
  .dat <- duckdb_read_csv(con, "trips", res[1])
  unlink(tmp)
  .dat
}

# months to iterate through
months <- stringr::str_pad(1:12, 2, "left", "0")

# download and write the date to duckdb
for (month in months) {
  write_trips(con, month)
}

dbDisconnect(con)
