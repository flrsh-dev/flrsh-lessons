library(dplyr)
library(dbplyr)
library(duckdb)

con <- dbConnect(duckdb("duckdb-deep-dive/taxi.duckdb"))

taxi <- tbl(
  con,
  "read_parquet('taxi-data-2019-partitioned/*/*.parquet')"
)

taxi |>
  filter(total_amount > 0) |>
  mutate(
    tip_pct = 100 * tip_amount / total_amount,
    dn = wday(pickup_datetime, label = TRUE),
    hr = hour(pickup_datetime)
  ) |>
  group_by(dn, hr) |>
  summarise(
    avg_tip_pct = mean(tip_pct),
    n = n()
  ) |>
  ungroup() |>
  arrange(-avg_tip_pct) |>
  collect()

boroughs <- tbl_file(con, "zone_lookups.parquet")

left_join(
  taxi,
  boroughs,
  by = c(pickup_location_id = "LocationID")
)

# Find the trips where the start borough is the same
# as the end borough
# count the number of trips per borough
# Include only trips where value was greater than 0
all_boroughs <- taxi |>
  filter(
    total_amount > 0,
    pickup_location_id == dropoff_location_id
  ) |>
  left_join(boroughs, by = c(pickup_location_id = "LocationID")) |>
  count(Borough)

all_boroughs |>
  ungroup() |>
  summarize(n = sum(n))
