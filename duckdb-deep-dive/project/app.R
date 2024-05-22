library(dplyr)
library(dbplyr)
library(duckdb)
library(ggplot2)
library(bslib)
library(shinyWidgets)
library(shiny)
library(sf)
library(rdeck)
library(plotly)
library(lubridate)

con <- dbConnect(duckdb("duckdb-deep-dive/taxi.duckdb"))

taxi <- tbl(
  con,
  "read_parquet('taxi-data-2019-partitioned/*/*.parquet')"
)

locs_raw <- sf::read_sf("data/taxi-zones/taxi_zones.shp") |>
  select(loc_id = LocationID) |>
  sf::st_transform(4326)

pickups <- taxi |>
  filter(fare_amount > 0, pickup_datetime <= "2019-01-07") |>
  group_by(pickup_location_id) |>
  summarise(frequency = n()) |>
  collect()

deck_df <- left_join(
  locs_raw,
  pickups,
  by = c("loc_id" = "pickup_location_id")
) |>
  mutate(frequency = coalesce(frequency, 0))



rd <- rdeck(
  initial_view_state = view_state(
    center = {
      sf::st_bbox(deck_df) |>
        sf::st_as_sfc() |>
        sf::st_centroid()
    }[[1]],
    zoom = 10,
    # bearing = 45,
    pitch = 90
  )
) |>
  add_polygon_layer(
    data = deck_df,
    id = "freqs",
    pickable = TRUE,
    auto_highlight = TRUE,
    extruded = TRUE,
    get_polygon = geometry,
    get_fill_color = scale_color_linear(frequency),
    opacity = 0.25
  )


times_range_raw <- taxi |>
  summarise(min_dt = min(pickup_datetime), max_dt = max(pickup_datetime)) |>
  collect()

# dataset for the initial plot to be rendered
initial_plot_data <- taxi |>
  filter(fare_amount > 0) |>
  mutate(
    hour = lubridate::hour(pickup_datetime),
    dow = lubridate::wday(pickup_datetime, label = TRUE),
  ) |>
  group_by(dow, hour) |>
  summarise(
    avg_tip = sum(tip_amount, na.rm = TRUE) / sum(total_amount),
    .groups = "drop"
  ) |>
  collect()



create_date_range <- function(month) {
  start_date <- as_date(paste0("2019-", month, "-01"))
  end_date <- ceiling_date(start_date, "month") - 1

  n_days <- end_date - start_date
  seq(start_date, end_date, length.out = as.integer(n_days))
}

# Helper to generate the heat map via ggplot
heatmap_plot <- function(data) {
  ggplot(data, aes(dow, hour, fill = avg_tip)) +
    geom_tile() +
    scale_fill_viridis_c(option = "magma") +
    theme_minimal() +
    labs(x = "", y = "")
}


ui <- page_sidebar(
  sidebar = sidebar(
    pickerInput(
      inputId = "month",
      label = "Choose a month",
      choices = month.abb
    ),
    sliderTextInput(
      inputId = "date_range",
      label = "Choose a range:",
      choices = seq(as.Date("2019-01-01"), as.Date("2019-01-31"), length.out = 31),
      selected = c(as.Date("2019-01-01"), as.Date("2019-01-31"))
    )
  ),
  layout_columns(
    card(
      card_body(rdeckOutput("map"))
    ),
    card(
      value_box("Number of records", 69420),
      card_body(plotly::plotlyOutput("heatmap"), fillable = TRUE)
    )
  )
)


server <- function(input, output, session) {
  # reactive data
  heatmap_data <- reactive({
    date_range <- as.POSIXct(input$date_range, tz = "UTC")
    res <- taxi |>
      filter(
        fare_amount > 0,
        pickup_datetime >= local(date_range[1]),
        pickup_datetime <= local(date_range[2])
      ) |>
      mutate(
        hour = lubridate::hour(pickup_datetime),
        dow = lubridate::wday(pickup_datetime, label = TRUE),
      ) |>
      group_by(dow, hour) |>
      summarise(
        avg_tip = sum(tip_amount, na.rm = TRUE) / sum(total_amount),
        # avg_tip = mean(total_amount, na.rm = TRUE),
        .groups = "drop"
      ) |>
      collect()

    res
  })

  filtered_data <- reactive({
    res <- trip_counts(taxi, locs_raw, input$date_range[1], input$date_range[2])
    res
  })


  # render the initial map
  output$map <- renderRdeck(rd)

  observe({
    rdeck_proxy("map") |>
      add_polygon_layer(
        data = filtered_data(),
        id = "freqs",
        pickable = TRUE,
        auto_highlight = TRUE,
        extruded = TRUE,
        get_polygon = geometry,
        get_fill_color = scale_color_linear(frequency),
        opacity = 0.25
      )
  })

  observeEvent(input$month, {
    updateSliderTextInput(
      session,
      "date_range",
      choices = create_date_range(input$month)
    )
  })

  output$heatmap <- plotly::renderPlotly({
    taxi |>
      filter(fare_amount > 0) |>
      mutate(
        hour = hour(pickup_datetime),
        dow = wday(pickup_datetime, label = TRUE),
      ) |>
      group_by(dow, hour) |>
      summarise(
        avg_tip = sum(tip_amount, na.rm = TRUE) / sum(total_amount),
        .groups = "drop"
      ) |>
      collect() |>
      heatmap_plot()
  })

  # render the first plot
  output$heatmap <- renderPlotly(heatmap_plot(heatmap_data()))
}



shiny::shinyApp(ui, server)

trip_counts <- function(.df, .sdf, min_date, max_date) {
  pickups <- .df |>
    filter(
      fare_amount > 0,
      trip_distance > 0,
      pickup_datetime >= min_date,
      pickup_datetime <= max_date
    ) |>
    group_by(pickup_location_id) |>
    summarise(frequency = n()) |>
    collect()

  left_join(
    .sdf,
    pickups,
    by = c("loc_id" = "pickup_location_id")
  ) |>
    mutate(frequency = coalesce(frequency, 0))
}

# use this to reactively update the plot
# we should only allow one month at a time

# update_polygon_layer(
#   rd,
#   id = "freqs",
#   data = mutate(deck_df, frequency = frequency / 1000),
#   get_elevation = frequency,
#   get_fill_color = scale_color_linear(frequency),
#   opacity = 0.25
# )
