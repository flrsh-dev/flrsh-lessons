library(bslib)
library(shiny)
library(rdeck)
library(dplyr)
library(duckdb)
library(plotly)
library(bsicons)
library(shinyWidgets)

con <- dbConnect(duckdb("data/dc-bike-share.duckdb"))

# This will be the table that we base everything off of.
trips_clean <- tbl(con, "trips") |>
  mutate(
    started_at = as_datetime(started_at),
    ended_at = as_datetime(ended_at),
    # this is a DuckDB function
    duration = datediff("second", started_at, ended_at),
    .before = 1
  ) |>
  # trip has to be longer than 1 minute and less than 18 hours
  filter(duration >= 60, duration < 18 * 60 * 60)

# create the first interactive graphic
# then we will render this and update it interactively
rd <- rdeck(
  mapbox_light(),
  initial_view_state = view_state(
    center = c(-77.10584, 38.95139),
    zoom = 10,
    pitch = 45
  )
) |>
  add_hexagon_layer(
    id = "trip_source",
    data = to_plot,
    coverage = 1,
    pickable = TRUE,
    auto_highlight = TRUE,
    radius = 1000,
    extruded = TRUE,
    opacity = 0.9,
    get_position = start_loc,
    color_range = c(
      rgb(1, 152, 189, maxColorValue = 255),
      rgb(73, 227, 206, maxColorValue = 255),
      rgb(216, 254, 181, maxColorValue = 255),
      rgb(254, 237, 177, maxColorValue = 255),
      rgb(254, 173, 84, maxColorValue = 255),
      rgb(209, 55, 78, maxColorValue = 255)
    )
  )

#' This function creates the data to create the
#' trip duration heatmaps
create_plot_data <- function(.data) {
  .data |>
    mutate(
      hour = lubridate::hour(started_at),
      dow = lubridate::wday(started_at, label = TRUE)
    ) |>
    group_by(hour, dow) |>
    summarise(avg_duration_mins = mean(duration / 60), .groups = "drop") |>
    collect()
}

ui <- page_sidebar(
  sidebar = sidebar(
    width = 325,
    dateRangeInput(
      "date_range",
      "Choose a date range",
      min = "2023-01-01",
      max = "2023-12-31",
      start = "2023-05-01",
      end = "2023-06-31"
    ),
    sliderTextInput(
      inputId = "trip_duration",
      label = "Max trip duration (hours)",
      choices = 0:12,
      selected = c(0, 12)
    ),
    pickerInput(
      "bike_type",
      "Rideshare type",
      choices = c(
        "Docked" = "docked_bike",
        "Classic" = "classic_bike",
        "Electric" = "electric_bike"
      ),
      multiple = TRUE,
      selected = c("docked_bike", "classic_bike", "electric_bike")
    ),
    sliderInput(
      "hex_width",
      "Hexagon Width (meters)",
      min = 10,
      max = 2000,
      value = 250,
      ticks = FALSE
    )
  ),
  layout_columns(
    card(
      card_header("Trip start location"),
      rdeckOutput("hex_map")
    ),
    layout_columns(
      value_box(
        "Total trips",
        value = textOutput("n_trips"),
        showcase = bs_icon("bicycle")
      ),
      card(
        card_header("Average trip duration"),
        plotlyOutput("heatmap")
      ),
      col_widths = 12
    ),
    col_widths = 6
  )
)


server <- function(input, output, session) {
  filtered_data <- reactive({
    trips_clean |>
      filter(
        rideable_type %in% local(input$bike_type),
        started_at <= local(input$date_range[2]),
        started_at >= local(input$date_range[1]),
        # less than duration in hours
        duration / 60 / 60 >= local(input$trip_duration[1]),
        duration / 60 / 60 <= local(input$trip_duration[2])
      ) |>
      collect() |>
      mutate(
        start_loc = wk::xy(start_lng, start_lat, 4326),
        end_loc = wk::xy(end_lng, end_lat, 4326)
      )
  })

  # update heatmap as data is filtered
  output$heatmap <- renderPlotly({
    plot_ly(
      create_plot_data(filtered_data()),
      x = ~dow,
      y = ~hour,
      z = ~avg_duration_mins, type = "heatmap"
    )
  })

  # initialize the rdeck output
  output$hex_map <- renderRdeck(rd)

  # this will update it as needed
  observe({
    rdeck_proxy("hex_map") |>
      add_hexagon_layer(
        id = "trip_source",
        data = filtered_data(),
        coverage = 1,
        pickable = TRUE,
        auto_highlight = TRUE,
        radius = input$hex_width,
        extruded = TRUE,
        opacity = 0.9,
        get_position = start_loc,
        color_range = c(
          rgb(1, 152, 189, maxColorValue = 255),
          rgb(73, 227, 206, maxColorValue = 255),
          rgb(216, 254, 181, maxColorValue = 255),
          rgb(254, 237, 177, maxColorValue = 255),
          rgb(254, 173, 84, maxColorValue = 255),
          rgb(209, 55, 78, maxColorValue = 255)
        )
      )
  })

  output$n_trips <- renderText({
    n_trips <- collect(count(filtered_data()))$n
    scales::comma(n_trips)
  })
}


shinyApp(ui, server)
