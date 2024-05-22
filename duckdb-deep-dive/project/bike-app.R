library(bslib)
library(dplyr)
library(duckdb)
library(plotly)
library(shinyWidgets)

con <- dbConnect(duckdb("data/dc-bike-share.duckdb"))

trips <- tbl(con, "trips")

trips_clean <- trips |>
  mutate(
    started_at = as_datetime(started_at),
    ended_at = as_datetime(ended_at),
    # this is a DuckDB function
    duration = datediff("second", started_at, ended_at),
    .before = 1
  ) |>
  # trip has to be longer than 1 minute and less than 18 hours
  filter(duration >= 60, duration < 18 * 60 * 60)



# multi drop down for this
count(trips, rideable_type)

# add switch for this
count(trips, member_casual)
trips

date_range <- seq(
  as.Date("2023-01-01"),
  as.Date("2023-12-31"),
  length.out = 365
)

ui <- page_sidebar(
  sidebar = sidebar(
    sliderTextInput(
      inputId = "date_range",
      label = "Choose a range:",
      choices = date_range,
      selected = c(date_range[1], date_range[31])
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
  ),
  layout_columns(
    card(
      plotlyOutput("heatmap")
    ),
    col_widths = c(8, 4)
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

server <- function(input, output, session) {
  filtered_data <- reactive({
    trips_clean |>
      filter(
        rideable_type %in% local(input$bike_type),
        started_at <= local(input$date_range[2]),
        started_at >= local(input$date_range[1]),
        # less than duration hours
        duration / 60 / 60 >= local(input$trip_duration[1]),
        duration / 60 / 60 <= local(input$trip_duration[2])
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
}

shiny::shinyApp(ui, server)
