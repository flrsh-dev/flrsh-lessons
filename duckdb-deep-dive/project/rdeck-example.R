library(rdeck)
library(dplyr)

#  https://deck.gl/playground
.df <- readr::read_csv("https://raw.githubusercontent.com/visgl/deck.gl-data/master/examples/3d-heatmap/heatmap-data.csv") |>
  mutate(geometry = wk::xy(lng, lat, 4326))

rdeck(
  initial_view_state = view_state(
    zoom = 6.6,
    center = c(-1.4157, 52.232),
    bearing = -27.5,
    pitch = 40.5
  )
) |>
  add_hexagon_layer(
    data = .df,
    id = "heatmap",
    coverage = 1,
    pickable = TRUE,
    auto_highlight = TRUE,
    elevation_range = c(0, 3000),
    elevation_scale = 50,
    extruded = TRUE,
    get_position = geometry,
    color_range = c(
      rgb(1, 152, 189, maxColorValue = 255),
      rgb(73, 227, 206, maxColorValue = 255),
      rgb(216, 254, 181, maxColorValue = 255),
      rgb(254, 237, 177, maxColorValue = 255),
      rgb(254, 173, 84, maxColorValue = 255),
      rgb(209, 55, 78, maxColorValue = 255)
    )
  )


