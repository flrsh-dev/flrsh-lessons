locs <- sf::read_sf("/Users/josiahparry/github/flrsh-lessons/data/data-raw/nyc-taxi-2023-taxi/taxi-zones/taxi_zones.shp") |>
  sf::st_transform(4326)

geoms <- locs[1:10, "geometry", drop = FALSE]


plot(geoms$geometry)
library(rdeck)
rdeck(
  initial_view_state = view_state(
    center = {
      sf::st_bbox(geoms) |>
        sf::st_as_sfc() |>
        sf::st_centroid()
    }[[1]],
    zoom = 10,
    bearing = 45,
    pitch = 40.5
  )
) |>
  add_polygon_layer(
    data = locs,
    id = "heatmap",
    coverage = 1,
    pickable = TRUE,
    auto_highlight = TRUE,
    # elevation_range = c(0, 3000),
    # elevation_scale = 50,
    extruded = TRUE,
    get_polygon = geometry,
    # color_range = c(
    #   rgb(1, 152, 189, maxColorValue = 255),
    #   rgb(73, 227, 206, maxColorValue = 255),
    #   rgb(216, 254, 181, maxColorValue = 255),
    #   rgb(254, 237, 177, maxColorValue = 255),
    #   rgb(254, 173, 84, maxColorValue = 255),
    #   rgb(209, 55, 78, maxColorValue = 255)
    # )
  )
