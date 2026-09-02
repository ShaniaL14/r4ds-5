#
#
#
#
#
#
#
#
#| message: false

library(tidyverse)
library(purrr)
library(leaflet)
library(rvest)
library(httr2)
library(jsonlite)
#
#
#
raw <- read_json("data/wildfires.geojson")
print(length(raw$features))
#
#
#
fire1 <- raw$features[[1]]
names(fire1)
#
#
#
first_pair <- fire1$geometry$coordinates[[1]][[1]]
first_pair
#
#
#
features <- tibble(features = raw$features)
print(features)
#
#
#
features <- features |> unnest_wider(features)
print(features)
#
#
#
features <- features |> unnest_wider(properties)
print(features)
#
#
#
#| cache: true

fires <- features |>
  unnest_wider(geometry, names_sep = "_") |>
  select(incident, gis_acres, fire_year, agency, state, geometry_coordinates) |>
  mutate(
    gis_acres = as.numeric(gis_acres),
    fire_year = as.integer(fire_year)
  )
#
#
#
fires |>
  group_by(state) |>
  summarise(total_acres = sum(gis_acres, na.rm = TRUE)) |>
  arrange(desc(total_acres)) |>
  slice_head(n = 10) |>
  ggplot(aes(x = fct_reorder(state, total_acres), y = total_acres)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Total Acres Burned by State",
    x = "State",
    y = "Total acres burned",
    caption = "Source: wildfire records in fires."
  )
#
#
#
big_fires <- fires |>
  filter(gis_acres >= 100000) |>
  mutate(
    lon = map_dbl(
      geometry_coordinates,
      \(coordinates) mean(map_dbl(purrr::flatten(coordinates), \(pair) pair[[1]]))
    ),
    lat = map_dbl(
      geometry_coordinates,
      \(coordinates) mean(map_dbl(purrr::flatten(coordinates), \(pair) pair[[2]]))
    )
  )
#
#
#
big_fires |>
  count(agency) |>
  ggplot(aes(x = agency, y = n)) +
  geom_col() +
  labs(
    title = "Number of Big Fires by Managing Agency",
    x = "Managing agency",
    y = "Number of fires"
  )
#
#
#
leaflet(big_fires) |>
  addTiles() |>
  addCircleMarkers(
    lng = ~lon,
    lat = ~lat,
    radius = ~sqrt(gis_acres) / 100,
    popup = ~incident
  ) |>
  setView(lng = -115, lat = 39, zoom = 4)
#
#
#
#
#
