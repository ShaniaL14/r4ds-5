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
top_fires <- big_fires |>
  arrange(desc(gis_acres)) |>
  slice_head(n = 10) |>
  mutate(
    polygon_lng = map(geometry_coordinates, \(coordinates) {
      map_dbl(coordinates[[1]], \(pair) pair[[1]])
    }),
    polygon_lat = map(geometry_coordinates, \(coordinates) {
      map_dbl(coordinates[[1]], \(pair) pair[[2]])
    })
  )

top_fires_sf <- top_fires |>
  mutate(
    geometry = map(
      geometry_coordinates,
      \(coordinates) sf::st_polygon(list(
        do.call(rbind, purrr::map(coordinates[[1]], unlist))
      ))
    )
  ) |>
  sf::st_as_sf(sf_column_name = "geometry", crs = 4326)

leaflet(top_fires_sf) |>
  addTiles() |>
  addPolygons(
    popup = ~paste0(
      "<strong>", incident, "</strong><br>",
      "Year: ", fire_year, "<br>",
      "Acres: ", format(gis_acres, big.mark = ",")
    )
  ) |>
  fitBounds(
    lng1 = min(unlist(top_fires$polygon_lng)),
    lat1 = min(unlist(top_fires$polygon_lat)),
    lng2 = max(unlist(top_fires$polygon_lng)),
    lat2 = max(unlist(top_fires$polygon_lat))
  )
#
#
#
imdb_snapshots <- readRDS("data/imdb_snapshots.rds")
print(imdb_snapshots)

imdb_snapshots |>
  count(snap_year)
#
#
#
rank_changes <- imdb_snapshots |>
  filter(snap_year %in% c(2015, 2022)) |>
  select(title, snap_year, rank) |>
  pivot_wider(names_from = snap_year, values_from = rank, names_prefix = "rank_") |>
  drop_na(rank_2015, rank_2022) |>
  mutate(rank_change = abs(rank_2022 - rank_2015)) |>
  arrange(desc(rank_change)) |>
  slice_head(n = 10)

rank_changes
#
#
#
#
#
