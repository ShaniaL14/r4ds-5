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
  group_by(fire_year) |>
  summarise(total_acres = sum(gis_acres, na.rm = TRUE)) |>
  ggplot(aes(x = fire_year, y = total_acres)) +
  geom_line() +
  labs(
    title = "Total Acres Burned by Year",
    x = "Fire year",
    y = "Total acres burned",
    caption = "Source: wildfire records in fires."
  )
#
#
#
#
#
