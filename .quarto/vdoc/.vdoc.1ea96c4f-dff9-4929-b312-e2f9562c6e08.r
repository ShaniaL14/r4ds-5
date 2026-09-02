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
  select(title, year, snap_year, rank) |>
  pivot_wider(names_from = snap_year, values_from = rank, names_prefix = "rank_") |>
  drop_na(rank_2015, rank_2022) |>
  mutate(
    release_decade = floor(year / 10) * 10,
    rank_change = abs(rank_2022 - rank_2015)
  )

rank_changes |>
  group_by(release_decade) |>
  summarise(avg_rank_change = mean(rank_change), .groups = "drop") |>
  arrange(release_decade)
#
#
#
imdb_snapshots |>
  filter(title %in% c(
    "The Dark Knight",
    "Inception",
    "Interstellar",
    "The Dark Knight Rises",
    "Memento",
    "The Prestige",
    "Batman Begins"
  )) |>
  select(title, snap_year, rank) |>
  pivot_wider(names_from = snap_year, values_from = rank, names_prefix = "rank_") |>
  arrange(title)
#
#
#
imdb_snapshots |>
  filter(title == "Interstellar") |>
  ggplot(aes(x = factor(snap_year), y = rank)) +
  geom_col() +
  labs(
    title = "Interstellar Rank by Snapshot Year",
    x = "Snapshot year",
    y = "Rank"
  )
#
#
#
film_snapshot_counts <- imdb_snapshots |>
  count(title, name = "n_snapshots")

film_snapshot_counts |>
  count(n_snapshots) |>
  ggplot(aes(x = n_snapshots, y = n)) +
  geom_col() +
  labs(
    title = "Distribution of Film Snapshot Appearances",
    x = "Number of snapshots",
    y = "Number of films"
  )
#
#
#
urls <- c(
  "https://web.archive.org/web/20150101/https://www.imdb.com/chart/top/",
  "https://web.archive.org/web/20170101/https://www.imdb.com/chart/top/",
  "https://web.archive.org/web/20190101/https://www.imdb.com/chart/top/",
  "https://web.archive.org/web/20210101/https://www.imdb.com/chart/top/",
  "https://web.archive.org/web/20220201012049/https://www.imdb.com/chart/top/"
)
years <- c(2015, 2017, 2019, 2021, 2022)

scrape_imdb <- \(address, snapshot_year) {
  response <- request(address) |>
    req_timeout(120) |>
    req_retry(max_tries = 3) |>
    req_perform()

  page <- resp_body_html(response)
  imdb_table <- html_table(html_element(page, "table"))
  rank_title_year <- str_match(
    imdb_table$`Rank & Title`,
    "^\\s*(\\d+)\\.\\s*(.*?)\\s*\\((\\d{4})\\)"
  )
  vote_counts <- html_attr(html_elements(page, "[title]"), "title") |>
    str_subset("based on") |>
    str_extract("(?<=based on )[0-9,]+") |>
    str_remove_all(",") |>
    as.numeric()

  tibble(
    snap_year = snapshot_year,
    rank = as.integer(rank_title_year[, 2]),
    title = rank_title_year[, 3],
    year = as.integer(rank_title_year[, 4]),
    rating = as.numeric(imdb_table$`IMDb Rating`),
    number = vote_counts
  )
}

imdb_snapshots <- map2(urls, years, scrape_imdb) |>
  list_rbind()
#
#
#
#
#
