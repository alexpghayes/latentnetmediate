library(SpatialEpi)
library(spdep)
library(tidygraph)

data(NYleukemia)

sp.obj <- NYleukemia$spatial.polygon
population <- NYleukemia$data$population
cases <- NYleukemia$data$cases
tract <- NYleukemia$data$censustract.FIPS

## Identify the 4 census tract to be merged into their surrounding census tracts
remove <- NYleukemia$surrounded
add <- NYleukemia$surrounding

## Merge population and case counts and geographical objects accordingly
population[add] <- population[add] + population[remove]
population <- population[-remove]

cases[add] <- cases[add] + cases[remove]
cases <- cases[-remove]

tract <- as.character(tract)
merged_tracts <- purrr::map2(tract[add], tract[remove], c)
tract <- as.list(tract)
tract[add] <- merged_tracts
tract <- tract[-remove]

sp.obj <- SpatialPolygons(
  sp.obj@polygons[-remove],
  proj4string = CRS("+proj=longlat +ellps=WGS84")
)

A <- nb2mat(poly2nb(sp.obj), style = "B")

node_data <- tibble(
  population = population,
  cases = cases,
  censustract_fips = tract
)

leukemia <- graph_from_adjacency_matrix(A, mode = "undirected") |>
  as_tbl_graph() |>
  activate(nodes) |>
  mutate(!!!node_data)

usethis::use_data(leukemia, overwrite = TRUE)
