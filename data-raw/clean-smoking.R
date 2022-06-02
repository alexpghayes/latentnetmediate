library(tidyverse)
library(here)
library(tidygraph)

smoking_raw <- read_csv(here("data-raw", "smoking.csv"))

smoking <- smoking_raw |>
  select(-1, -smoke, -gender) |>
  as.matrix() |>
  igraph::graph_from_adjacency_matrix() |>
  as_tbl_graph() |>
  mutate(
    smokes = if_else(smoking_raw$smoke == 1, "smokes", "doesn't smoke"),
    sex = if_else(smoking_raw$gender == 1, "female", "male")
  ) |>
  mutate_at(vars(smokes, sex), as.factor)

usethis::use_data(smoking, overwrite = TRUE)
