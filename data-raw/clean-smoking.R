library(tidyverse)
library(here)

smoking_raw <- read_csv(here("data-raw", "smoking.csv"))

smoking <- smoking_raw |>
  select(-1, -smoke, -gender) |>
  as.matrix() |>
  igraph::graph_from_adjacency_matrix() |>
  as_tbl_graph() |>
  mutate(
    smoking = if_else(smoking_raw$smoke == 1, "smokes", "doesn't smoke"),
    sex = if_else(smoking_raw$gender == 1, "female", "male")
  ) |>
  mutate_at(vars(smoking, sex), as.factor)

usethis::use_data(smoking, overwrite = TRUE)

smoking_raw |>
  select(smoke, gender) |>
  skimr::skim()
