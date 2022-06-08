library(here)
library(tidygraph)
library(tidyverse)

load(here("data-raw", "glasgow", "Glasgow-friendship.RData"))
load(here("data-raw", "glasgow", "Glasgow-demographic.RData"))
load(here("data-raw", "glasgow", "Glasgow-substances.RData"))
load(here("data-raw", "glasgow", "Glasgow-various.RData"))
load(here("data-raw", "glasgow", "Glasgow-lifestyle.RData"))
load(here("data-raw", "glasgow", "Glasgow-selections.RData"))

# turn into a list of tidygraphs with one graph for each of the three timepoints
glasgow <- list()

node_constant <- tibble(
  age = age,
  selection129 = selection129,
  selection50 = selection50,
  sex = sex.F
) |>
  mutate(
    sex = recode(sex.F, `1` = "male", `2` = "female")
  )

node1 <- tibble(
  alcohol = alcohol[, 1],
  cannabis = cannabis[, 1],
  familysmoking = familysmoking[, 1],
  money = money[, 1],
  romantic = romantic[, 1],
  tobacco = tobacco[, 1]
) |>
  bind_cols(leisure1, music1) |>
  janitor::clean_names()

node_constant

usethis::use_data(glasglow, overwrite = TRUE)
