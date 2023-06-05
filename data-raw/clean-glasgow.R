library(here)
library(tidygraph)
library(tidyverse)

load(here("data-raw", "glasgow", "Glasgow-friendship.RData"))
load(here("data-raw", "glasgow", "Glasgow-demographic.RData"))
load(here("data-raw", "glasgow", "Glasgow-substances.RData"))
load(here("data-raw", "glasgow", "Glasgow-various.RData"))
load(here("data-raw", "glasgow", "Glasgow-lifestyle.RData"))
load(here("data-raw", "glasgow", "Glasgow-selections.RData"))

yesno_from_12 <- function(x) {
  as.factor(if_else(x == 2, "yes", "no"))
}

node_constant <- tibble(
  age = age,                    # age on jan 1, 1995
  selection129 = selection129,
  selection50 = selection50
) |>
  mutate(
    sex_fct = as.factor(recode(sex.F, `1` = "Male", `2` = "Female"))
  ) |>
  bind_cols(familysmoking) |>
  janitor::clean_names() |>
  mutate_at(
    vars(contains("smoking")),
    yesno_from_12
  )

code_alcohol <- function(alcohol) {
  chr <- case_when(
    alcohol == 5 ~ "More than once a week",
    alcohol == 4 ~ "Once a week",
    alcohol == 3 ~ "Once a month",
    alcohol == 2 ~ "Once or twice a year",
    TRUE ~ "Never"
  )

  factor(
    chr,
    levels = c(
      "More than once a week",
      "Once a week",
      "Once a month",
      "Once or twice a year",
      "Never"
    ),
    ordered = TRUE
  )
}

code_cannabis <- function(cannabis) {
  case_when(
    cannabis == 4 ~ "Regular",
    cannabis == 3 ~ "Occasional",
    cannabis == 2 ~ "Tried once",
    TRUE ~ "Never"
  )
}

code_tobacco <- function(tobacco) {
  case_when(
    tobacco == 3 ~ "Regular",
    tobacco == 2 ~ "Occasional",
    TRUE ~ "Never"
  )
}

code_leisure <- function(activity) {
  case_when(
    activity == 1 ~ "Most days",
    activity == 2 ~ "Once a week",
    activity == 3 ~ "Once a month",
    TRUE ~ "Never"
  )
}

code_music <- function(genre) {
  case_when(
    genre == 1 ~ "yes",
    TRUE ~ "no"
  )
}

leisure_activities <- c(
  "music_at_home",
  "shopping",
  "reading",
  "watch_sports",
  "play_sports",
  "hang_streets",
  "videogames",
  "hobby",
  "organized",
  "movies",
  "concerts",
  "church",
  "pet",
  "dancing",
  "be_bored"
)

leisure_colnames <- paste0("leisure_", leisure_activities)

colnames(leisure1) <- leisure_colnames
colnames(leisure2) <- leisure_colnames
colnames(leisure3) <- leisure_colnames

music_genres <- paste0("music_", colnames(music1))

colnames(music1) <- music_genres
colnames(music2) <- music_genres
colnames(music3) <- music_genres

node1 <- tibble(
  alcohol_int = alcohol[, 1],
  tobacco_int = tobacco[, 1],
  cannabis_int = cannabis[, 1],
  alcohol_fct = as.factor(
    code_alcohol(alcohol_int)
  ),
  tobacco_fct = as.factor(
    code_tobacco(tobacco_int)
  ),
  cannabis_fct = as.factor(
    code_cannabis(cannabis_int)
  ),
  money = na_if(money[, 1], -1),
  romantic = yesno_from_12(romantic[, 1]),
) |>
  bind_cols(leisure1, music1) |>
  mutate_at(vars(all_of(leisure_colnames)), code_leisure) |>
  mutate_at(vars(all_of(music_genres)), code_music)

node2 <- tibble(
  alcohol_int = alcohol[, 2],
  tobacco_int = tobacco[, 2],
  cannabis_int = cannabis[, 2],
  alcohol_fct = as.factor(
    code_alcohol(alcohol_int)
  ),
  tobacco_fct = as.factor(
    code_tobacco(tobacco_int)
  ),
  cannabis_fct = as.factor(
    code_cannabis(cannabis_int)
  ),
  money = na_if(money[, 2], -1),
  romantic = yesno_from_12(romantic[, 2]),
) |>
  bind_cols(leisure2, music2) |>
  mutate_at(vars(all_of(leisure_colnames)), code_leisure) |>
  mutate_at(vars(all_of(music_genres)), code_music)

node3 <- tibble(
  alcohol_int = alcohol[, 3],
  tobacco_int = tobacco[, 3],
  cannabis_int = cannabis[, 3],
  alcohol_fct = as.factor(
    code_alcohol(alcohol_int)
  ),
  tobacco_fct = as.factor(
    code_tobacco(tobacco_int)
  ),
  cannabis_fct = as.factor(
    code_cannabis(cannabis_int)
  ),
  money = na_if(money[, 3], -1),
  romantic = yesno_from_12(romantic[, 3]),
) |>
  bind_cols(leisure3, music3) |>
  mutate_at(vars(all_of(leisure_colnames)), code_leisure) |>
  mutate_at(vars(all_of(music_genres)), code_music)

clean_friendships <- function(friendships) {
  friendships |>
    as_tibble(rownames = "from") |>
    pivot_longer(-from, names_to = "to", values_to = "friendship") |>
    filter(friendship > 0) |>
    mutate(
      friendship = case_when(
        friendship == 1 ~ "Best friend",
        friendship == 2 ~ "Friend",
        friendship == 10 ~ "Structurally missing"
      )
    ) |>
    as_tbl_graph(directed = TRUE)
}

graph1 <- friendship.1 |>
  clean_friendships() |>
  activate(nodes) |>
  mutate(!!!node_constant, !!!node1)

graph2 <- friendship.2 |>
  clean_friendships() |>
  activate(nodes) |>
  mutate(!!!node_constant, !!!node2)

graph3 <- friendship.3 |>
  clean_friendships() |>
  activate(nodes) |>
  mutate(!!!node_constant, !!!node3)

glasgow <- list(graph1, graph2, graph3)

usethis::use_data(glasgow, overwrite = TRUE)
