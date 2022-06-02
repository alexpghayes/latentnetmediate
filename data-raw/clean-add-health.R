library(tidyverse)
library(tidygraph)
library(here)
library(glue)

comms <- 1:84

# data for comm 1 clearly isn't coded correctly, comm 48 attribute data wasn't available online
remove <- c(1, 48)

addhealth <- list()

for (i in setdiff(comms, remove)) {

  edgelist_file <- here("data-raw", "addhealth", glue("edgelist{i}.dat"))
  node_data_file <- here("data-raw", "addhealth", glue("node_data{i}.dat"))

  edges <- read_table(
    edgelist_file,
    skip = 4,
    col_names = c(
      "from", "to", "weight"
    ),
    col_types = c("iid")
  ) |>
    mutate_at(vars(weight), as.integer)

  first_lines <- readLines(node_data_file, n = 15)

  multiple_schools <- any(str_detect(first_lines, "school"))

  extra_lines <- any(stringr::str_detect(first_lines, "LEVEL LABELS"))

  if (multiple_schools) {

    if (extra_lines) {
      skip <- 11
    } else {
      skip <- 9
    }

    col_names <- c("sex", "race", "grade", "school")
  } else {

    if (extra_lines) {
      skip <- 10
    } else {
      skip <- 8
    }

    col_names <- c("sex", "race", "grade")
  }

  # possibly also includes school

  node_data <- read_table(
    node_data_file,
    skip = skip,
    col_names = col_names
  )

  clean_node_data <- node_data |>
    mutate(
      sex = recode(
        sex,
        `1` = "male",
        `2` = "female",
        .default = NA_character_
      ),
      race = recode(
        race,
        `1` = "white",
        `2` = "black",
        `3` = "hispanic",
        `4` = "asian",
        `5` = "mixed/other",
        .default = NA_character_
      ),
      grade = na_if(grade, 0)
    ) |>
    mutate_at(vars(sex, race), as.factor) |>
    mutate_at(vars(grade), as.integer)

  if (multiple_schools) {
    clean_node_data <- clean_node_data |>
      mutate(
        school = recode(
          school,
          `0` = "A",
          `1` = "B",
          .default = NA_character_
        )
      ) |>
      mutate_at(vars(school), as.factor)
  }

  graph <- tbl_graph(nodes = clean_node_data, edges = edges)

  addhealth[[i]] <- graph
}

usethis::use_data(addhealth, overwrite = TRUE)
