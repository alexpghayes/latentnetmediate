library(tidyverse)
library(here)
library(tidygraph)
library(readxl)

data_path <- here("data-raw", "maltreatment", "Raw_Data_AFreier_MediationPersonalityFunctioning.xlsx")

raw <- read_xlsx(data_path, na = c("-1", "#NULL!"))
raw


recode_ctq <- function(item) {
  case_when(
    item == 1 ~ "never true",
    item == 2 ~ "rarely true",
    item == 3 ~ "sometimes true",
    item == 4 ~ "often true",
    item == 5 ~ "very often true"
  )
}

recode_ctq(raw$CTQ_Q1)

recode_phq <- function(item) {
  case_when(
    item == 1 ~ "not at all",
    item == 2 ~ "several days",
    item == 3 ~ "more than half the days",
    item == 4 ~ "nearly every day"
  )
}

recode_opd <- function(item) {
  case_when(
    item == 1 ~ "does not apply at all",
    item == 2 ~ "rather not applies",
    item == 3 ~ "sometimes applies",
    item == 4 ~ "rather applies",
    item == 5 ~ "fully applies"
  )
}

# not in the data dictionary, but backed out based on counts
# reported in summary tables
recode_gender <- function(gen12) {
  case_when(
    gen12 == 1 ~ "male",
    gen12 == 2 ~ "female"
  )
}

raw |>
  count(gender)

wide <- raw |>
  mutate(across(contains("Filter"), as.logical)) |>
  mutate(
    sex = recode_gender(gender),
    lfd_chr = as.character(lfd)
  ) |>
  filter(Filter_Age_PHQ_OPD) |>
  select(
    lfd_chr, sex, age, PHQ_sum,
    contains("ctq_", ignore.case = FALSE), contains("OPD"),
    -contains("Filter"), -OPD_sum
  ) |>
  mutate(across(contains("OPD"), list(chr = recode_opd)))

A <- wide |>
  select(contains("OPD"), -contains("chr")) |>
  as.matrix()

rownames(A) <- wide$lfd_chr

maltreatment <- A |>
  igraph::graph_from_incidence_matrix(weighted = TRUE) |>
  as_tbl_graph() |>
  mutate(
    node_type = ifelse(type, "OPH_item", "person")
  ) |>
  left_join(
    wide, by = c("name" = "lfd_chr")
  )

usethis::use_data(maltreatment, overwrite = TRUE, version = 3)
