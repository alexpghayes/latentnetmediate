library(jsonlite)
library(dplyr)
library(here)
library(tidytext)
library(tidygraph)
library(ids)

set.seed(27)

con_in <- gzfile(here("data-raw", "reddit", "2018.json"))

reddit_raw <- stream_in(con_in, pagesize = 5000)

veitch_subreddits <- c("keto", "okcupid", "childfree")

reddit_table <- reddit_raw |>
  as_tibble() |>
  select(gender, body, author, score, subreddit) |>
  filter(subreddit %in% veitch_subreddits) |>
  mutate(
    post_id = row_number()
  ) |>
  mutate_at(vars(score), as.integer)

reddit_table <- slice_sample(reddit_table, n = 100) # TODO: remove this once a large lazyload DB is no longer a nuisance

readr::write_csv(reddit_table, here("data-raw", "reddit", "veitch_subset.csv"))
# limited anonymization of author handles

pseudonyms <- reddit_table |>
  distinct(author) |>
  mutate(
    author_pseudonym = ids::adjective_animal(n = n())
  )

post_data <- reddit_table |>
  select(gender, author, score, subreddit, post_id) |>
  rename(author_gender = gender) |>
  mutate(post_id_chr = as.character(post_id)) |>
  left_join(pseudonyms, by = "author") |>
  select(-author)

tokenized <- reddit_table |>
  select(post_id, body) |>
  unnest_tokens(word, body, token = "tweets") |>
  count(post_id, word)

readr::write_csv(tokenized, here("data-raw", "reddit", "tokenized_post_text.csv"))

A <- cast_sparse(tokenized, row = post_id, column = word, value = n)

distinct(tokenized)

ig <- igraph::graph_from_incidence_matrix(A, weighted = TRUE)

reddit <- as_tbl_graph(ig) |>
  mutate(
    node_type = ifelse(type, "word", "post")
  ) |>
  left_join(
    post_data, by = c("name" = "post_id_chr")
  ) |>
  select(-post_id)

usethis::use_data(reddit, overwrite = TRUE)
