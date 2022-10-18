library(jsonlite)
library(dplyr)
library(here)
library(tidytext)
library(tidygraph)
library(ids)

set.seed(27)

con_in <- gzfile(here("data-raw", "reddit", "2018.json"))

reddit_raw <- stream_in(con_in, pagesize = 5000)

veitch_subreddits <- c("keto", "OkCupid", "childfree")

reddit_fulltext <- reddit_raw |>
  as_tibble() |>
  select(gender, body, author, score, subreddit) |>
  filter(subreddit %in% veitch_subreddits) |>
  mutate(
    post_id = row_number()
  ) |>
  mutate_at(vars(score), as.integer)

usethis::use_data(reddit_fulltext, overwrite = TRUE)

# reddit_fulltext <- slice_sample(reddit_fulltext, n = 100) # TODO: remove this once a large lazyload DB is no longer a nuisance

# limited anonymization of author handles

set.seed(28)

pseudonyms <- reddit_fulltext |>
  distinct(author) |>
  mutate(
    author_pseudonym = ids::adjective_animal(n = n())
  )

post_data <- reddit_fulltext |>
  select(gender, author, score, subreddit, post_id) |>
  rename(author_gender = gender) |>
  mutate(post_id_chr = as.character(post_id)) |>
  left_join(pseudonyms, by = "author") |>
  select(-author)

tokenized <- reddit_fulltext |>
  select(post_id, body) |>
  unnest_tokens(word, body, token = "tweets") |>
  count(post_id, word)

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
