library(jsonlite)
library(dplyr)
library(here)
library(tidytext)
library(tidygraph)

set.seed(27)

con_in <- gzfile(here("data-raw", "reddit", "2018.json"))

# id variable is unique for each post
reddit_raw <- stream_in(con_in, pagesize = 5000)

veitch_subreddits <- c("keto", "OkCupid", "childfree")

reddit_fulltext <- reddit_raw |>
  as_tibble() |>
  select(gender, body, author, score, subreddit, id) |>
  filter(subreddit %in% veitch_subreddits) |>
  mutate_at(vars(score), as.integer)

# usethis::use_data(reddit_fulltext, overwrite = TRUE)

# limited anonymization of author handles

set.seed(28)

post_data <- reddit_fulltext |>
  select(-body) |>
  rename(flair = gender) |>
  mutate(node_type = "post")

tokenized <- reddit_fulltext |>
  select(id, body) |>
  unnest_tokens(word, body, token = "words") |>
  count(id, word)

A <- cast_sparse(tokenized, row = id, column = word, value = n)

distinct(tokenized) |>
  arrange(desc(n))

# do any words match post ids? will cause trouble if so
tokenized |>
  filter(id == word)

ig <- igraph::graph_from_biadjacency_matrix(A, weighted = TRUE)

# why the node_type thing is needed below is unclear to me, but it seems to be
# important. consider yourself warned

reddit <- as_tbl_graph(ig) |>
  mutate(
    node_type = ifelse(type, "word", "post")
  ) |>
  left_join(
    post_data,
    by = c(
      "name" = "id",
      "node_type" = "node_type"  # needed to avoid joining post data to word nodes
    )
  )

# check that no post data was joined to word nodes. the following tibble
# should have exactly four rows

reddit |>
  as_tibble() |>
  count(node_type, subreddit)

usethis::use_data(reddit, overwrite = TRUE, version = 3)
