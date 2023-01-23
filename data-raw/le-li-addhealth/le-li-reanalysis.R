library(tidygraph)
library(tidyverse)
library(here)
library(igraph)
library(gdim)
library(vsp)
library(ggraph)

load(here("data-raw", "le-li-addhealth", "AddHealth84Communities.Rda"))

tmp_graph <- AddHealth[[36]]$A %>%
  graph_from_adjacency_matrix(mode = "undirected") %>%
  as_tbl_graph(directed = FALSE)

edgelist <- tmp_graph %>%
  as_edgelist() %>%
  as_tibble(.name_repair = "minimal") %>%
  set_names(c("to", "from"))

att <- AddHealth[[36]]$att %>%
  as_tibble() %>%
  mutate(
    id = row_number()
  ) %>%
  mutate_at(vars(sex, grade, race), na_if, 0) %>%
  mutate_at(vars(grade), na_if, 6) %>%
  mutate(
    school = paste0("school", school),
    sex = if_else(sex == 1, "male", "female"),
  ) %>%
  # mutate_at(vars(grade), forcats::fct_inseq) %>%
  mutate_at(vars(sex, race, school), as.factor) %>%
  # mutate_at(vars(race), forcats::fct_lump_prop, prop = 0.05) %>%
  dplyr::select(id, everything())

graph <- tbl_graph(nodes = att, edges = edgelist, node_key = "id", directed = FALSE) %>%
  mutate(
    degree = centrality_degree(mode = "all"),
    grade = forcats::fct_inseq(as.factor(grade))
  )

graph |>
  as_tibble() |>
  View()

# doesn't handle missing data at the moment
rank_curve <- graph |>
  filter(

  ) |>
  mutate(
    grade = as.numeric(grade)
  ) |>
  sensitivity_curve(
    level ~ sex + C(race, base="white") + grade,
    max_rank = 250,
    ranks_to_consider = 25
  )



# also ok: kk, graphopt

set.seed(27)

plot_grade <- ggraph(graph, layout = 'lgl') +
  geom_edge_fan(aes(alpha = stat(index)), show.legend = FALSE) +
  geom_node_point(aes(size = degree, color = grade)) +
  scale_color_viridis_d() +
  theme_graph() +
  theme(text = element_text(size = 16, family = "serif"))

ggsave(
  filename = here("figures/presentation/homophily-grade.png"),
  plot = plot_grade,
  dpi = 600
)


set.seed(27)

plot_race <- ggraph(graph, layout = 'lgl') +
  geom_edge_fan(aes(alpha = stat(index)), show.legend = FALSE) +
  geom_node_point(aes(size = degree, color = race)) +
  scale_color_viridis_d() +
  theme_graph() +
  theme(text = element_text(size = 16, family = "serif"))

ggsave(
  filename = here("figures/presentation/homophily-race.png"),
  plot = plot_race,
  dpi = 600
)

att
skimr::skim(att)

A <- as_adjacency_matrix(graph, sparse = TRUE)
A

rank_est <- eigcv(A, k_max = 200)
rank_est

k_plot <- plot(rank_est) +
  theme_minimal(16) +
  labs(
    y = "Cross-validated eigenvalue",
    x = "Latent dimension/number of communities k",
    title = "There are ~100 communities in the AddHealth network"
  )

k_plot

ggsave(
  plot = k_plot,
  filename = here("figures/presentation/rank.png"),
  dpi = 600
)
