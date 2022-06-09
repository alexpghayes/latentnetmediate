# library(tidygraph)
# library(tidyverse)
# library(here)
# library(igraph)
# library(gdim)
# library(vsp)
# library(ggraph)
#
#
# load(here("data/AddHealth84Communities.Rda"))
#
# tmp_graph <- AddHealth[[36]]$A %>%
#   graph_from_adjacency_matrix(mode = "undirected") %>%
#   as_tbl_graph(directed = FALSE)
#
# edgelist <- tmp_graph %>%
#   as_edgelist() %>%
#   as_tibble(.name_repair = "minimal") %>%
#   set_names(c("to", "from"))
#
# att <- AddHealth[[36]]$att %>%
#   as_tibble() %>%
#   mutate(
#     id = row_number()
#   ) %>%
#   mutate_at(vars(sex, grade, race), na_if, 0) %>%
#   mutate_at(vars(grade), na_if, 6) %>%
#   mutate(
#     school = paste0("school", school),
#     sex = if_else(sex == 1, "male", "female"),
#   ) %>%
#   # mutate_at(vars(grade), forcats::fct_inseq) %>%
#   mutate_at(vars(sex, race, school), as.factor) %>%
#   # mutate_at(vars(race), forcats::fct_lump_prop, prop = 0.05) %>%
#   dplyr::select(id, everything())
#
# graph <- tbl_graph(nodes = att, edges = edgelist, node_key = "id", directed = FALSE) %>%
#   mutate(
#     degree = centrality_degree(mode = "all"),
#     grade = forcats::fct_inseq(as.factor(grade))
#   )
#
#
# # also ok: kk, graphopt
#
# set.seed(27)
#
# plot_grade <- ggraph(graph, layout = 'lgl') +
#   geom_edge_fan(aes(alpha = stat(index)), show.legend = FALSE) +
#   geom_node_point(aes(size = degree, color = grade)) +
#   scale_color_viridis_d() +
#   theme_graph() +
#   theme(text = element_text(size = 16, family = "serif"))
#
# ggsave(
#   filename = here("figures/presentation/homophily-grade.png"),
#   plot = plot_grade,
#   dpi = 600
# )
#
#
# set.seed(27)
#
# plot_race <- ggraph(graph, layout = 'lgl') +
#   geom_edge_fan(aes(alpha = stat(index)), show.legend = FALSE) +
#   geom_node_point(aes(size = degree, color = race)) +
#   scale_color_viridis_d() +
#   theme_graph() +
#   theme(text = element_text(size = 16, family = "serif"))
#
# ggsave(
#   filename = here("figures/presentation/homophily-race.png"),
#   plot = plot_race,
#   dpi = 600
# )
#
# att
# skimr::skim(att)
#
# A <- as_adjacency_matrix(graph, sparse = TRUE)
# A
#
# rank_est <- eigcv(A, k_max = 200)
# rank_est
#
# k_plot <- plot(rank_est) +
#   theme_minimal(16) +
#   labs(
#     y = "Cross-validated eigenvalue",
#     x = "Latent dimension/number of communities k",
#     title = "There are ~100 communities in the AddHealth network"
#   )
#
# k_plot
#
# ggsave(
#   plot = k_plot,
#   filename = here("figures/presentation/rank.png"),
#   dpi = 600
# )
#
# k <- 120 # from rank plot
#
# fa <- vsp(graph, rank = 5, degree_normalize = TRUE)
# fa
#
# plot_mixing_matrix(fa)
#
# plot_ipr_pairs(fa)
#
# plot_varimax_y_pairs(fa, factors = 1:10)
#
# U <- fa %>%
#   get_varimax_y() %>%
#   dplyr::select(-id)
#
# df <- att %>% bind_cols(U) %>% dplyr::select(-school)
# df
#
# outcome <- lm(level ~ . - id, data = df)
# summary(outcome)
#
# latent_mediator <- lm(as.matrix(U) ~ . - id - level - school, data = att)
# # summary(latent_mediator)
# anova(latent_mediator)
#
#
# covariate_only <- lm(level ~ . - id - school, data = att)
# summary(covariate_only)
#
# plot(emmeans(covariate_only, ~ sex + grade + race))
#
# num_coefs <- length(coef(covariate_only))
#
# tibble(
#   term = names(coef(covariate_only)),
#   cde = coef(outcome)[1:num_coefs],
#   nie = drop(coef(latent_mediator) %*% coef(outcome)[-c(1:num_coefs)]),
#   total = coef(covariate_only)
# )
#
# # playing with k
# decompose_effects <- function(k) {
#   fa <- vsp(graph, rank = k, degree_normalize = TRUE)
#   U <- fa %>%
#     get_varimax_y() %>%
#     dplyr::select(-id)
#
#   df <- att %>% bind_cols(U) %>% dplyr::select(-school)
#   outcome <- lm(level ~ . - id, data = df)
#   latent_mediator <- lm(as.matrix(U) ~ . - id - level - school, data = att)
#   covariate_only <- lm(level ~ . - id - school, data = att)
#
#   tibble(
#     term = names(coef(covariate_only)),
#     cde = coef(outcome)[1:num_coefs],
#     nie = drop(coef(latent_mediator) %*% coef(outcome)[-c(1:num_coefs)]),
#     total = coef(covariate_only),
#     k = k
#   )
# }
#
# all_k <- round(seq(2, 250, length.out = 20))
#
# constr_trt_paths <- map_dfr(all_k, decompose_effects)
#
# constr_trt_paths %>%
#   filter(term != "(Intercept)") %>%
#   pivot_longer(
#     c("nie", "cde", "total"),
#     names_to = "effect"
#   ) %>%
#   filter(effect != "total") %>%
#   mutate(
#     effect = recode(
#       effect,
#       cde = "Natural Direct Effect",
#       nie = "Natural Indirect Effect"
#       # total = "Average Treatment Effect"
#     )
#   ) %>%
#   ggplot() +
#   aes(x = k, y = value, color = effect) +
#   geom_line() +
#   geom_point() +
#   geom_hline(yintercept = 0, linetype = "dashed") +
#   scale_color_viridis_d() +
#   theme_classic(14) +
#   facet_wrap(vars(term)) +
#   labs(
#     title = "Estimated effects as a function of latent space dimension",
#     color = "Effect",
#     x = "Latent dimension of network"
#   ) +
#   theme(
#     axis.title.y = element_blank()
#   )
#
# ggsave(here("figures/addhealth-effects.pdf"))
