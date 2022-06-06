# library(tidyverse)
# library(fastRG)
# library(GGally)
# library(here)
#
# ppsbm <- planted_partition(n = 100, k = 10, within_block = 0.5, between_block = 0.01)
#
# A <- sample_sparse(ppsbm, poisson_edges = FALSE)
#
# plot_expected_adjacency <- function(ufm) {
#
#   EA <- as(ufm$X %*% ufm$S %*% t(ufm$X), "sparseMatrix")
#
#   summary(EA) %>%
#     ggplot2::ggplot(ggplot2::aes(x = j, y = -i, fill = x)) +
#     ggplot2::geom_tile() +
#     scale_fill_gradient(low = "white", high = "black", limits = c(0, 1)) +
#     ggplot2::theme_void() +
#     theme(
#       legend.position = "none"
#     )
# }
#
# plot_sparse_adjacency_matrix <- function(A) {
#   sign(as(A, "dgCMatrix")) %>%
#     summary(A, uniqT = FALSE) %>%
#     ggplot(aes(j, -i, fill = x)) +
#     geom_tile(fill = "black") +
#     theme_void()
# }
#
# A_plot <- plot_sparse_adjacency_matrix(A)
#
# ggsave(
#   plot = A_plot,
#   here("figures/presentation/adjacency-sample.pdf"),
#   width = 8,
#   height = 8
# )
#
# EA_plot <- plot_expected_adjacency(ppsbm)
#
# ggsave(
#   plot = EA_plot,
#   here("figures/presentation/adjacency-population.pdf"),
#   width = 8,
#   height = 8
# )
