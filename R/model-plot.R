# rename for Z and X clarity

expected_z_pre_trt <- function(model) {
  stopifnot(inherits(model, "perfect"))
  untrt <- rep(0, length(model$trt))
  EZ <- matrix(1, nrow = model$n) %*% model$ztheta_0 + untrt %*% model$ztheta_t + model$zC %*% model$ztheta_c + untrt * model$zC %*% model$ztheta_tc
  EZ
}

expected_z_post_trt <- function(model) {
  stopifnot(inherits(model, "perfect"))

  EZ <- matrix(1, nrow = model$n) %*% model$ztheta_0 + model$trt %*% model$ztheta_t + model$zC %*% model$ztheta_c + model$trt * model$zC %*% model$ztheta_tc
  EZ
}

z_pre_trt <- function(model) {
  stopifnot(inherits(model, "perfect"))

  EZ_pre <- expected_z_pre_trt(model)
  EZ_post <- expected_z_post_trt(model)
  res <- model$zX - EZ_post

  EZ_pre + res
}

z_post_trt <- function(model) {
  stopifnot(inherits(model, "perfect"))
  model$zX
}

x_post_trt <- function(model) {
  stopifnot(inherits(model, "perfect"))
  model$X
}

# conditional on X, rather than T, C
expected_a_pre_trt <- function(model) {
  stopifnot(inherits(model, "perfect"))

  A_pre <- model$A_model
  A_pre$X <- z_pre_trt(model)
  X <- ASE(A_pre)
  Matrix::tcrossprod(X)
}

expected_a_post_trt <- function(model) {
  stopifnot(inherits(model, "perfect"))

  Matrix::tcrossprod(model$X)
}

threshold <- function(X, threshold = 1e-10) {
  X[abs(X) < threshold] <- 0
  X
}

#' Title
#'
#' @param model TODO
#' @param A TODO
#'
#' @return TODO
#' @export
#'
plot_expected_a_pre_trt <- function(model) {
  stopifnot(inherits(model, "perfect"))

  EA <- expected_a_pre_trt(model)
  EA_nice <- methods::as(methods::as(threshold(EA), "TsparseMatrix"), "dgTMatrix")

  Matrix::summary(EA_nice) |>
    ggplot2::ggplot(ggplot2::aes(x = j, y = -i, fill = x)) +
    ggplot2::geom_tile() +
    scale_fill_gradient(low = "white", high = "black", limits = c(0, 1)) +
    ggplot2::theme_void() +
    theme(
      legend.position = "none"
    )
}

#' @rdname plot_expected_a_pre_trt
#' @export
plot_expected_a_post_trt <- function(model) {
  stopifnot(inherits(model, "perfect"))

  EA <- expected_a_post_trt(model)
  EA_nice <- methods::as(methods::as(threshold(EA), "TsparseMatrix"), "dgTMatrix")

  Matrix::summary(EA_nice) |>
    ggplot2::ggplot(ggplot2::aes(x = j, y = -i, fill = x)) +
    ggplot2::geom_tile() +
    scale_fill_gradient(low = "white", high = "black", limits = c(0, 1)) +
    ggplot2::theme_void() +
    theme(
      legend.position = "none"
    )
}

#' @rdname plot_expected_a_pre_trt
#' @export
plot_expected_a_pre_post_diff <- function(model) {

  stopifnot(inherits(model, "perfect"))

  diff <- expected_a_post_trt(model) - expected_a_pre_trt(model)
  diff_nice <- methods::as(methods::as(threshold(diff), "TsparseMatrix"), "dgTMatrix")

  Matrix::summary(diff_nice) |>
    ggplot2::ggplot(ggplot2::aes(x = j, y = -i, fill = x)) +
    ggplot2::geom_tile() +
    scale_fill_gradient(low = "white", high = "black", limits = c(0, 1)) +
    ggplot2::theme_void() +
    theme(
      legend.position = "none"
    )
}

plot_shift <- function(data, mapping) {
  color_map <- c(
    "treated-stayed" = "#1B9E77",
    "start" = "#999999",
    "finish" = "#D95F02",
    "stayed" = "#999999"
  )

  ggplot(data = data, mapping = mapping) +
    geom_point(
      data = \(x) dplyr::filter(x, color != "start"),
      aes(color = color)
    ) +
    geom_line(aes(group = id), color = "#D95F02") +
    geom_vline(xintercept = 0) +
    geom_hline(yintercept = 0) +
    scale_color_manual(values = color_map) +
    expand_limits(x = 0, y = 0)
}

#' @rdname plot_expected_a_pre_trt
#' @export
plot_z_pre_post_diff <- function(model) {
  stopifnot(inherits(model, "perfect"))

  z_pre <- z_pre_trt(model)
  z_post <- z_post_trt(model)

  colnames(z_pre) <- paste0("block", 1:model$k)
  colnames(z_post) <- paste0("block", 1:model$k)

  moved <- Matrix::rowSums(abs(z_pre - z_post)) > 1e-10
  condition <- ifelse(model$trt == 1, "treatment", "control")

  z_pre_df <- as.matrix(z_pre) |>
    tibble::as_tibble() |>
    mutate(
      type = "pre",
      id = dplyr::row_number(),
      condition = condition,
      moved = moved
    )

  z_post_df <- as.matrix(z_post) |>
    tibble::as_tibble() |>
    mutate(
      type = "post",
      id = dplyr::row_number(),
      condition = condition,
      moved = moved
    )

  bound <- dplyr::bind_rows(z_pre_df, z_post_df) |>
    mutate(
      color = dplyr::case_when(
        condition == "treatment" & !moved ~ "treated-stayed",
        condition == "treatment" & type == "pre" & moved ~ "start",
        condition == "treatment" & type == "post" & moved ~ "finish",
        condition == "control" ~ "control-stayed"
      )
    )

  columns <- paste0("block", 1:model$k)

  p <- GGally::ggpairs(
    bound,
    mapping = aes(alpha = 0.1),
    columns = columns,
    lower = list(
      continuous = plot_shift
    ),
    upper = list(
      continuous = plot_shift
    ),
    diag = list(
      mapping = aes(color = NULL)
    )
  ) +
    theme_classic()

  p
}

# TODO: these should move to fastRG probably

#' @rdname plot_expected_a_pre_trt
#' @export
plot_sparse_adjacency_matrix <- function(A) {
  sign(methods::as(A, "dgCMatrix")) |>
    Matrix::summary(A, uniqT = FALSE) |>
    ggplot(aes(j, -i, fill = x)) +
    geom_tile(fill = "black") +
    theme_void()
}

#' @rdname plot_expected_a_pre_trt
#' @export
plot_tidygraph_matrix <- function(graph) {
  A <- igraph::as_adjacency_matrix(graph)
  plot_sparse_adjacency_matrix(A)
}

#' @rdname plot_expected_a_pre_trt
#' @export
plot_tidygraph_matrix_rownormalized <- function(graph) {
  A <- igraph::as_adjacency_matrix(graph)
  deg <- igraph::degree(graph)
  DinvA <- Matrix::rowScale(A, 1 / deg)
  plot_sparse_adjacency_matrix(DinvA)
}
