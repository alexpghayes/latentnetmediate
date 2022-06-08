expected_x_pre_trt <- function(model) {
  stopifnot(inherits(model, "mediator"))

  untrt <- rep(0, length(model$trt))

  EX <- matrix(1, nrow = model$n) %*% model$theta_0 + untrt %*% model$theta_t + model$C %*% model$theta_c + untrt * model$C %*% model$theta_tc
  EX
}

expected_x_post_trt <- function(model) {
  stopifnot(inherits(model, "mediator"))

  EX <- matrix(1, nrow = model$n) %*% model$theta_0 + model$trt %*% model$theta_t + model$C %*% model$theta_c + model$trt * model$C %*% model$theta_tc
  EX
}

x_pre_trt <- function(model) {
  stopifnot(inherits(model, "mediator"))

  EX_pre <- expected_x_pre_trt(model)
  EX_post <- expected_x_post_trt(model)
  res <- model$X - EX_post

  EX_pre + res
}

x_post_trt <- function(model) {
  stopifnot(inherits(model, "mediator"))
  model$X
}

# conditional on X, rather than T, C
expected_a_pre_trt <- function(model) {
  stopifnot(inherits(model, "mediator"))

  X <- x_pre_trt(model)
  Matrix::tcrossprod(X)
}

expected_a_post_trt <- function(model) {
  stopifnot(inherits(model, "mediator"))

  Matrix::tcrossprod(model$X)
}

threshold <- function(X, threshold = 1e-10) {
  X[X < threshold] <- 0
  X
}

#' Title
#'
#' @param model TODO
#'
#' @return TODO
#' @export
#'
plot_expected_a_pre_trt <- function(model) {
  stopifnot(inherits(model, "mediator"))

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
  stopifnot(inherits(model, "mediator"))

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
  stopifnot(inherits(model, "mediator"))

  diff <- expected_a_post_trt(model) - expected_a_pre_trt(model)
  diff_nice <- methods::as(methods::as(threshold(diff), "TsparseMatrix"), "dgTMatrix")

  Matrix::summary(diff_nice) |>
    ggplot2::ggplot(ggplot2::aes(x = j, y = -i, fill = x)) +
    ggplot2::geom_tile() +
    scale_fill_viridis_c() +
    scale_fill_gradient(low = "white", high = "black", limits = c(0, 1)) +
    ggplot2::theme_void() +
    theme(
      legend.position = "none"
    )
}

#' @rdname plot_expected_a_pre_trt
#' @export
plot_x_pre_post_diff <- function(model) {
  stopifnot(inherits(model, "mediator"))

  diff <- x_post_trt(model) - x_pre_trt(model)
  diff_nice <- methods::as(methods::as(threshold(diff), "TsparseMatrix"), "dgTMatrix")

  Matrix::summary(diff_nice) |>
    ggplot2::ggplot(ggplot2::aes(x = j, y = -i, fill = x)) +
    ggplot2::geom_tile() +
    scale_fill_viridis_c() +
    scale_fill_gradient(low = "white", high = "black", limits = c(0, 1)) +
    ggplot2::theme_void() +
    theme(
      legend.position = "none"
    )
}

#'
#' plot_shift <- function(data, mapping) {
#'   color_map <- c(
#'     "treated-stayed" = "blue1",
#'     "start" = "darkgray",
#'     "finish" = "firebrick2",
#'     "stayed" = "darkgray"
#'   )
#'
#'   ggplot(data = data, mapping = mapping) +
#'     geom_point(aes(color = color)) +
#'     geom_line(aes(group = id), color = "firebrick2") +
#'     geom_vline(xintercept = 0) +
#'     geom_hline(yintercept = 0) +
#'     scale_color_manual(values = color_map) +
#'     expand_limits(x = 0, y = 0)
#' }
#'
#' #' TODO
#' #'
#' #' @param model TODO
#' #'
#' #' @param post_multiply TODO
#' #'
#' #' @return TODO
#' #'
#' #' @import ggplot2 GGally
#' plot_intervention_on_x <- function(model, post_multiply = FALSE) {
#'
#'   # plot treatment assignments
#'
#'   if (post_multiply) {
#'     X <- model$X
#'     C <- model$C
#'   } else {
#'     X <- model$UX
#'     C <- model$UC
#'   }
#'
#'   X_df <- as.matrix(X) |>
#'     as_tibble() |>
#'     mutate(
#'       type = "post",
#'       id = 1:nrow(X),
#'       trt = ifelse(model$trt == 1, "treatment", "control"),
#'       moved = Matrix::rowSums(abs(X - C)) > 1e-10
#'     )
#'
#'   C_df <- as.matrix(C) |>
#'     as_tibble() |>
#'     mutate(
#'       type = "pre",
#'       id = 1:nrow(C),
#'       trt = ifelse(model$trt == 1, "treatment", "control"),
#'       moved = Matrix::rowSums(abs(X - C)) > 1e-10
#'     )
#'
#'   colnames(X_df) <- colnames(C_df)
#'
#'   bound <- dplyr::bind_rows(X_df, C_df) |>
#'     mutate(
#'       color = dplyr::case_when(
#'         trt == "treatment" & !moved ~ "treated-stayed",
#'         trt == "treatment" & type == "pre" & moved ~ "start",
#'         trt == "treatment" & type == "post" & moved ~ "finish",
#'         trt == "control" ~ "control-stayed"
#'       )
#'     )
#'
#'   columns <- setdiff(colnames(bound), c("trt", "id", "type", "color", "moved"))
#'
#'   p <- ggpairs(
#'     bound,
#'     mapping = aes(alpha = 0.1),
#'     columns = columns,
#'     lower = list(
#'       continuous = plot_shift
#'     ),
#'     upper = list(
#'       continuous = plot_shift
#'     ),
#'     diag = list(
#'       mapping = aes(color = NULL)
#'     )
#'   ) +
#'     theme_classic()
#'
#'   p
#' }
