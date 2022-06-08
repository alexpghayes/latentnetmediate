# models:
#
#  - T and C independent of X
#  - T determines X
#  - T + C determine X
#
# interventions to investigate:
#
#  - T & C change centrality of nodes
#  - T & C translate nodes in latent space
#  - T & C map multiple communities to one community
#  - T & C swap communities and centralities around
#
# investigate:
#
#  - coefficient recovery (up to rotation)
#  - causal effect recovery
#

#' Title
#'
#' @param n TODO
#' @param k TODO
#' @param prob_trt TODO
#' @param theta_0 TODO
#' @param theta_t TODO
#' @param theta_c TODO
#' @param theta_tc TODO
#' @param beta_0 TODO
#' @param beta_t TODO
#' @param beta_c TODO
#' @param beta_x TODO
#'
#' @return TODO
#' @export
#'
model_informative_controls <- function(n, k = 5, prob_trt = 0.5, theta_0 = NULL, theta_t = NULL,
                                       theta_c = NULL, theta_tc = NULL, beta_0 = NULL,
                                       beta_t = NULL, beta_c = NULL, beta_x = NULL) {

  # 1. sample C

  B <- matrix(0.01, nrow = k, ncol = k)
  diag(B) <- 0.8

  pi <- rep(1 / k, k)

  C_model <- fastRG::dcsbm(
    theta = stats::runif(n, min = 1, max = 3),
    B = B,
    pi = pi,
    expected_density = 0.1
  )

  # NOTE: designing an intervention on the ASE is a pain in the ass, so instead
  # we parameterize the intervention here on the left singular vectors and
  # then post-multiply everything to move from "U-space" to "U sqrt(S)-space"

  # C_eigs <- eigs_sym(C_model)
  S_eigs <- eigen(C_model$S)
  post_mult <- S_eigs$vectors %*% diag(sqrt(S_eigs$values))


  # C <- C_eigs$vectors %*% diag(sqrt(C_eigs$values))
  C <- C_model$X

  # 2. choose who to treat: everybody in blocks 1, 2, 3
  # trt <- rbinom(n, size = 1, prob = as.integer(C_model$z) < 4)
  trt <- stats::rbinom(n, size = 1, prob = 0.5)

  # 3. intervention to form X

  if (is.null(theta_0)) {
    theta_0 <- matrix(0, nrow = n, ncol = k)
  }

  if (is.null(theta_t)) {
    theta_t <- rbind(rep(0, k))
  }

  if (is.null(theta_c)) {
    theta_c <- diag(x = 1, nrow = k, ncol = k)
  }

  if (is.null(theta_tc)) {
    theta_tc <- matrix(0, nrow = k, ncol = k)
  }

  X <- theta_0 + trt %*% theta_t + C %*% theta_c + trt * C %*% theta_tc

  if (is.null(beta_0)) {
    beta_0 <- 0
  }

  if (is.null(beta_t)) {
    beta_t <- 0
  }

  if (is.null(beta_c)) {
    beta_c <- rep(0, ncol(C))
  }

  if (is.null(beta_x)) {
    beta_x <- rep(0, ncol(X))
  }

  y <- beta_0 + trt * beta_t + C %*% beta_c + X %*% beta_x + stats::rnorm(n)

  model <- list(
    n = n,
    UX = X,
    UC = C,
    post_mult = post_mult,
    X = X %*% post_mult,
    C = C %*% post_mult,
    UC_model = C_model,
    trt = trt,
    y = as.numeric(y),
    theta_0 = theta_0,
    theta_t = theta_t,
    theta_c = theta_c,
    theta_tc = theta_tc,
    beta_0 = beta_0,
    beta_t = beta_t,
    beta_c = beta_c,
    beta_x = beta_x
  )

  class(model) <- c("model1", "mediated_rdpg")

  model
}

#' Sample from a mediated RDPG object
#'
#' @param model A `mediated_rdpg` object
#' @param ... Ignored.
#'
#' @return A [tidygraph::tbl_graph()] object.
#'
#' @export
sample_tidygraph <- function(model, ...) {
  UseMethod("sample_tidygraph")
}

#' @method sample_tidygraph model1
#' @export
sample_tidygraph.model1 <- function(model, ...) {
  ufm <- model$UC_model
  ufm$X <- model$UX

  C_df <- as.matrix(model$C) |>
    as_tibble()

  colnames(C_df) <- paste0("C", 1:ncol(model$C))

  graph <- fastRG::sample_tidygraph(ufm, allow_self_loops = FALSE) |>
    dplyr::arrange(as.numeric(name)) |>
    dplyr::mutate(trt = model$trt, !!!C_df, y = model$y)

  graph
}

plot_shift <- function(data, mapping) {
  color_map <- c(
    "treated-stayed" = "blue1",
    "start" = "darkgray",
    "finish" = "firebrick2",
    "stayed" = "darkgray"
  )

  ggplot(data = data, mapping = mapping) +
    geom_point(aes(color = color)) +
    geom_line(aes(group = id), color = "firebrick2") +
    geom_vline(xintercept = 0) +
    geom_hline(yintercept = 0) +
    scale_color_manual(values = color_map) +
    expand_limits(x = 0, y = 0)
}

#' @import ggplot2 GGally
plot_intervention <- function(model, post_multiply = FALSE) {

  # plot treatment assignments

  if (post_multiply) {
    X <- model$X
    C <- model$C
  } else {
    X <- model$UX
    C <- model$UC
  }

  X_df <- as.matrix(X) |>
    as_tibble() |>
    mutate(
      type = "post",
      id = 1:nrow(X),
      trt = ifelse(model$trt == 1, "treatment", "control"),
      moved = Matrix::rowSums(abs(X - C)) > 1e-10
    )

  C_df <- as.matrix(C) |>
    as_tibble() |>
    mutate(
      type = "pre",
      id = 1:nrow(C),
      trt = ifelse(model$trt == 1, "treatment", "control"),
      moved = Matrix::rowSums(abs(X - C)) > 1e-10
    )

  colnames(X_df) <- colnames(C_df)

  bound <- dplyr::bind_rows(X_df, C_df) |>
    mutate(
      color = dplyr::case_when(
        trt == "treatment" & !moved ~ "treated-stayed",
        trt == "treatment" & type == "pre" & moved ~ "start",
        trt == "treatment" & type == "post" & moved ~ "finish",
        trt == "control" ~ "control-stayed"
      )
    )

  columns <- setdiff(colnames(bound), c("trt", "id", "type", "color", "moved"))

  p <- ggpairs(
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
