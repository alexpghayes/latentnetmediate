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
#  - coverage correctness
#  - hypothesis test of any network effect at all
#

#' Title
#'
#' @param n TODO
#' @param k TODO
#' @param dim_c TODO
#'
#' @return TODO
#' @export
#'
#' @examples
#'
#' library(estimatr)
#' library(ggplot2)
#' library(dplyr)
#'
#' set.seed(26)
#'
#' mu <- model_mediator_uninformative(n = 100, k = 5, dim_c = 3)
#'
#' plot_expected_a_pre_trt(mu)
#' plot_expected_a_post_trt(mu)
#' plot_expected_a_pre_post_diff(mu)
#'
#' graph <- sample_tidygraph(mu)
#' graph
#'
#' fit <- nodelm(US(A, 5) ~ trt + C1 + C2 + C3 , graph = graph)
#'
#' true_coefs <- tibble(
#'   outcome = 1:5,
#'   `(Intercept)` = drop(mu$theta_0),
#'   trt = drop(mu$theta_t),
#'   C1 = mu$theta_c[1, ],
#'   C2 = mu$theta_c[2, ],
#'   C3 = mu$theta_c[3, ]
#' ) |>
#'   tidyr::pivot_longer(
#'     cols = -outcome,
#'     names_to = "term",
#'     values_to = "estimate"
#'   )
#'
#' tidy(fit) |>
#'   ggplot(aes(x = term, ymin = conf.low, y = estimate, ymax = conf.high)) +
#'   geom_pointrange() +
#'   geom_point(
#'     data = true_coefs,
#'     aes(x = term, y = estimate),
#'     color = "red",
#'     shape = 4,
#'     size = 2,
#'     inherit.aes = FALSE
#'   ) +
#'   geom_hline(yintercept = 0, linetype = "dashed") +
#'   coord_flip() +
#'   facet_wrap(
#'     vars(outcome),
#'     labeller = "label_both"
#'   ) +
#'   theme_classic()
#'
model_mediator_uninformative <- function(n, k = 5, dim_c = 3) {

  B <- matrix(0.01, nrow = k, ncol = k)
  diag(B) <- 0.8

  pi <- rep(1 / k, k)

  # this is a little delicate. we want to simulate A from a dcsbm, but if we
  # compute the eigen decomposition of E[A|X] to get the population positions
  # X, X may have negative elements. when X has negative elements we can't
  # use the fastRG algorithm to sample A, but we very much want to sample
  # using the fastRG algorithm so that we can simulate large sparse graphs
  #
  # so the trick here is to use the fastRG parameterization of the dcsbm
  # for simulating A, and to derive X from this parameterization but to
  # avoid using X for simulations

  A_model <- fastRG::dcsbm(
    theta = stats::runif(n, min = 1, max = 3),
    B = B,
    pi = pi,
    expected_density = 0.1
  )

  # derive the population ASE from A_model. E[A|stuff] = X S X^T
  # compute population ASE as X %*% left_principle_components(S)
  #
  # note that X is orthogonal but not orthonormal in the fastRG
  # parameterization

  S_eigs <- eigen(A_model$S)
  S_pcs <- S_eigs$vectors %*% diag(sqrt(S_eigs$values))

  X <- A_model$X %*% S_pcs

  trt <- stats::rbinom(n, size = 1, prob = 0.5)

  C <- matrix(
    stats::rnorm(n * dim_c),
    nrow = n,
    ncol = dim_c
  )

  # coefficients to later compare estimates to

  theta_0 <- matrix(Matrix::colMeans(X), nrow = 1, ncol = k)
  theta_t <- matrix(0, nrow = 1, ncol = k)
  theta_c <- matrix(0, nrow = dim_c, ncol = k)
  theta_tc <- matrix(0, nrow = dim_c, ncol = k)

  # check that the residuals X - E[X|T, C] have zero mean

  EX <- matrix(1, nrow = n) %*% theta_0 + trt %*% theta_t + C %*% theta_c + trt * C %*% theta_tc
  avg_residuals <- Matrix::colMeans(X - EX)

  stopifnot(isTRUE(all.equal(avg_residuals, rep(0, k))))

  model <- list(
    n = n,
    k = k,
    X = X,
    trt = trt,
    C = C,
    A_model = A_model,
    theta_0 = theta_0,
    theta_t = theta_t,
    theta_c = theta_c,
    theta_tc = theta_tc
  )

  class(model) <- c("uniformative", "mediator", "mrdpg")

  model
}

#' Title
#'
#' @param n TODO
#' @param k TODO
#' @param dim_c TODO
#'
#' @return TODO
#' @export
#'
#' @examples
#'
#' library(estimatr)
#' library(ggplot2)
#' library(tidyr)
#'
#' set.seed(26)
#'
#' mblock <- model_mediator_block(n = 100, k = 5, dim_c = 3)
#'
#' plot_expected_a_pre_trt(mblock)
#' plot_expected_a_post_trt(mblock)
#' plot_expected_a_pre_post_diff(mblock)
#'
#' graph <- sample_tidygraph(mblock)
#' graph
#'
#' fit <- nodelm(US(A, 5) ~ trt + C1 + C2 + C3 , graph = graph)
#'
#' true_coefs <- tibble(
#'   outcome = 1:5,
#'   `(Intercept)` = drop(mblock$theta_0),
#'   trt = drop(mblock$theta_t),
#'   C1 = mblock$theta_c[1, ],
#'   C2 = mblock$theta_c[2, ],
#'   C3 = mblock$theta_c[3, ]
#' ) |>
#'   tidyr::pivot_longer(
#'     cols = -outcome,
#'     names_to = "term",
#'     values_to = "estimate"
#'   )
#'
#' tidy(fit) |>
#'   ggplot(aes(x = term, ymin = conf.low, y = estimate, ymax = conf.high)) +
#'   geom_pointrange() +
#'   geom_point(
#'     data = true_coefs,
#'     aes(x = term, y = estimate),
#'     color = "red",
#'     shape = 4,
#'     size = 2,
#'     inherit.aes = FALSE
#'   ) +
#'   geom_hline(yintercept = 0, linetype = "dashed") +
#'   coord_flip() +
#'   facet_wrap(
#'     vars(outcome),
#'     labeller = "label_both"
#'   ) +
#'   theme_classic()
#'
model_mediator_block <- function(n, k = 5, dim_c = 3) {

  B <- matrix(0.01, nrow = k, ncol = k)
  diag(B) <- 0.8

  pi <- rep(1 / k, k)

  # this is a little delicate. we want to simulate A from a dcsbm, but if we
  # compute the eigen decomposition of E[A|X] to get the population positions
  # X, X may have negative elements. when X has negative elements we can't
  # use the fastRG algorithm to sample A, but we very much want to sample
  # using the fastRG algorithm so that we can simulate large sparse graphs
  #
  # so the trick here is to use the fastRG parameterization of the dcsbm
  # for simulating A, and to derive X from this parameterization but to
  # avoid using X for simulations

  A_model <- fastRG::dcsbm(
    theta = stats::runif(n, min = 1, max = 3),
    B = B,
    pi = pi,
    expected_density = 0.1
  )

  # derive the population ASE from A_model. E[A|stuff] = X S X^T
  # compute population ASE as X %*% left_principle_components(S)
  #
  # note that X is orthogonal but not orthonormal in the fastRG
  # parameterization

  S_eigs <- eigen(A_model$S)
  S_pcs <- S_eigs$vectors %*% diag(sqrt(S_eigs$values))

  X <- A_model$X %*% S_pcs

  trt <- as.integer(as.integer(A_model$z) <= round(k / 2))

  C <- matrix(
    stats::rnorm(n * dim_c),
    nrow = n,
    ncol = dim_c
  )

  # coefficients to later compare estimates to

  # back out coefficients
  fit <- stats::lm(as.matrix(X) ~ trt)

  theta_0 <- stats::coef(fit)["(Intercept)", , drop = FALSE]
  theta_t <- stats::coef(fit)["trt", , drop = FALSE]
  theta_c <- matrix(0, nrow = dim_c, ncol = k)
  theta_tc <- matrix(0, nrow = dim_c, ncol = k)

  # check that the residuals X - E[X|T, C] have zero mean

  EX <- matrix(1, nrow = n) %*% theta_0 + trt %*% theta_t + C %*% theta_c + trt * C %*% theta_tc
  avg_residuals <- Matrix::colMeans(X - EX)

  stopifnot(isTRUE(all.equal(avg_residuals, rep(0, k))))

  model <- list(
    n = n,
    k = k,
    X = X,
    trt = trt,
    C = C,
    A_model = A_model,
    theta_0 = theta_0,
    theta_t = theta_t,
    theta_c = theta_c,
    theta_tc = theta_tc
  )

  class(model) <- c("block", "mediator", "mrdpg")

  model
}


#' Title
#'
#' @param beta_0 TODO
#' @param beta_t TODO
#' @param beta_c TODO
#' @param beta_x TODO
#'
#' @return TODO
#' @export
#'
model_mediator_informative <- function(n, k = 5, prob_trt = 0.5, theta_0 = NULL, theta_t = NULL,
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

