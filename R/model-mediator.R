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

  A_eigs <- fastRG::eigs_sym(A_model, k = k)
  X <- A_eigs$vectors %*% diag(sqrt(A_eigs$values))

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

  A_model <- fastRG::dcsbm(
    theta = stats::runif(n, min = 1, max = 3),
    B = B,
    pi = pi,
    expected_density = 0.1
  )

  A_eigs <- fastRG::eigs_sym(A_model, k = k)
  X <- A_eigs$vectors %*% diag(sqrt(A_eigs$values))

  trt <- as.integer(as.integer(A_model$z) <= round(k / 2))

  C <- matrix(
    stats::rnorm(n * dim_c),
    nrow = n,
    ncol = dim_c
  )

  # coefficients to later compare estimates to

  # back out coefficients by computing E[X|trt] on population data
  fit <- stats::lm(as.matrix(X) ~ trt)

  theta_0 <- stats::coef(fit)["(Intercept)", , drop = FALSE]
  theta_t <- stats::coef(fit)["trt", , drop = FALSE]
  theta_c <- matrix(0, nrow = dim_c, ncol = k)
  theta_tc <- matrix(0, nrow = dim_c, ncol = k)

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
#' @inheritParams model_mediator_uninformative
#'
#' @param theta_0 TODO
#' @param theta_t TODO
#' @param theta_c TODO
#' @param theta_tc TODO
#'
#' @return TODO
#' @export
#'
model_mediator_informative <- function(n, k = 5, theta_0 = NULL, theta_t = NULL,
                                       theta_c = NULL, theta_tc = NULL) {

  B <- matrix(0.01, nrow = k, ncol = k)
  diag(B) <- 0.8

  pi <- rep(1 / k, k)

  C_model <- fastRG::dcsbm(
    theta = stats::runif(n, min = 1, max = 3),
    B = B,
    pi = pi,
    expected_density = 0.1
  )

  C_eigs <- fastRG::eigs_sym(C_model, k = k)
  C <- C_eigs$vectors %*% diag(sqrt(C_eigs$values))

  A_model <- fastRG::dcsbm(
    theta = stats::runif(n, min = 1, max = 3),
    B = B,
    pi = pi,
    expected_density = 0.1
  )

  A_eigs <- fastRG::eigs_sym(A_model, k = k)
  X <- A_eigs$vectors %*% diag(sqrt(A_eigs$values))

  trt <- stats::rbinom(n = n, size = 1, prob = 0.5)

  # back out implied coefficients

  fit <- stats::lm(as.matrix(X) ~ trt + as.matrix(C))

  theta_c_terms <- paste0("as.matrix(C)", 1:k)
  dim_c <- k

  theta_0 <- stats::coef(fit)["(Intercept)", , drop = FALSE]
  theta_t <- stats::coef(fit)["trt", , drop = FALSE]
  theta_c <- stats::coef(fit)[theta_c_terms, , drop = FALSE]
  theta_tc <- matrix(0, nrow = dim_c, ncol = k)

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

  class(model) <- c("informative", "mediator", "mrdpg")

  model
}



#' Title
#'
#' @inheritParams model_mediator_uninformative
#'
#' @param ztheta_0 TODO
#' @param ztheta_t TODO
#' @param ztheta_c TODO
#' @param ztheta_tc TODO
#'
#' @return TODO
#' @export
#'
model_mediator_perfect <- function(n, k = 5, ztheta_0 = NULL, ztheta_t = NULL,
                                   ztheta_c = NULL, ztheta_tc = NULL) {

  B <- matrix(0.01, nrow = k, ncol = k)
  diag(B) <- 0.8

  pi <- rep(1 / k, k)

  C_model <- fastRG::dcsbm(
    theta = stats::runif(n, min = 1, max = 3),
    B = B,
    pi = pi,
    expected_density = 0.1
  )

  # transformations are applied to this matrix because it is easiest to
  # reason about
  zC <- C_model$X

  C_eigs <- fastRG::eigs_sym(C_model, k = k)
  C <- C_eigs$vectors %*% diag(sqrt(C_eigs$values))

  trt <- stats::rbinom(n, size = 1, prob = 0.5)

  dim_c <- k

  if (is.null(ztheta_0)) {
    ztheta_0 <- matrix(0, nrow = 1, ncol = k)
  }

  if (is.null(ztheta_t)) {
    ztheta_t <- matrix(0, nrow = 1, ncol = k)
  }

  if (is.null(ztheta_c)) {
    ztheta_c <- diag(x = 1, nrow = dim_c, ncol = k)
  }

  if (is.null(ztheta_tc)) {
    ztheta_tc <- matrix(0, nrow = dim_c, ncol = k)
  }

  # here we perform the intervention in Z space so we can take the post-trt
  # Z and plug it into a new fastRG object to simulate from

  ones <- matrix(1, nrow = n)
  zX <- ones %*% ztheta_0 + trt %*% ztheta_t + zC %*% ztheta_c + trt * zC %*% ztheta_tc

  A_model <- fastRG::undirected_factor_model(X = zX, S = C_model$S)
  A_eigs <- fastRG::eigs_sym(A_model, k = k)
  X <- A_eigs$vectors %*% diag(sqrt(A_eigs$values))

  # EX = X since we are in the noiseless case
  EX <- as.matrix(X)

  # back out the ASE space regression coefficients
  # this fit needs to be perfect or the parameterization is bad
  # we need to prove that this is always the case but have not yet
  fit <- stats::lm(EX ~ trt * as.matrix(C))

  # can check for perfect fit by running `summary(fit)`, which should
  # give a warning

  theta_c_terms <- paste0("as.matrix(C)", 1:k)
  theta_tc_terms <- paste0("trt:as.matrix(C)", 1:k)

  theta_0 <- stats::coef(fit)["(Intercept)", , drop = FALSE]
  theta_t <- stats::coef(fit)["trt", , drop = FALSE]
  theta_c <- stats::coef(fit)[theta_c_terms, , drop = FALSE]
  theta_tc <- stats::coef(fit)[theta_tc_terms, , drop = FALSE]

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
    theta_tc = theta_tc,
    C_model = C_model,
    zC = zC,
    zX = zX,
    ztheta_0 = ztheta_0,
    ztheta_t = ztheta_t,
    ztheta_c = ztheta_c,
    ztheta_tc = ztheta_tc
  )

  class(model) <- c("perfect", "mediator", "mrdpg")

  model
}

