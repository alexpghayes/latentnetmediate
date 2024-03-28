

#' Title
#'
#' @inheritParams model_mediator_uninformative
#'
#' @param ztheta_0 TODO
#' @param ztheta_t TODO
#' @param ztheta_c TODO
#' @param ztheta_tc TODO
#' @inheritParams fastRG::undirected_factor_model
#'
#' @return TODO
#' @export
#'
#' @examples
#'
#' inf <- model_mediator_informative(n = 100, k = 5)
#'
#' graph <- sample_tidygraph(inf)
#' graph
#'
#' nodelm(US(A, 5) ~ . - name - 1, graph = graph)
#'
#' coef(inf)
#'
#' # should be very close to zero
#' inf$zX - inf$zC_true
#'
#' inf$zX - inf$zC_obs
#' inf$W - inf$zC_obs
#'
model_mediator_informative <- function(n, k = 5, ztheta_0 = NULL, ztheta_t = NULL,
                                       ztheta_c = NULL, ztheta_tc = NULL, expected_degree = NULL) {

  B <- matrix(0.01, nrow = k, ncol = k)
  diag(B) <- 0.8

  pi <- rep(1 / k, k)
  theta <- stats::runif(n, min = 1, max = 3)

  # so ideally we would like to able to accommodate situations where the design
  # matrix (typically ~ T * C) is perfectly predictive of X. but our estimator
  # (probably?) can't handle this because then X and T * C are perfectly
  # colinear and there's an identification issue
  #
  # so here we simulate from a related situation. we generate T as usual, and
  # then generate two copies of C, called C_true and C_obs. we calculate X
  # as in the perfectly colinear model using C_true. but we only show the
  # estimator C_obs. since C_true and C_obs are two samples from the same
  # latent position model, C_obs should be informative, but not perfectly
  # predictive, of C_true
  #
  # in particular, since we set sort_nodes = TRUE, it's basically like we're
  # working with order statistics, and matching estimates for the same
  # "percentile" is how we induce imperfect dependence
  #
  # setting `sort_nodes = TRUE` in both calls below is key to get order
  # statistics to match up

  C_true_model <- fastRG::dcsbm(
    theta = theta,
    B = B,
    pi = pi,
    expected_degree = expected_degree,
    sort_nodes = TRUE,
    allow_self_loops = TRUE,
    poisson_edges = TRUE
  )

  C_obs_model <- fastRG::dcsbm(
    theta = theta,
    B = B,
    pi = pi,
    expected_degree = expected_degree,
    sort_nodes = TRUE,
    allow_self_loops = TRUE,
    poisson_edges = TRUE
  )

  # transformations are applied to this matrix because it is easiest to
  # reason about
  zC_true <- C_true_model$X
  colnames(zC_true) <- paste0("C", 1:ncol(zC_true))

  zC_obs <- C_obs_model$X
  colnames(zC_obs) <- paste0("C", 1:ncol(zC_obs))

  # we start building the design matrix here. we do this by hand and
  # conditionally on the coefficients actually specified
  W <- matrix(nrow = n, ncol = 0)
  zX <- matrix(0, nrow = n, ncol = k)

  trt <- stats::rbinom(n, size = 1, prob = 0.5)

  if (is.null(ztheta_t)) {
    ztheta_t <- matrix(0, nrow = 1, ncol = k)
  } else {
    W <- cbind(trt, W)
    colnames(W) <- c("trt", colnames(W))

    # here we perform the intervention in Z space
    zX <- zX + trt %*% ztheta_t
  }

  if (is.null(ztheta_0)) {
    ztheta_0 <- matrix(0, nrow = 1, ncol = k)
  } else {
    ones <- matrix(1, nrow = n)
    W <- cbind(ones, W)
    colnames(W) <- c("intercept", colnames(W))
    zX <- zX + ones %*% ztheta_0
  }

  dim_c <- k

  if (is.null(ztheta_c)) {
    ztheta_c <- diag(x = 1, nrow = dim_c, ncol = k)
  }

  # add the partially informative but not quite accurate version of zC
  # to W, but compute X based on the true zC

  W <- cbind(W, zC_obs)
  zX <- zX + zC_true %*% ztheta_c

  if (is.null(ztheta_tc)) {
    ztheta_tc <- matrix(0, nrow = dim_c, ncol = k)
  } else {
    TC_true <- trt * zC_true
    TC_obs <- trt * zC_obs

    colnames(TC_obs) <- paste0("TC", 1:ncol(zC_obs))
    W <- cbind(W, TC_obs)
    zX <- zX + TC_true %*% ztheta_tc
  }

  A_model <- fastRG::undirected_factor_model(X = zX, S = C_true_model$S)

  m <- model_mediator(A_model, W, subclass = "informative")

  m$C_true_model <- C_true_model
  m$C_obs_model <- C_obs_model
  m$zC_true <- zC_true
  m$zC_obs <- zC_obs
  m$zX <- zX
  m$ztheta_0 <- ztheta_0
  m$ztheta_t <- ztheta_t
  m$ztheta_c <- ztheta_c
  m$ztheta_tc <- ztheta_tc

  m
}
