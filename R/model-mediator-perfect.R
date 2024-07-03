

#' Title
#'
#' @inheritParams model_mediator_uninformative
#'
#' @param ztheta_0 TODO
#' @param ztheta_t TODO
#' @param ztheta_c TODO
#' @param ztheta_tc TODO
#'
#' @inheritParams fastRG::undirected_factor_model
#'
#' @return TODO
#' @export
#'
#' @examples
#'
#' ztheta_tc <- rbind(
#'   c(-1, 2, 0, 0, 0), # treated in block one become twice as central and jump to block two
#'   c(0, 0, 0, 0, 0),
#'   c(0, 1, -1, 0, 0), # treated in block three jump to block two
#'   c(0, 0, 0, 0, 0),
#'   c(0, 0, 0, 0, 0)
#' )
#'
#' perf <- model_mediator_perfect(n = 100, k = 5, ztheta_tc = ztheta_tc)
#'
model_mediator_perfect <- function(n, k = 5, ztheta_0 = NULL, ztheta_t = NULL,
                                   ztheta_c = NULL, ztheta_tc = NULL, expected_degree = NULL) {

  B <- matrix(0.01, nrow = k, ncol = k)
  diag(B) <- 0.8

  pi <- rep(1 / k, k)

  C_model <- fastRG::dcsbm(
    theta = stats::runif(n, min = 1, max = 3),
    B = B,
    pi = pi,
    expected_degree = expected_degree,
    allow_self_loops = TRUE,
    poisson_edges = TRUE
  )

  # transformations are applied to this matrix because it is easiest to
  # reason about
  zC <- C_model$X
  colnames(zC) <- paste0("C", 1:ncol(zC))

  # we start building the design matrix here. we do this by hand and
  # conditionally on the coefficients actually specified
  W <- matrix(nrow = n, ncol = 0)
  zX <- matrix(0, nrow = n, ncol = k)
  zX_no_trt <- matrix(0, nrow = n, ncol = k)

  trt <- stats::rbinom(n, size = 1, prob = 0.5)
  notrt <- rep(0, n)

  if (is.null(ztheta_t)) {
    ztheta_t <- matrix(0, nrow = 1, ncol = k)
  } else {
    W <- cbind(trt)
    colnames(W) <- c("trt")

    # here we perform the intervention in Z space
    zX <- zX + trt %*% ztheta_t
    zX_no_trt <- zX_no_trt + notrt %*% ztheta_t
  }

  if (is.null(ztheta_0)) {
    ztheta_0 <- matrix(0, nrow = 1, ncol = k)
  } else {
    ones <- matrix(1, nrow = n)
    W <- cbind(ones, W)
    colnames(W) <- c("intercept", colnames(W))
    zX <- zX + ones %*% ztheta_0
    zX_no_trt <- zX_no_trt + ones %*% ztheta_0
  }

  dim_c <- k

  if (is.null(ztheta_c)) {
    ztheta_c <- diag(x = 1, nrow = dim_c, ncol = k)
  }

  W <- cbind(W, zC)
  zX <- zX + zC %*% ztheta_c
  zX_no_trt <- zX_no_trt + zC %*% ztheta_c

  if (is.null(ztheta_tc)) {
    ztheta_tc <- matrix(0, nrow = dim_c, ncol = k)
  } else {
    TC <- trt * zC
    colnames(TC) <- paste0("TC", 1:ncol(zC))
    W <- cbind(W, TC)
    zX <- zX + TC %*% ztheta_tc

    TC_no_trt <- notrt * zC
    zX_no_trt <- zX_no_trt + TC_no_trt %*% ztheta_tc
  }

  A_model <- fastRG::undirected_factor_model(X = zX, S = C_model$S)

  m <- model_mediator(A_model, W, subclass = "perfect")

  m$C_model <- C_model
  m$zX_no_trt <- zX_no_trt
  m$trt <- trt
  m$zC <- zC
  m$zX <- zX
  m$ztheta_0 <- ztheta_0
  m$ztheta_t <- ztheta_t
  m$ztheta_c <- ztheta_c
  m$ztheta_tc <- ztheta_tc

  m
}
