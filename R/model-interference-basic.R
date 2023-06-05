#' Title
#'
#' @param n TODO
#' @param k TODO
#' @param dim_w TODO
#'
#' @return TODO
#' @export
#'
#' @examples
#'
#' set.seed(26)
#'
#' b <- model_interference_basic(n = 100, k = 5, dim_w = 3)
#'
#' graph <- sample_tidygraph(b)
#' graph
#'
#'
model_interference_basic <- function(n, k = 5, dim_w = 3) {

  B <- matrix(0.01, nrow = k, ncol = k)
  diag(B) <- 0.8

  pi <- rep(1, k) / k

  A_model <- fastRG::dcsbm(
    theta = stats::runif(n, min = 1, max = 3),
    B = B,
    pi = pi,
    expected_density = 0.1,
    allow_self_loops = TRUE,
    poisson_edges = TRUE
  )

  # nodal covariates
  trt <- stats::rbinom(n, size = 1, prob = 0.5)

  W <- matrix(
    stats::rnorm(n * dim_w, mean = 1),
    nrow = n,
    ncol = dim_w
  )

  W <- cbind(1, trt, W)

  colnames(W) <- c("intercept", "trt", paste0("C", 1:dim_w))

  model_interference(A_model, W, subclass = "basic")
}
