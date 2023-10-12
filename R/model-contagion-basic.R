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
#' b <- model_contagion_basic(n = 2000, k = 5, dim_c = 3)
#'
#' graph <- sample_tidygraph(b)
#' graph
#'
#'
model_contagion_basic <- function(n, k = 5, dim_c = 3, rho = 0.1) {

  B <- matrix(0.01, nrow = k, ncol = k)
  diag(B) <- 0.8

  pi <- rep(1, k) / k

  A_model <- fastRG::dcsbm(
    theta = stats::runif(n, min = 1, max = 3),
    B = B,
    pi = pi,
    expected_density = 0.1,
    allow_self_loops = FALSE,
    poisson_edges = FALSE
  )

  trt <- stats::rbinom(n, size = 1, prob = 0.5)

  C <- matrix(
    stats::rnorm(n * dim_c, mean = 1),
    nrow = n,
    ncol = dim_c
  )

  W <- cbind(1, trt, C)

  colnames(W) <- c("intercept", "trt", paste0("C", 1:dim_c))

  model_contagion(
    A_model = A_model,
    W = W,
    rho = rho,
    subclass = "basic"
  )
}
