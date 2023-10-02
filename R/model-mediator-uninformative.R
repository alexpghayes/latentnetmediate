
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
#' mu <- model_mediator_uninformative(n = 100, k = 5, dim_w = 3)
#'
#' graph <- sample_tidygraph(mu)
#' graph
#'
#' coef(mu)
#'
#' fit <- nodelm(US(A, 5) ~ . - name - 1, graph = graph)
#' fit
#'
model_mediator_uninformative <- function(n, k = 5, dim_w = 3) {

  B <- matrix(0.01, nrow = k, ncol = k)
  diag(B) <- 0.8

  pi <- rep(1, k) / k

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
    expected_density = 0.1,
    allow_self_loops = TRUE,
    poisson_edges = TRUE
  )

  W <- matrix(
    stats::rnorm(n * dim_w, mean = 1),
    nrow = n,
    ncol = dim_w
  )

  s <- svd(W)

  if (dim_w > 1) {
    S <- diag(s$d)
  } else {
    S <- matrix(s$d)
  }

  W <- cbind(1, W) # s$u %*% S)

  colnames(W) <- c("intercept", "trt", paste0("C", left_padded_sequence(1:(dim_w - 1))))

  model_mediator(A_model, W, subclass = "uninformative")
}
