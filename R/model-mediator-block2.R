

#' Title
#'
#' @param n TODO
#' @param k TODO
#' @param expected_degree TODO
#'
#' @return TODO
#' @export
#'
#' @examples
#'
#' set.seed(26)
#'
#' mblock <- model_mediator_block2(n = 100, k = 5)
#'
#' graph <- sample_tidygraph(mblock)
#' graph
#'
#' fit <- nodelm(US(A, 5) ~ . - name - 1, graph = graph)
#' fit
#'
model_mediator_block2 <- function(n, k = 5, expected_degree = NULL) {

  B <- matrix(0.03, nrow = k, ncol = k)
  diag(B) <- 0.8

  pi <- rep(1 / k, k)

  A_model <- fastRG::dcsbm(
    theta = stats::runif(n, min = 1, max = 3),
    B = B,
    pi = pi,
    expected_degree = expected_degree,
    allow_self_loops = TRUE,
    poisson_edges = TRUE
  )

  if (k > 1) {
    W <- stats::model.matrix(~ A_model$z + 0)
  } else {
    W <- matrix(1, nrow = n, ncol = 1)
  }

  # add intercept and subject one block membership column to
  # avoid identification/multicolinearity issue
  W <- cbind(1,  W[, -1, drop = FALSE])

  if (k == 2) {
    colnames(W) <- c("intercept", "trt")
  } else {
    colnames(W) <- c("intercept", "trt", paste0("C", 1:(k - 2)))
  }

  m <- model_mediator(A_model, W, subclass = "block2")
  m$model_name <- "block"
  m
}
