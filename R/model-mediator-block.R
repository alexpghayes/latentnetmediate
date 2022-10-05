
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
#' mblock <- model_mediator_block(n = 100, k = 5)
#'
#' graph <- sample_tidygraph(mblock)
#' graph
#'
#' fit <- nodelm(US(A, 5) ~ . - name - 1, graph = graph)
#' fit
#'
model_mediator_block <- function(n, k = 5, expected_degree = NULL) {

  B <- matrix(0.1, nrow = k, ncol = k)
  diag(B) <- 1

  pi <- rep(0.1, k)

  A_model <- fastRG::overlapping_sbm(
    n = n,
    B = B,
    pi = pi,
    expected_degree = expected_degree,
    allow_self_loops = TRUE,
    poisson_edges = TRUE
  )

  W <- as.matrix(A_model$Z[, 1:ceiling(k / 2)])
  colnames(W) <- c("trt", paste0("C", 2:ncol(W)))

  model_mediator(A_model, W, subclass = "block")
}
