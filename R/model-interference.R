new_interference <- function(n, k, W, beta_w, A_model, rho, sigma, ..., subclass = character()) {

  rlang::check_dots_unnamed()

  model <- list(
    n = n,
    k = k,
    W = W,
    beta_w = beta_w,
    A_model = A_model,
    rho = rho,
    sigma = sigma,
    model_name = subclass,
    ...
  )

  class(model) <- c(subclass, "interference")
  model
}

model_interference <- function(A_model, W, beta_w = NULL, rho = 1, sigma = 1, ...) {

  # needed: conditional expectation, network itself, nodal covariates, and interfered outcomes

  # this specifically requires that there is a treatment column in w
  # call the contagion term rho, such that
  #
  #   y = beta_w w + rho * sum_j A_ij trt_j + varepsilon
  #
  # note that W include trt as a column


  dim_w <- ncol(W)

  if (is.null(beta_w)) {
    beta_w <- stats::rnorm(dim_w, mean = 1, sd = 0.5)
  } else {
    stopifnot(length(beta_w) == dim_w)
  }

  names(beta_w) <- colnames(W)

  new_interference(
    n = nrow(W),
    k = A_model$k,
    W = W,
    beta_w = beta_w,
    A_model = A_model,
    rho = rho,
    sigma = sigma,
    ...
  )
}

#' @method sample_tidygraph interference
#' @export
sample_tidygraph.interference <- function(model, ...) {

  # model$W should already have appropriate column names
  W_df <- tibble::as_tibble(as.matrix(model$W))

  graph <- fastRG::sample_tidygraph(
    model$A_model
  ) |>
    tidygraph::arrange(as.numeric(name)) |>
    tidygraph::mutate(!!!W_df)

  varepsilon <- stats::rnorm(model$n, sd = model$sigma)

  # this column must exist in W in interference models
  trt <- model$W[, "trt"]

  # TODO: is this going to handle multi-edges okay?
  # TODO: degree-normalization goes here
  A <- igraph::as_adj(graph)
  deg <- igraph::degree(graph)
  L <- Matrix::rowScale(A, 1 / deg)

  y <- as.numeric(model$W %*% model$beta_w + model$rho * L %*% trt + varepsilon)

  graph <- graph %>%
    tidygraph::activate(nodes) %>%
    tidygraph::mutate(
      y = y
    )

  graph
}
