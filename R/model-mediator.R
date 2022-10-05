new_mediator <- function(n, k, X, W, A_model, Theta, ..., subclass = character()) {

  rlang::check_dots_unnamed()

  model <- list(
    n = n,
    k = k,
    X = X,
    W = W,
    A_model = A_model,
    Theta = Theta,
    model_name = subclass
  )

  class(model) <- c(subclass, "mediator")
  model
}

validate_mediator <- function(x) {

  # values <- unclass(x)

  x
}

model_mediator <- function(A_model, W, ...) {

  # infer X from the fastRG rdpg parameterization of the graph
  X <- ASE(A_model)

  # back out implied coefficients between X and W
  fit <- stats::lm(as.matrix(X) ~ as.matrix(W) + 0)
  Theta <- coef(fit)
  rownames(Theta) <- colnames(W)

  m <- new_mediator(
    n = nrow(X),
    k = ncol(X),
    X = X,
    W = W,
    A_model = A_model,
    Theta = Theta,
    ...
  )

  validate_mediator(m)
}

#' @importFrom stats coef
#' @export
#' @method coef mediator
coef.mediator <- function(object, ...) {
  object$Theta
}

#' Title
#'
#' @param ufm model
#'
#' @return ASE
#' @export
ASE <- function(ufm) {
  s <- fastRG::svds(ufm)
  k <- ufm$k

  if (k > 1) {
    S <- diag(sqrt(s$d * 2))
  } else {
    S <- matrix(sqrt(s$d * 2))
  }

  US <- s$u %*% S
  colnames(US) <- paste0("US", 1:ncol(US))
  US
}


#' Sample from a mediated RDPG object
#'
#' @param model A `mediated_rdpg` object
#' @param ... Ignored.
#'
#' @return A [tidygraph::tbl_graph()] object.
#'
#' @export
sample_tidygraph <- function(model, ...) {
  UseMethod("sample_tidygraph")
}


#' @method sample_tidygraph perfect
#' @export
sample_tidygraph.perfect <- function(model, intervene = TRUE, ...) {

  # model$W should already have appropriate column names
  W_df <- tibble::as_tibble(as.matrix(model$W))

  if (intervene) {
    A_model <- model$A_model
  } else {
    A_pre <- model$A_model
    A_pre$X <- z_pre_trt(model)
    A_model <- A_pre
  }

  graph <- fastRG::sample_tidygraph(A_model) |>
    dplyr::arrange(as.numeric(name)) |>
    dplyr::mutate(!!!W_df)

  graph
}

#' @method sample_tidygraph mediator
#' @export
sample_tidygraph.mediator <- function(model, ...) {

  # model$W should already have appropriate column names
  W_df <- tibble::as_tibble(as.matrix(model$W))

  graph <- fastRG::sample_tidygraph(
    model$A_model
  ) |>
    dplyr::arrange(as.numeric(name)) |>
    dplyr::mutate(!!!W_df)

  graph
}
