new_outcome <- function(y, beta_w, beta_x, mediator,..., subclass = character()) {

  rlang::check_dots_unnamed()

  model <- list(
    y = y,
    beta_w = beta_w,
    beta_x = beta_x,
    mediator = mediator
  )

  class(model) <- c(subclass, "outcome")
  model
}

validate_outcome <- function(x) {

  # check that beta_x and beta_w haave correct dimensions

  # check that mediator inherits medatior class

  # values <- unclass(x)

  x
}

model_outcome <- function(y, beta_w, beta_x, mediator, ...) {

  o <- new_outcome(
    y = y,
    beta_w = beta_w,
    beta_x = beta_x,
    mediator = mediator,
    ...
  )

  validate_outcome(o)
}

#' @importFrom stats coef
#' @export
#' @method coef outcome
coef.outcome <- function(object, ...) {
  c(object$beta_w, object$beta_x)
}

#' @method sample_tidygraph outcome
#' @export
sample_tidygraph.outcome <- function(model, ...) {

  graph <- sample_tidygraph(model$mediator, ...)

  tidygraph::mutate(graph, y = model$y)
}
