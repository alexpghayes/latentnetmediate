
#' Mediator-pass through with normal errors
#'
#' @param mediator TODO
#' @param df TODO
#' @param beta_w TODO
#' @param beta_x TODO
#'
#' @return TODO
#' @export
#'
#' @examples
#'
#' m <- model_mediator_informative(n = 100, k = 5)
#' o <- model_outcome_normal(m)
#'
#' o
#'
#' coef(o)
#'
#' graph <- sample_tidygraph(o)
#' graph
#'
model_outcome_t <- function(mediator, beta_w = NULL, beta_x = NULL, df = 5) {

  dim_w <- ncol(mediator$W)
  dim_x <- ncol(mediator$X)

  if (is.null(beta_w)) {
    beta_w <- stats::rnorm(dim_w, mean = 1, sd = 0.5)
  } else {
    stopifnot(length(beta_w) == dim_w)
  }

  if (is.null(beta_x)) {
    beta_x <- stats::rnorm(dim_x, mean = 1, sd = 0.5)
  } else {
    stopifnot(length(beta_x) == dim_x)
  }

  names(beta_w) <- colnames(mediator$W)
  names(beta_x) <- colnames(mediator$X)

  varepsilon <- stats::rt(mediator$n, df = df)

  y <- as.numeric(mediator$W %*% beta_w + mediator$X %*% beta_x + varepsilon)

  o <- model_outcome(
    y = y,
    beta_w = beta_w,
    beta_x = beta_x,
    mediator = mediator,
    subclass = "normal"
  )

  o$model_name <- paste0(mediator$model_name, "_normal")
  o
}
