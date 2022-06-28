#' Title
#'
#' @inheritParams stats::coef
#'
#' @return TODO
#'
#' @method coef perfect
#'
#' @examples
#'
#' mu <- model_mediator_uninformative(n = 100, k = 5, dim_c = 3)
#'
#' coef(mu)
#'
#'
#' @name mediator_coef
NULL

#' @rdname mediator_coef
#' @export
#' @method coef uninformative
coef.uninformative <- function(object, ...) {
  rbind(
    object$theta_0
  )
}

#' @rdname mediator_coef
#' @export
#' @method coef block
coef.block <- function(object, ...) {
  rbind(
    # object$theta_0,
    object$theta_t
  )
}

#' @rdname mediator_coef
#' @export
#' @method coef informative
coef.informative <- function(object, ...) {
  rbind(
    object$theta_0,
    object$theta_t,
    object$theta_c,
    object$theta_tc
  )
}

#' @rdname mediator_coef
#' @export
#' @method coef perfect
coef.perfect <- function(object, ...) {
  # rbind(
  #   object$theta_0,
  #   object$theta_t,
  #   object$theta_c,
  #   object$theta_tc
  # )
  rbind(
   object$theta_c
  )
}

