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
    object$theta_0,
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
  rbind(
    object$theta_0,
    object$theta_t,
    object$theta_c,
    object$theta_tc
  )
}

#' Title
#'
#' @param estimates TODO
#' @param model TODO
#'
#' @return TODO
#' @export
#'
mediator_loss <- function(estimates, model) {

  # TODO: add something like a Pillai test for any association at all?

  coef_estimates <- stats::coef(estimates)

  coef_model <- stats::coef(model)
  rownames(coef_model) <- rownames(coef_estimates)

  procrustes <- vegan::procrustes(coef_estimates, coef_model, scale = FALSE)

  # true coefficients after alignment with estimated coefs
  coef_rot <- procrustes$Yrot
  rownames(coef_rot) <- rownames(coef_estimates)
  colnames(coef_rot) <- 1:model$k

  tidy_estimates <- estimatr::tidy(estimates)

  coverage_table <- coef_rot |>
    dplyr::as_tibble(rownames = "term") |>
    tidyr::pivot_longer(
      -term,
      names_to = "outcome",
      values_to = "aligned_true_coef"
    ) |>
    dplyr::left_join(tidy_estimates, by = c("term", "outcome")) |>
    dplyr::mutate(
      aligned_coef_covered = conf.low <= aligned_true_coef & aligned_true_coef <= conf.high
    )

  aligned_diff <- coef_estimates - coef_rot

  list(
    procrustes = procrustes,
    frob_coef_loss = procrustes$ss,
    aligned_diff = aligned_diff,
    coverage_table = coverage_table
  )
}
