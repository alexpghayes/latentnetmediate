#' Title
#'
#' @inheritParams stats::coef
#'
#' @return TODO
#' @export
#'
#' @method coef mediator
#'
#' @examples
#'
#' mu <- model_mediator_uninformative(n = 100, k = 5, dim_c = 3)
#'
#' coef(mu)
#'
#'
coef.mediator <- function(object, ...) {
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
  rownames(model) <- rownames(coef_estimates)

  procrustes <- vegan::procrustes(coef_estimates, coef_model, scale = FALSE)

  # true coefficients after alignment with estimated coefs
  coef_rot <- procrustes$Yrot
  rownames(coef_rot) <- rownames(coef_estimates)
  colnames(coef_rot) <- 1:5

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
