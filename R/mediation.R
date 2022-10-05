#' Estimate network direct and indirect effects via a spectral method
#'
#' **TODO**: Allow embedding L_tau(A) and L(A) in addition to A
#'
#' @param graph A [tidygraph::tbl_graph()] object.
#' @param formula Details about the nodal design matrix. Of the form
#'   outcome ~ nodal_formula. For now, no interactiosn or fancy stuff
#'   are allowed in the formula.
#' @param rank Integer describing desired rank of embedding dimension.
#'
#' @return A `network_mediation` object.
#' @importFrom tidygraph mutate as_tibble
#' @importFrom broom tidy
#' @importFrom estimatr tidy
#' @export
#'
#' @examples
#'
#' library(tidygraph)
#'
#' smokeint <- smoking |>
#'   mutate(
#'     smokes_int = as.integer(smokes) - 1
#'   )
#'
#' netmed <- smokeint |>
#'   netmediate(smokes_int ~ sex + 0, rank = 7)
#'
#' netmed
#'
netmediate <- function(graph, formula, rank) {

  node_data <- tidygraph::as_tibble(graph)
  A <- igraph::as_adjacency_matrix(graph, sparse = TRUE)

  X <- US(A, rank = rank)

  mf <- model.frame(formula, data = node_data)
  y <- stats::model.response(mf)
  W <- stats::model.matrix(mf, node_data)

  outcome_model <- stats::lm(y ~ W + X + 0)
  mediator_model <- stats::lm(X ~ W + 0)



  browser()

  num_coefs <- ncol(W)

  nde_table <- tidy(outcome_model, conf.int = TRUE)[1:num_coefs, ] |>
    mutate(estimand = "nde") |>
    select(term, estimand, estimate, conf.low, conf.high)

  nde_table

  betax <- stats::coef(outcome_model)[-c(1:num_coefs)]
  theta <- stats::coef(mediator_model)

  sigmabetax <- vcov(outcome_model)[-c(1:num_coefs), -c(1:num_coefs)]
  sigmatheta <- vcov(mediator_model)

  nie_hat <- drop(theta %*% betax)
  nie_hat

  rep(betax, each = num_coefs) %*% sigmatheta %*% rep(betax, each = num_coefs)


  theta %*% sigmabetax %*% t(theta)

  nde_hat <- stats::coef(outcome_model)[1:num_coefs]
  covariate_names <- names(nde_hat)

  effects <- tibble::tibble(
    term = names(W_coefs),
    nde = W_coefs,
    nie = drop( %*% stats::coef(outcome_model)[-c(1:num_coefs)]),
    total = nde + nie
  )

  object <- list(
    formula = formula,
    rank = rank,
    outcome_model = outcome_model,
    mediator_model = mediator_model,
    effects = effects
  )

  class(object) <- "network_mediation"
  object
}

#' @method print network_mediation
#' @export
print.network_mediation <- function(x, ...) {

  cat("A network mediation object\n")
  cat("------------------------\n\n")

  cat("Outcome model:\n")
  cat("--------------\n")

  print(summary(x$outcome_model))

  cat("Mediator model:\n")
  cat("---------------\n\n")

  print(stats::anova(x$mediator_model))

  cat("\nEstimated direct and indirect effects:\n")
  cat("---------------------------------------\n\n")

  print(x$effects)
  invisible(x)
}

#' Estimate mediated effects for a variety of embedding dimensions
#'
#' @inheritParams netmediate
#' @param max_rank Maximum rank to consider (integer).
#' @param ranks_to_consider How many distinct ranks to consider (integer).
#'   Optional, defaults to 10.
#'
#' @return A `rank_sensitivity_curve` object, which is a subclass of a
#'   [tibble::tibble()].
#' @export
#'
#' @examples
#'
#' library(tidygraph)
#'
#' rank_curve <- smoking |>
#'   mutate(
#'     smokes_int = as.integer(smokes) - 1
#'   ) |>
#'  sensitivity_curve(smokes_int ~ sex, max_rank = 25, 25)
#'
#' rank_curve
#' plot(rank_curve)
#'
sensitivity_curve <- function(graph, formula, max_rank, ranks_to_consider = 10) {

  node_data <- tidygraph::as_tibble(graph)
  A <- igraph::as_adjacency_matrix(graph, sparse = TRUE)

  s <- irlba::irlba(A, nv = max_rank)
  U_max <- s$u %*% diag(sqrt(s$d))

  mf <- stats::model.frame(formula, data = node_data)
  y <- stats::model.response(mf)
  Z <- stats::model.matrix(mf, node_data)
  num_coefs <- ncol(Z)

  effects_at_rank <- function(rank) {

    U <- U_max[, 1:rank]

    outcome_model <- stats::lm(y ~ Z + U + 0)
    mediator_model <- stats::lm(U ~ Z + 0)

    Z_coefs <- stats::coef(outcome_model)[1:num_coefs]

    effects <- tibble::tibble(
      rank = rank,
      term = names(Z_coefs),
      nde = Z_coefs,
      nie = drop(stats::coef(mediator_model) %*% stats::coef(outcome_model)[-c(1:num_coefs)]),
      total = nde + nie
    )
  }

  ranks <- seq(2, max_rank, length.out = min(ranks_to_consider, max_rank - 1))

  curve <- purrr::map_dfr(ranks, effects_at_rank)
  class(curve) <- c("rank_sensitivity_curve", class(curve))
  curve
}

#' @method plot rank_sensitivity_curve
#' @export
#' @import ggplot2
plot.rank_sensitivity_curve <- function(x, ...) {

  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop("Must install `ggplot2` package for this functionality.", call. = FALSE)
  }

  x |>
    tidyr::pivot_longer(
      c("nie", "nde", "total"),
      names_to = "effect"
    ) |>
    dplyr::mutate(
      effect = dplyr::recode(
        effect,
        nde = "Natural Direct Effect",
        nie = "Natural Indirect Effect",
        total = "Average Treatment Effect"
      )
    ) |>
    ggplot() +
    aes(x = rank, y = value, color = effect) +
    geom_line() +
    geom_point() +
    geom_hline(yintercept = 0, linetype = "dashed") +
    scale_color_viridis_d() +
    theme_classic(14) +
    facet_wrap(vars(term)) +
    labs(
      title = "Estimated effects as a function of latent space dimension",
      color = "Effect",
      x = "Latent dimension of network"
    ) +
    theme(
      axis.title.y = element_blank()
    )
}
