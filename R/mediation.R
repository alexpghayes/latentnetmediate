#' Estimate network direct and indirect effects via a spectral method
#'
#' **TODO**: Allow embedding L_tau(A) and L(A) in addition to A
#'
#' @param graph A [tidygraph::tbl_graph()] object.
#' @param formula Details about the nodal design matrix. Of the form
#'   outcome ~ nodal_formula. For now, no interactions or fancy stuff
#'   are allowed in the formula.
#' @param rank Integer describing desired rank of embedding dimension.
#' @param coembedding TODO
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
#' data(smoking)
#'
#' smokeint <- smoking |>
#'   mutate(
#'     smokes_int = as.integer(smokes) - 1
#'   )
#'
#' netmed <- smokeint |>
#'   netmediate(smokes_int ~ sex, rank = 7)
#'
#' netmed
#'
netmediate <- function(graph, formula, rank, coembedding = c("U", "V", "Z", "Y")) {

  # doesn't handle missing data

  node_data <- tidygraph::as_tibble(graph)

  if (igraph::is.bipartite(graph)) {
    A <- igraph::as_incidence_matrix(graph, sparse = TRUE, attr = "weight")
    A <- methods::as(A, "CsparseMatrix")
  } else {
    A <- igraph::as_adjacency_matrix(graph, sparse = TRUE)
  }

  coembedding <- rlang::arg_match(coembedding)

  if (coembedding == "U") {
    X <- US(A, rank = rank)  # use scaled left singular vectors
  } else if (coembedding == "V") {
    X <- VS(A, rank = rank)  # use scaled right singular vectors
  } else if (coembedding == "Z") {
    X <- Z(A, rank = rank)  # use scaled right singular vectors
  } else if (coembedding == "Y") {
    X <- Y(A, rank = rank)  # use scaled right singular vectors
  } else {
    stop("Invalid coembedding")
  }

  mf <- stats::model.frame(formula, data = node_data)
  y <- stats::model.response(mf)
  W <- stats::model.matrix(mf, node_data)

  outcome_model <- stats::lm(y ~ W + X + 0)
  mediator_model <- stats::lm(X ~ W + 0)

  num_coefs <- nrow(coef(mediator_model))

  nde_table <- broom::tidy(outcome_model, conf.int = TRUE)[1:num_coefs, ] |>
    dplyr::mutate(estimand = "nde") |>
    dplyr::select(term, estimand, estimate, conf.low, conf.high)

  betaw_hat <- stats::coef(outcome_model)[1:num_coefs]
  betax_hat <- stats::coef(outcome_model)[-c(1:num_coefs)]
  theta_hat <- stats::coef(mediator_model)

  nie_hat <- drop(theta_hat %*% betax_hat)

  nie_table <- tibble::enframe(nie_hat, name = "term", value = "estimate") |>
    dplyr::mutate(estimand = "nie") |>
    dplyr::select(term, estimand, estimate)

  sigmabetax_hat <- stats::vcov(outcome_model)[-c(1:num_coefs), -c(1:num_coefs)]
  sigmatheta_hat <- stats::vcov(mediator_model)

  # need to re-arrange sigmatheta_hat from enormous square into something
  # more tensor-y / considering each covariate one at a time

  coef_names <- names(betaw_hat)

  # everything following is under the assumption that Theta_tc = 0
  # for convenience since it's a pain to handle Theta_tc != 0

  for (i in seq_along(coef_names)) {

    nm <- coef_names[i]

    indices <- which(
      stringr::str_detect(
        colnames(sigmatheta_hat),
        stringr::coll(nm)
      )
    )

    thetat_hat <- theta_hat[i, ]
    sigmathetat_hat <- sigmatheta_hat[indices, indices, drop = FALSE]

    # delta method
    nie_var <- t(betax_hat) %*% sigmathetat_hat %*% betax_hat +
      t(thetat_hat) %*% sigmabetax_hat %*% thetat_hat

    nie_table[i, "conf.low"] <- nie_hat[i] - 1.96 * sqrt(nie_var)
    nie_table[i, "conf.high"] <- nie_hat[i] + 1.96 * sqrt(nie_var)
  }

  effects <- dplyr::bind_rows(nde_table, nie_table) |>
    dplyr::filter(!stringr::str_detect(term, "Intercept")) |>
    dplyr::mutate(
      term = stringr::str_replace(term, "W", "")
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

#' @method plot network_mediation
#' @export
plot.network_mediation <- function(x, ...) {

  x$effects |>
    ggplot() +
    aes(
      x = term,
      ymin = conf.low,
      y = estimate,
      ymax = conf.high,
      color = estimand
    ) +
    geom_pointrange(position = position_dodge(width = 1))

}

#' Estimate mediated effects for a variety of embedding dimensions
#'
#' @inheritParams netmediate
#' @param max_rank Maximum rank to consider (integer).
#' @param ranks_to_consider How many distinct ranks to consider (integer).
#'   Optional, defaults to 10.
#' @inheritDotParams estimatr::lm_robust
#'
#' @return A `rank_sensitivity_curve` object, which is a subclass of a
#'   [tibble::tibble()].
#' @export
#'
#' @examples
#'
#' library(tidygraph)
#' library(dplyr)
#'
#' data(smoking, package = "latentnetmediate")
#'
#' # example with fully observed node data
#'
#' rank_curve <- smoking |>
#'   mutate(
#'     smokes_int = as.integer(smokes) - 1
#'   ) |>
#'  sensitivity_curve(
#'    smokes_int ~ sex,
#'    max_rank = 25,
#'    ranks_to_consider = 24,
#'    se_type = "stata"
#'  )
#'
#' rank_curve
#' plot(rank_curve)
#'
#' # example with some missing node data. in this case, all edges are
#' # used to estimate embeddings, but once the embeddings are in hand,
#' # the regression only considers complete cases
#'
#' data(glasgow, package = "latentnetmediate")
#'
#' glasgow1 <- glasgow[[1]] |>
#'   activate(nodes) |>
#'   filter(selection129) |>
#'   mutate(
#'     smokes_dimaria = as.numeric(tobacco_int > 1)
#'   ) |>
#'   activate(edges) |>
#'   filter(friendship != "Structurally missing") |>
#'   activate(nodes)
#'
#' # verify that there is some missing data, in this case treatment indicators
#' glasgow1 |>
#'   as_tibble() |>
#'   count(is.na(money))
#'
#' glasgow1 |>
#'   sensitivity_curve(smokes_dimaria ~ money, max_rank = 15, coembedding = "V") |>
#'   plot()
#'
sensitivity_curve <- function(graph, formula, max_rank, ..., ranks_to_consider = 10,
                              coembedding = c("U", "V")) {

  rlang::check_dots_used()

  node_data <- tidygraph::as_tibble(graph)

  if (igraph::is.bipartite(graph)) {
    A <- igraph::as_incidence_matrix(graph, sparse = TRUE, attr = "weight")
    A <- methods::as(A, "CsparseMatrix")
  } else {
    A <- igraph::as_adjacency_matrix(graph, sparse = TRUE)
  }

  coembedding <- rlang::arg_match(coembedding)

  if (coembedding == "U") {
    X_max <- US(A, rank = max_rank)  # use scaled left singular vectors
  } else {
    X_max <- VS(A, rank = max_rank)  # use scaled right singular vectors
  }

  # if there is missing node-level data, model.frame() will apply the
  # na.action argument, which defaults to na.omit(). this causes problems
  # later: mf will be a node-level data frame with num_complete_cases rows
  # and X_max will be a matrix with num_all_nodes rows. so we need to match
  # these two data sources up.

  mf <- stats::model.frame(formula, data = node_data)
  y <- stats::model.response(mf)
  W <- stats::model.matrix(mf, node_data)

  # NULL when no data is dropped, otherwise a named integer vector of
  dropped_indices <- attr(mf, "na.action")

  # drop observations from X_max with missing node-level data
  if (!is.null(dropped_indices)) {
    X_max <- X_max[-dropped_indices, ]
  }

  effects_at_rank <- function(rank) {

    X <- X_max[, 1:rank, drop = FALSE]

    outcome_model <- estimatr::lm_robust(y ~ W + X + 0, ...)
    mediator_model <- estimatr::lm_robust(X ~ W + 0, ...)

    num_coefs <- nrow(coef(mediator_model))

    nde_table <- broom::tidy(outcome_model, conf.int = TRUE)[1:num_coefs, ] |>
      dplyr::mutate(estimand = "nde") |>
      dplyr::select(term, estimand, estimate, conf.low, conf.high)

    betaw_hat <- stats::coef(outcome_model)[1:num_coefs]
    betax_hat <- stats::coef(outcome_model)[-c(1:num_coefs)]
    theta_hat <- stats::coef(mediator_model)

    nie_hat <- drop(theta_hat %*% betax_hat)

    nie_table <- tibble::enframe(nie_hat, name = "term", value = "estimate") |>
      dplyr::mutate(estimand = "nie") |>
      dplyr::select(term, estimand, estimate)

    sigmabetax_hat <- stats::vcov(outcome_model)[-c(1:num_coefs), -c(1:num_coefs)]
    sigmatheta_hat <- stats::vcov(mediator_model)

    # need to re-arrange sigmatheta_hat from enormous square into something
    # more tensor-y / considering each covariate one at a time

    # matching terms with "(Intercept)" is messy because regex interprets
    # the parentheses as specials. to avoid this, we do a hack and replace
    # the "(Intercept)" wherever we see it. this same issue may come up
    # if other term names include regex special characters
    clean_term_names <- function(term_names) {
      stringr::str_replace_all(term_names, stringr::fixed("(Intercept)"), "Intercept")
    }

    coef_names <- clean_term_names(names(betaw_hat))
    sigma_names <- clean_term_names(colnames(sigmatheta_hat))

    # everything following is under the assumption that Theta_tc = 0
    # for convenience since it's a pain to handle Theta_tc != 0

    for (i in seq_along(coef_names)) {

      nm <- coef_names[i]

      indices <- which(
        stringr::str_detect(
          sigma_names,
          paste0(nm, "$")
        )
      )

      if (length(indices) != length(betax_hat)) {
        stop("Could not correctly match regression terms with variance estimates. Please open an issue with a reproducible example at <https://github.com/alexpghayes/netmediate/issues>. In the meantime, consider trying again, but avoid using any regex special characters in nodal covariate feature names.")
      }

      thetat_hat <- theta_hat[i, ]
      sigmathetat_hat <- sigmatheta_hat[indices, indices, drop = FALSE]

      # delta method
      nie_var <- t(betax_hat) %*% sigmathetat_hat %*% betax_hat +
        t(thetat_hat) %*% sigmabetax_hat %*% thetat_hat

      nie_table[i, "conf.low"] <- nie_hat[i] - 1.96 * sqrt(nie_var)
      nie_table[i, "conf.high"] <- nie_hat[i] + 1.96 * sqrt(nie_var)
    }

    effects <- dplyr::bind_rows(nde_table, nie_table) |>
      dplyr::filter(!stringr::str_detect(term, "Intercept")) |>
      dplyr::mutate(
        term = stringr::str_replace(term, "W", ""),
        rank = rank
      )
  }

  ranks <- seq(2, max_rank, length.out = min(ranks_to_consider, max_rank - 1))

  curve <- purrr::map_dfr(ranks, effects_at_rank)
  class(curve) <- c("rank_sensitivity_curve", class(curve))
  curve
}

#' Estimate mediated effects for a variety of embedding dimensions using custom embedding
#'
#' @inheritParams netmediate
#' @param X_max TODO
#' @param node_data TODO
#' @inheritDotParams estimatr::lm_robust
#'
#' @return A `rank_sensitivity_curve` object, which is a subclass of a
#'   [tibble::tibble()].
#' @export
#'
#' @examples
#'
#' library(tidygraph)
#' library(invertiforms)
#'
#' # suppose you want to use the degree-normalized Laplacian embedding
#' # instead of the adjacency spectral embedding. you can do that as
#' # follows
#'
#' data(smoking)
#'
#' smoking2 <- smoking |>
#'   mutate(
#'     smokes_int = as.integer(smokes) - 1
#'   )
#'
#' A <- igraph::as_adj(smoking2)
#'
#' # here we construct our "custom" embeddings
#'
#' iform <- NormalizedLaplacian(A)
#' L <- transform(iform, A)
#'
#' s_max <- RSpectra::svds(L, 10, 10)
#' X_max <- s_max$u %*% diag(sqrt(s_max$d))
#'
#' # and now we plug them into the product-of-coefs estimator
#'
#' curve_custom <- sensitivity_curve_custom(smoking2, smokes_int ~ sex, X_max)
#' curve_custom
#'
#' plot(curve_custom)
#'
sensitivity_curve_custom <- function(graph, formula, X_max, ..., node_data = NULL) {

  rlang::check_dots_used()

  if (is.null(node_data)) {
    node_data <- tidygraph::as_tibble(graph)
  }

  mf <- stats::model.frame(formula, data = node_data)
  y <- stats::model.response(mf)
  W <- stats::model.matrix(mf, node_data)

  # NULL when no data is dropped, otherwise a named integer vector of
  dropped_indices <- attr(mf, "na.action")

  # drop observations from X_max with missing node-level data
  if (!is.null(dropped_indices)) {
    X_max <- X_max[-dropped_indices, ]
  }

  effects_at_rank <- function(rank) {

    X <- X_max[, 1:rank, drop = FALSE]
    colnames(X) <- paste0("X", 1:ncol(X))

    outcome_model <- estimatr::lm_robust(y ~ W + X + 0, ...)
    mediator_model <- estimatr::lm_robust(X ~ W + 0, ...)

    num_coefs <- nrow(coef(mediator_model))

    nde_table <- broom::tidy(outcome_model, conf.int = TRUE)[1:num_coefs, ] |>
      dplyr::mutate(estimand = "nde") |>
      dplyr::select(term, estimand, estimate, conf.low, conf.high)

    betaw_hat <- stats::coef(outcome_model)[1:num_coefs]
    betax_hat <- stats::coef(outcome_model)[-c(1:num_coefs)]
    theta_hat <- stats::coef(mediator_model)

    nie_hat <- drop(theta_hat %*% betax_hat)

    nie_table <- tibble::enframe(nie_hat, name = "term", value = "estimate") |>
      dplyr::mutate(estimand = "nie") |>
      dplyr::select(term, estimand, estimate)

    sigmabetax_hat <- stats::vcov(outcome_model)[-c(1:num_coefs), -c(1:num_coefs)]
    sigmatheta_hat <- stats::vcov(mediator_model)

    clean_term_names <- function(term_names) {
      stringr::str_replace_all(term_names, stringr::fixed("(Intercept)"), "Intercept")
    }

    coef_names <- clean_term_names(names(betaw_hat))
    sigma_names <- clean_term_names(colnames(sigmatheta_hat))

    # everything following is under the assumption that Theta_tc = 0
    # for convenience since it's a pain to handle Theta_tc != 0

    for (i in seq_along(coef_names)) {

      nm <- coef_names[i]

      indices <- which(
        stringr::str_detect(
          sigma_names,
          paste0(nm, "$")
        )
      )

      if (length(indices) != length(betax_hat)) {
        stop("Could not correctly match regression terms with variance estimates. Please open an issue with a reproducible example at <https://github.com/alexpghayes/netmediate/issues>. In the meantime, consider trying again, but avoid using any regex special characters in nodal covariate feature names.")
      }

      thetat_hat <- theta_hat[i, ]
      sigmathetat_hat <- sigmatheta_hat[indices, indices, drop = FALSE]

      # delta method
      nie_var <- t(betax_hat) %*% sigmathetat_hat %*% betax_hat +
        t(thetat_hat) %*% sigmabetax_hat %*% thetat_hat

      nie_table[i, "conf.low"] <- nie_hat[i] - 1.96 * sqrt(nie_var)
      nie_table[i, "conf.high"] <- nie_hat[i] + 1.96 * sqrt(nie_var)
    }

    effects <- dplyr::bind_rows(nde_table, nie_table) |>
      dplyr::filter(!stringr::str_detect(term, "Intercept")) |>
      dplyr::mutate(
        term = stringr::str_replace(term, "W", ""),
        rank = rank
      )
  }

  ranks <- 2:ncol(X_max)

  curve <- purrr::map_dfr(ranks, effects_at_rank)
  class(curve) <- c("rank_sensitivity_curve", class(curve))
  curve
}

#' Estimate mediated effects for a variety of embedding dimensions
#'
#' @inheritParams netmediate
#' @inheritDotParams estimatr::lm_robust
#' @param node_data TODO
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
#' library(dplyr)
#'
#' data(smoking, package = "latentnetmediate")
#'
#' # example with fully observed node data
#'
#' rank_curve <- smoking |>
#'   mutate(
#'     smokes_int = as.integer(smokes) - 1
#'   ) |>
#'  sensitivity_curve(
#'    smokes_int ~ sex,
#'    max_rank = 25,
#'    ranks_to_consider = 24,
#'    se_type = "stata"
#'  )
#'
#' rank_curve
#' plot(rank_curve)
#'
#' # example with some missing node data. in this case, all edges are
#' # used to estimate embeddings, but once the embeddings are in hand,
#' # the regression only considers complete cases
#'
#' data(glasgow, package = "latentnetmediate")
#'
#' glasgow1 <- glasgow[[1]] |>
#'   activate(nodes) |>
#'   filter(selection129) |>
#'   mutate(
#'     smokes_dimaria = as.numeric(tobacco_int > 1)
#'   ) |>
#'   activate(edges) |>
#'   filter(friendship != "Structurally missing") |>
#'   activate(nodes)
#'
#' # verify that there is some missing data, in this case treatment indicators
#' glasgow1 |>
#'   as_tibble() |>
#'   count(is.na(money))
#'
#' glasgow1 |>
#'   sensitivity_curve(smokes_dimaria ~ money, max_rank = 15, coembedding = "V") |>
#'   plot()
#'
sensitivity_curve_long <- function(graph, formula, max_rank, ..., ranks_to_consider = 10,
                                   coembedding = c("U", "V"), node_data = NULL) {

  # rlang::check_dots_used()

  # node_data must have a column called name that matches the name feature of the graph

  if (igraph::is.bipartite(graph)) {
    A <- igraph::as_incidence_matrix(graph, sparse = TRUE, attr = "weight")
    A <- methods::as(A, "CsparseMatrix")
  } else {
    A <- igraph::as_adjacency_matrix(graph, sparse = TRUE)
  }

  coembedding <- rlang::arg_match(coembedding)

  if (coembedding == "U") {
    X_max <- US(A, rank = max_rank)  # use scaled left singular vectors
    rownames(X_max) <- rownames(A)
  } else {
    X_max <- VS(A, rank = max_rank)  # use scaled right singular vectors
    rownames(X_max) <- colnames(A)
  }

  # if there is missing node-level data, model.frame() will apply the
  # na.action argument, which defaults to na.omit(). this causes problems
  # later: mf will be a node-level data frame with num_complete_cases rows
  # and X_max will be a matrix with num_all_nodes rows. so we need to match
  # these two data sources up.

  X_max_df <- X_max |>
    as.data.frame() |>
    as_tibble(rownames = "name")

  node_data_and_X_max <- node_data |>
    dplyr::left_join(X_max_df, by = "name")

  mf <- stats::model.frame(formula, data = node_data_and_X_max)
  y <- stats::model.response(mf)
  W <- stats::model.matrix(mf, node_data)

  # NULL when no data is dropped, otherwise a named integer vector of
  dropped_indices <- attr(mf, "na.action")

  if (!is.null(dropped_indices)) {
    node_data_and_X_max <- node_data_and_X_max[-dropped_indices, ]
  }

  X_max <- node_data_and_X_max |>
    dplyr::select(dplyr::all_of(colnames(X_max))) |>
    as.matrix()

  effects_at_rank <- function(rank, ...) {

    X <- X_max[, 1:rank, drop = FALSE]

    outcome_model <- estimatr::lm_robust(y ~ W + X + 0, ...)
    mediator_model <- estimatr::lm_robust(X ~ W + 0, ...)

    num_coefs <- nrow(coef(mediator_model))

    nde_table <- broom::tidy(outcome_model, conf.int = TRUE)[1:num_coefs, ] |>
      dplyr::mutate(estimand = "nde") |>
      dplyr::select(term, estimand, estimate, conf.low, conf.high)

    betaw_hat <- stats::coef(outcome_model)[1:num_coefs]
    betax_hat <- stats::coef(outcome_model)[-c(1:num_coefs)]
    theta_hat <- stats::coef(mediator_model)

    nie_hat <- drop(theta_hat %*% betax_hat)

    nie_table <- tibble::enframe(nie_hat, name = "term", value = "estimate") |>
      dplyr::mutate(estimand = "nie") |>
      dplyr::select(term, estimand, estimate)

    sigmabetax_hat <- stats::vcov(outcome_model)[-c(1:num_coefs), -c(1:num_coefs)]
    sigmatheta_hat <- stats::vcov(mediator_model)

    # need to re-arrange sigmatheta_hat from enormous square into something
    # more tensor-y / considering each covariate one at a time

    # matching terms with "(Intercept)" is messy because regex interprets
    # the parentheses as specials. to avoid this, we do a hack and replace
    # the "(Intercept)" wherever we see it. this same issue may come up
    # if other term names include regex special characters
    clean_term_names <- function(term_names) {
      stringr::str_replace_all(term_names, stringr::fixed("(Intercept)"), "Intercept")
    }

    coef_names <- clean_term_names(names(betaw_hat))
    sigma_names <- clean_term_names(colnames(sigmatheta_hat))

    # everything following is under the assumption that Theta_tc = 0
    # for convenience since it's a pain to handle Theta_tc != 0

    for (i in seq_along(coef_names)) {

      nm <- coef_names[i]

      indices <- which(
        stringr::str_detect(
          sigma_names,
          paste0(nm, "$")
        )
      )

      if (length(indices) != length(betax_hat)) {
        stop("Could not correctly match regression terms with variance estimates. Please open an issue with a reproducible example at <https://github.com/alexpghayes/netmediate/issues>. In the meantime, consider trying again, but avoid using any regex special characters in nodal covariate feature names.")
      }

      thetat_hat <- theta_hat[i, ]
      sigmathetat_hat <- sigmatheta_hat[indices, indices, drop = FALSE]

      # delta method
      nie_var <- t(betax_hat) %*% sigmathetat_hat %*% betax_hat +
        t(thetat_hat) %*% sigmabetax_hat %*% thetat_hat

      nie_table[i, "conf.low"] <- nie_hat[i] - 1.96 * sqrt(nie_var)
      nie_table[i, "conf.high"] <- nie_hat[i] + 1.96 * sqrt(nie_var)
    }

    effects <- dplyr::bind_rows(nde_table, nie_table) |>
      dplyr::filter(!stringr::str_detect(term, "Intercept")) |>
      dplyr::mutate(
        term = stringr::str_replace(term, "W", ""),
        rank = rank
      )
  }

  ranks <- seq(2, max_rank, length.out = min(ranks_to_consider, max_rank - 1))

  curve <- purrr::map_dfr(ranks, \(rank) effects_at_rank(rank, ...))
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
    dplyr::mutate(
      effect = dplyr::recode(
        estimand,
        nde = "Direct",
        nie = "Indirect"
      )
    ) |>
    ggplot() +
    aes(
      x = rank, ymin = conf.low, y = estimate, ymax = conf.high,
      color = effect, fill = effect
    ) +
    geom_ribbon(alpha = 0.3) +
    geom_line() +
    geom_point() +
    scale_fill_brewer(palette = "Dark2") +
    scale_color_brewer(palette = "Dark2") +
    facet_wrap(vars(term)) +
    labs(
      title = "Estimated effects as a function of latent space dimension",
      color = "Natural Effect",
      fill = "Natural Effect",
      x = "Latent dimension of network"
    )
}
