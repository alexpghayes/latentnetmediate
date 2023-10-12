new_contagion <- function(n, k, W, beta_w, beta_x, A_model, rho, sigma, ..., subclass = character()) {

  rlang::check_dots_unnamed()

  model <- list(
    n = n,
    k = k,
    W = W,
    beta_w = beta_w,
    beta_x = beta_x,
    A_model = A_model,
    rho = rho,
    sigma = sigma,
    model_name = subclass,
    ...
  )

  class(model) <- c(subclass, "contagion")
  model
}

model_contagion <- function(A_model, W, ..., beta_w = NULL, beta_x = NULL, rho = 1, sigma = 1) {

  # this class is a bit of hack. the idea is that, we when sample from it,
  # we actually sample from four different, but closely related models, all
  # at once. the modeling options come from a 2x2 grid:
  #
  #  (peer contagion vs latent contagion) x (X in regression, X not in regression)
  #
  # notation: coefficients in peer contagion models called beta, coefficients
  #   in latent contagion models called gamma. also, let W = [1 T C] be
  #   observed nodal covariates, A be the adjacency matrix, d_i be ith node
  #   degree, P be the expected value of A, Y be nodal outcomes, and
  #   varepsilon be i.i.d. errors. let D be a diagonal matrix of node degrees
  #
  # peer contagion models:
  #
  #   Y = W betaw + D^{-1} A Y betay + varepsilon
  #   Y = W betaw + X betax + D^{-1} A Y betay + varepsilon
  #
  # latent contagion models:
  #
  #   Y = W gammaw + E[D|X]^{-1} E[A|X] Y gammay + varepsilon
  #   Y = W gammaw + X gammax + E[D|X]^{-1} E[A|X] Y gammay + varepsilon
  #
  # all of these outcome models can be rearranged into "reduced form"
  #
  # peer contagion models:
  #
  #   Y = (I - betay D^{-1} A)^{-1} (W betaw + varepsilon)
  #   Y = (I - betay D^{-1} A)^{-1} (W betaw + X betax + varepsilon)
  #
  # latent contagion models:
  #
  #   Y = (I - gammay E[D|X]^{-1} E[A|X])^{-1} (W gammaw + varepsilon)
  #   Y = (I - gammay E[D|X]^{-1} E[A|X])^{-1} (W gammaw + X gammax + varepsilon)
  #
  # we will set betay = gammay = rho, and betax = gammax, and betaw = gammaw
  # and sigma will correspond to the variance of N(0, sigma^2) varepsilon

  # NOTE: this is *not* A_model$X. there is a notational clash here. we use X
  #   to mean the population ASE. fastRG uses X to mean the varimax X matrix.
  #   not the same!

  dim_w <- ncol(W)
  dim_x <- A_model$k

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

  names(beta_w) <- colnames(W)
  names(beta_x) <- colnames(X)

  new_contagion(
    n = nrow(W),
    k = A_model$k,
    W = W,
    beta_w = beta_w,
    beta_x = beta_x,
    A_model = A_model,
    rho = rho,
    sigma = sigma,
    ...
  )
}

#' @method sample_tidygraph contagion
#' @export
sample_tidygraph.contagion <- function(model, ...) {

  # model$W should already have appropriate column names
  W_df <- tibble::as_tibble(as.matrix(model$W))

  graph <- fastRG::sample_tidygraph(
    model$A_model
  ) |>
    tidygraph::arrange(as.numeric(name)) |>
    tidygraph::mutate(!!!W_df)

  varepsilon <- stats::rnorm(model$n, sd = model$sigma)
  X <- ASE(model$A_model)

  I <- Matrix::Diagonal(model$n, 1)

  #### peer models

  A <- igraph::as_adj(graph)
  deg <- igraph::degree(graph)
  DinvA <- Matrix::rowScale(A, 1 / deg)

  # fingers crossed this doesn't start exploding. this will break if there
  # are identification issues. possible wrap in a tryCatch to alert if this
  # happens deep in a simulation loop
  pre_mult_peer <- solve(I - model$rho * DinvA)

  y_peer_no_x <- pre_mult_peer %*% (model$W %*% model$beta_w + varepsilon)
  y_peer_x <- pre_mult_peer %*% (model$W %*% model$beta_w + X %*% model$beta_x + varepsilon)

  ##### latent models

  # this will be memory intensive because these matrices are going to be dense
  # if this turns out to be prohibitive, we'll want to compute the inverse
  # of the pre-multiplication matrix more efficiently, probably by leverage
  # the svds(A_model) idea. that is, we can compute the pseudoinverse
  # from the svd of I - rho E[D]^{-1} P, and we can make a truncated svd
  # of that very fast

  P <- tcrossprod(X)
  ED <- rowSums(P)
  EDinvP <- Matrix::rowScale(P, 1 / ED)

  # fingers crossed this doesn't start exploding. this will break if there
  # are identification issues. possible wrap in a tryCatch to alert if this
  # happens deep in a simulation loop
  pre_mult_latent <- solve(I - model$rho * EDinvP)

  y_latent_no_x <- pre_mult_latent %*% (model$W %*% model$beta_w + varepsilon)
  y_latent_x <- pre_mult_latent %*% (model$W %*% model$beta_w + X %*% model$beta_x + varepsilon)

  ##### auxiliary data for oracle estimators

  # TODO: return this later, it's actually a bit more involved
  # because the oracle estimator depends on the model, plus the precise
  # definition of the oracle is up in the air to some degree: do we
  # use the true degree or the latent degrees
  y_oracle_EDinvP <- NULL
  y_oracle_DinvP <- NULL

  graph %>%
    tidygraph::activate(nodes) %>%
    tidygraph::mutate(
      y_peer_no_x = drop(y_peer_no_x),
      y_peer_x = drop(y_peer_x),
      y_latent_no_x = drop(y_latent_no_x),
      y_latent_x = drop(y_latent_x),
      latent_deg = drop(ED)
    )
}
