#' Helper functions for including node embeddings in formulas
#'
#' Quickly compute top `rank` singular vectors of a matrix `A` and
#' return them in a matrix. Especially useful in formulas in regression
#' objects.
#'
#' @param A A [matrix()] or [Matrix::Matrix()] object.
#' @param rank Rank of desired decomposition.
#' @inheritDotParams irlba::irlba
#'
#' @details
#'
#' In all cases, computes a top-`rank` decomposition of `A`, such that
#' \eqn{A \approx U S V^T}.
#'
#' - `U` returns \eqn{U}
#' - `US` returns \eqn{U S^{1/2}}
#' - `V` returns \eqn{V}
#' - `VS` returns \eqn{V S^{1/2}}
#'
#' @name ase_specials
#' @seealso nodelm, vsp_specials
#' @aliases U, V, US, VS
NULL

#' @rdname ase_specials
#' @export
U <- function(A, rank, ...) {
  s <- RSpectra::svds(A, k = rank, nu = rank, nv = rank, ...) # irlba::irlba(A, nu = rank, nv = rank, ...)
  u <- s$u
  colnames(u) <- as.character(1:rank)
  u
}

#' @rdname ase_specials
#' @export
US <- function(A, rank, ...) {
  s <- RSpectra::svds(A, k = rank, nu = rank, nv = rank, ...) # irlba::irlba(A, nu = rank, nv = rank, ...)

  if (rank > 1) {
    us <- s$u %*% diag(sqrt(s$d))
  } else {
    us <- s$u %*% matrix(sqrt(s$d))
  }

  colnames(us) <- as.character(1:rank)
  us
}

#' @rdname ase_specials
#' @export
V <- function(A, rank, ...) {
  s <- RSpectra::svds(A, k = rank, nu = rank, nv = rank, ...) # irlba::irlba(A, nu = rank, nv = rank, ...)
  v <- s$v
  colnames(v) <- as.character(1:rank)
  v
}

#' @rdname ase_specials
#' @export
VS <- function(A, rank, ...) {
  s <- RSpectra::svds(A, k = rank, nu = rank, nv = rank, ...) # irlba::irlba(A, nu = rank, nv = rank, ...)

  if (rank > 1) {
    vs <- s$v %*% diag(sqrt(s$d))
  } else {
    vs <- s$v %*% matrix(sqrt(s$d))
  }

  colnames(vs) <- as.character(1:rank)
  vs
}

#' Helper functions for including rotated node embeddings in formulas
#'
#' In all cases, computes a top-`rank` decomposition of `A`, such that
#' \eqn{A \approx U S V^T = Z B Y^T}, where \eqn{Z} is \eqn{U} after
#' varimax rotation and \eqn{Y} is \eqn{V} after varimax rotation. See
#' [vsp::vsp()] for details.
#'
#' @param A A [matrix()] or [Matrix::Matrix()] object.
#' @param rank Rank of desired decomposition.
#' @inheritDotParams vsp::vsp
#' @inheritParams vsp::vsp
#'
#' @name vsp_specials
#' @seealso nodelm, ase_specials
#' @aliases Z, Y
NULL

#' @rdname vsp_specials
#' @export
Z <- function(A, rank, ..., degree_normalize = FALSE) {
  fa <- vsp::vsp(A, rank, ..., degree_normalize = degree_normalize)
  z <- as.matrix(fa$Z)
  colnames(z) <- as.character(1:rank)
  z
}

#' @rdname vsp_specials
#' @export
Y <- function(A, rank, ..., degree_normalize = FALSE) {
  fa <- vsp::vsp(A, rank, ..., degree_normalize = degree_normalize)
  y <- as.matrix(fa$Y)
  colnames(y) <- as.character(1:rank)
  y
}

#' Use spectral node embeddings in ordinary least squares regression
#'
#' A helper function that exposes the adjacency matrix `A`, normalized
#' graph Laplacian `L`, and regularized graph Laplacian `L_tau` to
#' model formulas for convenient network regression. Primarily designed
#' to work with [tidygraph::tbl_graph()] objects, but can also be used
#' with a matrix representation of a graph together with a [data.frame()]
#' of nodal covariates.
#'
#' @param formula A regression formula that can include [ase_specials]
#'   and [vsp_specials], which encode node embeddings. Data for non-
#'   embedding terms can come from the global environment, `data`, or
#'   can be named attributes of an `igraph` object. It is likely most
#'   convenient and intuitive to but nodal covariates in the `nodes` table
#'   of a [tidygraph::tbl_graph()] object to expose nodal data. See [reddit],
#'   [addhealth] and [smoking] for examples.
#' @param graph An optional [igraph::graph()] or [tidygraph::tbl_graph()]
#'   object. If specified, the graph adjacency matrix `A`, normalized
#'   graph Laplacian `L`, and regularized graph Laplacian `L_tau` are
#'   injected into the environment of formula, so these matrices may be used
#'   freely in `formula`. See [igraph::as_adjacency_matrix()] for
#'   details about the construction of `A`, and
#'   [invertiforms::NormalizedLaplacian()] and
#'   [invertiforms::RegularizedLaplacian()] for details about the construction
#'   of `L` and `L_tau`. Note that you can also use node embeddings based
#'   on arbitrary matrix representations of a graph--see the examples.
#' @param data A [data.frame()] with one row for each node in the graph.
#' @inheritParams igraph::as_adjacency_matrix
#' @inheritDotParams stats::lm
#'
#' @return An object of class `lm`. See [stats::lm()] for
#'   details.
#'
#' @import invertiforms
#' @export
#'
#' @examples
#'
#' ### some examples where data is specified as a tidygraph
#'
#' # a regression that does not use any node embeddings
#' nodelm(grade ~ sex, graph = addhealth[[36]])
#'
#' # a regression including left and right singular embeddings of
#' # the adjacency matrix and the normalized graph Laplacian
#' nodelm(grade ~ sex + U(A, 5) + V(L, 3), graph = addhealth[[36]])
#'
#' nodelm(as.integer(smokes) ~ sex + U(A, 5) , graph = smoking)
#'
#' library(Matrix)
#' library(tidygraph)
#'
#' B <- igraph::as_adjacency_matrix(addhealth[[36]], attr = "weight")
#'
#' node <- addhealth[[36]] |>
#'   as_tibble() |>
#'   mutate(level = rowSums(B))
#'
#' node[5, "sex"] <- NA
#' node
#'
#' fit <- nodelm(level ~ sex + grade + race + U(sign(B), 10), data = node)
#' summary(fit)
#'
nodelm <- function(formula, graph = NULL, data = NULL, attr = NULL, ...) {

  if (!is.null(graph)) {
    if (!inherits(graph, "igraph")) {
      stop("`graph` must be an `igraph` or `tbl_graph` object.")
    }

    # inject A, L and L_tau into formula environment
    A <- igraph::as_adjacency_matrix(graph, attr = attr)
    L <- transform(NormalizedLaplacian(A), A)
    L_tau <- transform(RegularizedLaplacian(A), A)

    if (!is.null(data)) {
      warning("Ignoring `data` since `graph` was specified.")
    }

    data <- tidygraph::as_tibble(tidygraph::as_tbl_graph(graph))

    environment(formula) <- rlang::env(A = A, L = L, L_tau = L_tau, parent = parent.frame())
  }

  stats::lm(formula, data = data, ...)
}

#' Use spectral node embeddings in OLS with robust standard errors
#'
#' A helper function that exposes the adjacency matrix `A`, normalized
#' graph Laplacian `L`, and regularized graph Laplacian `L_tau` to
#' model formulas for convenient network regression. Primarily designed
#' to work with [tidygraph::tbl_graph()] objects, but can also be used
#' with a matrix representation of a graph together with a [data.frame()]
#' of nodal covariates.
#'
#' @param formula A regression formula that can include [ase_specials]
#'   and [vsp_specials], which encode node embeddings. Data for non-
#'   embedding terms can come from the global environment, `data`, or
#'   can be named attributes of an `igraph` object. It is likely most
#'   convenient and intuitive to but nodal covariates in the `nodes` table
#'   of a [tidygraph::tbl_graph()] object to expose nodal data. See [reddit],
#'   [addhealth] and [smoking] for examples.
#' @param graph An optional [igraph::graph()] or [tidygraph::tbl_graph()]
#'   object. If specified, the graph adjacency matrix `A`, normalized
#'   graph Laplacian `L`, and regularized graph Laplacian `L_tau` are
#'   injected into the environment of formula, so these matrices may be used
#'   freely in `formula`. See [igraph::as_adjacency_matrix()] for
#'   details about the construction of `A`, and
#'   [invertiforms::NormalizedLaplacian()] and
#'   [invertiforms::RegularizedLaplacian()] for details about the construction
#'   of `L` and `L_tau`. Note that you can also use node embeddings based
#'   on arbitrary matrix representations of a graph--see the examples.
#' @param data A [data.frame()] with one row for each node in the graph.
#' @inheritParams igraph::as_adjacency_matrix
#' @inheritDotParams estimatr::lm_robust
#'
#' @return An object of class `lm_robust`. See [estimatr::lm_robust()] for
#'   details.
#'
#' @import invertiforms
#' @export
#'
#' @examples
#'
#' ### some examples where data is specified as a tidygraph
#'
#' # a regression that does not use any node embeddings
#' nodelm_robust(grade ~ sex, graph = addhealth[[36]])
#'
#' # a regression including left and right singular embeddings of
#' # the adjacency matrix and the normalized graph Laplacian
#' nodelm_robust(grade ~ sex + U(A, 5) + V(L, 3), graph = addhealth[[36]])
#'
#' nodelm_robust(as.integer(smokes) ~ sex + U(A, 5) , graph = smoking)
#'
#' library(Matrix)
#' library(tidygraph)
#'
#' B <- igraph::as_adjacency_matrix(addhealth[[36]], attr = "weight")
#'
#' node <- addhealth[[36]] |>
#'   as_tibble() |>
#'   mutate(level = rowSums(B))
#'
#' node[5, "sex"] <- NA
#' node
#'
#' fit <- nodelm_robust(level ~ sex + grade + race + U(sign(B), 10), data = node)
#' summary(fit)
#'
nodelm_robust <- function(formula, graph = NULL, data = NULL, attr = NULL, ...) {

  if (!is.null(graph)) {
    if (!inherits(graph, "igraph")) {
      stop("`graph` must be an `igraph` or `tbl_graph` object.")
    }

    # inject A, L and L_tau into formula environment
    A <- igraph::as_adjacency_matrix(graph, attr = attr)
    L <- transform(NormalizedLaplacian(A), A)
    L_tau <- transform(RegularizedLaplacian(A), A)

    if (!is.null(data)) {
      warning("Ignoring `data` since `graph` was specified.")
    }

    data <- tidygraph::as_tibble(tidygraph::as_tbl_graph(graph))

    environment(formula) <- rlang::env(A = A, L = L, L_tau = L_tau, parent = parent.frame())
  }

  estimatr::lm_robust(formula, data = data, ...)
}
