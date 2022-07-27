# completely uninformative
# trt is theta in dc sbm
# trt is block in dc sbm
# localized theta-tc treatment in dc-sbm, informative controls only


#' Title
#'
#' @inheritParams model_mediator_uninformative
#' @inheritDotParams model_mediator_uninformative
#'
#' @return TODO
#' @export
#'
#' @examples
#'
#' set.seed(26)
#'
#' mrdpg <- model_uninformative(n = 100, k = 5)
#'
#' graph <- sample_tidygraph(mrdpg)
#' graph
#'
#' m_fit <- nodelm(US(A, 5) ~ . - name - y - 1, graph = graph)
#' o_fit <- nodelm(y ~ . - name - 1 + US(A, 5), graph = graph)
#'
#' m_fit
#' o_fit
#'
model_uninformative <- function(n, k, ...) {
  m <- model_mediator_uninformative(n, k, ...)
  model_outcome_normal(m, beta_x = rep(3, k))
}

#' Title
#'
#' @inheritParams model_mediator_block3
#' @inheritDotParams model_mediator_block3
#'
#' @return TODO
#' @export
#'
#' @examples
#'
#' set.seed(26)
#'
#' mrdpg <- model_degree(n = 100, k = 5)
#'
#' graph <- sample_tidygraph(mrdpg)
#' graph
#'
#' m_fit <- nodelm(US(A, 5) ~ . - name - y - 1, graph = graph)
#' o_fit <- nodelm(y ~ . - name - 1 + US(A, 5), graph = graph)
#'
#' m_fit
#' o_fit
#'
model_degree <- function(n, k, ...) {
  m <- model_mediator_block3(n, k, ...)
  model_outcome_normal(m)
}

#' Title
#'
#' @inheritParams model_mediator_block2
#' @inheritDotParams model_mediator_block2
#'
#' @return TODO
#' @export
#'
#' @examples
#'
#' set.seed(26)
#'
#' mrdpg <- model_block(n = 100, k = 5)
#'
#' graph <- sample_tidygraph(mrdpg)
#' graph
#'
#' m_fit <- nodelm(US(A, 5) ~ . - name - y - 1, graph = graph)
#' o_fit <- nodelm(y ~ . - name - 1 + US(A, 5), graph = graph)
#'
#' m_fit
#' o_fit
#'
model_block <- function(n, k, ...) {
  m <- model_mediator_block2(n, k, ...)
  model_outcome_normal(m)
}

#' Title
#'
#' @inheritParams model_mediator_informative
#' @inheritDotParams model_mediator_informative
#'
#' @return TODO
#' @export
#'
#' @examples
#'
#' set.seed(26)
#'
#' mrdpg <- model_shift(n = 100, k = 5)
#'
#' graph <- sample_tidygraph(mrdpg)
#' graph
#'
#' m_fit <- nodelm(US(A, 5) ~ . - name - y - 1, graph = graph)
#' o_fit <- nodelm(y ~ . - name - 1 + US(A, 5), graph = graph)
#'
#' m_fit
#' o_fit
#'
model_shift <- function(n, k, ...) {
  m <- model_mediator_informative(n, k, ...)
  model_outcome_normal(m)
}

#' Title
#'
#' @inheritParams model_mediator_informative
#' @inheritDotParams model_mediator_informative
#'
#' @return TODO
#' @export
#'
#' @examples
#'
#' set.seed(26)
#'
#' mrdpg <- model_canonical(n = 100, k = 5)
#'
#' graph <- sample_tidygraph(mrdpg)
#' graph
#'
#' m_fit <- nodelm(US(A, 5) ~ . - name - y - 1, graph = graph)
#' o_fit <- nodelm(y ~ . - name - 1 + US(A, 5), graph = graph)
#'
#' m_fit
#' o_fit
#'
model_canonical <- function(n, k, ...) {
  m <- model_mediator_informative(n, k, ...)
  model_outcome_normal(m)
}

#' Title
#'
#' @inheritParams model_mediator_perfect
#' @inheritDotParams model_mediator_perfect
#'
#' @return TODO
#' @export
#'
#' @examples
#'
#' set.seed(26)
#'
#' mrdpg <- model_perfect(n = 100, k = 5)
#'
#' graph <- sample_tidygraph(mrdpg)
#' graph
#'
#' m_fit <- nodelm(US(A, 5) ~ . - name - y - 1, graph = graph)
#' o_fit <- nodelm(y ~ . - name - 1 + US(A, 5), graph = graph)
#'
#' m_fit
#' o_fit
#'
model_perfect <- function(n, k, ...) {
  m <- model_mediator_perfect(n, k, ...)
  model_outcome_normal(m)
}
