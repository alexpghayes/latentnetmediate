
#' Sample from a mediated RDPG object
#'
#' @param model A `mediated_rdpg` object
#' @param ... Ignored.
#'
#' @return A [tidygraph::tbl_graph()] object.
#'
#' @export
sample_tidygraph <- function(model, ...) {
  UseMethod("sample_tidygraph")
}

#' @method sample_tidygraph mediator
#' @export
sample_tidygraph.mediator <- function(model, ...) {

  C <- as.matrix(model$C)
  colnames(C) <- paste0("C", 1:ncol(model$C))
  C_df <- as_tibble(C)

  graph <- fastRG::sample_tidygraph(
    model$A_model,
    allow_self_loops = FALSE,
    poisson_edges = FALSE
  ) |>
    dplyr::arrange(as.numeric(name)) |>
    dplyr::mutate(trt = model$trt, !!!C_df, y = model$y)

  graph
}
