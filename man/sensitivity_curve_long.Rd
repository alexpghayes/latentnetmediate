% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mediation.R
\name{sensitivity_curve_long}
\alias{sensitivity_curve_long}
\title{Estimate mediated effects for a variety of embedding dimensions}
\usage{
sensitivity_curve_long(
  graph,
  formula,
  max_rank,
  ...,
  ranks_to_consider = 10,
  coembedding = c("U", "V"),
  node_data = NULL
)
}
\arguments{
\item{graph}{A \code{\link[tidygraph:tbl_graph]{tidygraph::tbl_graph()}} object.}

\item{formula}{Details about the nodal design matrix. Of the form
outcome ~ nodal_formula. For now, no interactions or fancy stuff
are allowed in the formula.}

\item{max_rank}{Maximum rank to consider (integer).}

\item{...}{
  Arguments passed on to \code{\link[estimatr:lm_robust]{estimatr::lm_robust}}
  \describe{
    \item{\code{data}}{A \code{data.frame}}
    \item{\code{weights}}{the bare (unquoted) names of the weights variable in the
supplied data.}
    \item{\code{subset}}{An optional bare (unquoted) expression specifying a subset
of observations to be used.}
    \item{\code{clusters}}{An optional bare (unquoted) name of the variable that
corresponds to the clusters in the data.}
    \item{\code{fixed_effects}}{An optional right-sided formula containing the fixed
effects that will be projected out of the data, such as \code{~ blockID}. Do not
pass multiple-fixed effects with intersecting groups. Speed gains are greatest for
variables with large numbers of groups and when using "HC1" or "stata" standard errors.
See 'Details'.}
    \item{\code{se_type}}{The sort of standard error sought. If \code{clusters} is
not specified the options are "HC0", "HC1" (or "stata", the equivalent),
 "HC2" (default), "HC3", or
"classical". If \code{clusters} is specified the options are "CR0", "CR2" (default), or "stata". Can also specify "none", which may speed up estimation of the coefficients.}
    \item{\code{ci}}{logical. Whether to compute and return p-values and confidence
intervals, TRUE by default.}
    \item{\code{alpha}}{The significance level, 0.05 by default.}
    \item{\code{return_vcov}}{logical. Whether to return the variance-covariance
matrix for later usage, TRUE by default.}
    \item{\code{try_cholesky}}{logical. Whether to try using a Cholesky
decomposition to solve least squares instead of a QR decomposition,
FALSE by default. Using a Cholesky decomposition may result in speed gains, but should only
be used if users are sure their model is full-rank (i.e., there is no
perfect multi-collinearity)}
  }}

\item{ranks_to_consider}{How many distinct ranks to consider (integer).
Optional, defaults to 10.}

\item{coembedding}{TODO}

\item{node_data}{TODO}
}
\value{
A \code{rank_sensitivity_curve} object, which is a subclass of a
\code{\link[tibble:tibble]{tibble::tibble()}}.
}
\description{
Estimate mediated effects for a variety of embedding dimensions
}
\examples{

library(tidygraph)
library(dplyr)

data(smoking, package = "latentnetmediate")

# example with fully observed node data

rank_curve <- smoking |>
  mutate(
    smokes_int = as.integer(smokes) - 1
  ) |>
 sensitivity_curve(
   smokes_int ~ sex,
   max_rank = 25,
   ranks_to_consider = 24,
   se_type = "stata"
 )

rank_curve
plot(rank_curve)

# example with some missing node data. in this case, all edges are
# used to estimate embeddings, but once the embeddings are in hand,
# the regression only considers complete cases

data(glasgow, package = "latentnetmediate")

glasgow1 <- glasgow[[1]] |>
  activate(nodes) |>
  filter(selection129) |>
  mutate(
    smokes_dimaria = as.numeric(tobacco_int > 1)
  ) |>
  activate(edges) |>
  filter(friendship != "Structurally missing") |>
  activate(nodes)

# verify that there is some missing data, in this case treatment indicators
glasgow1 |>
  as_tibble() |>
  count(is.na(money))

glasgow1 |>
  sensitivity_curve(smokes_dimaria ~ money, max_rank = 15, coembedding = "V") |>
  plot()

}
