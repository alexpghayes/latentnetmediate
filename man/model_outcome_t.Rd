% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/model-outcome-t.R
\name{model_outcome_t}
\alias{model_outcome_t}
\title{Mediator-pass through with normal errors}
\usage{
model_outcome_t(mediator, beta_w = NULL, beta_x = NULL, df = 5)
}
\arguments{
\item{mediator}{TODO}

\item{beta_w}{TODO}

\item{beta_x}{TODO}

\item{df}{TODO}
}
\value{
TODO
}
\description{
Mediator-pass through with normal errors
}
\examples{

m <- model_mediator_informative(n = 100, k = 5)
o <- model_outcome_normal(m)

o

coef(o)

graph <- sample_tidygraph(o)
graph

}
