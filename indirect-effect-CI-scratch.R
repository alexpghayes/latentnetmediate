library(geex)
library(tidygraph)
library(tidyverse)

data(smoking)

# symmetric
graph <- smoking |>
  mutate(
    y = as.integer(smokes) - 1
  )

rank <- 7

A <- igraph::as_adjacency_matrix(graph, sparse = TRUE)
X <- netmediate::US(A, rank)

node_data <- tidygraph::as_tibble(graph) |>
  select(y, sex)

o_fit <- estimatr::lm_robust(y ~ . + X, data = node_data)
colnames(X) <- paste0("X", 1:rank)
m_fit <- estimatr::lm_robust(X ~ . - y, data = node_data)

num_coefs <- nrow(coef(m_fit))

nde_table <- broom::tidy(o_fit, conf.int = TRUE)[1:num_coefs, ] |>
  mutate(estimand = "nde") |>
  select(term, estimand, estimate, conf.low, conf.high)

betaw_hat <- stats::coef(o_fit)[1:num_coefs]
betax_hat <- stats::coef(o_fit)[-c(1:num_coefs)]
theta_hat <- stats::coef(m_fit)

betaw_hat
betax_hat
theta_hat

sigmabetax_hat <- vcov(o_fit)[-c(1:num_coefs), -c(1:num_coefs)]
sigmatheta_hat <- vcov(m_fit)

sigmabetax_hat
sigmatheta_hat

length(betax_hat)
dim(sigmatheta_hat)

nie_hat <- drop(theta_hat %*% betax_hat)
nie_hat

effects <- tibble::tibble(
  term = names(betaw_hat),
  nde = betaw_hat,
  nie = nie_hat,
  total = betaw_hat + nie_hat
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


eefun <- function(data, model){
  X <- model.matrix(model, data = data)
  Y <- model.response(model.frame(model, data = data))
  function(theta) {
    lp  <- X %*% theta
    rho <- plogis(lp)

    score_eqns <- apply(X, 2, function(x) sum((Y - rho) * x))
    score_eqns
  }
}



mglm    <- glm(A ~ X1, data = vaccinesim, family = binomial)

estimates <- m_estimate(
  estFUN = eefun,
  data = vaccinesim,
  root_control = setup_root_control(start = c(-.35, 0)),
  outer_args = list(model = mglm)
)

coef(estimates)

## [1] -0.36869683 -0.02037916

coef(mglm) # from the GLM function

## (Intercept)          X1
## -0.36869683 -0.02037916

# Compare variance estimates
vcov(estimates)

##               [,1]          [,2]
## [1,]  0.0028345579 -0.0007476536
## [2,] -0.0007476536  0.0003870030

sandwich::sandwich(mglm)
