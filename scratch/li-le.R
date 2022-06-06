#
# n <- 100
#
# k1 <- 5
# k2 <- 8
#
# r <- 2
#
# basis_dim <- k1 + k2 - r
#
# basis <- matrix(
#   rnorm(n * basis_dim),
#   nrow = n,
#   ncol = basis_dim
# )
#
# basis
#
#
# beta1 <- runif(k1)
# beta2 <- runif(k2)
#
# X <- basis[, 1:k1] %*% diag(beta1)
# U <- basis[, (k1 - r + 1):basis_dim] %*% diag(beta2)
#
# X
# U
#
# qr(X)$rank
# qr(U)$rank
#
# XU <- cbind(X, U)
# qr(XU)$rank
#
# dim(XU)
#
# y <- rnorm(n)
#
# fit <- lm(y ~ X + U)
# summary(fit)
#
# alias(y ~ XU + 0)
#
#
# alias(fit)
#
#
#
#
# XX <- basis[, 1:k1] %*% diag(beta1) + rnorm(n)
# UU <- basis[, (k1 - r + 1):basis_dim] %*% diag(beta2) + rnorm(n)
#
# alias(y ~ XX + UU, partial = TRUE)
#
