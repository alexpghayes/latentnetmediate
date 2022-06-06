# n <- 200
# k <- 5
#
# T <- rbinom(n = n, size = 1, prob = 0.5)
#
# plogis(100 * T)
#
# T %*% matrix(c(100, -100), nrow = 1) + 100
#
# Z <- matrix(0, nrow = n, ncol = k)
#
# for (j in 1:k) {
#   Z[, j] <- rbinom(n = n, size = 1, prob = 0.1 * j * T) + rexp(n = n, rate = 1/5)
# }
#
# pairs(Z)
#
#
# # overlapping block model
# z1 <- rbinom(n = n, size = 1, prob = 0.1)
# z2 <- rbinom(n = n, size = 1, prob = 0.1)
#
# Z <- cbind(z1, z2)
#
# pairs(
#   cbind(z1 + e1, z2 + e2)
# )
#
# pairs(Z)
#
# theta <- matrix(c(1, -1), nrow = 1)
# theta
#
# mu <- matrix(c(0, 1), nrow = n, ncol = 2, byrow = TRUE)
# mu
#
# rate <- 0.1
# avg_noise <- 1 / rate
#
# e1 <- rexp(n, rate) - avg_noise
# e2 <- rexp(n, rate) - avg_noise
# e <- cbind(e1, e2)
#
# T <- rbinom(n = n, size = 1, prob = 0.5)
#
# T <- rgamma(n, shape = 1, rate = 1)
# Z <- T %*% theta + mu
#
# pairs(Z)
#
# ##### BLERGH
#
# library(rgl)
#
# open3d()
# x <- sort(rnorm(1000))
# y <- rnorm(1000)
# z <- rnorm(1000) + atan2(x, y)
# plot3d(x, y, z, col = rainbow(1000))
#
#
# C <- rbind(
#   c(1, 0, 0),
#   c(0, 0, 1),
#   c(0, 1, 0)
# )
#
# C
# Phi <- matrix(1:9, nrow = 3, byrow = TRUE)
#
# C %*% Phi
#
#
# C <- rbind(
#   c(1, 0, 0),
#   c(2, 0, 0)
# )
#
# Phi <- rbind(-1, 0, 0)
#
# C %*% Phi
