test_that("uninformative model residuals are zero", {

  mu <- model_mediator_uninformative(n = 100, k = 5, dim_c = 3)

  EX <- expected_x_post_trt(mu)
  X <- x_post_trt(mu)

  avg_residuals <- Matrix::colMeans(X - EX)

  expect_equal(
    avg_residuals,
    rep(0, mu$k)
  )
})

test_that("block model residuals are zero", {

  mblock <- model_mediator_block(n = 100, k = 5, dim_c = 3)

  EX <- expected_x_post_trt(mblock)
  X <- x_post_trt(mblock)

  avg_residuals <- Matrix::colMeans(X - EX)

  expect_equal(
    avg_residuals,
    rep(0, mblock$k)
  )
})


