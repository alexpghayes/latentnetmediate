test_that("uninformative model residuals are zero", {

  mu <- model_mediator_uninformative(n = 100, k = 5, dim_c = 3)

  EX <- expected_x_post_trt(mu)
  X <- x_post_trt(mu)

  avg_residuals <- Matrix::colMeans(X - EX)

  expect_equal(
    avg_residuals,
    rep(0, mu$k)
  )

  expect_silent(
    graph <- sample_tidygraph(mu)
  )
})

test_that("block model residuals are zero", {

  mblock <- model_mediator_block(n = 100, k = 5, dim_c = 3)

  EX <- expected_x_post_trt(mblock)
  X <- x_post_trt(mblock)

  avg_residuals <- unname(Matrix::colMeans(X - EX))

  expect_equal(
    avg_residuals,
    rep(0, mblock$k)
  )

  expect_silent(
    graph <- sample_tidygraph(mblock)
  )

})

test_that("informative model residuals are zero", {

  informative <- model_mediator_informative(n = 100, k = 5)

  EX <- expected_x_post_trt(informative)
  X <- x_post_trt(informative)

  avg_residuals <- Matrix::colMeans(X - EX)

  expect_equal(
    avg_residuals,
    rep(0, informative$k)
  )

  expect_silent(
    graph <- sample_tidygraph(informative)
  )
})

test_that("perfect model residuals are zero", {

  ztheta_tc <- rbind(
    c(-1, 1, 0, 0, 0), # treated in block one jump to block two,
    c(0, 0, 0, 0, 0),
    c(0, 1, -1, 0, 0), # treated in block three jump to block two
    c(0, 0, 0, 0, 0),
    c(0, 0, 0, 0, 0)
  )

  perfect <- model_mediator_perfect(n = 100, k = 5, ztheta_tc = ztheta_tc)

  EX <- expected_x_post_trt(perfect)
  X <- x_post_trt(perfect)

  avg_residuals <- Matrix::colMeans(X - EX)

  expect_equal(
    avg_residuals,
    rep(0, perfect$k)
  )

  expect_silent(
    graph <- sample_tidygraph(perfect)
  )
})
