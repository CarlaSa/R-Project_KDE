test_that("get_kde", {
  h <- 0.1
  Kernel <- kernels$gaussian
  data <- 0.5
  points <- runif(100, min=-100, max=100)
  my_kde <- get_kde(h, Kernel, data)
  # greater than zero 
  expect_true(all(my_kde(points) >= 0))
  # should integrate to 1
  integral <- integrate(my_kde, lower=-Inf, upper=Inf)$value
  expect_true(abs(1-integral) < 1e-4 )
})
