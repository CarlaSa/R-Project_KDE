test_that("bandwidth selection: expected errors", {
  data <- rejection_sample(500, kernels$gaussian)
  kernel <- kernels$gaussian
  expect_error(bandwidth_selection("invalid", kernel, data))
})

test_that("bandwidth selection: PCO", {
  data <- rejection_sample(500, kernels$gaussian)
  kernel <- kernels$gaussian
  h <- bandwidth_selection("PCO", kernel, data, maxEval = 100, set_up_cluster = FALSE)
  expect_true(is.numeric(h))
  expect_gte(h, 0)
  expect_length(h, 1)
})

test_that("bandwidth selection: CV", {
  #data <- rejection_sample(500, pdfs$Mix2Gauss)
  data <- rnorm(100)
  kernel <- kernels$gaussian
  h <- bandwidth_selection("CV", kernel, data, maxEval = 100, set_up_cluster = FALSE)
  expect_true(is.numeric(h))
  expect_gte(h, 0)
  expect_length(h, 1)
})

test_that("bandwidth selection: GL", {
  data <- rejection_sample(100, kernels$gaussian)
  kernel <- kernels$gaussian
  h <- bandwidth_selection("GL", kernel, data, maxEval = 10, lower=0.1, set_up_cluster = FALSE)
  expect_true(is.numeric(h))
  expect_gte(h, 0)
  expect_length(h, 1)
  expect_warning(bandwidth_selection("GL", kernel, data, maxEval = 10, set_up_cluster =  FALSE))
})
