test_that("rejection_sample", {
  n_obs <- 10
  f <- pdfs$Uniform
  data <- rejection_sample(n_obs,f)
  expect_length(data, 10)
  expect_true(all(pdfs$Uniform(data) >= 0))
})
