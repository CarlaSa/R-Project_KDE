test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})
tolerance = 1e-3

# get different density functions
# test standart values and random values

pdfs_test <- list(
  cauchy_std = get_cauchy(),
  cauchy_rand = get_cauchy(x0 = runif(1, -10, 10), gamma = runif(1, 0.001, 10)),
  unif_std = get_uniform(),
  unif_rand = get_uniform(a = runif(1, -1000, 0) , b = runif(1, 0, 1000)),
  normal_std = get_normal(),
  normal_rand = get_normal(mu =  runif(1, -100, 100), runif(1, 0.001, 100)),
  exp_std = get_exponential(),
  exp_rand  =get_exponential(runif(1, 0.001, 10)),
  lap_std = get_laplace(),
  lap_rand = get_laplace(runif(1, -100, 100), runif(1, 0.001, 10))
)

for (namef in names(pdfs_test)){
  print(namef)
  f <- pdfs_test[[namef]]
  test_that(paste0(namef, " integral of probability distribution should be 1"),
            expect_lt(abs(1- integrate(f, upper = Inf, lower = -Inf)$value), tolerance))
  values = runif(100, -1000, 1000)
  rangef = range(f(values))
  test_that("propability distribution should evaluate to values greater than  0",
          expect_gte(rangef[[1]], 0))
  test_that("propability distribution should evaluate to values smaller than  1",
            expect_lte(rangef[[2]], 1))
  }
