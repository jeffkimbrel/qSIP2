test_that("calculations work", {
  expect_equal(calculate_EAF(M_labeled = 308.2527,
                             M = 308.0259,
                             M_labeledmax = 317.6637,
                             isotope = "13C"),
               0.02327084,
               tolerance = 8)
})
