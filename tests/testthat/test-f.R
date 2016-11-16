context("f")

# In this file we test the f function

test_that("function f returns valus from paper", {

  ## this just takes too long?
  skip_on_cran()
      
  n <- 100
  a <- list( function(u) {return(0.8+0.19*sin(4*pi*u))} )
  sigma <- function (u) {return(1)}
  
  Ns <- seq( floor((n/2)^(4/5)), floor(n^(4/5)), ceiling((floor(n^(4/5)) - floor((n/2)^(4/5)))/25) )
  which.deltas <- c(0, 0.01, 0.05, 0.1, 0.15, 0.2, 0.4, 0.6)
  P_max <- 7
  H <- 1
  m <- floor(n^(.85)/4)
  
  res1 <- f( which.deltas, P_max, h = 1, n, Ns, m, a, sigma )
  expRes1 <- c(0.000000e+00, 5.349073e-05, 5.069266e-05, 2.326551e-03, 5.347122e-02,
               1.046159e-01, 3.091946e-01, 1.261984e-01)

  res5 <- f( which.deltas, P_max, h = 5, n, Ns, m, a, sigma )
  expRes5 <- c(0.000000e+00, 8.988492e-05, 5.753086e-05, 7.388015e-04, 1.835028e-04,
               3.178430e-04, 1.589932e-02, 3.535490e-01)
  
  expect_equal(res1, expRes1, tolerance = 1e-5)
  expect_equal(res5, expRes5, tolerance = 1e-5)

})
