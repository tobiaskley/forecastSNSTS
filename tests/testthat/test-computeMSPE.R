context("computeMSPE")

# In this file we compute the linear forecasts as defined in
# Section 3.4 of the paper naively (with plain R) and compare
# to the C++ implementation that is more efficient.

# We will do it for one t, h, p and N

T <- 25
m <- 7

T <- 250
m <- 50

X <- rnorm(T)

p <- 5
h <- 1

coef <- predCoef(X, p, h, (T-m-h+1):(T-h), c(0,seq(p+1,T-m-h)))$coef

naiveRes <- matrix(0, nrow=p, ncol=dim(coef)[5])
for (j in 1:dim(coef)[5])
for (i in 1:p) {
  for (t in (T-m-h+1):(T-h)) {
    # compute forecast
    Xhat <- sum(coef[i,1:i,h,t-(T-m-h),j] * X[t:(t-i+1)])
    naiveRes[i,j] <- naiveRes[i,j] + ( X[t+h] - Xhat )^2
  }
  naiveRes[i,j] <- naiveRes[i,j] / m
}
 

cppRes <- .computeMSPE(X, coef, 1, (T-m-h+1):(T-h))

test_that("mspe computation works", {
  expect_equal(sum( (cppRes - naiveRes)^2 ), 0)
})
