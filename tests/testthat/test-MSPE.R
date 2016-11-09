context("MSPE")

# In this file we compute the linear forecasts as defined in
# Section 3.4 of the paper naively (with plain R) and compare
# to the C++ implementation that is more efficient.

# We will do it for one t, h, p and N

T <- 25
m1 <- 20
m2 <- 25

X <- rnorm(T)

P <- 5
H <- 2

N <- 10:12

naiveRes <- array(0, dim=c(H, P, length(N)))
for (h in 1:H) {
  for (j.N in 1:length(N)) {
    for (i in 1:P) {
      for (t in (m1-h):(m2-h)) { 
        # compute forecast
        coef <- predCoef(X, P, h, t, N[j.N])$coef[i, 1:i, h, 1, 1]
        Xhat <- sum( coef * X[t:(t-i+1)])
        naiveRes[h, i, j.N] <- naiveRes[h, i, j.N] + ( X[t+h] - Xhat )^2
      }
      naiveRes[h, i, j.N] <- naiveRes[h, i, j.N] / (m2-m1+1)
    }
  }
}

predcoef <- predCoef(X, P, H, (m1-H):(m2-1), N )
cppRes <- MSPE(X, predcoef, m1, m2, P, H, N)

#library(microbenchmark)
#microbenchmark(
#    {cppRes <- MSPE(X, coef, m1, m2, p, h, N)},
#    {cppRes <- MSPE(X, m1=m1, m2=m2, P=p, H=h, N=N)}
#  )

#naiveRes[2, , ]
#cppRes$mspe[2, , ]


test_that("mspe computation works", {
  expect_equal(sum( (cppRes$mspe - naiveRes)^2 ), 0)
})
