library(testthat)
library(DILDEGMEZ)

set.seed(123)
df <- data.frame(
  A1 = rnorm(200), A2 = rnorm(200), A3 = rnorm(200),
  T1 = rnorm(200), T2 = rnorm(200), T3 = rnorm(200)
)
model <- list(
  Algi  = c("A1","A2","A3"),
  Tutum = c("T1","T2","T3")
)
out <- metrics(df, model)

test_that("Output is a 'metrics' object with correct names", {
  expect_s3_class(out, "metrics")
  expect_named(out, c("AVE", "CR", "HTMT"))
})

test_that("AVE and CR are correct length and within [0,1]", {
  expect_length(out$AVE, length(model))
  expect_length(out$CR,  length(model))
  expect_true(all(out$AVE >= 0 & out$AVE <= 1))
  expect_true(all(out$CR  >= 0 & out$CR  <= 1))
})

test_that("HTMT is square and symmetric with diagonal of 1", {
  ht <- out$HTMT
  expect_true(is.matrix(ht))
  expect_equal(dim(ht), c(length(model), length(model)))
  expect_equal(unname(diag(ht)), rep(1, length(model)))
  expect_equal(ht, t(ht))
})

test_that("Function works on different random data without error", {
  mat <- matrix(rnorm(300), ncol = 3)
  df2 <- as.data.frame(cbind(mat, mat + 5))
  names(df2) <- c("X1","X2","X3","Y1","Y2","Y3")
  model2 <- list(X = c("X1","X2","X3"), Y = c("Y1","Y2","Y3"))
  out2 <- metrics(df2, model2)
  expect_s3_class(out2, "metrics")
  expect_length(out2$AVE, 2)
  expect_length(out2$CR,  2)
})
