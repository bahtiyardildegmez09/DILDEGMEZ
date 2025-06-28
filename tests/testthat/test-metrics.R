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

test_that("ML yöntemi için tibble ve HTMT matrisi dönüyor", {
  out <- metrics(df, model, method = "ML")
  expect_s3_class(out, "tbl_df")
  expect_named(out, c("factor", "CR", "AVE", "sqrt_AVE"))
  ht <- attr(out, "HTMT")
  expect_true(is.matrix(ht))
  expect_equal(dim(ht), c(2, 2))
  expect_equal(diag(ht), rep(1, 2))
})

test_that("WLSMV yöntemi için tibble ve HTMT matrisi dönüyor", {
  skip_if_not_installed("lavaan")
  skip_if_not_installed("semTools")
  out <- metrics(df, model, method = "WLSMV")
  expect_s3_class(out, "tbl_df")
  expect_named(out, c("factor", "CR", "AVE", "sqrt_AVE"))
  ht <- attr(out, "HTMT")
  expect_true(is.matrix(ht))
  expect_equal(dim(ht), c(2, 2))
  expect_equal(diag(ht), rep(1, 2))
})

test_that("Geçersiz method argümanı hata fırlatır", {
  expect_error(
    metrics(df, model, method = "YANLIŞ"),
    "must be one of"
  )
})

test_that("3’ten az maddeye sahip faktör hata fırlatır", {
  model2 <- list(A = c("A1","A2"))
  expect_error(
    metrics(df, model2),
    "en az 3 madde"
  )
})

test_that("Farklı boyutlarda veri setlerinde hata vermeden çalışır", {
  mat <- matrix(rnorm(300), ncol = 3)
  df2 <- as.data.frame(cbind(mat, mat + 5))
  names(df2) <- c("X1","X2","X3","Y1","Y2","Y3")
  model2 <- list(X = c("X1","X2","X3"), Y = c("Y1","Y2","Y3"))
  out2 <- metrics(df2, model2)
  expect_s3_class(out2, "tbl_df")
  expect_equal(nrow(out2), 2)
})
