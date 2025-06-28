#' Measurement Model Metrics (AVE, CR, HTMT)
#'
#' Calculate Average Variance Extracted (AVE), Composite Reliability (CR),
#' and Heterotrait–Monotrait ratio (HTMT) using base R.
#'
#' @param data A data frame or matrix of observed variables.
#' @param model A named list: each element name is a factor, value is a character vector of variable names.
#' @return A list with components:
#'   - AVE: numeric vector of AVE values.
#'   - CR: numeric vector of composite reliability values.
#'   - HTMT: square matrix of HTMT ratios.
#' @examples
#' df <- data.frame(
#'   A1 = rnorm(100), A2 = rnorm(100), A3 = rnorm(100),
#'   T1 = rnorm(100), T2 = rnorm(100), T3 = rnorm(100)
#' )
#' mod <- list(Algi = c("A1","A2","A3"), Tutum = c("T1","T2","T3"))
#' metrics(df, mod)
#' @export
metrics <- function(data, model) {
  stopifnot(is.list(model), is.data.frame(data) || is.matrix(data))
  data <- as.data.frame(data)
  lam <- tht <- vector("list", length(model))
  names(lam) <- names(tht) <- names(model)

  # Factor loadings and uniqueness from one-factor ML
  for (f in names(model)) {
    vars <- model[[f]]
    fa <- stats::factanal(data[, vars, drop = FALSE], factors = 1)
    lam[[f]] <- as.numeric(fa$loadings[, 1])
    tht[[f]] <- fa$uniq
  }

  # Compute AVE and CR
  AVE <- vapply(lam, function(x) mean(x^2), numeric(1))
  CR <- vapply(seq_along(lam), function(i) {
    li <- lam[[i]]
    sum(li)^2 / (sum(li)^2 + sum(tht[[i]]))
  }, numeric(1))
  names(CR) <- names(lam)

  # Compute HTMT
  cors <- stats::cor(data, use = "pair")
  K <- length(model)
  HTMT <- matrix(NA_real_, K, K, dimnames = list(names(model), names(model)))
  for (i in seq_len(K - 1)) {
    for (j in (i + 1):K) {
      Xi <- model[[i <- names(model)[i]]]
      Xj <- model[[j <- names(model)[j]]]
      num <- mean(abs(cors[Xi, Xj]))
      den <- sqrt(
        mean(abs(cors[Xi, Xi][lower.tri(cors[Xi, Xi])])) *
          mean(abs(cors[Xj, Xj][lower.tri(cors[Xj, Xj])]))
      )
      HTMT[i, j] <- HTMT[j, i] <- num / den
    }
  }
  diag(HTMT) <- 1

  structure(list(AVE = AVE, CR = CR, HTMT = HTMT), class = "metrics")
}
