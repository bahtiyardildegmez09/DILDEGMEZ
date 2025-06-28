#’ Ölçme Modeli Metrikleri (AVE, CR, HTMT) — ML ve WLSMV
#’
#’ Sürekli veriler için ML (stats::factanal) veya ordinal veriler için
#’ WLSMV (lavaan + semTools) ile AVE, CR, √AVE ve HTMT hesaplar.
#’
#’ @param data Veri çerçevesi veya matris; sütunlar gözlenen değişkenler.
#’ @param model İsimli liste: her öğe bir faktör adı, değeri o faktörün madde isimleri.
#’ @param method Karakter; “ML” (varsayılan) veya “WLSMV”.
#’ @return Tibble (faktör başına bir satır) ve öznitelik `"HTMT"`’de HTMT matrisi.
#’ @export
metrics <- function(data, model, method = c("ML","WLSMV")) {
  method <- match.arg(method)
  data   <- as.data.frame(data)
  facs   <- names(model)
  K      <- length(facs)

  check_pkg <- function(pkg) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(sprintf("'%s' paketi yüklü olmalı for method = \"%s\".", pkg, method), call. = FALSE)
    }
  }

  out_list <- vector("list", K)
  names(out_list) <- facs

  for (i in seq_along(facs)) {
    vars <- model[[i]]
    if (length(vars) < 3) {
      stop(sprintf("Faktör '%s' için en az 3 madde gerekli, şu an %d var.",
                   facs[i], length(vars)), call. = FALSE)
    }

    if (method == "ML") {
      fa  <- stats::factanal(data[, vars, drop = FALSE], factors = 1)
      lam <- as.numeric(fa$loadings[,1])
      tht <- fa$uniq
    } else {
      check_pkg("lavaan"); check_pkg("semTools")
      form <- paste0(facs[i], " =~ ", paste(vars, collapse = " + "))
      fit  <- lavaan::cfa(
        model   = form, data      = data,
        estimator = "WLSMV", std.lv = TRUE,
         missing   = "pairwise"
      )
      ss   <- lavaan::standardizedSolution(fit)
      lam  <- ss$est.std[ ss$op == "=~" & ss$lhs == facs[i] ]
      tht  <- 1 - lam^2
    }

    CR  <- sum(lam)^2 / (sum(lam)^2 + sum(tht))
    AVE <- mean(lam^2)
    out_list[[i]] <- tibble::tibble(
      factor   = facs[i],
      CR       = CR,
      AVE      = AVE,
      sqrt_AVE = sqrt(AVE)
    )
  }

  out_tbl <- dplyr::bind_rows(out_list)

  cors <- stats::cor(data, use = "pair")
  HTMT <- matrix(NA_real_, nrow = K, ncol = K, dimnames = list(facs, facs))
  for (i in seq_len(K-1)) {
    for (j in (i+1):K) {
      Xi <- model[[i]]; Xj <- model[[j]]
      num <- mean(abs(cors[Xi, Xj]), na.rm = TRUE)
      den_i <- mean(abs(cors[Xi, Xi])[lower.tri(cors[Xi, Xi])], na.rm = TRUE)
      den_j <- mean(abs(cors[Xj, Xj])[lower.tri(cors[Xj, Xj])], na.rm = TRUE)
      val <- num / sqrt(den_i * den_j)
      HTMT[i, j] <- val; HTMT[j, i] <- val
    }
  }
  diag(HTMT) <- 1L
  attr(out_tbl, "HTMT") <- HTMT

  out_tbl
}
