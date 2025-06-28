# DILDEGMEZ

**DILDEGMEZ** paketi, ölçme modellerinin yapı geçerliğini değerlendirirken  
kullanılan üç temel metriği hesaplamanızı sağlar:

- **Average Variance Extracted (AVE)**
- **Composite Reliability (CR)**
- **Heterotrait–Monotrait (HTMT)** oranı

İlave olarak √AVE (square‐root of AVE) da döndürülür.

---

## Kurulum

```r
# CRAN’dan (paket yayımlandıktan sonra):
install.packages("DILDEGMEZ")

# Geliştirme sürümünü GitHub’dan yüklemek için:
# install.packages("remotes")
# remotes::install_github("bahtiyardildegmez09/DILDEGMEZ")

Kullanım
r
Kopyala
Düzenle
library(DILDEGMEZ)

# Örnek veri ve model tanımı
set.seed(123)
df <- data.frame(
  A1 = rnorm(100), A2 = rnorm(100), A3 = rnorm(100),
  T1 = rnorm(100), T2 = rnorm(100), T3 = rnorm(100)
)
mod <- list(
  Algi  = c("A1","A2","A3"),
  Tutum = c("T1","T2","T3")
)

# 1) ML yöntemi (sürekli veriler için)
tbl_ml <- metrics(df, mod, method = "ML")
print(tbl_ml)
#> # A tibble: 2 × 4
#>   factor    CR   AVE sqrt_AVE
#>   <chr>  <dbl> <dbl>    <dbl>
#> 1 Algi    0.85 0.65     0.806
#> 2 Tutum   0.91 0.72     0.849

# HTMT matrisini almak için:
ht_ml <- attr(tbl_ml, "HTMT")
print(ht_ml)

# 2) WLSMV yöntemi (ordinal/veri tipi karışık olan durumlar için)
tbl_wls <- metrics(df, mod, method = "WLSMV")
print(tbl_wls)
