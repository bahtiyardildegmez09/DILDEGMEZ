# DILDEGMEZ

DILDEGMEZ paketi, ölçme modellerinin yapı geçerliğini değerlendirmek için  
kullanılan üç temel metriği hesaplar:

- **Average Variance Extracted (AVE)**
- **Composite Reliability (CR)**
- **Heterotrait–Monotrait (HTMT)** oranı

## Kurulum

```r
# CRAN’dan yüklemek için (paket CRAN’da yayımlandıktan sonra)
install.packages("DILDEGMEZ")

# Geliştirme sürümünü GitHub’dan yüklemek için:
# install.packages("remotes")
# remotes::install_github("yourusername/DILDEGMEZ")

# ÖRNEK
library(DILDEGMEZ)

# Örnek veri
set.seed(123)
df <- data.frame(
  A1 = rnorm(200), A2 = rnorm(200), A3 = rnorm(200),
  T1 = rnorm(200), T2 = rnorm(200), T3 = rnorm(200)
)

# Faktör tanımı
model <- list(
  Algi  = c("A1","A2","A3"),
  Tutum = c("T1","T2","T3")
)

# Metrikleri hesapla
result <- metrics(df, model)

# Sonuçlara göz at
print(result$AVE)
print(result$CR)
print(result$HTMT)
