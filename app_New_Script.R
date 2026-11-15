# Memanggil package
library(readxl)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(DT)
library(shiny)
library(bslib)
library(ggplot2)
library(knitr)
library(corrplot)
library(GGally)
library(broom)
library(car)
library(lmtest)

Life_Expectancy_Data <- read_excel("Life Expectancy Data.xlsx")

# Menetapkan data awal
data_awal <- Life_Expectancy_Data
# Melihat struktur data
str(Life_Expectancy_Data)
# Melihat dimensi data
dim(Life_Expectancy_Data)
# Melihat nama variabel
names(Life_Expectancy_Data)
# Melihat beberapa baris pertama
head(Life_Expectancy_Data)

# Melihat ringkasan seluruh data
summary(Life_Expectancy_Data)
# Memilih variabel penelitian
data_eda <- Life_Expectancy_Data %>%
  select(
    Country,
    Status,
    `Adult Mortality`,
    `HIV/AIDS`,
    `BMI`,
    GDP,
    Schooling
  )

# Melihat data terpilih
#View(data_eda)
# Memeriksa struktur data terpilih
str(data_eda)
# Fungsi konversi angka
konversi_angka <- function(x) {
  x <- str_trim(as.character(x))
  hasil <- ifelse(
    str_detect(x, "^\\d{1,2}:\\d{2}(:\\d{2})?$"),
    {
      bagian <- str_split_fixed(x, ":", 3)
      as.numeric(bagian[,1]) +
        as.numeric(bagian[,2]) / 100 +
        ifelse(
          bagian[,3] == "",
          0,
          as.numeric(bagian[,3]) / 10000
        )
    },
    parse_number(x)
  )
  as.numeric(hasil)
}
# Memilih variabel penelitian dan mengubah tipe data
data_eda <- data_awal %>%
  select(
    Country,
    Status,
    `Adult Mortality`,
    `HIV/AIDS`,
    BMI,
    GDP,
    Schooling
  ) %>%
  mutate(
    `Adult Mortality` = konversi_angka(`Adult Mortality`),
    `HIV/AIDS` = konversi_angka(`HIV/AIDS`),
    BMI = konversi_angka(BMI),
    GDP = konversi_angka(GDP),
    Schooling = konversi_angka(Schooling),
    Country = as.factor(Country),
    Status = as.factor(Status)
  )
# Memeriksa hasil
str(data_eda)
#View(data_eda)
# Menghitung missing value
missing_data <- data_eda %>%
  summarise(
    across(everything(), ~ sum(is.na(.)))
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Variabel",
    values_to = "Jumlah Missing"
  ) %>%
  mutate(
    Persentase = round(
      `Jumlah Missing` / nrow(data_eda) * 100, 2
    )
  )

# Fungsi menghitung modus
hitung_modus <- function(x) {
  
  x <- x[!is.na(x)]
  
  if (length(x) == 0) return(NA_real_)
  
  frekuensi <- table(x)
  
  modus <- names(frekuensi)[
    frekuensi == max(frekuensi)
  ]
  
  as.numeric(modus[1])
}


# Statistik deskriptif numerik
deskriptif_numerik <- data_eda %>%
  
  summarise(
    across(
      c(
        `Adult Mortality`,
        `HIV/AIDS`,
        BMI,
        GDP,
        Schooling
      ),
      
      list(
        N = ~ sum(!is.na(.)),
        Mean = ~ mean(., na.rm = TRUE),
        Median = ~ median(., na.rm = TRUE),
        Modus = ~ hitung_modus(.),
        Minimum = ~ min(., na.rm = TRUE),
        Maksimum = ~ max(., na.rm = TRUE),
        Range = ~ diff(range(., na.rm = TRUE)),
        Q1 = ~ quantile(., 0.25, na.rm = TRUE),
        Q3 = ~ quantile(., 0.75, na.rm = TRUE),
        Varians = ~ var(., na.rm = TRUE),
        SD = ~ sd(., na.rm = TRUE),
        CV = ~ sd(., na.rm = TRUE) /
          mean(., na.rm = TRUE) * 100
      )
    )
  ) %>%
  
  pivot_longer(
    cols = everything(),
    names_to = c("Variabel", ".value"),
    names_pattern =
      "^(.*)_(N|Mean|Median|Modus|Minimum|Maksimum|Range|Q1|Q3|Varians|SD|CV)$"
  ) %>%
  
  mutate(
    across(
      -Variabel,
      ~ round(., 3)
    )
  )


# Menampilkan tabel di R Markdown
knitr::kable(
  deskriptif_numerik,
  caption = "Tabel Statistik Deskriptif Variabel Numerik",
  align = "c"
)
## deskriptif kategorik
deskriptif_status <- data_eda %>%
  
  count(Status, name = "Frekuensi") %>%
  
  mutate(
    Persentase = round(
      Frekuensi / sum(Frekuensi) * 100,
      2
    )
  )

knitr::kable(
  deskriptif_status,
  caption = "Tabel Distribusi Frekuensi Status",
  align = "c"
)
deskriptif_country <- data_eda %>%
  
  count(Country, name = "Frekuensi") %>%
  
  mutate(
    Persentase = round(
      Frekuensi / sum(Frekuensi) * 100,
      2
    )
  ) %>%
  
  arrange(desc(Frekuensi))

knitr::kable(
  deskriptif_country,
  caption = "Tabel Distribusi Frekuensi Country",
  align = "c"
)
## UNIVARIAT VARIABEL NUMERIK

# Mengubah data numerik ke format long
data_numerik_long <- data_eda %>%
  select(
    `Adult Mortality`,
    `HIV/AIDS`,
    BMI,
    GDP,
    Schooling
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = "Variabel",
    values_to = "Nilai"
  )

dp_histogram <- ggplot(
  data_numerik_long,
  aes(x = Nilai)
) +
  geom_histogram(
    bins = 30,
    fill = "skyblue",
    color = "black",
    na.rm = TRUE
  ) +
  facet_wrap(~ Variabel, scales = "free", ncol = 2) +
  labs(
    title = "Histogram Distribusi Variabel Numerik",
    x = "Nilai",
    y = "Frekuensi"
  ) +
  theme_minimal()
dir.create("output/grafik", recursive = TRUE, showWarnings = FALSE)
ggsave(
  "output/grafik/01_histogram.png",
  dp_histogram,
  width = 10,
  height = 7
)
## BOXPLOT
ggplot(data_numerik_long, aes(x = "", y = Nilai)) +
  geom_boxplot(
    fill = "lightgreen",
    color = "black",
    na.rm = TRUE
  ) +
  facet_wrap(~ Variabel, scales = "free", ncol = 2) +
  labs(
    title = "Boxplot Variabel Numerik",
    x = "Variabel",
    y = "Nilai"
  ) +
  theme_minimal()
## UNIVARIAT VARIABEL KATEGORIK
ggplot(data_eda, aes(x = Status, fill = Status)) +
  geom_bar(color = "black") +
  labs(
    title = "Distribusi Frekuensi Status",
    x = "Status",
    y = "Frekuensi",
    fill = "Status"
  ) +
  theme_minimal()
data_country_top5 <- data_eda %>%
  count(Country, name = "Frekuensi") %>%
  slice_max(order_by = Frekuensi, n = 5)

ggplot(
  data_country_top5,
  aes(x = reorder(Country, Frekuensi), y = Frekuensi)
) +
  geom_col(fill = "skyblue", color = "black") +
  coord_flip() +
  labs(
    title = "5 Negara dengan Frekuensi Observasi Tertinggi",
    x = "Negara",
    y = "Frekuensi"
  ) +
  theme_minimal()
# BIVARIAT
## Korelasi Pearson
variabel_numerik <- c(
  "Adult Mortality",
  "HIV/AIDS",
  "BMI",
  "GDP",
  "Schooling"
)

pasangan_variabel <- combn(
  variabel_numerik,
  2,
  simplify = FALSE
)

hasil_korelasi <- map_dfr(pasangan_variabel, function(p) {
  
  data_pasangan <- data_eda %>%
    select(all_of(p)) %>%
    drop_na()
  
  uji <- cor.test(
    data_pasangan[[1]],
    data_pasangan[[2]],
    method = "pearson"
  )
  
  data.frame(
    Variabel_1 = p[1],
    Variabel_2 = p[2],
    N = nrow(data_pasangan),
    Korelasi_Pearson = unname(uji$estimate),
    P_Value = uji$p.value
  )
})

hasil_korelasi <- hasil_korelasi %>%
  mutate(
    Korelasi_Pearson = round(Korelasi_Pearson, 3),
    P_Value = round(P_Value, 4)
  )

knitr::kable(
  hasil_korelasi,
  caption = "Hasil Uji Korelasi Pearson",
  align = "c"
)
## Boxplot berdasarkan Status
data_boxplot <- data_eda %>%
  select(
    Status,
    `Adult Mortality`,
    `HIV/AIDS`,
    BMI,
    GDP,
    Schooling
  ) %>%
  pivot_longer(
    cols = -Status,
    names_to = "Variabel",
    values_to = "Nilai"
  )

ggplot(
  data_boxplot,
  aes(x = Status, y = Nilai, fill = Status)
) +
  geom_boxplot(na.rm = TRUE) +
  facet_wrap(~ Variabel, scales = "free", ncol = 2) +
  labs(
    title = "Boxplot Variabel Numerik Berdasarkan Status",
    x = "Status Negara",
    y = "Nilai",
    fill = "Status"
  ) +
  theme_minimal()
## Scatter Plot dengan Garis Linear
ggplot(
  data_eda,
  aes(x = Schooling, y = `Adult Mortality`)
) +
  geom_point(
    color = "steelblue",
    size = 2,
    alpha = 0.7,
    na.rm = TRUE
  ) +
  geom_smooth(
    method = "lm",
    se = TRUE,
    color = "red",
    na.rm = TRUE
  ) +
  labs(
    title = "Hubungan Schooling dengan Adult Mortality",
    x = "Schooling",
    y = "Adult Mortality"
  ) +
  theme_minimal()
## Correlation Plot

matriks_korelasi <- data_eda %>%
  select(all_of(variabel_numerik)) %>%
  cor(
    use = "pairwise.complete.obs",
    method = "pearson"
  )

corrplot(
  matriks_korelasi,
  method = "color",
  type = "upper",
  addCoef.col = "black",
  tl.col = "black",
  tl.srt = 45,
  number.cex = 0.7,
  title = "Correlation Plot Antarvariabel Numerik",
  mar = c(0, 0, 2, 0)
)
## MULTIVARIAT

data_multivariat <- data_eda %>%
  select(
    `Adult Mortality`,
    `HIV/AIDS`,
    BMI,
    GDP,
    Schooling
  ) %>%
  drop_na()

# Melihat jumlah observasi
nrow(data_multivariat)
vektor_rata_rata <- colMeans(data_multivariat)
vektor_rata_rata
matriks_kovariansi <- cov(data_multivariat)
matriks_kovariansi
## Scatter Plot Multivariat menggunakan GGally::ggpairs
GGally::ggpairs(
  data_multivariat,
  upper = list(
    continuous = GGally::wrap(
      "cor",
      colour = "darkgreen",
      size = 3
    )
  ),
  lower = list(
    continuous = GGally::wrap(
      "points",
      colour = "red",
      alpha = 0.4,
      size = 0.8
    )
  ),
  diag = list(
    continuous = GGally::wrap(
      "densityDiag",
      fill = "darkgreen",
      colour = "red",
      alpha = 0.5
    )
  )
) +
  ggtitle("Scatter Plot Multivariat Antarvariabel Numerik")
# Memeriksa variansi setiap variabel
apply(data_multivariat, 2, var)

data_standar <- as.data.frame(scale(data_multivariat))

# Melihat variansi setelah standardisasi
apply(data_standar, 2, var)
matriks_korelasi <- cor(data_standar)
round(matriks_korelasi, 4)
matriks_kovariansi <- cov(data_standar)
qr(matriks_kovariansi)$rank
ncol(data_standar)
det(matriks_kovariansi)
# Menghitung matriks kovariansi data standar
matriks_kovariansi_standar <- cov(data_standar)

# Menghitung jarak Mahalanobis
jarak_mahalanobis <- mahalanobis(
  data_standar,
  center = colMeans(data_standar),
  cov = matriks_kovariansi_standar
)
# Menampilkan hasil
head(jarak_mahalanobis)
# Derajat bebas
df <- ncol(data_standar)

# Batas Chi-Square dengan alpha 0,025
batas_mahalanobis <- qchisq(
  0.975,
  df = df
)
batas_mahalanobis
hasil_outlier <- data.frame(
  Observasi = 1:nrow(data_standar),
  Jarak_Mahalanobis = jarak_mahalanobis,
  Status = ifelse(
    jarak_mahalanobis > batas_mahalanobis,
    "Outlier",
    "Tidak Outlier"
  )
)

# Menampilkan data outlier
hasil_outlier %>%
  filter(Status == "Outlier")
ggplot(
  hasil_outlier,
  aes(
    x = Observasi,
    y = Jarak_Mahalanobis,
    color = Status
  )
) +
  geom_point(size = 2) +
  geom_hline(
    yintercept = batas_mahalanobis,
    color = "darkgreen",
    linetype = "dashed",
    linewidth = 1
  ) +
  scale_color_manual(
    values = c(
      "Outlier" = "red",
      "Tidak Outlier" = "darkgreen"
    )
  ) +
  labs(
    title = "Deteksi Outlier Multivariat dengan Jarak Mahalanobis",
    x = "Observasi",
    y = "Jarak Mahalanobis Kuadrat",
    color = "Status"
  ) +
  theme_minimal()
## Evaluasi Normalitas Multivariat dengan Chi-Square Q-Q Plot
# Mengurutkan jarak Mahalanobis
jarak_urut <- sort(jarak_mahalanobis)

# Menentukan jumlah observasi dan variabel
n <- length(jarak_urut)
p <- ncol(data_standar)

# Menghitung kuantil Chi-Square
kuantil_chisquare <- qchisq(
  ppoints(n),
  df = p
)

# Membuat data Q-Q Plot
data_qq <- data.frame(
  Chi_Square = kuantil_chisquare,
  Mahalanobis = jarak_urut
)
# Membuat Chi-Square Q-Q Plot
ggplot(
  data_qq,
  aes(x = Chi_Square, y = Mahalanobis)
) +
  geom_point(
    color = "red",
    alpha = 0.6
  ) +
  geom_abline(
    intercept = 0,
    slope = 1,
    color = "darkgreen",
    linewidth = 1
  ) +
  labs(
    title = "Chi-Square Q-Q Plot Normalitas Multivariat",
    x = "Kuantil Teoretis Chi-Square",
    y = "Jarak Mahalanobis Kuadrat"
  ) +
  theme_minimal()
## Konfirmasi Normalitas Variat dengan Shapiro-Wilk
hasil_shapiro <- lapply(
  names(data_multivariat),
  function(variabel) {
    
    uji <- shapiro.test(
      data_multivariat[[variabel]]
    )
    
    data.frame(
      Variabel = variabel,
      Statistik_W = unname(uji$statistic),
      P_Value = uji$p.value,
      Keputusan = ifelse(
        uji$p.value > 0.05,
        "Tidak menolak H0",
        "Menolak H0"
      )
    )
  }
) %>%
  bind_rows()

# Menampilkan hasil pengujian
hasil_shapiro %>%
  mutate(
    Statistik_W = round(Statistik_W, 4),
    P_Value = signif(P_Value, 4)
  ) %>%
  knitr::kable(
    caption = "Hasil Uji Normalitas Shapiro-Wilk",
    align = "c"
  )
## REGRESI LINEAR BERGANDA

data_regresi <- data_awal %>%
  select(
    `Life expectancy`,
    `Adult Mortality`,
    `HIV/AIDS`,
    BMI,
    GDP,
    Schooling
  ) %>%
  mutate(
    across(
      everything(),
      konversi_angka
    )
  ) %>%
  drop_na()

# Melihat struktur data
str(data_regresi)

# Melihat jumlah observasi
nrow(data_regresi)
## Identifikasi pola hubungan X dan Y
variabel_x <- c(
  "Adult Mortality",
  "HIV/AIDS",
  "BMI",
  "GDP",
  "Schooling"
)


data_scatter <- data_regresi %>%
  pivot_longer(
    cols = all_of(variabel_x),
    names_to = "Variabel_X",
    values_to = "Nilai_X"
  )

ggplot(
  data_scatter,
  aes(x = Nilai_X, y = `Life expectancy`)
) +
  geom_point(
    color = "#800000",
    alpha = 0.4,
    size = 1.5
  ) +
  geom_smooth(
    method = "lm",
    se = TRUE,
    color = "#000080"
  ) +
  facet_wrap(
    ~ Variabel_X,
    scales = "free_x",
    ncol = 2
  ) +
  labs(
    title = "Pola Hubungan Variabel X dengan Life Expectancy",
    x = "Variabel Bebas (X)",
    y = "Life Expectancy (Y)"
  ) +
  theme_minimal()
## Estimasi Regresi Linear Berganda
model_regresi <- lm(
  `Life expectancy` ~
    `Adult Mortality` +
    `HIV/AIDS` +
    BMI +
    GDP +
    Schooling,
  data = data_regresi
)

# Menampilkan hasil estimasi
summary(model_regresi)
hasil_estimasi <- broom::tidy(
  model_regresi,
  conf.int = TRUE
)

hasil_estimasi %>%
  mutate(
    across(
      where(is.numeric),
      ~ round(., 4)
    )
  ) %>%
  knitr::kable(
    caption = "Hasil Estimasi Regresi Linear Berganda",
    align = "c"
  )
hasil_model <- broom::glance(model_regresi)

hasil_model %>%
  select(
    r.squared,
    adj.r.squared,
    sigma,
    statistic,
    p.value
  ) %>%
  mutate(
    across(
      everything(),
      ~ round(., 4)
    )
  ) %>%
  knitr::kable(
    caption = "Ringkasan Model Regresi Linear Berganda",
    align = "c"
  )
## PENGUJIAN ASUMSI KLASIK
# UJI LINEARITAS
data_diagnostik <- data.frame(
  Fitted = fitted(model_regresi),
  Residual = residuals(model_regresi)
)

ggplot(
  data_diagnostik,
  aes(x = Fitted, y = Residual)
) +
  geom_point(
    color = "#800000",
    alpha = 0.5
  ) +
  geom_smooth(
    method = "loess",
    se = FALSE,
    color = "#000080"
  ) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "black"
  ) +
  labs(
    title = "Plot Pemeriksaan Linearitas",
    x = "Nilai Prediksi",
    y = "Residual"
  ) +
  theme_minimal()
# UJI NORMALITAS RESIDUAL
uji_shapiro_regresi <- shapiro.test(
  residuals(model_regresi)
)

uji_shapiro_regresi

# Q Q PLOT RESIDUAL
ggplot(
  data_diagnostik,
  aes(sample = Residual)
) +
  stat_qq(
    color = "#800000",
    alpha = 0.6
  ) +
  stat_qq_line(
    color = "#000080",
    linewidth = 1
  ) +
  labs(
    title = "Normal Q-Q Plot Residual",
    x = "Kuantil Teoretis",
    y = "Kuantil Sampel"
  ) +
  theme_minimal()
## Uji multikolinearitas
hasil_vif <- car::vif(model_regresi)

hasil_vif
## VISUALISASI VIF
data_vif <- data.frame(
  Variabel = names(hasil_vif),
  VIF = as.numeric(hasil_vif)
)

ggplot(
  data_vif,
  aes(x = reorder(Variabel, VIF), y = VIF)
) +
  geom_col(
    fill = "#000080",
    color = "#800000"
  ) +
  geom_hline(
    yintercept = 10,
    color = "#800000",
    linetype = "dashed"
  ) +
  coord_flip() +
  labs(
    title = "Plot Nilai Variance Inflation Factor",
    x = "Variabel Bebas",
    y = "Nilai VIF"
  ) +
  theme_minimal()
## UJI HETEROS KEDASTISITAS
uji_bp <- lmtest::bptest(model_regresi)

uji_bp
# VISUALISASI SCALE LOCATION
data_diagnostik$Scale_Location <- sqrt(
  abs(rstandard(model_regresi))
)

ggplot(
  data_diagnostik,
  aes(x = Fitted, y = Scale_Location)
) +
  geom_point(
    color = "#800000",
    alpha = 0.5
  ) +
  geom_smooth(
    method = "loess",
    se = FALSE,
    color = "#000080"
  ) +
  labs(
    title = "Scale-Location Plot",
    x = "Nilai Prediksi",
    y = "Akar Nilai Absolut Residual Terstandarisasi"
  ) +
  theme_minimal()
# Uji Durbin-Watson
uji_dw <- lmtest::dwtest(model_regresi)

uji_dw

## RINGKASAN ASUMSI KLASIK
hasil_asumsi <- data.frame(
  Pengujian = c(
    "Normalitas Residual",
    "Multikolinearitas",
    "Heteroskedastisitas",
    "Autokorelasi (Durbin-Watson)"
  ),
  Statistik = c(
    unname(uji_shapiro_regresi$statistic),
    NA,
    unname(uji_bp$statistic),
    unname(uji_dw$statistic)
  ),
  P_Value = c(
    uji_shapiro_regresi$p.value,
    NA,
    uji_bp$p.value,
    uji_dw$p.value
  ),
  Keputusan = c(
    ifelse(uji_shapiro_regresi$p.value > 0.05,
           "Tidak menolak H0", "Menolak H0"),
    "Lihat nilai VIF",
    ifelse(uji_bp$p.value > 0.05,
           "Tidak menolak H0", "Menolak H0"),
    ifelse(uji_dw$p.value > 0.05,
           "Tidak menolak H0", "Menolak H0")
  )
)

knitr::kable(
  hasil_asumsi,
  caption = "Ringkasan Pengujian Asumsi Klasik",
  digits = 4,
  align = "c"
)
## FOREST PLOT KOEFISIEN REGRESI
data_forest <- broom::tidy(
  model_regresi,
  conf.int = TRUE,
  conf.level = 0.95
) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    term = factor(
      term,
      levels = term[order(estimate)]
    )
  )

ggplot(
  data_forest,
  aes(x = estimate, y = term)
) +
  geom_vline(
    xintercept = 0,
    linetype = "dashed",
    color = "#800000"
  ) +
  geom_errorbar(
    aes(
      xmin = conf.low,
      xmax = conf.high
    ),
    width = 0.2,
    color = "#000080",
    linewidth = 0.8
  ) +
  geom_point(
    color = "#800000",
    size = 3
  ) +
  labs(
    title = "Forest Plot Koefisien Regresi Linear Berganda",
    x = "Estimasi Koefisien dan Interval Kepercayaan 95%",
    y = "Variabel Bebas"
  ) +
  theme_minimal()
# Mengatur tampilan menjadi 2 baris dan 2 kolom
par(mfrow = c(2, 2))

# Menampilkan empat plot diagnostik regresi
plot(model_regresi, which = 1:4)

# Mengembalikan tampilan seperti semula
par(mfrow = c(1, 1))


# =========================================================
# PERSIAPAN 1: OBJEK DASHBOARD
# LIFE EXPECTANCY DATA
# =========================================================

# ---------------------------------------------------------
# 1. PERSIAPAN DATA
# ---------------------------------------------------------

data_awal <- Life_Expectancy_Data

data_country_top5 <- data_eda %>%
  count(Country, name = "Frekuensi") %>%
  slice_max(
    order_by = Frekuensi,
    n = 5,
    with_ties = FALSE
  )

# Data kategorik Status
data_status_dashboard <- data_eda %>%
  count(Status, name = "Frekuensi") %>%
  mutate(
    Persentase = Frekuensi / sum(Frekuensi) * 100
  )

# ---------------------------------------------------------
# 2. PERSIAPAN HASIL ASUMSI REGRESI
# ---------------------------------------------------------

hasil_asumsi <- data.frame(
  Pengujian = c(
    "Normalitas Residual",
    "Multikolinearitas",
    "Heteroskedastisitas",
    "Autokorelasi"
  ),
  
  Statistik = c(
    unname(uji_shapiro_regresi$statistic),
    NA,
    unname(uji_bp$statistic),
    unname(uji_dw$statistic)
  ),
  
  P_Value = c(
    uji_shapiro_regresi$p.value,
    NA,
    uji_bp$p.value,
    uji_dw$p.value
  ),
  
  Keputusan = c(
    ifelse(
      uji_shapiro_regresi$p.value > 0.05,
      "Tidak menolak H0",
      "Menolak H0"
    ),
    
    "Lihat nilai VIF",
    
    ifelse(
      uji_bp$p.value > 0.05,
      "Tidak menolak H0",
      "Menolak H0"
    ),
    
    ifelse(
      uji_dw$p.value > 0.05,
      "Tidak menolak H0",
      "Menolak H0"
    )
  )
)

# ---------------------------------------------------------
# 3. PERSIAPAN INTERPRETASI REGRESI
# ---------------------------------------------------------

# Koefisien regresi
koefisien_regresi <- hasil_estimasi %>%
  filter(term != "(Intercept)")

# Tabel VIF
data_vif <- as.data.frame(data_vif)

# Memastikan nama kolom VIF
if (!"VIF" %in% names(data_vif)) {
  names(data_vif)[2] <- "VIF"
}

# ---------------------------------------------------------
# 4. PERSIAPAN WARNA
# ---------------------------------------------------------

warna_regresi <- c(
  biru_dongker = "#000080",
  maroon = "#800000"
)

warna_multivariat <- c(
  merah = "red",
  hijau = "darkgreen"
)


# =========================================================
# PERSIAPAN 2: R SHINY DASHBOARD
# =========================================================

# =========================================================
# USER INTERFACE
# =========================================================

ui <- fluidPage(
  
  theme = bs_theme(
    version = 5,
    bootswatch = "flatly"
  ),
  
  titlePanel(
    "LIFE EXPECTANCY STATISTICAL DASHBOARD"
  ),
  
  p(
    "Eksplorasi Data, Analisis Multivariat, dan Regresi Linear Berganda"
  ),
  
  tabsetPanel(
    
    # =====================================================
    # 1. RINGKASAN DATA
    # =====================================================
    
    tabsetPanel(
      
      # =====================================================
      # 0. PENDAHULUAN
      # =====================================================
      
      tabPanel(
        "Pendahuluan",
        
        br(),
        
        h3("Analisis Eksplorasi Data,Univariat, Bivariat, Multivariat, dan Regresi Linear Berganda Terhadap Angka Harapan Hidup (Life Expectancy) Antarnegara"),
        
        h4("Latar Belakang"),
        p(
          "Angka harapan hidup merupakan salah satu indikator penting
      untuk menggambarkan tingkat kesehatan dan kesejahteraan
      penduduk di suatu negara. Perbedaan kondisi kesehatan,
      ekonomi, dan pendidikan dapat berkaitan dengan perbedaan
      angka harapan hidup antarnegara."
        ),
        p(
          "Penelitian ini menggunakan analisis eksploratif,
      multivariat, dan regresi linear berganda untuk
      menggambarkan karakteristik data serta menganalisis
      hubungan indikator kesehatan, ekonomi, dan pendidikan
      dengan angka harapan hidup."
        ),
        
        h4("Tujuan Penelitian"),
        tags$ol(
          tags$li("Mengetahui karakteristik data angka harapan hidup."),
          tags$li("Menggambarkan distribusi variabel numerik dan kategorik."),
          tags$li("Mengetahui hubungan antarvariabel menggunakan korelasi Pearson."),
          tags$li("Mengidentifikasi pencilan multivariat menggunakan Mahalanobis."),
          tags$li("Mengevaluasi normalitas dan asumsi regresi."),
          tags$li("Membentuk model regresi linear berganda.")
        ),
        
        h4("Karakteristik Data"),
        p(
          paste0(
            "Dataset terdiri dari ",
            nrow(Life_Expectancy_Data),
            " observasi dan ",
            ncol(Life_Expectancy_Data),
            " variabel."
          )
        ),
        
        h4("Alasan Memilih Topik"),
        p(
          "Topik dipilih karena angka harapan hidup merupakan
      indikator kesejahteraan yang berkaitan dengan aspek
      kesehatan, ekonomi, dan pendidikan."
        ),
        
        h4("Alasan Pemilihan Variabel"),
        tags$ul(
          tags$li("Life expectancy: variabel respons."),
          tags$li("Adult Mortality: indikator risiko kematian penduduk dewasa."),
          tags$li("HIV/AIDS: indikator masalah kesehatan."),
          tags$li("BMI: indikator indeks massa tubuh."),
          tags$li("GDP: indikator kondisi ekonomi."),
          tags$li("Schooling: indikator lama pendidikan."),
          tags$li("Country dan Status: variabel kategorik untuk eksplorasi.")
        ),
        
        h4("Bentuk Model Regresi"),
        p(
          "Life expectancy = β0 + β1 Adult Mortality +
      β2 HIV/AIDS + β3 BMI + β4 GDP + β5 Schooling + ε"
        )
      ),
      
      # =====================================================
      # 1. RINGKASAN DATA
      # =====================================================
      
      tabPanel(
        "Ringkasan Data",
        
        br(),
        
        h4("Informasi Data"),
        verbatimTextOutput("info_data"),
        
        h4("Statistik Deskriptif Numerik"),
        DTOutput("tabel_numerik")
        
        # Lanjutkan kode Ringkasan Data kamu yang lama di sini
      ),
      
      # Tab Univariat, Bivariat, Multivariat, dan Regresi Berganda tetap dilanjutkan
    ),
    
    # =====================================================
    # 2. UNIVARIAT
    # =====================================================
    
    tabPanel(
      "Univariat",
      
      br(),
      
      h4("Histogram Variabel Numerik"),
      plotOutput(
        "plot_histogram",
        height = "650px"
      ),
      
      br(),
      
      h4("Boxplot Variabel Numerik"),
      plotOutput(
        "plot_boxplot",
        height = "650px"
      ),
      
      br(),
      
      h4("Diagram Batang Distribusi Status"),
      plotOutput(
        "plot_status",
        height = "450px"
      ),
      
      h4("Diagram Lingkaran Distribusi Status"),
      plotOutput(
        "plot_pie_status",
        height = "450px"
      ),
      h4("Diagram Lingkaran Distribusi Status"),
      plotOutput(
        "plot_pie_status",
        height = "450px"
      ),
      
      br(),
      
      h4("Lima Negara dengan Frekuensi Tertinggi"),
      plotOutput(
        "plot_country",
        height = "400px"
      ),
      
      br(),
      
      h4("Interpretasi"),
      wellPanel(
        textOutput("interpretasi_univariat")
      )
    ),
    
    # =====================================================
    # 3. BIVARIAT
    # =====================================================
    
    tabPanel(
      "Bivariat",
      
      br(),
      
      h4("Hasil Korelasi Pearson"),
      DTOutput("tabel_korelasi"),
      
      br(),
      
      h4("Scatter Plot Schooling dan Adult Mortality"),
      plotOutput(
        "plot_scatter",
        height = "450px"
      ),
      
      br(),
      
      h4("Boxplot Variabel Numerik Berdasarkan Status"),
      plotOutput(
        "plot_boxplot_status",
        height = "650px"
      ),
      
      br(),
      
      h4("Correlation Plot"),
      plotOutput(
        "plot_corrplot",
        height = "550px"
      ),
      
      br(),
      
      h4("Interpretasi"),
      wellPanel(
        textOutput("interpretasi_bivariat")
      )
    ),
    
    # =====================================================
    # 4. MULTIVARIAT
    # =====================================================
    
    tabPanel(
      "Multivariat",
      
      br(),
      
      h4("Vektor Rata-Rata"),
      DTOutput("tabel_mean"),
      
      br(),
      
      h4("Matriks Kovariansi"),
      DTOutput("tabel_kovariansi"),
      
      br(),
      
      h4("Scatter Plot Multivariat"),
      plotOutput(
        "plot_ggpairs",
        height = "750px"
      ),
      
      br(),
      
      h4("Deteksi Outlier Mahalanobis"),
      plotOutput(
        "plot_mahalanobis",
        height = "450px"
      ),
      
      DTOutput("tabel_outlier"),
      
      br(),
      
      h4("Chi-Square Q-Q Plot"),
      plotOutput(
        "plot_qq",
        height = "450px"
      ),
      
      br(),
      
      h4("Uji Normalitas Shapiro-Wilk"),
      DTOutput("tabel_shapiro"),
      
      br(),
      
      h4("Interpretasi"),
      wellPanel(
        textOutput("interpretasi_multivariat")
      )
    ),
    
    # =====================================================
    # 5. REGRESI LINEAR BERGANDA
    # =====================================================
    
    tabPanel(
      "Regresi Berganda",
      
      br(),
      
      h4("Hasil Estimasi Regresi"),
      DTOutput("tabel_estimasi"),
      
      br(),
      
      h4("Ringkasan Model"),
      DTOutput("tabel_model"),
      
      br(),
      
      h4("Hasil Uji t (Parsial)"),
      DTOutput("tabel_uji_t"),
      
      br(),
      
      h4("Hasil Summary Regresi"),
      verbatimTextOutput("hasil_summary_regresi"),
      
      br(),
      
      h4("Pola Hubungan Variabel X dengan Life Expectancy"),
      plotOutput(
        "plot_regresi",
        height = "650px"
      ),
      
      br(),
      
      h4("Forest Plot Koefisien Regresi"),
      plotOutput(
        "plot_forest",
        height = "450px"
      ),
      
      br(),
      
      h4("Pengujian Asumsi Klasik"),
      DTOutput("tabel_asumsi"),
      
      br(),
      
      h4("Visualisasi Nilai VIF"),
      plotOutput(
        "plot_vif",
        height = "400px"
      ),
      
      DTOutput("tabel_vif"),
      
      br(),
      
      h4("Empat Plot Diagnostik Residual"),
      plotOutput(
        "plot_diagnostik",
        height = "650px"
      ),
      
      br(),
      
      h4("Interpretasi"),
      wellPanel(
        textOutput("interpretasi_regresi")
      )
    ),
# =====================================================
# 6. INTERPRETASI DAN KESIMPULAN
# =====================================================

tabPanel(
  "Interpretasi dan Kesimpulan",
  
  br(),
  
  h3("Interpretasi Hasil Penelitian"),
  
  h4("1. Kesimpulan Analisis Deskriptif"),
  textOutput("kesimpulan_deskriptif"),
  
  br(),
  
  h4("2. Kesimpulan Analisis Univariat"),
  textOutput("kesimpulan_univariat"),
  
  br(),
  
  h4("3. Kesimpulan Analisis Bivariat"),
  textOutput("kesimpulan_bivariat"),
  
  br(),
  
  h4("4. Kesimpulan Analisis Multivariat"),
  textOutput("kesimpulan_multivariat"),
  
  br(),
  
  h4("5. Kesimpulan Regresi Linear Berganda"),
  textOutput("kesimpulan_regresi"),
  
  br(),
  
  h4("6. Kesimpulan Evaluasi Asumsi"),
  textOutput("kesimpulan_asumsi"),
  
  br(),
  
  h4("Kesimpulan Umum"),
  textOutput("kesimpulan_umum")
)

)
)

# =========================================================
# SERVER
# =========================================================

server <- function(input, output, session) {
  
  # =====================================================
  # 1. RINGKASAN DATA
  # =====================================================
  
  output$info_data <- renderPrint({
    
    cat(
      "Jumlah observasi:", nrow(data_awal), "\n",
      "Jumlah variabel:", ncol(data_awal), "\n",
      "Jumlah negara:", dplyr::n_distinct(data_eda$Country), "\n"
    )
    
  })
  
  output$tabel_numerik <- renderDT({
    
    datatable(
      deskriptif_numerik,
      options = list(
        pageLength = 10,
        scrollX = TRUE
      ),
      rownames = FALSE
    )
    
  })
  
  output$tabel_status <- renderDT({
    
    datatable(
      data_status_dashboard,
      options = list(
        dom = "t",
        scrollX = TRUE
      ),
      rownames = FALSE
    )
    
  })
  
  output$tabel_country <- renderDT({
    
    datatable(
      deskriptif_country,
      options = list(
        pageLength = 10,
        scrollX = TRUE
      ),
      rownames = FALSE
    )
    
  })
  
  output$interpretasi_ringkasan <- renderText({
    
    paste0(
      "Data terdiri dari ",
      nrow(data_awal),
      " observasi dan ",
      ncol(data_awal),
      " variabel. Terdapat ",
      dplyr::n_distinct(data_eda$Country),
      " negara. Analisis regresi menggunakan Life expectancy ",
      "sebagai variabel dependen serta Adult Mortality, HIV/AIDS, ",
      "BMI, GDP, dan Schooling sebagai variabel independen. ",
      "Statistik deskriptif digunakan untuk mengetahui karakteristik ",
      "pemusatan dan penyebaran data."
    )
    
  })
  
  
  # =====================================================
  # 2. UNIVARIAT
  # =====================================================
  
  output$plot_histogram <- renderPlot({
    
    ggplot(
      data_numerik_long,
      aes(x = Nilai)
    ) +
      geom_histogram(
        bins = 30,
        fill = "skyblue",
        color = "black",
        na.rm = TRUE
      ) +
      facet_wrap(
        ~ Variabel,
        scales = "free",
        ncol = 2
      ) +
      labs(
        title = "Histogram Distribusi Variabel Numerik",
        x = "Nilai",
        y = "Frekuensi"
      ) +
      theme_minimal()
    
  })
  
  output$plot_boxplot <- renderPlot({
    
    ggplot(
      data_numerik_long,
      aes(
        x = "",
        y = Nilai
      )
    ) +
      geom_boxplot(
        fill = "lightgreen",
        color = "black",
        na.rm = TRUE
      ) +
      facet_wrap(
        ~ Variabel,
        scales = "free",
        ncol = 2
      ) +
      labs(
        title = "Boxplot Variabel Numerik",
        x = "Variabel",
        y = "Nilai"
      ) +
      theme_minimal()
    
  })
  
  output$plot_pie_status <- renderPlot({
    
    ggplot(
      data_status_dashboard,
      aes(
        x = "",
        y = Frekuensi,
        fill = Status
      )
    ) +
      geom_col(color = "black") +
      coord_polar(theta = "y") +
      geom_text(
        aes(
          label = paste0(
            round(Persentase, 1),
            "%"
          )
        ),
        position = position_stack(vjust = 0.5),
        color = "white",
        size = 5
      ) +
      labs(
        title = "Distribusi Status Negara",
        fill = "Status"
      ) +
      theme_void()
    
  })
  
  output$plot_country <- renderPlot({
    
    ggplot(
      data_country_top5,
      aes(
        x = reorder(Country, Frekuensi),
        y = Frekuensi
      )
    ) +
      geom_col(
        fill = "#000080",
        color = "#800000"
      ) +
      coord_flip() +
      labs(
        title = "Lima Negara dengan Frekuensi Observasi Tertinggi",
        x = "Country",
        y = "Frekuensi"
      ) +
      theme_minimal()
    
  })
  
  output$interpretasi_univariat <- renderText({
    
    status_tertinggi <- data_status_dashboard %>%
      slice_max(
        order_by = Frekuensi,
        n = 1,
        with_ties = FALSE
      )
    
    paste0(
      "Histogram menunjukkan pola distribusi masing-masing variabel ",
      "numerik, sedangkan boxplot memperlihatkan median, penyebaran, ",
      "dan kemungkinan pencilan. Kategori Status dengan frekuensi ",
      "tertinggi adalah ",
      status_tertinggi$Status,
      " sebanyak ",
      status_tertinggi$Frekuensi,
      " observasi atau ",
      round(status_tertinggi$Persentase, 2),
      "%. Grafik Country menampilkan lima negara dengan frekuensi ",
      "observasi tertinggi."
    )
    
  })
  
  
  # =====================================================
  # 3. BIVARIAT
  # =====================================================
  
  output$tabel_korelasi <- renderDT({
    
    datatable(
      hasil_korelasi,
      options = list(
        pageLength = 10,
        scrollX = TRUE
      ),
      rownames = FALSE
    )
    
  })
  
  output$plot_scatter <- renderPlot({
    
    ggplot(
      data_eda,
      aes(
        x = Schooling,
        y = `Adult Mortality`
      )
    ) +
      geom_point(
        color = "steelblue",
        size = 2,
        alpha = 0.7,
        na.rm = TRUE
      ) +
      geom_smooth(
        method = "lm",
        se = TRUE,
        color = "red",
        na.rm = TRUE
      ) +
      labs(
        title = "Hubungan Schooling dan Adult Mortality",
        x = "Schooling",
        y = "Adult Mortality"
      ) +
      theme_minimal()
    
  })
  
  output$plot_boxplot_status <- renderPlot({
    
    ggplot(
      data_boxplot,
      aes(
        x = Status,
        y = Nilai,
        fill = Status
      )
    ) +
      geom_boxplot(na.rm = TRUE) +
      facet_wrap(
        ~ Variabel,
        scales = "free",
        ncol = 2
      ) +
      labs(
        title = "Distribusi Variabel Berdasarkan Status",
        x = "Status",
        y = "Nilai",
        fill = "Status"
      ) +
      theme_minimal()
    
  })
  
  output$plot_corrplot <- renderPlot({
    
    corrplot(
      matriks_korelasi,
      method = "color",
      type = "upper",
      addCoef.col = "black",
      tl.col = "black",
      tl.srt = 45,
      number.cex = 0.7,
      col = colorRampPalette(
        c("red", "white", "darkgreen")
      )(200),
      title = "Correlation Plot Antarvariabel",
      mar = c(0, 0, 2, 0)
    )
    
  })
  
  output$interpretasi_bivariat <- renderText({
    
    korelasi_tertinggi <- hasil_korelasi %>%
      filter(!is.na(Korelasi_Pearson)) %>%
      slice_max(
        order_by = abs(Korelasi_Pearson),
        n = 1,
        with_ties = FALSE
      )
    
    paste0(
      "Pasangan variabel dengan korelasi absolut tertinggi adalah ",
      korelasi_tertinggi$Variabel_1,
      " dan ",
      korelasi_tertinggi$Variabel_2,
      " dengan koefisien Pearson sebesar ",
      round(korelasi_tertinggi$Korelasi_Pearson, 4),
      ". Scatter plot memperlihatkan pola hubungan Schooling ",
      "dan Adult Mortality. Boxplot digunakan untuk membandingkan ",
      "distribusi numerik berdasarkan Status negara."
    )
    
  })
  
  
  # =====================================================
  # 4. MULTIVARIAT
  # =====================================================
  
  output$tabel_mean <- renderDT({
    
    datatable(
      data.frame(
        Variabel = names(vektor_rata_rata),
        Rata_Rata = as.numeric(vektor_rata_rata)
      ),
      options = list(
        dom = "t",
        scrollX = TRUE
      ),
      rownames = FALSE
    )
    
  })
  
  output$tabel_kovariansi <- renderDT({
    
    datatable(
      as.data.frame(matriks_kovariansi_standar),
      options = list(
        dom = "t",
        scrollX = TRUE
      ),
      rownames = TRUE
    )
    
  })
  
  output$plot_ggpairs <- renderPlot({
    
    print(
      GGally::ggpairs(
        data_multivariat,
        upper = list(
          continuous = GGally::wrap(
            "cor",
            colour = "darkgreen",
            size = 3
          )
        ),
        lower = list(
          continuous = GGally::wrap(
            "points",
            colour = "red",
            alpha = 0.4,
            size = 0.8
          )
        ),
        diag = list(
          continuous = GGally::wrap(
            "densityDiag",
            fill = "darkgreen",
            colour = "red",
            alpha = 0.5
          )
        )
      ) +
        ggtitle(
          "Scatter Plot Multivariat Antarvariabel"
        )
    )
    
  })
  
  output$plot_mahalanobis <- renderPlot({
    
    ggplot(
      hasil_outlier,
      aes(
        x = Observasi,
        y = Jarak_Mahalanobis,
        color = Status
      )
    ) +
      geom_point(size = 2) +
      geom_hline(
        yintercept = batas_mahalanobis,
        color = "darkgreen",
        linetype = "dashed",
        linewidth = 1
      ) +
      scale_color_manual(
        values = c(
          "Outlier" = "red",
          "Tidak Outlier" = "darkgreen"
        )
      ) +
      labs(
        title = "Deteksi Outlier Mahalanobis",
        x = "Observasi",
        y = "Jarak Mahalanobis Kuadrat",
        color = "Status"
      ) +
      theme_minimal()
    
  })
  
  output$tabel_outlier <- renderDT({
    
    datatable(
      hasil_outlier,
      options = list(
        pageLength = 10,
        scrollX = TRUE
      ),
      rownames = FALSE
    )
    
  })
  
  output$plot_qq <- renderPlot({
    
    ggplot(
      data_qq,
      aes(
        x = Chi_Square,
        y = Mahalanobis
      )
    ) +
      geom_point(
        color = "red",
        alpha = 0.6
      ) +
      geom_abline(
        intercept = 0,
        slope = 1,
        color = "darkgreen",
        linewidth = 1
      ) +
      labs(
        title = "Chi-Square Q-Q Plot",
        x = "Kuantil Teoretis Chi-Square",
        y = "Jarak Mahalanobis Kuadrat"
      ) +
      theme_minimal()
    
  })
  
  output$tabel_shapiro <- renderDT({
    
    datatable(
      hasil_shapiro,
      options = list(
        pageLength = 10,
        scrollX = TRUE
      ),
      rownames = FALSE
    )
    
  })
  
  output$interpretasi_multivariat <- renderText({
    
    jumlah_outlier <- sum(
      hasil_outlier$Status == "Outlier",
      na.rm = TRUE
    )
    
    paste0(
      "Analisis multivariat menggunakan lima variabel numerik. ",
      "Jarak Mahalanobis digunakan untuk mendeteksi observasi ",
      "yang memiliki karakteristik berbeda secara bersamaan. ",
      "Berdasarkan batas Chi-Square sebesar ",
      round(batas_mahalanobis, 4),
      ", ditemukan ",
      jumlah_outlier,
      " observasi yang terindikasi sebagai outlier. ",
      "Chi-Square Q-Q Plot digunakan untuk mengevaluasi ",
      "kesesuaian jarak Mahalanobis terhadap distribusi Chi-Square."
    )
    
  })
  
  
  # =====================================================
  # 5. REGRESI LINEAR BERGANDA
  # =====================================================
  
  output$tabel_estimasi <- renderDT({
    
    datatable(
      hasil_estimasi,
      options = list(
        pageLength = 10,
        scrollX = TRUE
      ),
      rownames = FALSE
    ) %>%
      formatRound(
        columns = c(
          "estimate",
          "std.error",
          "statistic",
          "p.value",
          "conf.low",
          "conf.high"
        ),
        digits = 4
      )
    
  })
  
  output$hasil_summary_regresi <- renderPrint({
    
    summary(model_regresi)
    
  })
  
  # -----------------------------------------------------
  # UJI t
  # -----------------------------------------------------
  
  output$tabel_uji_t <- renderDT({
    
    hasil_estimasi %>%
      filter(term != "(Intercept)") %>%
      select(
        Variabel = term,
        Estimate = estimate,
        Std_Error = std.error,
        t_value = statistic,
        P_Value = p.value
      ) %>%
      mutate(
        Keputusan = ifelse(
          P_Value < 0.05,
          "Signifikan",
          "Tidak Signifikan"
        )
      ) %>%
      datatable(
        options = list(
          pageLength = 10,
          scrollX = TRUE
        ),
        rownames = FALSE
      ) %>%
      formatRound(
        columns = c(
          "Estimate",
          "Std_Error",
          "t_value",
          "P_Value"
        ),
        digits = 4
      )
    
  })
  
  # -----------------------------------------------------
  # RINGKASAN MODEL DAN UJI F
  # -----------------------------------------------------
  
  output$tabel_model <- renderDT({
    
    data.frame(
      Keterangan = c(
        "Residual Standard Error",
        "Degrees of Freedom",
        "Multiple R-squared",
        "Adjusted R-squared",
        "F-statistic",
        "P-value Uji F"
      ),
      
      Nilai = c(
        hasil_model$sigma,
        hasil_model$df.residual,
        hasil_model$r.squared,
        hasil_model$adj.r.squared,
        unname(hasil_model$fstatistic[1]),
        pf(
          hasil_model$fstatistic[1],
          hasil_model$fstatistic[2],
          hasil_model$fstatistic[3],
          lower.tail = FALSE
        )
      )
      
    ) %>%
      datatable(
        options = list(
          dom = "t",
          scrollX = TRUE
        ),
        rownames = FALSE
      ) %>%
      formatRound(
        columns = "Nilai",
        digits = 4
      )
    
  })
  
  # -----------------------------------------------------
  # PLOT REGRESI
  # -----------------------------------------------------
  
  output$plot_regresi <- renderPlot({
    
    ggplot(
      data_scatter,
      aes(
        x = Nilai_X,
        y = `Life expectancy`
      )
    ) +
      geom_point(
        color = "#800000",
        alpha = 0.4,
        size = 1.5
      ) +
      geom_smooth(
        method = "lm",
        se = TRUE,
        color = "#000080"
      ) +
      facet_wrap(
        ~ Variabel_X,
        scales = "free_x",
        ncol = 2
      ) +
      labs(
        title = "Pola Hubungan Variabel X dengan Life Expectancy",
        x = "Variabel Bebas",
        y = "Life Expectancy"
      ) +
      theme_minimal()
    
  })
  
  # -----------------------------------------------------
  # FOREST PLOT
  # -----------------------------------------------------
  
  output$plot_forest <- renderPlot({
    
    ggplot(
      data_forest,
      aes(
        x = estimate,
        y = term
      )
    ) +
      geom_vline(
        xintercept = 0,
        linetype = "dashed",
        color = "#800000"
      ) +
      geom_errorbar(
        aes(
          xmin = conf.low,
          xmax = conf.high
        ),
        width = 0.2,
        color = "#000080",
        linewidth = 0.8
      ) +
      geom_point(
        color = "#800000",
        size = 3
      ) +
      labs(
        title = "Forest Plot Koefisien Regresi",
        x = "Koefisien dan Interval Kepercayaan 95%",
        y = "Variabel"
      ) +
      theme_minimal()
    
  })
  
  # -----------------------------------------------------
  # UJI ASUMSI
  # -----------------------------------------------------
  
  output$tabel_asumsi <- renderDT({
    
    datatable(
      hasil_asumsi,
      options = list(
        dom = "t",
        scrollX = TRUE
      ),
      rownames = FALSE
    ) %>%
      formatRound(
        columns = c("Statistik", "P_Value"),
        digits = 4
      )
    
  })
  
  output$plot_vif <- renderPlot({
    
    ggplot(
      data_vif,
      aes(
        x = reorder(Variabel, VIF),
        y = VIF
      )
    ) +
      geom_col(
        fill = "#000080",
        color = "#800000"
      ) +
      geom_hline(
        yintercept = 5,
        color = "#800000",
        linetype = "dashed"
      ) +
      coord_flip() +
      labs(
        title = "Variance Inflation Factor",
        x = "Variabel Bebas",
        y = "Nilai VIF"
      ) +
      theme_minimal()
    
  })
  
  output$tabel_vif <- renderDT({
    
    datatable(
      data_vif,
      options = list(
        dom = "t",
        scrollX = TRUE
      ),
      rownames = FALSE
    ) %>%
      formatRound(
        columns = "VIF",
        digits = 4
      )
    
  })
  
  # -----------------------------------------------------
  # PLOT DIAGNOSTIK
  # -----------------------------------------------------
  
  output$plot_diagnostik <- renderPlot({
    
    par(
      mfrow = c(2, 2)
    )
    
    plot(
      model_regresi,
      which = 1:4,
      col = "#800000"
    )
    
    par(
      mfrow = c(1, 1)
    )
    
  })
  
  # -----------------------------------------------------
  # INTERPRETASI REGRESI OTOMATIS
  # -----------------------------------------------------
  
  output$interpretasi_regresi <- renderText({
    
    r2 <- hasil_model$r.squared
    adj_r2 <- hasil_model$adj.r.squared
    
    p_model <- pf(
      hasil_model$fstatistic[1],
      hasil_model$fstatistic[2],
      hasil_model$fstatistic[3],
      lower.tail = FALSE
    )
    
    keputusan_model <- if (p_model < 0.05) {
      "Model signifikan secara simultan."
    } else {
      "Model tidak signifikan secara simultan."
    }
    
    keputusan_normalitas <- if (
      uji_shapiro_regresi$p.value > 0.05
    ) {
      "Residual tidak menunjukkan penyimpangan normalitas yang signifikan."
    } else {
      "Residual menunjukkan penyimpangan dari normalitas."
    }
    
    keputusan_bp <- if (
      uji_bp$p.value > 0.05
    ) {
      "Tidak terdapat bukti heteroskedastisitas."
    } else {
      "Terdapat indikasi heteroskedastisitas."
    }
    
    keputusan_vif <- if (
      all(data_vif$VIF < 5, na.rm = TRUE)
    ) {
      "Seluruh nilai VIF berada di bawah 5."
    } else {
      "Terdapat nilai VIF yang perlu diperhatikan."
    }
    
    signifikan <- hasil_estimasi %>%
      filter(
        term != "(Intercept)",
        p.value < 0.05
      )
    
    variabel_signifikan <- if (nrow(signifikan) > 0) {
      paste(
        signifikan$term,
        collapse = ", "
      )
    } else {
      "Tidak terdapat variabel yang signifikan secara parsial."
    }
    
    paste0(
      "Model regresi menggunakan Life expectancy sebagai variabel ",
      "dependen serta Adult Mortality, HIV/AIDS, BMI, GDP, dan ",
      "Schooling sebagai variabel independen. Nilai R-squared sebesar ",
      round(r2, 4),
      " atau ",
      round(r2 * 100, 2),
      "% menunjukkan proporsi variasi Life expectancy yang dapat ",
      "dijelaskan oleh model. Adjusted R-squared sebesar ",
      round(adj_r2, 4),
      ". P-value uji F sebesar ",
      signif(p_model, 4),
      ". ",
      keputusan_model,
      " Variabel yang signifikan secara parsial adalah: ",
      variabel_signifikan,
      ". ",
      keputusan_normalitas,
      " ",
      keputusan_bp,
      " ",
      keputusan_vif,
      " Plot diagnostik digunakan untuk memeriksa pola residual, ",
      "normalitas, kesamaan varians, dan pengamatan berpengaruh."
    )
    
  })
  # =====================================================
  # INTERPRETASI DAN KESIMPULAN LENGKAP
  # =====================================================
  
  output$kesimpulan_deskriptif <- renderText({
    
    paste0(
      "Data terdiri dari ",
      nrow(Life_Expectancy_Data),
      " observasi dan ",
      ncol(Life_Expectancy_Data),
      " variabel. Statistik deskriptif menunjukkan ukuran
    pemusatan dan penyebaran lima variabel numerik.
    Hasil ini memberikan gambaran awal mengenai
    karakteristik data sebelum analisis lanjutan."
    )
    
  })
  
  output$kesimpulan_univariat <- renderText({
    
    status_tertinggi <- deskriptif_status %>%
      slice_max(Frekuensi, n = 1)
    
    paste0(
      "Berdasarkan distribusi Status, kategori dengan
    frekuensi tertinggi adalah ",
      as.character(status_tertinggi$Status),
      " sebanyak ",
      status_tertinggi$Frekuensi,
      " observasi atau ",
      round(status_tertinggi$Persentase, 2),
      "%. Histogram dan boxplot digunakan untuk
    menggambarkan distribusi, penyebaran, serta
    kemungkinan pencilan pada variabel numerik."
    )
    
  })
  
  output$kesimpulan_bivariat <- renderText({
    
    korelasi_tertinggi <- hasil_korelasi %>%
      filter(!is.na(Korelasi_Pearson)) %>%
      slice_max(
        order_by = abs(Korelasi_Pearson),
        n = 1,
        with_ties = FALSE
      )
    
    paste0(
      "Pasangan variabel dengan korelasi absolut
    tertinggi adalah ",
      korelasi_tertinggi$Variabel_1,
      " dan ",
      korelasi_tertinggi$Variabel_2,
      " dengan koefisien sebesar ",
      korelasi_tertinggi$Korelasi_Pearson,
      ". Hasil ini menunjukkan pasangan variabel
    yang memiliki hubungan linear paling kuat
    di antara pasangan yang dianalisis.
    Korelasi tidak dapat diartikan sebagai hubungan sebab-akibat."
    )
    
  })
  
  output$kesimpulan_multivariat <- renderText({
    
    jumlah_outlier <- sum(
      hasil_outlier$Status == "Outlier",
      na.rm = TRUE
    )
    
    paste0(
      "Analisis multivariat dilakukan menggunakan lima
    variabel numerik yang telah distandardisasi.
    Berdasarkan jarak Mahalanobis dengan batas
    Chi-Square sebesar ",
      round(batas_mahalanobis, 4),
      ", ditemukan ",
      jumlah_outlier,
      " observasi yang terindikasi sebagai pencilan
    multivariat. Hasil tersebut perlu diperiksa
    lebih lanjut sebelum menentukan penanganannya.
    Chi-Square Q-Q Plot digunakan untuk menilai
    kesesuaian distribusi multivariat secara visual."
    )
    
  })
  
  output$kesimpulan_regresi <- renderText({
    
    r2 <- hasil_model$r.squared
    adjr2 <- hasil_model$adj.r.squared
    p_f <- hasil_model$p.value
    
    keputusan_f <- if (p_f < 0.05) {
      "Model signifikan secara simultan."
    } else {
      "Model tidak signifikan secara simultan."
    }
    
    signifikan <- hasil_estimasi %>%
      filter(
        term != "(Intercept)",
        p.value < 0.05
      )
    
    tidak_signifikan <- hasil_estimasi %>%
      filter(
        term != "(Intercept)",
        p.value >= 0.05
      )
    
    daftar_signifikan <- if (nrow(signifikan) > 0) {
      paste(signifikan$term, collapse = ", ")
    } else {
      "Tidak ada variabel bebas yang signifikan secara parsial"
    }
    
    daftar_tidak_signifikan <- if (nrow(tidak_signifikan) > 0) {
      paste(tidak_signifikan$term, collapse = ", ")
    } else {
      "Tidak ada variabel bebas yang tidak signifikan secara parsial"
    }
    
    paste0(
      "Nilai R-squared sebesar ",
      round(r2, 4),
      " menunjukkan bahwa ",
      round(r2 * 100, 2),
      "% variasi angka harapan hidup dapat dijelaskan
    oleh variabel bebas dalam model. Adjusted R-squared
    sebesar ",
      round(adjr2, 4),
      ". P-value uji F sebesar ",
      signif(p_f, 4),
      ". ",
      keputusan_f,
      " Berdasarkan uji t, variabel yang signifikan
    secara parsial adalah: ",
      daftar_signifikan,
      ". Variabel yang tidak signifikan adalah: ",
      daftar_tidak_signifikan,
      "."
    )
    
  })
  
  output$kesimpulan_asumsi <- renderText({
    
    hasil_normalitas <- if (
      uji_shapiro_regresi$p.value < 0.05
    ) {
      "Residual belum memenuhi asumsi normalitas berdasarkan Shapiro-Wilk."
    } else {
      "Tidak terdapat bukti yang cukup bahwa residual menyimpang dari normalitas."
    }
    
    hasil_bp <- if (uji_bp$p.value < 0.05) {
      "Terdapat indikasi heteroskedastisitas berdasarkan Breusch-Pagan."
    } else {
      "Tidak terdapat bukti heteroskedastisitas berdasarkan Breusch-Pagan."
    }
    
    hasil_dw <- if (uji_dw$p.value < 0.05) {
      "Uji Durbin-Watson menunjukkan indikasi autokorelasi."
    } else {
      "Uji Durbin-Watson tidak menunjukkan bukti autokorelasi."
    }
    
    hasil_vif_text <- if (all(hasil_vif < 10)) {
      "Seluruh nilai VIF berada di bawah 10."
    } else {
      "Terdapat nilai VIF yang mencapai atau melebihi 10."
    }
    
    paste(
      hasil_normalitas,
      hasil_vif_text,
      hasil_bp,
      hasil_dw,
      "Interpretasi asumsi perlu mempertimbangkan
    bahwa data memiliki pengamatan berulang
    antarnegara dan tahun."
    )
    
  })
  
  output$kesimpulan_umum <- renderText({
    
    paste0(
      "Penelitian ini telah melakukan eksplorasi data,
    analisis univariat, bivariat, multivariat, dan
    regresi linear berganda untuk mengkaji hubungan
    indikator kesehatan, ekonomi, dan pendidikan
    dengan angka harapan hidup. Model regresi
    menjelaskan sebagian variasi angka harapan hidup
    berdasarkan nilai R-squared sebesar ",
      round(hasil_model$r.squared, 4),
      ". Hasil pengujian parsial dan simultan serta
    evaluasi asumsi menjadi dasar dalam menilai
    hasil model. Kesimpulan penelitian harus
    mempertimbangkan keterbatasan model dan struktur
    data pengamatan antarnegara dan tahun."
    )
    
  }) 
}


# =========================================================
# MENJALANKAN DASHBOARD
# =========================================================

shinyApp(
  ui = ui,
  server = server
)


