# ============================================================
# TEMPLATE LENGKAP UJI MULTIVARIAT
# ============================================================
# Alpha = 0.05
#
# Uji:
# 1. Mardia
# 2. Henze-Zirkler
# 3. Mahalanobis Distance + Chi-Square
# 4. Box's M
# 5. Hotelling T2 One Sample
# 6. Hotelling T2 Two Sample
# 7. MANOVA
# 8. Wilks Lambda
# 9. Pillai Trace
# 10. Hotelling-Lawley Trace
# 11. Roy's Largest Root
# 12. ANOVA Univariate
# 13. Two-Way MANOVA
# ============================================================


# ============================================================
# 1. INPUT DATA
# ============================================================

# Masukkan data sendiri sebelum menjalankan bagian analisis.
#
# Excel:
# data <- readxl::read_excel("data.xlsx")
#
# CSV:
# data <- read.csv("data.csv")

alpha <- 0.05


# ============================================================
# 2. PENGATURAN VARIABEL
# ============================================================

# GANTI sesuai nama variabel pada data

variabel <- c("X1", "X2", "X3")

# GANTI sesuai nama variabel kelompok

kelompok <- "Kelompok"


# ============================================================
# 3. PACKAGE
# ============================================================

packages <- c(
  "MVN",
  "biotools",
  "ICSNP",
  "Hotelling"
)

for (p in packages) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p)
  }
}

library(MVN)
library(biotools)
library(ICSNP)
library(Hotelling)


# ============================================================
# 4. DATA
# ============================================================

X <- data[, variabel]

G <- as.factor(data[[kelompok]])

cat("\n====================================================")
cat("\nDATA")
cat("\n====================================================\n")

print(head(data))

cat("\nVariabel yang digunakan:\n")
print(variabel)

cat("\nKelompok:\n")
print(levels(G))


# ============================================================
# 5. FUNGSI KEPUTUSAN
# ============================================================

keputusan <- function(
    p_value,
    alpha = 0.05,
    h0_tolak = "",
    h0_gagal = ""
) {

  cat("\nP-value :", p_value)
  cat("\nAlpha   :", alpha)

  if (p_value < alpha) {

    cat("\nKeputusan : TOLAK H0\n")

    if (h0_tolak != "") {
      cat("Kesimpulan:", h0_tolak, "\n")
    }

  } else {

    cat("\nKeputusan : GAGAL TOLAK H0\n")

    if (h0_gagal != "") {
      cat("Kesimpulan:", h0_gagal, "\n")
    }
  }
}


# ============================================================
# 6. UJI MARDIA
# ============================================================

cat("\n\n====================================================")
cat("\n1. UJI NORMALITAS MULTIVARIAT - MARDIA")
cat("\n====================================================\n")

hasil_mardia <- mvn(
  data = X,
  mvnTest = "mardia"
)

print(hasil_mardia$multivariateNormality)

# Ambil p-value
p_mardia_skew <- hasil_mardia$multivariateNormality[
  1, "p value"
]

p_mardia_kurt <- hasil_mardia$multivariateNormality[
  2, "p value"
]

cat("\n--- MARDIA SKEWNESS ---\n")

keputusan(
  p_mardia_skew,
  alpha,
  "Data tidak berdistribusi normal multivariat.",
  "Data berdistribusi normal multivariat."
)

cat("\n--- MARDIA KURTOSIS ---\n")

keputusan(
  p_mardia_kurt,
  alpha,
  "Data tidak berdistribusi normal multivariat.",
  "Data berdistribusi normal multivariat."
)


# ============================================================
# 7. UJI HENZE-ZIRKLER
# ============================================================

cat("\n\n====================================================")
cat("\n2. UJI NORMALITAS MULTIVARIAT - HENZE-ZIRKLER")
cat("\n====================================================\n")

hasil_hz <- hz(X)

print(hasil_hz)

p_hz <- hasil_hz$p.value

keputusan(
  p_hz,
  alpha,
  "Data tidak berdistribusi normal multivariat.",
  "Data berdistribusi normal multivariat."
)


# ============================================================
# 8. MAHALANOBIS DISTANCE
# ============================================================

cat("\n\n====================================================")
cat("\n3. MAHALANOBIS DISTANCE + CHI-SQUARE")
cat("\n====================================================\n")

mean_X <- colMeans(X)

S <- cov(X)

D2 <- mahalanobis(
  X,
  center = mean_X,
  cov = S
)

cat("\nSquared Mahalanobis Distance:\n")

print(D2)

p_dimensi <- ncol(X)

chi50 <- qchisq(
  0.50,
  df = p_dimensi
)

cat("\nChi-Square 50% :", chi50)

proporsi <- mean(D2 <= chi50)

cat("\nProporsi dalam kontur 50% :", proporsi)

cat("\nPersentase :", proporsi * 100, "%\n")

if (abs(proporsi - 0.50) <= 0.10) {

  cat("\nKesimpulan:")
  cat("\nProporsi mendekati 50%.")
  cat("\nNormalitas multivariat didukung berdasarkan pendekatan ini.\n")

} else {

  cat("\nKesimpulan:")
  cat("\nProporsi cukup jauh dari 50%.")
  cat("\nTerdapat indikasi penyimpangan normalitas multivariat.\n")
}


# ============================================================
# 9. CHI-SQUARE Q-Q PLOT
# ============================================================

cat("\n\n====================================================")
cat("\n4. CHI-SQUARE Q-Q PLOT")
cat("\n====================================================\n")

n <- nrow(X)

prob <- (1:n - 0.5) / n

chi_square <- qchisq(
  prob,
  df = p_dimensi
)

plot(
  chi_square,
  sort(D2),
  xlab = "Chi-Square Quantiles",
  ylab = "Squared Mahalanobis Distance",
  main = "Chi-Square Q-Q Plot"
)

abline(
  0,
  1
)


# ============================================================
# 10. BOX'S M
# ============================================================

cat("\n\n====================================================")
cat("\n5. UJI HOMOGENITAS MATRIKS KOVARIANS - BOX'S M")
cat("\n====================================================\n")

hasil_box <- boxM(
  X,
  G
)

print(hasil_box)

p_box <- hasil_box$p.value

keputusan(
  p_box,
  alpha,
  "Matriks kovarians tidak homogen.",
  "Matriks kovarians homogen."
)


# ============================================================
# 11. HOTELLING T2 ONE SAMPLE
# ============================================================

cat("\n\n====================================================")
cat("\n6. HOTELLING'S T2 - ONE SAMPLE")
cat("\n====================================================\n")

# GANTI sesuai nilai mean hipotesis

mu0 <- c(
  75,
  80,
  78
)

hasil_one <- HotellingsT2(
  X,
  mu = mu0
)

print(hasil_one)

p_one <- hasil_one$p.value

keputusan(
  p_one,
  alpha,
  "Vektor rata-rata berbeda dengan mu0.",
  "Tidak terdapat perbedaan vektor rata-rata dengan mu0."
)


# ============================================================
# 12. HOTELLING T2 TWO SAMPLE
# ============================================================

cat("\n\n====================================================")
cat("\n7. HOTELLING'S T2 - TWO SAMPLE")
cat("\n====================================================\n")

# GANTI sesuai nama kelompok

X1 <- data[
  G == "A1",
  variabel
]

X2 <- data[
  G == "A2",
  variabel
]

hasil_two <- hotelling.test(
  X1,
  X2,
  var.equal = TRUE
)

print(hasil_two)

p_two <- hasil_two$pval

keputusan(
  p_two,
  alpha,
  "Terdapat perbedaan vektor rata-rata kedua populasi.",
  "Tidak terdapat perbedaan vektor rata-rata kedua populasi."
)


# ============================================================
# 13. MANOVA
# ============================================================

cat("\n\n====================================================")
cat("\n8. MANOVA - UJI MULTIVARIAT")
cat("\n====================================================\n")

formula_manova <- as.formula(
  paste(
    "cbind(",
    paste(variabel, collapse = ","),
    ") ~",
    kelompok
  )
)

model_manova <- manova(
  formula_manova,
  data = data
)

print(model_manova)


# ============================================================
# 14. WILKS' LAMBDA
# ============================================================

cat("\n\n====================================================")
cat("\n9. MANOVA - WILKS' LAMBDA")
cat("\n====================================================\n")

hasil_wilks <- summary(
  model_manova,
  test = "Wilks"
)

print(hasil_wilks)

p_wilks <- hasil_wilks$stats[
  1,
  "Pr(>F)"
]

keputusan(
  p_wilks,
  alpha,
  "Terdapat perbedaan vektor mean antar kelompok.",
  "Tidak terdapat perbedaan vektor mean antar kelompok."
)


# ============================================================
# 15. PILLAI'S TRACE
# ============================================================

cat("\n\n====================================================")
cat("\n10. MANOVA - PILLAI'S TRACE")
cat("\n====================================================\n")

hasil_pillai <- summary(
  model_manova,
  test = "Pillai"
)

print(hasil_pillai)

p_pillai <- hasil_pillai$stats[
  1,
  "Pr(>F)"
]

keputusan(
  p_pillai,
  alpha,
  "Terdapat perbedaan vektor mean antar kelompok.",
  "Tidak terdapat perbedaan vektor mean antar kelompok."
)


# ============================================================
# 16. HOTELLING-LAWLEY TRACE
# ============================================================

cat("\n\n====================================================")
cat("\n11. MANOVA - HOTELLING-LAWLEY TRACE")
cat("\n====================================================\n")

hasil_hl <- summary(
  model_manova,
  test = "Hotelling-Lawley"
)

print(hasil_hl)

p_hl <- hasil_hl$stats[
  1,
  "Pr(>F)"
]

keputusan(
  p_hl,
  alpha,
  "Terdapat perbedaan vektor mean antar kelompok.",
  "Tidak terdapat perbedaan vektor mean antar kelompok."
)


# ============================================================
# 17. ROY'S LARGEST ROOT
# ============================================================

cat("\n\n====================================================")
cat("\n12. MANOVA - ROY'S LARGEST ROOT")
cat("\n====================================================\n")

hasil_roy <- summary(
  model_manova,
  test = "Roy"
)

print(hasil_roy)

p_roy <- hasil_roy$stats[
  1,
  "Pr(>F)"
]

keputusan(
  p_roy,
  alpha,
  "Terdapat perbedaan vektor mean antar kelompok.",
  "Tidak terdapat perbedaan vektor mean antar kelompok."
)


# ============================================================
# 18. UJI UNIVARIAT / ANOVA
# ============================================================

cat("\n\n====================================================")
cat("\n13. UJI UNIVARIAT - ANOVA")
cat("\n====================================================\n")

hasil_aov <- summary.aov(
  model_manova
)

print(hasil_aov)


# ============================================================
# 19. TWO-WAY MANOVA
# ============================================================

cat("\n\n====================================================")
cat("\n14. TWO-WAY MANOVA")
cat("\n====================================================\n")

if (
  "Faktor1" %in% names(data) &&
  "Faktor2" %in% names(data)
) {

  data$Faktor1 <- as.factor(data$Faktor1)

  data$Faktor2 <- as.factor(data$Faktor2)

  formula_two_way <- as.formula(
    paste(
      "cbind(",
      paste(variabel, collapse = ","),
      ") ~ Faktor1 * Faktor2"
    )
  )

  model_two_way <- manova(
    formula_two_way,
    data = data
  )

  # ----------------------------------------------------------
  # WILKS
  # ----------------------------------------------------------

  cat("\n--- TWO-WAY MANOVA: WILKS ---\n")

  hasil_two_wilks <- summary(
    model_two_way,
    test = "Wilks"
  )

  print(hasil_two_wilks)

  p_two_wilks <- hasil_two_wilks$stats[
    ,
    "Pr(>F)"
  ]

  keputusan_two_wilks <- data.frame(
    Efek = rownames(
      hasil_two_wilks$stats
    ),
    P_Value = p_two_wilks,
    Keputusan = ifelse(
      p_two_wilks < alpha,
      "TOLAK H0",
      "GAGAL TOLAK H0"
    )
  )

  print(
    keputusan_two_wilks
  )


  # ----------------------------------------------------------
  # PILLAI
  # ----------------------------------------------------------

  cat("\n--- TWO-WAY MANOVA: PILLAI ---\n")

  hasil_two_pillai <- summary(
    model_two_way,
    test = "Pillai"
  )

  print(hasil_two_pillai)

  p_two_pillai <- hasil_two_pillai$stats[
    ,
    "Pr(>F)"
  ]

  keputusan_two_pillai <- data.frame(
    Efek = rownames(
      hasil_two_pillai$stats
    ),
    P_Value = p_two_pillai,
    Keputusan = ifelse(
      p_two_pillai < alpha,
      "TOLAK H0",
      "GAGAL TOLAK H0"
    )
  )

  print(
    keputusan_two_pillai
  )


  # ----------------------------------------------------------
  # ANOVA UNIVARIAT
  # ----------------------------------------------------------

  cat("\n--- ANOVA UNIVARIAT TWO-WAY MANOVA ---\n")

  print(
    summary.aov(model_two_way)
  )


} else {

  cat("\nKolom Faktor1 dan Faktor2 tidak ditemukan.")
  cat("\nTwo-Way MANOVA dilewati.\n")
}


# ============================================================
# 20. RINGKASAN H0
# ============================================================

cat("\n\n====================================================")
cat("\nRINGKASAN HIPOTESIS")
cat("\n====================================================\n")

cat("
1. MARDIA
H0 : Data berdistribusi normal multivariat.
H1 : Data tidak berdistribusi normal multivariat.

2. HENZE-ZIRKLER
H0 : Data berdistribusi normal multivariat.
H1 : Data tidak berdistribusi normal multivariat.

3. BOX'S M
H0 : Matriks kovarians homogen.
H1 : Minimal terdapat satu matriks kovarians yang berbeda.

4. HOTELLING T2 ONE SAMPLE
H0 : mu = mu0.
H1 : mu != mu0.

5. HOTELLING T2 TWO SAMPLE
H0 : mu1 = mu2.
H1 : mu1 != mu2.

6. MANOVA
H0 : Tidak terdapat perbedaan vektor mean antar kelompok.
H1 : Terdapat perbedaan vektor mean antar kelompok.

7. TWO-WAY MANOVA
H0 : Tidak terdapat efek Faktor 1.
H0 : Tidak terdapat efek Faktor 2.
H0 : Tidak terdapat efek interaksi Faktor 1 x Faktor 2.

ATURAN KEPUTUSAN:

Jika p-value < 0.05
=> TOLAK H0

Jika p-value >= 0.05
=> GAGAL TOLAK H0
")


cat("\n====================================================")
cat("\nSELESAI")
cat("\n====================================================\n")
