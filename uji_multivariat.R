# ============================================================
# TEMPLATE LENGKAP UJI MULTIVARIAT - BERDASARKAN PDF
# ============================================================

# -------------------- 1. INPUT DATA -------------------------

data <- "......"       # GANTI dengan lokasi file
alpha <- 0.05

# Jika Excel:
library(readxl)
data <- "......"

# Jika CSV, gunakan ini sebagai gantinya:
# data <- read.csv(data)

View(data)
names(data)


# ============================================================
# 2. TENTUKAN VARIABEL
# ============================================================

# GANTI sesuai nama kolom pada data
variabel <- c("X1", "X2", "X3")

# GANTI sesuai nama kolom kelompok
kelompok <- "Kelompok"

X <- data[, variabel]
G <- as.factor(data[[kelompok]])

cat("\n====================================================")
cat("\nDATA DAN VARIABEL")
cat("\n====================================================\n")
print(head(data))


# ============================================================
# FUNGSI KEPUTUSAN
# ============================================================

keputusan <- function(p_value, alpha = 0.05,
                      kesimpulan_tolak = "",
                      kesimpulan_gagal = "") {

  cat("\nP-value =", p_value)
  cat("\nAlpha    =", alpha)

  if (p_value < alpha) {

    cat("\nKeputusan : TOLAK H0")
    if (kesimpulan_tolak != "")
      cat("\nKesimpulan:", kesimpulan_tolak)

  } else {

    cat("\nKeputusan : GAGAL TOLAK H0")
    if (kesimpulan_gagal != "")
      cat("\nKesimpulan:", kesimpulan_gagal)
  }

  cat("\n")
}


# ============================================================
# 3. UJI NORMALITAS MULTIVARIAT
#    Henze-Zirkler
# ============================================================

cat("\n\n====================================================")
cat("\n1. UJI NORMALITAS MULTIVARIAT")
cat("\n====================================================\n")

if (!require(MVN)) install.packages("MVN")
library(MVN)

hasil_normal <- hz(X)

print(hasil_normal)

p_normal <- hasil_normal$p.value

keputusan(
  p_normal,
  alpha,
  "Data tidak berdistribusi normal multivariat.",
  "Data berdistribusi normal multivariat."
)


# ============================================================
# 4. UJI NORMALITAS MULTIVARIAT PER KELOMPOK
# ============================================================

cat("\n\n====================================================")
cat("\n2. NORMALITAS MULTIVARIAT PER KELOMPOK")
cat("\n====================================================\n")

for (g in levels(G)) {

  cat("\n--------------------------------------------")
  cat("\nKelompok:", g)
  cat("\n--------------------------------------------\n")

  Xg <- data[G == g, variabel]

  hasil_g <- hz(Xg)

  print(hasil_g)

  p_g <- hasil_g$p.value

  keputusan(
    p_g,
    alpha,
    paste("Kelompok", g,
          "tidak berdistribusi normal multivariat."),
    paste("Kelompok", g,
          "berdistribusi normal multivariat.")
  )
}


# ============================================================
# 5. MAHALANOBIS DISTANCE + CHI-SQUARE
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

print(D2)

p <- ncol(X)

chi50 <- qchisq(
  0.50,
  df = p
)

cat("\nChi-Square 50% =", chi50)

proporsi <- mean(D2 <= chi50)

cat("\nProporsi data dalam kontur 50% =", proporsi)
cat("\nPersentase =", proporsi * 100, "%\n")

if (abs(proporsi - 0.50) <= 0.10) {

  cat("Kesimpulan: Proporsi mendekati 50%.\n")
  cat("Normalitas multivariat didukung berdasarkan pendekatan ini.\n")

} else {

  cat("Kesimpulan: Proporsi cukup jauh dari 50%.\n")
  cat("Terdapat indikasi penyimpangan normalitas multivariat.\n")
}


# ============================================================
# 6. CHI-SQUARE Q-Q PLOT
# ============================================================

cat("\n\n====================================================")
cat("\n4. CHI-SQUARE Q-Q PLOT")
cat("\n====================================================\n")

n <- nrow(X)

prob <- (1:n - 0.5) / n

chi_square <- qchisq(
  prob,
  df = p
)

plot(
  chi_square,
  sort(D2),
  xlab = "Chi-Square Quantiles",
  ylab = "Squared Mahalanobis Distance",
  main = "Chi-Square Q-Q Plot"
)

abline(0, 1)


# ============================================================
# 7. UJI HOMOGENITAS MATRIKS KOVARIANS
#    BOX'S M TEST
# ============================================================

cat("\n\n====================================================")
cat("\n5. UJI HOMOGENITAS MATRIKS KOVARIANS")
cat("\n====================================================\n")

if (!require(biotools)) install.packages("biotools")
library(biotools)

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
# 8. HOTELLING T2 - ONE SAMPLE
# ============================================================

cat("\n\n====================================================")
cat("\n6. HOTELLING'S T2 - ONE SAMPLE")
cat("\n====================================================\n")

if (!require(ICSNP)) install.packages("ICSNP")
library(ICSNP)

# GANTI sesuai vektor mean yang dihipotesiskan
mu0 <- c(75, 80, 78)

hasil_one <- HotellingsT2(
  X,
  mu = mu0
)

print(hasil_one)

p_one <- hasil_one$p.value

keputusan(
  p_one,
  alpha,
  "Terdapat perbedaan vektor rata-rata dengan mu0.",
  "Tidak terdapat perbedaan vektor rata-rata dengan mu0."
)


# ============================================================
# 9. HOTELLING T2 - TWO SAMPLE
# ============================================================

cat("\n\n====================================================")
cat("\n7. HOTELLING'S T2 - TWO SAMPLE")
cat("\n====================================================\n")

if (!require(Hotelling)) install.packages("Hotelling")
library(Hotelling)

# GANTI nama kelompok jika bukan A1 dan A2
X1 <- data[G == "A1", variabel]
X2 <- data[G == "A2", variabel]

hasil_two <- hotelling.test(
  X1,
  X2,
  var.equal = TRUE
)

print(hasil_two)

# P-value Hotelling
p_two <- hasil_two$pval

keputusan(
  p_two,
  alpha,
  "Terdapat perbedaan vektor rata-rata kedua populasi.",
  "Tidak terdapat perbedaan vektor rata-rata kedua populasi."
)


# ============================================================
# 10. MANOVA
# ============================================================

cat("\n\n====================================================")
cat("\n8. MANOVA")
cat("\n====================================================\n")

# Membuat formula otomatis
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
# 11. MANOVA - WILKS' LAMBDA
# ============================================================

cat("\n\n====================================================")
cat("\n9. MANOVA - WILKS' LAMBDA")
cat("\n====================================================\n")

hasil_wilks <- summary(
  model_manova,
  test = "Wilks"
)

print(hasil_wilks)

p_wilks <- hasil_wilks$stats[1, "Pr(>F)"]

keputusan(
  p_wilks,
  alpha,
  "Terdapat perbedaan multivariat antar kelompok.",
  "Tidak terdapat perbedaan multivariat antar kelompok."
)


# ============================================================
# 12. MANOVA - PILLAI'S TRACE
# ============================================================

cat("\n\n====================================================")
cat("\n10. MANOVA - PILLAI'S TRACE")
cat("\n====================================================\n")

hasil_pillai <- summary(
  model_manova,
  test = "Pillai"
)

print(hasil_pillai)

p_pillai <- hasil_pillai$stats[1, "Pr(>F)"]

keputusan(
  p_pillai,
  alpha,
  "Terdapat perbedaan multivariat antar kelompok.",
  "Tidak terdapat perbedaan multivariat antar kelompok."
)


# ============================================================
# 13. MANOVA - HOTELLING-LAWLEY TRACE
# ============================================================

cat("\n\n====================================================")
cat("\n11. MANOVA - HOTELLING-LAWLEY TRACE")
cat("\n====================================================\n")

hasil_hl <- summary(
  model_manova,
  test = "Hotelling-Lawley"
)

print(hasil_hl)

p_hl <- hasil_hl$stats[1, "Pr(>F)"]

keputusan(
  p_hl,
  alpha,
  "Terdapat perbedaan multivariat antar kelompok.",
  "Tidak terdapat perbedaan multivariat antar kelompok."
)


# ============================================================
# 14. MANOVA - ROY'S LARGEST ROOT
# ============================================================

cat("\n\n====================================================")
cat("\n12. MANOVA - ROY'S LARGEST ROOT")
cat("\n====================================================\n")

hasil_roy <- summary(
  model_manova,
  test = "Roy"
)

print(hasil_roy)

p_roy <- hasil_roy$stats[1, "Pr(>F)"]

keputusan(
  p_roy,
  alpha,
  "Terdapat perbedaan multivariat antar kelompok.",
  "Tidak terdapat perbedaan multivariat antar kelompok."
)


# ============================================================
# 15. ANOVA UNIVARIAT SETELAH MANOVA
# ============================================================

cat("\n\n====================================================")
cat("\n13. ANOVA UNIVARIAT LANJUTAN")
cat("\n====================================================\n")

hasil_aov <- summary.aov(model_manova)

print(hasil_aov)


# ============================================================
# 16. TWO-WAY MANOVA
# ============================================================

cat("\n\n====================================================")
cat("\n14. TWO-WAY MANOVA")
cat("\n====================================================\n")

# GANTI nama Faktor1 dan Faktor2
# Jika tidak membutuhkan Two-Way MANOVA, bagian ini bisa dilewati.

if ("Faktor1" %in% names(data) &&
    "Faktor2" %in% names(data)) {

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

  print(model_two_way)


  # ----------------------------------------------------------
  # 16a. TWO-WAY MANOVA - WILKS
  # ----------------------------------------------------------

  cat("\n\n--- TWO-WAY MANOVA: WILKS ---\n")

  hasil_two_wilks <- summary(
    model_two_way,
    test = "Wilks"
  )

  print(hasil_two_wilks)

  p_two_wilks <- hasil_two_wilks$stats[, "Pr(>F)"]

  hasil_keputusan <- data.frame(
    Efek = rownames(hasil_two_wilks$stats),
    P_Value = p_two_wilks,
    Keputusan = ifelse(
      p_two_wilks < alpha,
      "TOLAK H0",
      "GAGAL TOLAK H0"
    )
  )

  print(hasil_keputusan)


  # ----------------------------------------------------------
  # 16b. TWO-WAY MANOVA - PILLAI
  # ----------------------------------------------------------

  cat("\n\n--- TWO-WAY MANOVA: PILLAI ---\n")

  hasil_two_pillai <- summary(
    model_two_way,
    test = "Pillai"
  )

  print(hasil_two_pillai)

  p_two_pillai <- hasil_two_pillai$stats[, "Pr(>F)"]

  hasil_keputusan_pillai <- data.frame(
    Efek = rownames(hasil_two_pillai$stats),
    P_Value = p_two_pillai,
    Keputusan = ifelse(
      p_two_pillai < alpha,
      "TOLAK H0",
      "GAGAL TOLAK H0"
    )
  )

  print(hasil_keputusan_pillai)


  # ----------------------------------------------------------
  # 16c. ANOVA LANJUTAN TWO-WAY MANOVA
  # ----------------------------------------------------------

  cat("\n\n--- ANOVA LANJUTAN TWO-WAY MANOVA ---\n")

  print(summary.aov(model_two_way))

} else {

  cat("\nKolom Faktor1/Faktor2 tidak ditemukan.")
  cat("\nTwo-Way MANOVA dilewati.\n")
}


# ============================================================
# 17. RINGKASAN ATURAN KEPUTUSAN
# ============================================================

cat("\n\n====================================================")
cat("\nRINGKASAN ATURAN KEPUTUSAN")
cat("\n====================================================\n")

cat("
Jika p-value < 0.05
-> TOLAK H0

Jika p-value >= 0.05
-> GAGAL TOLAK H0

----------------------------------------------------

NORMALITAS:
H0 = Data berdistribusi normal multivariat

BOX'S M:
H0 = Matriks kovarians homogen

HOTELLING T2 ONE SAMPLE:
H0 = μ = μ0

HOTELLING T2 TWO SAMPLE:
H0 = μ1 = μ2

MANOVA:
H0 = Tidak terdapat perbedaan vektor mean antar kelompok

TWO-WAY MANOVA:

Faktor 1:
H0 = Tidak terdapat efek Faktor 1

Faktor 2:
H0 = Tidak terdapat efek Faktor 2

Interaksi:
H0 = Tidak terdapat efek interaksi Faktor 1 × Faktor 2
")

cat("\n====================================================")
cat("\nSELESAI")
cat("\n====================================================\n")
