set.seed(123)

# =========================
# 1) PARAMETRELER
# =========================

# Her Monte Carlo tekrarındaki gözlem sayıları
n_vec <- c(100, 500, 1000, 5000)

# Her senaryonun Monte Carlo tekrar sayıları
n_rep_vec <- c(100, 500, 1000)

mean_vec <- c(124, 125, 126, 127, 128)
sd_vec   <- c(0.2, 0.5, 1, 2)

threshold <- 126
precision_levels <- c(0, 1, 2)


# =========================
# 2) SONUÇLAR İÇİN LİSTE
# =========================

results_list <- list()
counter <- 1


# =========================
# 3) MONTE CARLO SİMÜLASYONU
# =========================

for (n in n_vec) {

  for (n_rep in n_rep_vec) {

    for (mu in mean_vec) {

      for (sigma in sd_vec) {

        for (p in precision_levels) {

          mc_errors <- replicate(n_rep, {

            glucose <- rnorm(
              n = n,
              mean = mu,
              sd = sigma
            )

            # Yuvarlama öncesi gerçek sınıflama
            clean_class <- glucose >= threshold

            # Ölçüm hassasiyetine göre yuvarlama
            glucose_round <- round(glucose, digits = p)

            # Yuvarlanmış değerlere göre sınıflama
            pred_class <- glucose_round >= threshold

            # Yanlış sınıflandırma oranı
            mean(clean_class != pred_class)
          })


          # =========================
          # 4) ÖZET İSTATİSTİKLER
          # =========================

          error_mean <- mean(mc_errors)
          error_sd   <- sd(mc_errors)

          # Ortalama hata tahmininin Monte Carlo standart hatası
          mc_se <- error_sd / sqrt(n_rep)

          # Tekrarlar arasında gözlenen hata oranlarının %95 ampirik aralığı
          empirical_low <- unname(
            quantile(mc_errors, probs = 0.025)
          )

          empirical_high <- unname(
            quantile(mc_errors, probs = 0.975)
          )

          # Ortalama hata tahmini için yaklaşık %95 güven aralığı
          mean_ci_low <- max(
            0,
            error_mean - 1.96 * mc_se
          )

          mean_ci_high <- min(
            1,
            error_mean + 1.96 * mc_se
          )


          # =========================
          # 5) SONUÇLARI KAYDET
          # =========================

          results_list[[counter]] <- data.frame(
            n = n,
            n_rep = n_rep,
            mean = mu,
            sd = sigma,
            precision = p,
            error_mean = error_mean,
            error_sd = error_sd,
            mc_se = mc_se,
            empirical_low = empirical_low,
            empirical_high = empirical_high,
            mean_ci_low = mean_ci_low,
            mean_ci_high = mean_ci_high
          )

          counter <- counter + 1
        }
      }
    }
  }
}


# =========================
# 6) SONUÇLARI BİRLEŞTİR
# =========================

results <- do.call(rbind, results_list)

# Satır isimlerini temizle
rownames(results) <- NULL

print(results)