# ============================================================
#   ANALIZA STATISTIKORE (INFERENCIAL) — ÇMIMET E PRODUKTEVE
#   R Studio | E-commerce Price Analysis
# ============================================================

library(ggplot2)
library(dplyr)
library(corrplot)
library(scales)

df <- read.csv("C:/Users/COMPUTER SHOP/OneDrive/Desktop/FikrijeIdrizi_DeepLearning/price_data_clean.csv",
               stringsAsFactors = FALSE)
df$date <- as.Date(df$date)

# ── 1. Matrica e Korrelacionit ───────────────────────────────
cat("====================================================\n")
cat("  [1] MATRICA E KORRELACIONIT\n")
cat("====================================================\n")
num_cols <- df[, c("own_price", "competitor_price", "discount_rate",
                   "stock_level", "price_change_rate", "price_volatility",
                   "price_diff", "demand_encoded")]
cor_matrix <- round(cor(num_cols, use = "complete.obs"), 3)
print(cor_matrix)

png("C:/Users/COMPUTER SHOP/OneDrive/Desktop/FikrijeIdrizi_DeepLearning/grafik_korrelacioni.png",
    width = 900, height = 800, res = 120)
corrplot(cor_matrix, method = "color", type = "upper",
         tl.cex = 0.8, addCoef.col = "black", number.cex = 0.7,
         col = colorRampPalette(c("#e74c3c", "white", "#2980b9"))(200),
         title = "Matrica e Korrelacionit", mar = c(0,0,2,0))
dev.off()

# ── 2. Regresion Linear ──────────────────────────────────────
cat("\n====================================================\n")
cat("  [2] REGRESION LINEAR\n")
cat("====================================================\n")
model <- lm(own_price ~ competitor_price + discount_rate +
              stock_level + promotion_flag + demand_encoded +
              season_encoded, data = df)
print(summary(model))

# ── 3. Test Khi-Katror ───────────────────────────────────────
cat("\n====================================================\n")
cat("  [3] TEST KHI-KATROR\n")
cat("  Lidhja mes sezonit dhe nivelit te kerkeses\n")
cat("====================================================\n")
print(chisq.test(table(df$season, df$demand_level)))

# ── 4. Grafiku Mujor ─────────────────────────────────────────
monthly <- df %>%
  group_by(month) %>%
  summarise(avg_price = round(mean(own_price), 2))

p4 <- ggplot(monthly, aes(x = month, y = avg_price)) +
  geom_line(color = "#2980b9", linewidth = 1.2) +
  geom_point(color = "#e74c3c", size = 3) +
  scale_y_continuous(labels = dollar_format(prefix = "$")) +
  scale_x_continuous(breaks = 1:12,
                     labels = c("Jan","Shk","Mar","Pri","Maj","Qer",
                                "Kor","Gus","Sht","Tet","Nen","Dhj")) +
  labs(title = "Evolucioni i Çmimit Mesatar Mujor (2024)",
       x = "Muaji", y = "Çmimi Mesatar ($)") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", size = 14))
ggsave("C:/Users/COMPUTER SHOP/OneDrive/Desktop/FikrijeIdrizi_DeepLearning/grafik_mujor.png",
       p4, width = 9, height = 5, dpi = 150)

cat("\n  Grafiqet: grafik_korrelacioni.png, grafik_mujor.png\n")
cat("  Analiza statistikore u krye me sukses!\n")
cat("====================================================\n")

