# ============================================================
#   ANALIZA DESKRIPTIVE — ÇMIMET E PRODUKTEVE
#   R Studio | E-commerce Price Analysis
# ============================================================

library(ggplot2)
library(dplyr)
library(scales)

df <- read.csv("C:/Users/COMPUTER SHOP/OneDrive/Desktop/FikrijeIdrizi_DeepLearning/price_data_clean.csv",
               stringsAsFactors = FALSE)
df$date <- as.Date(df$date)

cat("====================================================\n")
cat("  [1] STATISTIKA DESKRIPTIVE\n")
cat("====================================================\n")
print(summary(df[, c("own_price", "competitor_price", "discount_rate",
                      "stock_level", "price_change_rate", "price_volatility")]))

cat("\n  [2] Çmimi mesatar sipas kategorisë:\n")
cat_stats <- df %>%
  group_by(category) %>%
  summarise(
    n            = n(),
    avg_price    = round(mean(own_price), 2),
    min_price    = round(min(own_price), 2),
    max_price    = round(max(own_price), 2),
    avg_discount = round(mean(discount_rate) * 100, 2)
  ) %>% arrange(desc(avg_price))
print(cat_stats)

cat("\n  [3] Frekuenca e nivelit të kërkesës:\n")
demand_freq <- table(df$demand_level)
print(demand_freq)
print(round(prop.table(demand_freq) * 100, 2))

cat("\n  [4] Çmimi mesatar sipas sezonit:\n")
season_stats <- df %>%
  group_by(season) %>%
  summarise(
    avg_price    = round(mean(own_price), 2),
    avg_discount = round(mean(discount_rate) * 100, 2),
    promotions   = sum(promotion_flag),
    avg_stock    = round(mean(stock_level), 0)
  )
print(season_stats)

# ── Grafiqet ─────────────────────────────────────────────────
p1 <- ggplot(cat_stats, aes(x = reorder(category, avg_price),
                             y = avg_price, fill = category)) +
  geom_bar(stat = "identity", width = 0.6) +
  coord_flip() +
  scale_y_continuous(labels = dollar_format(prefix = "$")) +
  labs(title = "Çmimi Mesatar sipas Kategorisë",
       x = "Kategoria", y = "Çmimi Mesatar ($)") +
  theme_minimal() + theme(legend.position = "none",
                          plot.title = element_text(face = "bold", size = 14))
ggsave("C:/Users/COMPUTER SHOP/OneDrive/Desktop/FikrijeIdrizi_DeepLearning/grafik_kategoria.png",
       p1, width = 8, height = 5, dpi = 150)

demand_df <- as.data.frame(demand_freq)
names(demand_df) <- c("demand_level", "count")
p2 <- ggplot(demand_df, aes(x = demand_level, y = count, fill = demand_level)) +
  geom_bar(stat = "identity", width = 0.5) +
  scale_fill_manual(values = c("Low" = "#e74c3c", "Medium" = "#f39c12", "High" = "#27ae60")) +
  labs(title = "Shpërndarje e Nivelit të Kërkesës",
       x = "Niveli i Kërkesës", y = "Numri i Rekordeve") +
  theme_minimal() + theme(legend.position = "none",
                          plot.title = element_text(face = "bold", size = 14))
ggsave("C:/Users/COMPUTER SHOP/OneDrive/Desktop/FikrijeIdrizi_DeepLearning/grafik_kerkesa.png",
       p2, width = 7, height = 5, dpi = 150)

p3 <- ggplot(df, aes(x = season, y = own_price, fill = season)) +
  geom_boxplot(alpha = 0.7) +
  scale_y_continuous(labels = dollar_format(prefix = "$")) +
  scale_fill_manual(values = c("Winter"="#74b9ff","Spring"="#55efc4",
                                "Summer"="#fdcb6e","Fall"="#e17055")) +
  labs(title = "Shpërndarje e Çmimit sipas Sezonit",
       x = "Sezoni", y = "Çmimi ($)") +
  theme_minimal() + theme(legend.position = "none",
                          plot.title = element_text(face = "bold", size = 14))
ggsave("C:/Users/COMPUTER SHOP/OneDrive/Desktop/FikrijeIdrizi_DeepLearning/grafik_sezoni.png",
       p3, width = 8, height = 5, dpi = 150)

cat("\n  3 grafiqe u ruajten!\n")
cat("  Analiza deskriptive u krye me sukses!\n")
cat("====================================================\n")

