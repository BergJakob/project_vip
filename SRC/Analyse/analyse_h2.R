library(tidyverse)
library(readxl)
library(stringr)

full_dataset_ger <- read.csv("Data/VIP/data_vip_ger_final.csv")

full_dataset_usa <- read.csv("Data/VIP/data_vip_usa_final.csv", sep = ";")

# DESKRIPTIV

full_dataset_ger <- full_dataset_ger %>%
  mutate(
    total_engagement = num_likes + num_comments,
    interaction_rate = (total_engagement / num_follower)
  ) %>% 
  select(-total_engagement)

mean(full_dataset_ger$interaction_rate)
sd(full_dataset_ger$interaction_rate)
median(full_dataset_ger$interaction_rate)

full_dataset_ger_pol <- full_dataset_ger %>%
  filter(pol == 1)

mean(full_dataset_ger_pol$interaction_rate)
sd(full_dataset_ger_pol$interaction_rate)
median(full_dataset_ger_pol$interaction_rate)

full_dataset_ger_not_pol <- full_dataset_ger %>%
  filter(pol == 0)

mean(full_dataset_ger_not_pol$interaction_rate)
sd(full_dataset_ger_not_pol$interaction_rate)
median(full_dataset_ger_not_pol$interaction_rate)

full_dataset_ger_person <- full_dataset_ger %>%
  filter(person == 1)

mean(full_dataset_ger_person$interaction_rate)
sd(full_dataset_ger_person$interaction_rate)
median(full_dataset_ger_person$interaction_rate)

full_dataset_ger_partei <- full_dataset_ger %>%
  filter(partei == 1)

mean(full_dataset_ger_partei$interaction_rate)
sd(full_dataset_ger_partei$interaction_rate)
median(full_dataset_ger_partei$interaction_rate)

full_dataset_ger_inhalt <- full_dataset_ger %>%
  filter(inhalt == 1)

mean(full_dataset_ger_inhalt$interaction_rate)
sd(full_dataset_ger_inhalt$interaction_rate)
median(full_dataset_ger_inhalt$interaction_rate)

# INFERENZ (Multi-Regression)

# 1. Log trans

full_dataset_ger$log_rate <- log(full_dataset_ger$interaction_rate + 0.001)

# 2. Modellschätzung

model <- lm(log_rate ~ inhalt + person + partei, data = full_dataset_ger)

# 3. Annahmen-Prüfung

# 3.1. Linearität

plot(model, which = 1)

# 3.2. Homoskedaszidität

#siehe 3.1.

# 3.3 Normalverteilung der R

plot(model, which = 2)

# 3.4 Unahängigkeit der R

install.packages("lmtest") 
library(lmtest)

dwtest(model)

# 4 Regression

library(lmtest)
library(sandwich)

coeftest(model, vcov = NeweyWest(model, lag = 5, prewhite = FALSE))

summary(model)

# GRAFIKEN

library(dplyr)
library(tidyr)
library(ggplot2)

# Schritt 1: ins Long-Format transformieren
df_long <- full_dataset_ger %>%
  pivot_longer(
    cols = c(person, inhalt, partei),
    names_to = "category",
    values_to = "value"
  ) %>%
  filter(value == 1) %>%          # nur zutreffende Kategorie
  select(-value)                  # Dummy-Spalte entfernen

# Kategorien für bessere Lesbarkeit umbenennen (optional)
df_long$category <- factor(df_long$category,
                           levels = c("inhalt", "person", "partei"),
                           labels = c("Issue", "Person", "Party"))

# Schritt 2: Violinplot zeichnen
ggplot(df_long, aes(x = category, y = log_rate, fill = category)) +
  geom_violin(trim = FALSE, alpha = 0.7, color = "gray30") +
  stat_summary(fun = median, geom = "point", size = 3, color = "black") +
  stat_summary(fun = median, geom = "crossbar", width = 0.5, fatten = 0,
               color = "black", size = 0.3) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Interaction Rate by Presentation Type of Political Communication",
    x = "",
    y = "log-transformied Interaction Rate"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 14)
  ) 



library(broom)
library(sandwich)
library(lmtest)
library(ggplot2)
library(dplyr)

# Robuste (Newey-West) Standardfehler berechnen
nw_se <- sqrt(diag(NeweyWest(model, lag = 5)))

# Tidy-Datensatz aus Koeffizienten + robuste SEs
coef_df <- broom::tidy(model) %>%
  mutate(
    std.error = nw_se,
    conf.low = estimate - 1.96 * std.error,
    conf.high = estimate + 1.96 * std.error
  ) %>%
  # nur die drei Kategorien (keine Intercept)
  filter(term %in% c("inhalt", "person", "partei")) %>%
  mutate(
    term = recode(term,
                  "inhalt" = "issue",
                  "person" = "person",
                  "partei" = "party")
  )

# Coefficient Plot
ggplot(coef_df, aes(x = estimate, y = term)) +
  geom_point(size = 3, color = "black") +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 height = 0.2, color = "black") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  labs(
    title = "Effects of Presentation Type on the Interaction Rate",
    subtitle = "Linear Regression Model with Newey–West Robust Standard Errors",
    x = "Regression Coefficient (log‑transformed Interaction Rate)",
    y = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 14),
    axis.title.x = element_text(margin = margin(t = 10))
  ) + scale_x_continuous(
    breaks = seq(-0.4, 1.0, by = 0.1),
    labels = seq(-0.4, 1.0, by = 0.1)
  )

## Res v Fitted


# Residuen und fitted values extrahieren
df_resid <- data.frame(
  fitted = fitted(model),
  residuals = resid(model)
)

# Plot
ggplot(df_resid, aes(x = fitted, y = residuals)) +
  geom_point(alpha = 0.3, size = 1, color = "black") +
  geom_smooth(method = "loess", se = FALSE, color = "red", size = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  labs(
    title = "Residuals vs. Fitted",
    subtitle = "Diagnostikplot zum Regressionsmodell",
    x = "Fitted Values",
    y = "Residuals"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 14)
  )

## QQ

# Residuen extrahieren
df_qq <- data.frame(
  residuals = resid(model)
)

# Q-Q-Plot
ggplot(df_qq, aes(sample = residuals)) +
  stat_qq(color = "black", alpha = 0.7, size = 1) +
  stat_qq_line(color = "red", size = 0.8) +
  labs(
    title = "Normal Q–Q Plot der Residuen",
    subtitle = "Diagnostikplot zum Regressionsmodell",
    x = "Theoretische Quantile",
    y = "Empirische Residuen"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", size = 14)
  )





