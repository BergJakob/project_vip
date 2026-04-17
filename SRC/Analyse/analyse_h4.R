library(sjPlot)
library(tidyverse)
library(readxl)
library(stringr)
library(car)
library(lme4)
library(lmerTest)
library(Matrix)
library(DHARMa)

df <- read_csv("Data/POL/CLASS/final_df_btw_class.csv")

df$log_rate <- log(df$interaction_rate + 0.001)

t.test(log_rate ~ vip, data = df, var.equal = FALSE)

# Mixed-Effects-Modell

# Modell durchführen

# Einfaches Modell: fester Effekt für pol, zufälliger Effekt für Promi-ID
model <- lmer(log_rate ~ vip + (1 | acc_name), data = df)

summary(model)

exp(0.3669)

plot_model(model, type = "pred", terms = "vip")

# 1. Prüfung Normalverteilung der Residuen

resid <- residuals(model)

hist(resid, main = "Histogramm der Residuen", xlab = "Residuen")

qqnorm(resid)
qqline(resid)

sim_resid <- simulateResiduals(model)
plot(sim_resid)

# 2. Prüfung Homoskedastizität

plot(fitted(model), resid)

# Data-Manage

df <- df %>%
  mutate(vip = ifelse(id == "DGOlvOAtKUw", 0, vip))

df <- df %>%
  mutate(vip_raw = ifelse(id == "DGOlvOAtKUw", 0, vip_raw))

df <- df %>%
  mutate(changed = ifelse(id == "DGOlvOAtKUw", 0, changed))

# GRAFIKEN

library(ggplot2)
library(dplyr)

# Faktorvariable für VIP-Bezug erzeugen
df$vip <- factor(df$vip,
                 levels = c(0, 1),
                 labels = c("no Support", "Support"))

# Violinplot
ggplot(df, aes(x = vip, y = log_rate, fill = vip)) +
  geom_violin(trim = FALSE, alpha = 0.7, color = "gray30") +
  stat_summary(fun = median, geom = "point", size = 3, color = "black") +
  stat_summary(fun = median, geom = "crossbar",
               width = 0.5, fatten = 0, size = 0.3, color = "black") +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Interaction Rate by Support from Prominent 
    Opinion Leaders",
    x = "",
    y = "log-transformed Interaction Rate"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold")
  )

#qq

# Residuen extrahieren
df_qq <- data.frame(residuals = resid(model))

# Q-Q-Plot
ggplot(df_qq, aes(sample = residuals)) +
  stat_qq(color = "black", alpha = 0.7, size = 1) +
  stat_qq_line(color = "red", size = 0.8) +
  labs(
    title = "Normal Q–Q-Plot der Residuen",
    subtitle = "Mixed-Effects-Modell: log-transformierte Interaktionsrate",
    x = "Theoretische Quantile",
    y = "Empirische Residuen"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold")
  )

# res v fit

library(ggplot2)

# Residuen und Fitted Values extrahieren
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
    title = "Residuals-vs-Fitted-Plot",
    subtitle = "Mixed-Effect-Modell: log-transformierte Interaktionsrate",
    x = "Fitted Values",
    y = "Residuals"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold")
  )

library(ggplot2)
library(dplyr)

# Nur gültige VIP-Werte (falls noch NA vorhanden)
df_jit <- df %>% filter(!is.na(vip))

# Jitterplot
ggplot(df_jit, aes(x = vip, y = log_rate, color = vip)) +
  geom_jitter(width = 0.15, alpha = 0.15, size = 0.8) +
  scale_color_brewer(palette = "Set2") +
  labs(
    title = "Interaktionsrate nach VIP-Bezug",
    subtitle = "Jitterplot (H4, Explorative Analyse)",
    x = "",
    y = "Log-Interaktionsrate"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold")
  )

library(dplyr)

df %>%
  group_by(vip) %>%
  summarise(
    n = n(),
    mean_log = mean(log_rate, na.rm = TRUE),
    median_log = median(log_rate, na.rm = TRUE),
    sd_log = sd(log_rate, na.rm = TRUE),
    min_log = min(log_rate, na.rm = TRUE),
    max_log = max(log_rate, na.rm = TRUE)
  )

library(knitr)

df %>%
  group_by(vip) %>%
  summarise(
    n = n(),
    mean_log = round(mean(log_rate), 3),
    median_log = round(median(log_rate), 3),
    sd_log = round(sd(log_rate), 3)
  ) %>%
  kable(caption = "Deskriptive Statistik der log-transformierten Interaktionsrate (VIP vs. kein VIP)")

### Sesitivitätsanalyse

library(dplyr)

vip_vals <- df$log_rate[df$vip == 1]
non_vals <- df$log_rate[df$vip == 0]

n_vip_obs <- length(vip_vals)
recall <- 0.7271

n_vip_true <- n_vip_obs / recall
n_vip_missing <- round(n_vip_true - n_vip_obs)

cat("Fehlende VIP Fälle:", n_vip_missing, "\n")

mean_vip <- mean(vip_vals, na.rm = TRUE)
mean_non <- mean(non_vals, na.rm = TRUE)

sd_vip  <- sd(vip_vals, na.rm = TRUE)
sd_non  <- sd(non_vals, na.rm = TRUE)

# Robust falls 0-Varianz:
if (sd_vip == 0 | is.na(sd_vip)) sd_vip <- 0.01
if (sd_non == 0 | is.na(sd_non)) sd_non <- 0.01

set.seed(123)

scenarios <- list(
  low  = rnorm(n_vip_missing, mean_non - sd_non, sd_non),
  mid  = rnorm(n_vip_missing, mean_vip, sd_vip),
  high = rnorm(n_vip_missing, mean_vip + sd_vip, sd_vip)
)

log_effect <- function(vip_new) {
  mean(c(vip_vals, vip_new)) - mean(non_vals)
}

results <- lapply(scenarios, function(x) {
  est <- log_effect(x)
  tibble(
    log_estimate = est,
    increase = exp(est)
  )
})

bind_rows(results, .id = "scenario")