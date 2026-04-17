install.packages("Matrix")
install.packages("lme4")
install.packages("lmerTest")
install.packages("sjPlot")
install.packages("tidyverse")
install.packages("readxl")
install.packages("stringr")
install.packages("car")
install.packages("DHARMa")
                 
library(sjPlot)
library(tidyverse)
library(readxl)
library(stringr)
library(car)
library(lme4)
library(lmerTest)
library(Matrix)

full_dataset_ger <- read.csv("Data/VIP/data_vip_ger_final.csv")

full_dataset_ger <- full_dataset_ger %>%
  select(-X)

# GER

full_dataset_ger <- full_dataset_ger %>%
  mutate(
    total_engagement = num_likes + num_comments,
    interaction_rate = (total_engagement / num_follower)
  ) %>% 
  select(-total_engagement)

full_dataset_ger$log_rate <- log(full_dataset_ger$interaction_rate + 0.001)

write.csv(full_dataset_ger, file = "full_dataset_ger.csv")

# 1. Annahme: Normalverteilung der AV

full_dataset_ger$log_rate <- as.numeric(as.character(full_dataset_ger$log_rate))

# Für politische Posts
shapiro.test(full_dataset_ger$log_rate[full_dataset_ger$pol == 1])

# Für unpolitische Posts
shapiro.test(full_dataset_ger$log_rate[full_dataset_ger$pol == 0])

# Optional: Histogramme
hist(full_dataset_ger$log_rate[full_dataset_ger$pol == 1],
     main = "Politische Posts", xlab = "log(Interaktionsrate)")
hist(full_dataset_ger$log_rate[full_dataset_ger$pol == 0],
     main = "Unpolitische Posts", xlab = "log(Interaktionsrate)")

# 2. Annahme Varianzhomogenität

full_dataset_ger$pol <- as.factor(full_dataset_ger$pol)

# Test durchführen
leveneTest(log_rate ~ pol, data = full_dataset_ger)

# 3. Annahme Unabhängigkeit

full_dataset_ger %>%
  group_by(acc_name, pol) %>%
  summarise(anzahl_posts = n()) %>%
  arrange(desc(anzahl_posts))

# Explorativer T-Test

t.test(log_rate ~ pol, data = full_dataset_ger, var.equal = FALSE)

#wilcox.test(log_rate ~ pol, data = full_dataset_ger)

# Mixed-Effects-Modell

# Modell durchführen

# Einfaches Modell: fester Effekt für pol, zufälliger Effekt für Promi-ID
model <- lmer(log_rate ~ pol + (1 | acc_name), data = full_dataset_ger)

summary(model)

exp(-0.15)

plot_model(model, type = "pred", terms = "pol")

# 1. Prüfung Normalverteilung der Residuen

resid <- residuals(model)

hist(resid, main = "Histogramm der Residuen", xlab = "Residuen")

qqnorm(resid)
qqline(resid)

#install.packages("DHARMa")
library(DHARMa)
sim_resid <- simulateResiduals(model)
plot(sim_resid)

# 2. Prüfung Homoskedastizitä

plot(fitted(model), resid)

# GRAFIKEN

# 1. Falls pol ein Factor oder Char ist → zuerst sauber in numeric umwandeln
full_dataset_ger$pol <- as.numeric(as.character(full_dataset_ger$pol))

# 2. Dann korrekt als Faktor labeln
full_dataset_ger$pol <- factor(full_dataset_ger$pol,
                               levels = c(0, 1),
                               labels = c("non-political", "political"))

# Violinplot
ggplot(full_dataset_ger, aes(x = pol, 
                             y = log_rate, 
                             fill = pol)) +
  geom_violin(trim = FALSE, alpha = 0.7, color = "gray30") +
  stat_summary(fun = median, geom = "point", size = 3, color = "black") +
  stat_summary(fun = median, geom = "crossbar", 
               width = 0.5, fatten = 0, color = "black", size = 0.3) +
  scale_fill_brewer(palette = "Set2") +
  labs(
    title = "Interaction Rate of Political vs. Non-Political Posts",
    x = "",
    y = "log-transformed Interaction Rate"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold")
  )


library(broom.mixed)
library(ggplot2)
library(dplyr)

# Modell tidy machen (nur Fixed Effects)
coef_df <- tidy(model_lmm, effects = "fixed") %>%
  mutate(
    term = recode(term,
                  "(Intercept)" = "Intercept",
                  "pol1" = "Politischer Inhalt"),
    # Konfidenzintervalle berechnen (Wald-Konfidenzintervalle)
    conf.low = estimate - 1.96 * std.error,
    conf.high = estimate + 1.96 * std.error
  )

# Coefficient Plot
ggplot(coef_df, aes(x = estimate, y = term)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
  labs(
    title = "Effekt politischer Inhalte auf die Interaktionsrate",
    subtitle = "Mixed-Effects-Modell mit zufälligen Interzepten pro Account",
    x = "Regressionskoeffizient (log-transformierte Interaktionsrate)",
    y = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold"),
    axis.title.x = element_text(margin = margin(t = 10))
  )

install.packages("broom.mixed")
library(broom.mixed)

# Modell tidy machen (nur Fixed Effects)
coef_df <- tidy(model, effects = "fixed") %>%
  mutate(
    term = recode(term,
                  "(Intercept)" = "Intercept",
                  "pol" = "Politischer Inhalt"),
    # Konfidenzintervalle berechnen (Wald-Konfidenzintervalle)
    conf.low = estimate - 1.96 * std.error,
    conf.high = estimate + 1.96 * std.error
  )

# Coefficient Plot

coef_df <- tidy(model, effects = "fixed") %>%
  mutate(
    term = case_when(
      term == "(Intercept)" ~ "Intercept",
      term == "pol" ~ "Politischer Inhalt",
      TRUE ~ term
    ),
    # Wald-Konfidenzintervalle
    conf.low = estimate - 1.96 * std.error,
    conf.high = estimate + 1.96 * std.error
  )

ggplot(coef_df, aes(x = estimate, y = term)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
  labs(
    title = "Effekt politischer Inhalte auf die Interaktionsrate",
    subtitle = "Mixed-Effects-Modell mit zufälligen Interzepten pro Account",
    x = "Regressionskoeffizient (log-transformierte Interaktionsrate)",
    y = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold"),
    axis.title.x = element_text(margin = margin(t = 10))
  )

df %>% filter(is.na(vip))

library(ggplot2)

# res v fit

# Residuen + Fitted Values extrahieren
df_resid_h3 <- data.frame(
  fitted = fitted(model),
  residuals = resid(model)
)

# Plot
ggplot(df_resid_h3, aes(x = fitted, y = residuals)) +
  geom_point(alpha = 0.3, size = 1, color = "black") +
  geom_smooth(method = "loess", se = FALSE, color = "red", size = 0.8) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  labs(
    title = "Residuals-vs-Fitted-Plot",
    subtitle = "Mixed-Effects-Modell: log-transformierte Interaktionsrate",
    x = "Fitted Values",
    y = "Residuals"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold")
  )

# QQ

# Residuen extrahieren
df_qq_h3 <- data.frame(
  residuals = resid(model)
)

# Q-Q-Plot
ggplot(df_qq_h3, aes(sample = residuals)) +
  stat_qq(color = "black", alpha = 0.7, size = 1) +
  stat_qq_line(color = "red", size = 0.8) +
  labs(
    title = "Normal Q-Q-Plot der Residuen",
    subtitle = "Mixed-Effects-Modell: log-transformierte Interaktionsrate",
    x = "Theoretische Quantile",
    y = "Empirische Residuen"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold")
  )


full_dataset_ger %>%
  group_by(pol) %>%
  summarise(
    n = n(),
    mean = mean(log_rate, na.rm = TRUE),
    median = median(log_rate, na.rm = TRUE),
    sd = sd(log_rate, na.rm = TRUE)
  )

full_dataset_ger %>%
  group_by(pol) %>%
  summarise(
    n = n(),
    mean = round(mean(log_rate, na.rm = TRUE), 4),
    median = round(median(log_rate, na.rm = TRUE), 4),
    sd = round(sd(log_rate, na.rm = TRUE), 4)
  )

options(digits = 4)

print(
  full_dataset_ger %>%
    group_by(pol) %>%
    summarise(
      n = n(),
      mean = round(mean(log_rate, na.rm = TRUE), 4),
      median = round(median(log_rate, na.rm = TRUE), 4),
      sd = round(sd(log_rate, na.rm = TRUE), 4)
    ),
  digits = 4
)

full_dataset_ger %>%
  group_by(pol) %>%
  summarise(
    n = n(),
    mean = format(round(mean(log_rate, na.rm = TRUE), 4), nsmall = 4),
    median = format(round(median(log_rate, na.rm = TRUE), 4), nsmall = 4),
    sd = format(round(sd(log_rate, na.rm = TRUE), 4), nsmall = 4)
  )