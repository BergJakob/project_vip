library(tidyverse)
library(readxl)
library(stringr)

full_dataset_ger <- read.csv("Data/VIP/data_vip_ger_final.csv")

full_dataset_usa <- read.csv("full_dataset_usa.csv")

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
