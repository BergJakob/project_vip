library(tidyverse)
library(readxl)
library(stringr)

#GER

full_dataset_ger <- read_csv2("full_dataset_ger.csv")

übersicht_ger <- read_excel("übersicht.xlsx")

full_dataset_ger <- full_dataset_ger %>%
  left_join(übersicht %>% 
              select(SourceFile, Follower), # Wählt nur die benötigten Spalten aus df_ger
            by = "SourceFile")

full_dataset_ger <- full_dataset_ger %>%
  mutate(Follower = if_else(post_source_domain == "https://www.instagram.com/matthiasschweighoefer/", 1385308, Follower))

na_ger <- full_dataset_ger[is.na(full_dataset_ger$Follower), ] %>% 
  select(post_source_domain, SourceFile)

write.csv(full_dataset_ger, file = "full_dataset_ger.csv")

#USA

full_dataset_usa <- read_csv2("full_dataset_usa.csv")

übersicht <- read_excel("übersicht.xlsx", sheet = 2)

full_dataset_usa <- full_dataset_usa %>%
  left_join(übersicht %>% 
              select(SourceFile, Follower), # Wählt nur die benötigten Spalten aus df_ger
            by = "SourceFile")

na_usa <- full_dataset_usa[is.na(full_dataset_usa$Follower), ] %>% 
  select(post_source_domain, SourceFile)

write.csv(full_dataset_usa, file = "full_dataset_usa.csv")



