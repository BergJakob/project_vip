library(tidyverse)
library(readxl)
library(stringr)

#GER

full_dataset_ger <- read.csv("full_dataset_ger.csv")

df_mismatch <- full_dataset_ger %>%
  filter(author != Accountname) %>% 
  select(url, author, Accountname)

df_mismatch <- df_mismatch %>%
  filter(!author %in% full_dataset_ger$Accountname)


df_mismatch <- df_mismatch[!df_mismatch$author %in% c("premierleague", "finanzfluss",
                                                       "easportsfcde", "easportsofficial",
                                                       "euro2024", "fifamuseum",
                                                       "europaleague", "galatasaraycologne"), ]

author_freq <- df_mismatch %>%
  count(author, sort = TRUE)


full_dataset_ger <- full_dataset_ger %>%
  mutate(Accountname = if_else(SourceFile == "schweighöfer", 
                               "matthiasschweighoefer", 
                               Accountname))

na_anzahl <- sum(is.na(full_dataset_ger$Accountname))

print(paste("Anzahl der fehlenden Werte in 'Accountname':", na_anzahl))

full_dataset_ger <- full_dataset_ger %>% 
  select(-X.1, -X, -thread_id, -parent_id, -author_avatar_url, -image_url, -media_url, -...27)

full_dataset_ger <- full_dataset_ger %>%
  rename(
    source_file = SourceFile,
    acc_name = Accountname,
    num_follower = Follower
  )

full_dataset_ger <- janitor::clean_names(full_dataset_ger)

full_dataset_ger <- full_dataset_ger %>%
  relocate(num_follower, .after = num_media) %>% 
  relocate(acc_name, .after = source_file)

full_dataset_ger[full_dataset_ger$id == "44DZ4nAxwa", "processed"] <- 1

write.csv(full_dataset_ger, file = "full_dataset_ger.csv")

#USA

full_dataset_usa <- read.csv("full_dataset_usa.csv")

df_mismatch_usa <- full_dataset_usa %>%
  filter(author != Accountname) %>% 
  select(url, author, Accountname)

df_mismatch_usa <- df_mismatch_usa %>%
  filter(!author %in% full_dataset_usa$Accountname)


df_mismatch_usa <- df_mismatch_usa[!df_mismatch_usa$author %in% c("thebigpodwithshaq", "cravingsbychrissyteigen",
                                                      "katyperrycollections", "djdiesel",
                                                      "grancoramino", "hotboxinpodcast",
                                                      "itstyson20", "playmaker"), ]

author_freq <- df_mismatch_usa %>%
  count(author, sort = TRUE)

na_anzahl <- sum(is.na(full_dataset_usa$Accountname))

print(paste("Anzahl der fehlenden Werte in 'Accountname':", na_anzahl))

na_zeilen <- full_dataset_usa[is.na(full_dataset_usa$Accountname), ]

full_dataset_usa <- full_dataset_usa[!is.na(full_dataset_usa$id), ]

full_dataset_usa <- full_dataset_usa %>% 
  select(-...35, -...34)

full_dataset_usa <- full_dataset_usa %>%
  rename(
    source_file = SourceFile,
    acc_name = Accountname,
    num_follower = Follower
  )

full_dataset_usa <- janitor::clean_names(full_dataset_usa)

full_dataset_usa <- full_dataset_usa %>%
  relocate(num_follower, .after = num_media) %>% 
  relocate(acc_name, .after = source_file)

write.csv(full_dataset_usa, file = "full_dataset_usa.csv")

names(full_dataset_usa)
