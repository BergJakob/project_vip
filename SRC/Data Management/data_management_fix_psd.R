library(tidyverse)
library(readxl)
library(stringr)

#GER

full_dataset_ger <- read.csv("full_dataset_ger.csv")

übersicht_ger <- read_excel("übersicht.xlsx")

full_dataset_ger <- full_dataset_ger %>% 
  select(-Accountname)

übersicht_subset <- übersicht_ger %>%
  select(SourceFile, Accountname)

full_dataset_ger <- full_dataset_ger %>%
  left_join(übersicht_subset, 
            by = "SourceFile")

extract_handle <- function(url) {
  url_clean <- str_remove(url, "/$")
  handle <- str_extract(url_clean, "[^/]+$")
  return(handle)
}

df_inkonsistenzen_account <- full_dataset_ger %>%
  mutate(Extracted_Handle = extract_handle(post_source_domain)) %>%
  filter(tolower(Accountname) != tolower(Extracted_Handle)) %>%
  select(-Extracted_Handle) %>% 
  select(post_source_domain, author, SourceFile, Accountname)

alter_wert <- "https://www.instagram.com/p/BEgcJPDj1dh/"
neuer_wert <- "https://www.instagram.com/heidiklum/"

full_dataset_ger$post_source_domain[full_dataset_ger$post_source_domain == alter_wert] <- neuer_wert

write.csv(full_dataset_ger, file = "full_dataset_ger.csv")

#USA

full_dataset_usa <- read.csv("full_dataset_usa.csv")

übersicht_usa <- read_excel("übersicht.xlsx", sheet = 2)

full_dataset_usa <- full_dataset_usa %>% 
  select(-Accountname)

übersicht_subset <- übersicht_usa %>%
  select(SourceFile, Accountname)

full_dataset_usa <- full_dataset_usa %>%
  left_join(übersicht_subset, 
            by = "SourceFile")

extract_handle <- function(url) {
  url_clean <- str_remove(url, "/$")
  handle <- str_extract(url_clean, "[^/]+$")
  return(handle)
}

df_inkonsistenzen_account <- full_dataset_usa %>%
  mutate(Extracted_Handle = extract_handle(post_source_domain)) %>%
  filter(tolower(Accountname) != tolower(Extracted_Handle)) %>%
  select(-Extracted_Handle) %>% 
  select(post_source_domain, author, SourceFile, Accountname)

alter_wert <- "https://www.instagram.com/p/UGw1TrG-B1/"
neuer_wert <- "https://www.instagram.com/krisjenner/"

full_dataset_usa$post_source_domain[full_dataset_usa$post_source_domain == alter_wert] <- neuer_wert

full_dataset_usa <- full_dataset_usa[full_dataset_usa$author != c("rosbergxracing", "tennistv",
                                                                  "greenforce.food", "yvonnedilauro",
                                                                  "viueyewear", "mischazverevofficial",
                                                                  "kejf.de", "super_fleamarket"), ]

write.csv(full_dataset_usa, file = "full_dataset_usa.csv")



















