library(tidyverse)
library(readxl)
library(stringr)

full_dataset_ger <- read.csv("full_dataset_ger.csv")

full_dataset_usa <- read.csv("full_dataset_usa.csv")

full_dataset_ger$pol <- as.numeric(
  full_dataset_ger$person == 1 |
    full_dataset_ger$inhalt == 1 |
    full_dataset_ger$partei == 1
)

full_dataset_usa$pol <- as.numeric(
  full_dataset_usa$person == 1 |
    full_dataset_usa$inhalt == 1 |
    full_dataset_usa$partei == 1
)

write.csv(full_dataset_ger, file = "full_dataset_ger.csv")

write.csv(full_dataset_usa, file = "full_dataset_usa.csv")

library(openxlsx)

write.xlsx(full_dataset_ger, file = "full_dataset_ger.xlsx")

write.xlsx(full_dataset_usa, file = "full_dataset_usa.xlsx")

