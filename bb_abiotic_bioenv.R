#load libraries
library(tidyverse)
library(vegan)
library(ggplot2)
library(readxl)
library(ggrepel)

#pull in data
phab_all_df <- read.csv("data/phab_bb.csv") %>% as_tibble

phab_ma <- phab_all_df %>%
  mutate(sampleID = paste(stationcode, month, year, sep = "_")) %>% 
  pivot_wider(names_from = analytename, values_from = result, values_fill = 0) %>% 
  column_to_rownames("sampleID")

unique(phab_all_df$sampledate)
