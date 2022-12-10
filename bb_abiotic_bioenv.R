#load libraries
library(tidyverse)
library(vegan)
library(ggplot2)
library(readxl)
library(ggrepel)

#pull in data
phab_all_df <- read.csv("data/phab_bb.csv") %>% as_tibble

phab_ma <- phab_all_df %>%
  mutate(Year = lubridate::year(sampledate), Month = lubridate::month(sampledate, label=TRUE, abbr = F))%>%
  mutate(date = paste(Year, Month, sep = "_")) %>% 
  mutate(sampleID = paste(stationcode, date, sep = "_")) %>% 
  group_by(sampleID, analytename) %>% 
  ungroup() %>% 
  pivot_wider(names_from = analytename, values_from = result, values_fill = 0) %>% 
  column_to_rownames("sampleID")

unique(phab_all_df$sampledate)
