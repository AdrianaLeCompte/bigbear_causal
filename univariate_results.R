#load libraries
library(tidyverse)
library(vegan)
library(ggplot2)
library(readxl)

#pull in data of interest
benthic_all_df <- read_excel("BBTrib_Benthic_2018_2019.xlsx") %>% as_tibble

#select columns of interest
benthic_df <- benthic_all_df %>% filter(CollectionMethodName == "BMI_Reach-WideBenthos") %>% 
  mutate(CollectionMethod = "BMI_RWB", SampleID=paste(StationCode, SampleDate, CollectionMethod, CollectionReplicate, sep="_"))

#create abundance matrix
benthic_df_abun <- benthic_df %>% 
  group_by(SampleID, FinalID) %>% 
  summarize(BAResult_sum = sum(BAResult)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = FinalID, values_from = BAResult_sum, values_fill = 0) %>% 
  mutate(H = diversity(select(.,2:147), index = "shannon"))

univ_results <- benthic_df %>% 
  group_by(SampleID) %>% 
  summarise(S = length(FinalID)) %>% 
  ungroup() %>% 
  left_join(., (select(benthic_df_abun, SampleID, H)), by = "SampleID") %>%
  mutate(E = (H/log(S)))