library(tidyverse)
library(vegan)
library(ggplot2)
library(readxl)

#pull in data, collect columns of interest

benthic_all_df <- read_excel("BBTrib_Benthic_2018_2019.xlsx") %>% as_tibble

benthic_df <- select(benthic_all_df, StationCode, SampleDate, CollectionMethodName, FinalID, BAResult, CollectionReplicate) %>% 
  filter(CollectionMethodName == "BMI_Reach-WideBenthos") %>% 
  mutate(CollectionMethod = "BMI_RWB")

benthic_df_ID <- benthic_df %>% 
  unite("SampleID", StationCode, SampleDate, CollectionMethod, CollectionReplicate) %>% 
  mutate(P_A = 1) %>% 
  select(-c(CollectionMethodName, BAResult)) %>% 
  group_by(SampleID, FinalID) %>% 
  summarize(P_A = sum(P_A)) %>% 
  ungroup() %>% 
  mutate(P_A = case_when(P_A >= 1 ~ 1,
                   TRUE ~ 0))

unique(benthic_df_ID$FinalID) #146 species
unique(benthic_df_ID$SampleID) #24 unique sample IDs

benthic_df_ID %>% pivot_wider(names_from = FinalID, values_from = P_A, values_fill = 0) %>% View()

benthic_df_ID %>% select(-P_A) %>% duplicated()

