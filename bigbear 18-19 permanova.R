#load libraries
library(tidyverse)
library(vegan)
library(readxl)
library(broom)

#pull in data
bb_input_data <- read_excel("BBTrib_Benthic_2018_2019.xlsx") %>% as_tibble

#select columns of interest
bb_edited_data <- bb_input_data %>% filter(CollectionMethodName == "BMI_Reach-WideBenthos") %>% 
  mutate(CollectionMethod = "BMI_RWB", SampleID=paste(StationCode, SampleDate, CollectionMethod, CollectionReplicate, sep="_"))

#create dataframe for p_a matrix
bb_pa_matrix <- bb_edited_data %>% 
  group_by(SampleID, FinalID) %>% 
  summarize(BAResult_sum = sum(BAResult)) %>% 
  ungroup() %>% 
  mutate(p_a = 1) %>%
  select(-BAResult_sum) %>% 
  pivot_wider(names_from = FinalID, values_from = p_a, values_fill = 0) %>% 
  mutate(.before = "Agabus", Site = str_sub(SampleID,1,9), SamDate = str_sub(SampleID,11,20), 
         SamDate = lubridate::as_date(SamDate), Year = lubridate::year(SamDate), 
         Month = lubridate::month(SamDate, label=TRUE, abbr = F)) %>% 
  select(-c(SampleID, SamDate))

#permanova function
bb_perm_pa <- adonis2(formula = (select(bb_pa_matrix,4:149))~Site*Year, data = (select(bb_pa_matrix,1:3)),
                        permutations = 10000, method = "bray")

bb_perm_pa_2 <- adonis2(formula = (select(bb_pa_matrix,4:149))~Site+Year, data = (select(bb_pa_matrix,1:3)),
                        permutations = 10000, method = "bray")

summary(bb_perm_pa)

#export results
write.csv(bb_perm_pa, file = "results/big bear 18-19 permanova results.csv")

