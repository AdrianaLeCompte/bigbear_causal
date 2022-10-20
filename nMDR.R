#load libraries
library(tidyverse)
library(vegan)
library(ggplot2)
library(readxl)

#pull in data
benthic_all_df <- read_excel("BBTrib_Benthic_2018_2019.xlsx") %>% as_tibble

#select columns of interest
benthic_df <- select(benthic_all_df, StationCode, SampleDate, CollectionMethodName, FinalID, BAResult, CollectionReplicate) %>% 
  filter(CollectionMethodName == "BMI_Reach-WideBenthos") %>% 
  mutate(CollectionMethod = "BMI_RWB") #create column for later ID identification

#create dataframe as p_a matrix (24 unique sampleIDs, 146 unique species)
benthic_df_ID <- benthic_df %>% 
  unite("SampleID", StationCode, SampleDate, CollectionMethod, CollectionReplicate) %>% #create sampleID
  mutate(P_A = 1) %>% #add presence absence column
  group_by(SampleID, FinalID) %>% #group the presence by sample ID and final ID
  summarize(P_A = sum(P_A)) %>% #summarize the presence by sample ID and final ID
  ungroup() %>% 
  mutate(P_A = case_when(P_A >= 1 ~ 1, #make any presence above 1 to 1 (due to summarizing)
                   TRUE ~ 0)) %>% 
  pivot_wider(names_from = FinalID, values_from = P_A, values_fill = 0) %>% #pivot dataframe
  rownames_to_column() #add rownames

#create numeric matrix from dataframe
ben_matrix <- data.matrix(benthic_df_ID)

#run nMDS function to get ordination object
benthic_ord <- metaMDS(ben_matrix, k = 2, try = 20, trymax = 50, autotransform = F)

#turn object into dataframe to extract scores
benthic_samples <- data.frame(scores(benthic_ord, display = "sites")) %>% 
  add_column(benthic_df_ID$SampleID)

#create csv with results
write.csv(benthic_samples, file = "nMDR results.csv", row.names = FALSE)

#calculate other metrics

#richness
richness_col <- benthic_df %>%
  group_by(SampleID, FinalID) %>% 
  summarize(BAResult)

#evenness
diversity(benthic_df)
