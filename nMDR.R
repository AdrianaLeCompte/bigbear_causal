#load libraries
library(tidyverse)
library(vegan)
library(ggplot2)
library(readxl)

#pull in data
benthic_all_df <- read_excel("BBTrib_Benthic_2018_2019.xlsx") %>% as_tibble

#select columns of interest
benthic_df <- benthic_all_df %>% filter(CollectionMethodName == "BMI_Reach-WideBenthos") %>% 
  mutate(CollectionMethod = "BMI_RWB", SampleID=paste(StationCode, SampleDate, CollectionMethod, CollectionReplicate, sep="_"))

#create dataframe as p_a matrix (24 unique sampleIDs, 146 unique species)
benthic_df_pa <- benthic_df %>% 
  unite("SampleID", StationCode, SampleDate, CollectionMethod, CollectionReplicate) %>%
  mutate(p_a = 1) %>% #create sampleID
  group_by(SampleID, FinalID) %>% #group the presence by sample ID and final ID
  summarize(p_a = sum(p_a)) %>% #summarize the presence by sample ID and final ID
  ungroup() %>% 
  pivot_wider(names_from = FinalID, values_from = p_a, values_fill = 0) %>% #pivot dataframe
  column_to_rownames("SampleID") #move SampleID column into rownames

#create numeric matrix from dataframe
ben_matrix <- data.matrix(benthic_df_ID)

#run nMDS function to get ordination object
benthic_ord <- metaMDS(ben_matrix, k = 2, try = 20, trymax = 50, autotransform = F)

#turn object into dataframe to extract scores
benthic_samples <- data.frame(scores(benthic_ord, display = "sites")) %>% 
  add_column(.before = "NMDS1", benthic_df_pa$SampleID)

#create csv with results
write.csv(benthic_samples, file = "nMDR results.csv", row.names = FALSE)

#calculate other metrics

#richness
richness_col <- benthic_df %>%
  group_by(SampleID, FinalID) %>% 
  summarize(BAResult)

#evenness
diversity(benthic_df)
