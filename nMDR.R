library(tidyverse)
library(vegan)
library(ggplot2)
library(readxl)

#pull in data, collect columns of interest

benthic_all_df <- read_excel("BBTrib_Benthic_2018_2019.xlsx") %>% as_tibble

benthic_df <- select(benthic_all_df, StationCode, SampleDate, CollectionMethodName, FinalID, BAResult, CollectionReplicate) %>% 
  filter(CollectionMethodName == "BMI_Reach-WideBenthos")

benthic_df_ID <- benthic_df_BMI %>% 
  unite("SampleID", StationCode, SampleDate, CollectionReplicate, remove = FALSE)

