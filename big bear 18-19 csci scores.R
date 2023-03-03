install.packages("Rtools")

library(devtools)
library(readxl)
library(tidyverse)
library(CSCI)

install_github("SCCWRP/BMIMetrics")
install_github("SCCWRP/CSCI")

#clean bug data from heather
bugs_heather <- read_excel("data/BBTrib_Benthic_2018_2019.xlsx") %>% as_tibble

#select columns of interest
bugs_df <- bugs_heather %>%
  filter(CollectionMethodName == "BMI_Reach-WideBenthos") %>% 
  mutate(CollectionMethod = "BMI_RWB",
         SampleID=paste(StationCode, SampleDate, CollectionMethod, CollectionReplicate, sep="_")) %>%
  select(SampleID, StationCode, FinalID, BAResult, LifeStageName) %>% 
  rename(LifeStageCode = LifeStageName)

bugs_df <- bugs_df %>%
  mutate(LifeStageCode = recode(LifeStageCode, Adult = 'A', Pupa = 'P', Larva = 'L', Undefined = 'X'))
  # filter(StationCode == "801BBAG01", StationCode == "801BBGC01",
  #        StationCode == "801BBGC02", StationCode == "801M15547")
#note: not all bugs above in undefined are necessarily non-insects, I just made them so for time constraints

stations_df <- read.csv("data/Rachel_bigbear_metrics.csv") %>% 
  select(stationcode, new_lat, new_long, site_elev, elev_range, area_sqkm, temp_00_09, ppt_00_09, sumave_p, kfct_ave, bdh_ave, p_mean)

report <- CSCI(bugs = bugs_df, stations = stations_df)

