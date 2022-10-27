#load libraries
library(tidyverse)
library(vegan)
library(ggplot2)
library(readxl)

install.packages("paletteer")
library(paletteer)

#pull in data
benthic_all_df <- read_excel("BBTrib_Benthic_2018_2019.xlsx") %>% as_tibble

#select columns of interest
benthic_df <- benthic_all_df %>% filter(CollectionMethodName == "BMI_Reach-WideBenthos") %>% 
  mutate(CollectionMethod = "BMI_RWB", SampleID=paste(StationCode, SampleDate, CollectionMethod, CollectionReplicate, sep="_"))

#create dataframe for abundance matrix
benthic_df_abun <- benthic_df %>% 
  group_by(SampleID, FinalID) %>% 
  summarize(BAResult_sum = sum(BAResult)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = FinalID, values_from = BAResult_sum, values_fill = 0) %>% 
  column_to_rownames("SampleID")

#create dataframe for p_a matrix
benthic_df_pa <- benthic_df %>% 
  group_by(SampleID, FinalID) %>% 
  summarize(BAResult_sum = sum(BAResult)) %>% 
  ungroup() %>% 
  mutate(p_a = 1) %>%
  select(-BAResult_sum) %>% 
  pivot_wider(names_from = FinalID, values_from = p_a, values_fill = 0) %>% 
  column_to_rownames("SampleID")

#run nMDS function to get ordination object
benthic_ord_pa <- metaMDS(benthic_df_pa, k = 2, try = 20, trymax = 50, autotransform = F)

stress_pa = benthic_ord_pa$stress

benthic_ord_abun <- metaMDS(benthic_df_abun, k = 2, try = 20, trymax = 50, autotransform = F)

stress_abun = benthic_ord_abun$stress

#extract scores from ordination
benthic_scores_pa <- data.frame(scores(benthic_ord_pa, display = "sites")) %>% 
  rownames_to_column("SampleID") %>% 
  mutate(StationID = str_sub(SampleID,1,9), SamDate = str_sub(SampleID,11,20), 
         Replicate = str_sub(SampleID, -1), SamDate = lubridate::as_date(SamDate),
         Year = lubridate::year(SamDate), Month = lubridate::month(SamDate, label=TRUE, abbr = F)) %>% 
  mutate(Year = as.factor(Year))

benthic_scores_abun <- data.frame(scores(benthic_ord_abun, display = "sites")) %>% 
  rownames_to_column("SampleID") %>% 
  mutate(StationID = str_sub(SampleID,1,9), SamDate = str_sub(SampleID,11,20), 
         Replicate = str_sub(SampleID, -1), SamDate = lubridate::as_date(SamDate),
         Year = lubridate::year(SamDate), Month = lubridate::month(SamDate, label=TRUE, abbr = F)) %>% 
  mutate(Year = as.factor(Year))

#plot ordination scores
bb.pa.plot <- benthic_scores_pa %>% 
  ggplot(., aes(x = NMDS1, y = NMDS2)) +
  theme_bw()+ theme(panel.grid = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(),
                    axis.title = element_text(face = "bold"))+
  geom_point(aes(fill = StationID, shape = Year), size = 2.5)+
  scale_shape_manual(values = c(21,22))+
  geom_text(aes(label = Month), nudge_y = -0.025)+
  guides(fill=guide_legend(override.aes = list(shape=21)))+
  labs(title = 'Presence',
       subtitle = 'Big bear causal assessment 2018-2019'
         )
bb.pa.plot

bb.pa.plot2 <- benthic_scores_abun %>% 
  ggplot(., aes(x = NMDS1, y = NMDS2)) +
  theme_bw()+ theme(panel.grid = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(),
                    axis.title = element_text(face = "bold"))+
  geom_point(aes(fill = StationID, shape = Year), size = 2.5)+
  scale_shape_manual(values = c(21,22))+
  geom_text(aes(label = Month), nudge_y = -0.025)+
  guides(fill=guide_legend(override.aes = list(shape=21)))+
  labs(title = 'Abundance',
       subtitle = 'Big bear causal assessment 2018-2019'
  )
bb.pa.plot2

