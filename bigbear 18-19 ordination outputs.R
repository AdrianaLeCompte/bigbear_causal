#load libraries
library(tidyverse)
library(vegan)
library(ggplot2)
library(readxl)
library(ggrepel)

#pull in data
benthic_all_df <- read_excel("BBTrib_Benthic_2018_2019.xlsx") %>% as_tibble

#select columns of interest
benthic_df <- benthic_all_df %>% filter(CollectionMethodName == "BMI_Reach-WideBenthos") %>% 
  mutate(CollectionMethod = "BMI_RWB", SampleID=paste(StationCode, SampleDate, CollectionMethod, CollectionReplicate, sep="_"))

short_names_df <- benthic_df %>% 
  mutate(
    StationShrName = case_when(
      StationCode == "801BBAG01" ~ "Aspen Glen Creek",
      StationCode == "801BBGC01" ~ "Lower Grout Creek",
      StationCode == "801BBGC02" ~ "Mid Grout Creek",
      StationCode == "801MFC100" ~ "Upper Metcalf Creek",
      StationCode == "801BBMC01" ~ "Lower Metcalf Creek",
      StationCode == "801BBMC02" ~ "Mid Metcalf Creek",
      StationCode == "801BBRC02" ~ "Upper Rathbun Creek",
      StationCode == "801BBRC01" ~ "Lower Rathbun Creek",
      StationCode == "801S31343" ~ "Mid Rathbun Creek",
      StationCode == "801BBSCW1" ~ "Summit Creek",
      StationCode == "801M15547" ~ "Boulder Creek"
    )
  )

# Ordination with both years ----------------------------------------------


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
  geom_text_repel(aes(NMDS1, NMDS2, label = Month))+
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
  guides(fill=guide_legend(override.aes = list(shape=21)))+
  geom_text_repel(aes(NMDS1, NMDS2, label = Month))+
  labs(title = 'Abundance',
       subtitle = 'Big bear causal assessment 2018-2019'
  )
bb.pa.plot2

ggsave("bigbear nMDS ord abundance.png", bb.pa.plot2)
ggsave("bigbear nMDS ord presence.png", bb.pa.plot)



# Abundance ordination by year --------------------------------------------

#create dataframe for abundance matrix 
benthic_df_abun_18 <- benthic_df %>%
  mutate(Year = lubridate::year(SampleDate))%>% 
  filter(Year == "2018") %>% 
  group_by(SampleID, FinalID) %>% 
  summarize(BAResult_sum = sum(BAResult)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = FinalID, values_from = BAResult_sum, values_fill = 0) %>% 
  column_to_rownames("SampleID")

benthic_df_abun_19 <- benthic_df %>%
  mutate(Year = lubridate::year(SampleDate))%>% 
  filter(Year == "2019") %>% 
  group_by(SampleID, FinalID) %>% 
  summarize(BAResult_sum = sum(BAResult)) %>% 
  ungroup() %>% 
  pivot_wider(names_from = FinalID, values_from = BAResult_sum, values_fill = 0) %>% 
  column_to_rownames("SampleID")

#run nMDS function to get ordination object & stress
benthic_ord_abun_18 <- metaMDS(benthic_df_abun_18, k = 2, try = 20, trymax = 50, autotransform = F)
stress_abun_18 = benthic_ord_abun_18$stress

benthic_ord_abun_19 <- metaMDS(benthic_df_abun_19, k = 2, try = 20, trymax = 50, autotransform = F)
stress_abun_19 = benthic_ord_abun_19$stress

#extract scores from ordination
benthic_scores_abun_18 <- data.frame(scores(benthic_ord_abun_18, display = "sites")) %>% 
  rownames_to_column("SampleID") %>% 
  mutate(StationCode = str_sub(SampleID,1,9), SamDate = str_sub(SampleID,11,20), 
         Replicate = str_sub(SampleID, -1), SamDate = lubridate::as_date(SamDate),
         Year = lubridate::year(SamDate), Month = lubridate::month(SamDate, label=TRUE, abbr = F)) %>% 
  mutate(Year = as.factor(Year)) %>% 
  mutate(StationShrName = case_when(
    StationCode == "801BBAG01" ~ "Aspen Glen Creek",
    StationCode == "801BBGC01" ~ "Lower Grout Creek",
    StationCode == "801BBGC02" ~ "Mid Grout Creek",
    StationCode == "801MFC100" ~ "Upper Metcalf Creek",
    StationCode == "801BBMC01" ~ "Lower Metcalf Creek",
    StationCode == "801BBMC02" ~ "Mid Metcalf Creek",
    StationCode == "801BBRC02" ~ "Upper Rathbun Creek",
    StationCode == "801BBRC01" ~ "Lower Rathbun Creek",
    StationCode == "801S31343" ~ "Mid Rathbun Creek",
    StationCode == "801BBSCW1" ~ "Summit Creek",
    StationCode == "801M15547" ~ "Boulder Creek"))

benthic_scores_abun_19 <- data.frame(scores(benthic_ord_abun_19, display = "sites")) %>% 
  rownames_to_column("SampleID") %>% 
  mutate(StationCode = str_sub(SampleID,1,9), SamDate = str_sub(SampleID,11,20), 
         Replicate = str_sub(SampleID, -1), SamDate = lubridate::as_date(SamDate),
         Year = lubridate::year(SamDate), Month = lubridate::month(SamDate, label=TRUE, abbr = F)) %>% 
  mutate(Year = as.factor(Year))

#plot ordination scores
bb.abun.plot.18 <- benthic_scores_abun_18 %>% 
  ggplot(., aes(x = NMDS1, y = NMDS2)) +
  theme_bw()+ theme(panel.grid = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(),
                    axis.title = element_text(face = "bold"))+
  geom_point(aes(fill = StationShrName), pch = 21, size = 2.5)+
  scale_fill_discrete(name = "Station Name")+
  geom_text_repel(aes(NMDS1, NMDS2, label = Month))+
  labs(title = 'Abundance',
       subtitle = 'Big bear causal assessment 2018')
bb.abun.plot.18

bb.abun.plot.19 <- benthic_scores_abun_19 %>% 
  ggplot(., aes(x = NMDS1, y = NMDS2)) +
  theme_bw()+ theme(panel.grid = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(),
                    axis.title = element_text(face = "bold"))+
  geom_point(aes(fill = StationCode), pch = 21, size = 2.5)+
  geom_text_repel(aes(NMDS1, NMDS2, label = Month))+
  labs(title = 'Abundance',
       subtitle = 'Big bear causal assessment 2019'
  )
bb.abun.plot.19


# Pres/Abs ordination by year ---------------------------------------------
