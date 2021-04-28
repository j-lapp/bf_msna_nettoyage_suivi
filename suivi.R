####################################################################################################
##### SUIVI ########################################################################################

source("nettoyage.R")
source("R/utils.R")

library(formattable)
library(kableExtra)
library(htmltools)
library(sparkline)
library(tidyverse)
library(highcharter)
library(purrr)


plus_recent <- max(data_cleaned$today)

# importez l'echantillonage de MSNA 2020
ech_ch_msna <- read.csv("input/echantillonage/clusterSample.csv")%>%  
  mutate(ADM2_FR_propre = tolower(ADM2_FR), na.rm=T)

ech_ch_msna$ADM2_FR_propre <- supp_accents(ech_ch_msna$ADM2_FR_propre)

# grouper par admin2
ech_ch_msna_admin2 <- ech_ch_msna %>% 
  group_by(ADM2_FR_propre) %>% 
  summarize(echantillon = sum(survey_buffer))

##### operations group by et summarize pour peupler le rapport markdown

# grouper par region et modalite
nombre_modalite <- data_cleaned  %>% 
  group_by(admin1, admin2, group_pop, modalite) %>% 
  summarise(n_enquetes_ch = n())

# grouper par region - pop_local
nombre_admin1_ch <- data_cleaned  %>% 
  filter(group_pop == "pop_local") %>% 
  group_by(admin1) %>% 
  summarise(n_enquetes_ch = n())

# grouper par province - pop_local
nombre_admin2_ch <- data_cleaned  %>% 
  filter(group_pop == "pop_local") %>% 
  group_by(admin1, admin2) %>% 
  summarise(n_enquetes_ch = n())


# grouper par region - PDI
nombre_admin1_pdi <- data_cleaned  %>% 
  filter(group_pop == "pdi") %>% 
  group_by(admin1) %>% 
  summarise(n_enquetes_pdi = n()) 

# grouper par province - PDI
nombre_admin2_pdi <- data_cleaned  %>% 
  filter(group_pop == "pdi") %>% 
  group_by(admin1, admin2) %>% 
  summarise(n_enquetes_pdi = n())%>% 
  arrange(desc(n_enquetes_pdi))

# grouper par enumerateur
temps_enumerateur <- data_cleaned %>% 
  filter(group_pop == "pop_local") %>% 
  group_by(admin1, admin2, enumerator_id) %>% 
  summarise(n_enquetes_ch = n(),
            moy_temps = round(mean(duration_entretien),0))

# joignez pour faire la comparaison echantillonage - actuelle
progres_admin2 <- left_join(nombre_admin2_ch, 
                    ech_ch_msna_admin2, 
                    by = c("admin2" = "ADM2_FR_propre")) %>% 
  mutate(pc_complet = round(n_enquetes_ch / echantillon * 100,1))

# calculez les valeurs de pourcentage de progres
progres_admin2 <-  progres_admin2 %>% 
  mutate(pc_complet = ifelse(pc_complet > 100, 100, pc_complet))

#### CLEANING LOG
### groupez par type de probleme dans le cleaning log

problemes_comun <- logbook %>% 
  group_by(probleme) %>% 
  summarize(nombre = n()) 

### groupez par type de probleme dans le cleaning log
### filtrez pour dates les plus récentes
problemes_comun_recents <- logbook %>% 
  filter(today  >= plus_recent - days(7)) %>% 
  group_by(probleme) %>% 
  summarize(nombre = n()) 


### preparez donnees pour le graphique de series temps
n_par_jour <- data_cleaned  %>% 
  filter(!is.na(group_pop)) %>% 
  group_by(group_pop) %>% 
  count(today)

