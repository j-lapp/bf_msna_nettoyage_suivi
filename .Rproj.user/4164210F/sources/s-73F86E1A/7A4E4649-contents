####################################################################################################
##### SUIVI ########################################################################################

source("nettoyage.R")
source("R/utils.R")

supp_accents <- function (string) {
  
  unwanted_array = list(    'À'='A', 'Á'='A', 'Â'='A', 'Ã'='A', 'Ä'='A', 'Å'='A', 'Ç'='C', 'È'='E', 'É'='E',
                            'Ê'='E', 'Ë'='E','à'='a', 'á'='a', 'â'='a', 'ã'='a', 'ä'='a', 'å'='a', 'ç'='c',
                            'è'='e', 'é'='e', 'ê'='e')
  
  removed_accents <- gsubfn(paste(names(unwanted_array),collapse='|'), unwanted_array,string)
  
  return(removed_accents)
}


plus_recent <- max(data$today)

# importez l'echantillonage de MSNA 2020
ech_ch_msna <- read.csv("input/echantillonage/clusterSample.csv")%>%  
  mutate(ADM2_FR_propre = tolower(ADM2_FR), na.rm=T)

ech_ch_msna$ADM2_FR_propre <- supp_accents(ech_ch_msna$ADM2_FR_propre)

# grouper par admin2
ech_ch_msna_admin2 <- ech_ch_msna %>% 
  group_by(ADM2_FR_propre) %>% 
  summarize(echantillon = sum(survey_buffer))

##### operations group by et summarize pour peupler le rapport markdown

# grouper par region - pop_local
nombre_admin1 <- data  %>% 
  filter(group_pop == "pop_local") %>% 
  group_by(admin1) %>% 
  summarise(n_enquetes_ch = n())

# grouper par province - pop_local
nombre_admin2 <- data  %>% 
  filter(group_pop == "pop_local") %>% 
  group_by(admin1, admin2) %>% 
  summarise(n_enquetes_ch = n())

# grouper par enumerateur
temps_enumerateur <- data %>% 
  filter(group_pop == "pop_local") %>% 
  group_by(admin1, admin2, enumerator_id) %>% 
  summarise(n_enquetes_ch = n(),
            moy_temps = round(mean(duration_entretien),0), na.rm=T)

# joignez pour faire la comparaison echantillonage - actuelle
progres_admin2 <- left_join(nombre_admin2, 
                    ech_ch_msna_admin2, 
                    by = c("admin2" = "ADM2_FR_propre")) %>% 
  mutate(pc_complet = round(n_enquetes_ch / echantillon * 100,1), na.rm=T)

# calculez les va;eurs de pourcentage de progres
progres_admin2 <-  progres_admin2 %>% 
  mutate(pc_complet = ifelse(pc_complet > 100, 100, pc_complet), na.rm=T)

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

