library(readxl)
library(dplyr)
library(lubridate)
library(janitor)
library(gsubfn)
library(readr)

# spatial analysis tools
library(sf)                     # vector data tools
library(sfheaders)             # tools for simplifying sf headers
library(data.table)

# script de fonctions
source("R/utils.R")
source("R/fonctions_nettoyage.R")
source("R/audit_function_full.R")



# creez cleaning log
logbook<-data.frame(
  today       = character(),
  base        = character(),
  enumerateur = character(),
  uuid        = character(),
  question.name = character(),
  ancienne.valeur   = character(),
  nouvelle.valeur   = character(),
  parent.other.question = character(),
  parent.other.answer   = character(),
  probleme    = character(),
  checkid     = character(),
  action      = character()
)

# limites des biens durations de les entretiens
duration_min <- 20
duration_max <- 200
today <- today()

# distance minimale autorisée entre le point de levé et l'emplacement du village (m)
min_distance <- 25000

# importez les donnees brutes
data <- read_excel("input/donnees_brutes/bfa2002_msna_jour35_nettoyage_2020.xlsx", sheet ="bfa2002", na = c("NA", "", "N/A"), .name_repair = "unique")%>% 
  cleanheaders(TRUE) %>% 
  dplyr::select(-uuid...545) %>% 
  rename(uuid = uuid...2, index = index...1)



# vecteurs des colonnes autres, nsp etc
sm_autre <- names(data)[grep(".[.]autre", names(data))]
sm_nsp <- names(data)[grep(".[.](nsp|ne_sait_pas|je_ne_sais_pas)", names(data))]

####################################################################################################
# creation des variables de date et duration
# date de collection de donnees

# check du temps start - end avec la fonction de temps
data <- time_check(data, duration_min, duration_max)

var_consomm <- names(data)[grep("(jr_consom|jr_moins_prefere|jr_emprunt_nourriture|jr_nbr_repas|jr_rest_consommation|jr_diminu_quantite)", names(data))]

data <- data %>% 
  dplyr::mutate_at(var_consomm, as.numeric)

data <-  data %>% 
  mutate(fcs_score = jr_consom_cereale*2 + jr_consom_noix*3 + jr_consom_lait*4 + jr_consom_viande*4 +jr_consom_legume*1 + jr_consom_fruit*1 + jr_consom_huile*0.5 + jr_consom_sucre*0.5,
         aucun_aliment_score = case_when(
           aucun_aliment == "non" ~ 0,
           nbr_aucun_aliment %in% c("rarement","parfois") ~ 1,
           nbr_aucun_aliment %in% c("souvent") ~ 2
         ),
         dormir_affame_score = case_when(
           dormir_affame == "non" ~ 0,
           nbr_dormir_affame %in% c("rarement","parfois") ~ 1,
           nbr_dormir_affame %in% c("souvent") ~ 2
         ),
         pas_assez_nourriture_score = case_when(
           pas_assez_nourriture == "non" ~ 0,
           nbr_pas_assez_nourriture %in% c("rarement","parfois") ~ 1,
           nbr_pas_assez_nourriture %in% c("souvent") ~ 2
         ),
         hhs = aucun_aliment_score+dormir_affame_score+pas_assez_nourriture_score,
         rcsi =jr_moins_prefere+jr_emprunt_nourriture*2+jr_nbr_repas+jr_rest_consommation*3+jr_diminu_quantite
         
  )

data <- data %>% 
  mutate(moins_prefere_1=ifelse(moins_prefere=="oui", 1,0),
         emprunt_nourriture_1=ifelse(emprunt_nourriture=="oui",1,0),
         diminu_quantite_1=ifelse(diminu_quantite=="oui",1,0),
         rest_consommation_1=ifelse(rest_consommation=="oui",1,0),
         nbr_repas_1=ifelse(nbr_repas=="oui",1,0),
         redu_quant_eau_1=ifelse(redu_quant_eau=="oui",1,0),
         eau_sure_1=ifelse(eau_sure=="oui",1,0),
         nmbre_strategie=moins_prefere_1+emprunt_nourriture_1+diminu_quantite_1+rest_consommation_1+nbr_repas_1+redu_quant_eau_1+eau_sure_1
         
  )

# Plus de deux naissances dans le ménage or lemari est monogame
data$total_13_17_femmes <- as.numeric(data$total_13_17_femmes)
data$femme <- as.numeric(data$femme)

# importez le script de carte pour faire les checks de GPS
source("R/carte.R")


#######################################################################################################
### checks de données

### checks generales
#  uuid duplicee
index    <- unique(pulluuid(data, duplicated(data$uuid)))
logbook  <- make_log(data, logbook,"id01", index, "uuid", "UUID doublee")

##duree d'enquete ne depasse pas la duration_min
index    <- pulluuid(data, data$duration_entretien < duration_min)
logbook  <- make_log(data, logbook,"id01", index,"duration_entretien", paste0("Duree d'enquete trop courte - moins de ", duration_min, "minutes"))

##duree d'enquete depasse la duration_max
index    <-   pulluuid(data, data$duration_entretien > duration_max)
logbook  <-   make_log(data, logbook,"id02",index,"duration_entretien", paste0("Duree d'enquete trop longue - plus de ", duration_max, " minutes"))

##Transversal (nsp)
data$nb_nsp   <- apply(data, 1, function(x) {grep("nsp|ne_sait_pas|je_ne_sais_pas",x) %>%  unlist %>% length})
index         <- pulluuid(data, data$nb_nsp > 3)
logbook       <- make_log(data, logbook, "id03", index, "nb_nsp", "Nombre important de reponses nsp par enqueteur")

##Transversal (autre)
data$nb_autre <- apply(data, 1, function(x) {grep("autre", x) %>%  unlist %>% length})
index         <- pulluuid(data, data$nb_autre > 3)
logbook       <- make_log(data, logbook, "id04",index, "nb_autre", "Nombre important de reponses autre par enqueteur")

# consentement suppression
index   <- pulluuid(data, data$consensus_note =="non")
logbook <- make_log(data, logbook, "id05", index,"consensus_note","pas de consentement pour enquete", action = "++++++++++++++++++++++++++++++++++++")

# modalite direct mais il ny a pas coordonees
index   <- pulluuid(data, data$modalite == "direct" && is.na(data$gpsmenage_latitude))
logbook <- make_log(data, logbook, "id06", index,"point_gps_na","Modalite direct mais pas de point GPS")

### Checks particulaires des indicateurs
# total_deces > 2 --> signalé dans le journal de nettoyage des données 
index   <- pulluuid(data, data$total_deces > 2)
logbook <- make_log(data, logbook, "id07", index, "total_deces", "Nombre de personnes decedees au cours du dernier mois eleve ")

# jr_consom_cereale < 5 jours par semaine
index   <- pulluuid(data, data$jr_consom_cereale < 5)
logbook <- make_log(data, logbook, "id08", index, "jr_consom_cereale", "Le menage consomme des cereales moins de 5 jours par semaine.")

# Cas des enfants vivant hors du ménage plus de 3
index   <- pulluuid(data, data$enfant_absent > 3)
logbook <- make_log(data, logbook, "id09", index, "enfant_absent", "Plus que 3 enfants vivant hors du menage")

# Nombre élevé de pdi hébergé par un ménage
index   <- pulluuid(data, data$pdi_present_combien > 10)
logbook <- make_log(data, logbook, "id10", index, "pdi_present_combien", "Plus de 10 PDIs acceuillis par le ménage")

# Nombre élevé de pdi hébergé par un ménage
index   <- pulluuid(data, data$total_naissance > 2)
logbook <- make_log(data, logbook, "id11", index, "total_naissance", "Plus de deux naissances dans le ménage")

# Nombre élevé de pdi hébergé par un ménage
index   <- pulluuid(data, data$total_naissance>2 & data$situation_matrimoniale=="marie_monogame" & (data$total_13_17_femmes+ data$femme)>1)
logbook <- make_log(data, logbook, "id12", index, "total_naissance", "Plus de deux naissances dans le ménage avec mari monogame")

# Nombe de personnes ayant des difficultés de marche supérieur à 3
index   <- pulluuid(data, data$nombre_difficulte_marche>3)
logbook <- make_log(data, logbook, "id13", index, "nombre_difficulte_marche", "Nombe de personnes ayant des difficultes de marche superieur à 3")

# Nombe de personnes ayant des difficultes à prendre soins d'eux-meme superieur à 3 
index   <- pulluuid(data, data$nombre_soins_difficile > 3)
logbook <- make_log(data, logbook, "id14", index, "nombre_soins_difficile", "Nombre de personnes ayant des difficultés à prendre soins superieur à 3")

#Nombre de personnes ayant des difficultés à communiquer superieur à 3
index   <- pulluuid(data, data$nombre_difficulte_communication > 3)
logbook <- make_log(data, logbook, "id15", index, "nombre_difficulte_communication", "Nombre de personnes ayant des difficultés à communiquer superieur à 3")

#Nombre de personnes ayant des difficultés de concentration superieur à 3
index   <- pulluuid(data, data$nombre_difficulte_concentration > 3)
logbook <- make_log(data, logbook, "id16", index, "nombre_difficulte_concentration", "Nombre de personnes ayant des difficultés de concentration superieur à 3")

#Nombre de personnes ayant des difficultés de vision superieur à 3
index   <- pulluuid(data, data$nombre_difficulte_vision > 3)
logbook <- make_log(data, logbook, "id17", index, "nombre_difficulte_vision", "Nombre de personnes ayant des difficultés de vision superieur à 3")

# Nombre de personnes ayant des difficultés à entendre superieur à 3
index   <- pulluuid(data, data$nombre_difficulte_entendre > 3)
logbook <- make_log(data, logbook, "id18", index, "nombre_difficulte_entendre", "Nombre de personnes ayant des difficultés à entendre superieur à 3")

# Nombre de deces supérieur à 2
# index   <- pulluuid(data, data$deces_hh > 2)
# logbook <- make_log(data, logbook, "id19", index, "deces_hh", "Nombre de personnes decedees superieur à 2")

# L'eau se trouve dans la concession mais le temps de collecte est différent de eau dans la concession
index   <- pulluuid(data, data$temps_eau=="eau_concession" & data$temps_collecte!="eau_concession")
logbook <- make_log(data, logbook, "id20", index, "temps_eau", "L'eau se trouve dans la concession mais le temps de collecte est different de eau dans la concession")

# L'eau se trouve dans la concession mais le temps de collecte est différent de eau dans la concession
index   <- pulluuid(data, data$temps_collecte!="eau_concession" & data$temps_eau=="eau_concession")
logbook <- make_log(data, logbook, "id21", index, "temps_collecte", "Le temps de collecte est eau dans la concession or il n'y a pas d'eau dans la concession")

# Le temps de collecte est eau dans la concession et l'eau n'est pas dans la concession
index   <- pulluuid(data, data$dorme_exterieur>5)
logbook <- make_log(data, logbook, "id22", index, "dorme_exterieur", "Plus de 5 personnes dorment a l'exterieur")

# Les problèmes d'abri dans le ménage dépass
data$prob_abri_nombre <- sapply(strsplit(data$probleme_abri, " "), length)
index <- pulluuid(data, data$prob_abri_nombre > 4)
logbook <- make_log(data, logbook, "id23",index, "prob_abri_nombre", "Le nombre de problemes d'abri selectionne est superieur a 4")

# FCS < 21
index <- pulluuid(data, data$fcs_score < 21)
logbook <- make_log(data, logbook, "id24",index, "fcs_score", "Score de consommation alimentaire est faible")

# FCS <= 6 et hhs=0
index <- pulluuid(data, data$fcs_score <=6 & data$hhs==0)
logbook <- make_log(data, logbook, "id25",index, "fcs_score", "Score de consommation alimentaire anormalement bas par rapport à l'indice de la faim des ménages",action = "remove")

# FCS <=6 -->  & rCSI_score <= 3 
index <- pulluuid(data, data$fcs_score <=6 & data$rcsi <=3)
logbook <- make_log(data, logbook, "id26",index, "fcs_score", "Score de consommation alimentaire anormalement bas vu l'indice simplifié des stratégies de survie")

# FCS <=6 -->  & rCSI_score <= 3 
index <- pulluuid(data, data$hhs==0 & data$rcsi>18)
logbook <- make_log(data, logbook, "id27",index, "hhs", "Indice simplifie des stratégies de survie anormalement élevé par rapport a l'indice de la faim")

# FCS <=6 -->  & rCSI_score <= 3 
index <- pulluuid(data, data$fcs_score <=6 & data$rcsi <=3 & data$hhs==0 & data$nmbre_strategie>=3 )
logbook <- make_log(data, logbook, "id28",index, "fcs_score_hhs_rcsi", "Nombre eleve de stratégies de subsistance par rapport a une situation correcte en terme de SC")

#moyen_pref_info = appel ET telephone = non OU acces_reseau_tel = non --> signalé dans le journal de nettoyage des données
index <- pulluuid(data, data$moy_pref_info=="appel" & (data$telephone=="non" | data$acces_reseau_tel=="non"))
logbook <- make_log(data, logbook, "id29",index, "moy_pref_info", "Le moyen prefere de communication est appel or pas de telephone ou reseau")

#acqui_cereale = aide ou acqui_noix = aide ou acqui_huile = aide mais assistance = non --> signalé dans le journal de nettoyage des données
index <- pulluuid(data, data$acqui_cereale=="aide" & data$acqui_noix=="aide" & data$assistance=="non")
logbook <- make_log(data, logbook, "id30",index, "acqui_cereale", "Aide recu mais pas d'assistance")

# check de admin2 selectionee
# index <- pulluuid(data, data$admin2 != data$admin2_gps)
# logbook <- make_log(data, logbook, "id31",index, "admin2_gps", "admin2 mal sélectionné")
# 
# # check de admin3 selectionee
# index <- pulluuid(data, data$admin3 != data$admin3_gps)
# logbook <- make_log(data, logbook, "id32",index, "admin3_gps", "admin3 mal sélectionné")

# check distance entre les points des enquetes et les villages
index <- pulluuid(data, data$distance_m >= min_distance)
logbook <- make_log(data, logbook, "id33",index, "distance_m", paste0("distance entre l'enquete et village plus grande que ",min_distance, "m"))

# exportez le cleaning log
write.csv(logbook, paste0("output/cleaning_log/cleaning_log_", today, ".csv"), row.names = FALSE)

########################################################################################################################################################
# importez le cleaning log complet 
clog_complet <- read.csv("input/cleaning_log_complet/cleaning_log_2021-04-01_complet.csv")
survey       <- readxl::read_excel("input/outil/bfa_msna_outil_V2.xlsx", sheet="survey")
choices      <- readxl::read_excel("input/outil/bfa_msna_outil_V2.xlsx", sheet="choices")

# faire le cleaning
# supprimer les colonnes contenant des informations personnellement identifiables. 
# colonnes_ipi <- c("")

data_cleaned <- cleaning_data(data, clog_complet, survey, choices)

