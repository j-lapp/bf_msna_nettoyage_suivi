library(readxl)
library(dplyr)
library(lubridate)
library(janitor)
library(gsubfn)

# script de fonctions
source("R/fonctions_nettoyage.R")
source("R/utils.R")


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

# 
duration_min <- 20
duration_max <- 200
today <- today()

# importez les donnees brutes
data <- read_excel("input/donnees_brutes/bfa2002_msna_jour35_nettoyage_2020.xlsx", sheet ="bfa2002", na = c("NA", "", "N/A"))%>% 
  cleanheaders(TRUE) %>% 
  rename(uuid = uuid...2)

# vecteurs des colonnes autres, nsp etc
sm_autre <- names(data)[grep(".[.]autre", names(data))]
sm_nsp <- names(data)[grep(".[.](nsp|ne_sait_pas|je_ne_sais_pas)", names(data))]

####################################################################################################
# creation des variables de date et duration
# date de collection de donnees
data$today <- as.Date(data$start, "%Y-%m-%d")

# check du temps start - end avec la fonction de temps
data <- ver_temps(data)

#######################################################################################################
### checks de données

### checks generales
#  uuid duplicee
index    <- pulluuid(data, duplicated(data$uuid))
logbook  <- make_log(data, logbook, "id25", index, "uuid", "UUID doublee")

##duree d'enquete ne depasse pas quinze minte
index    <- pulluuid(data, data$duration_entretien < duration_min)
logbook  <- make_log(data, logbook,"id01", index,"duration_entretien","Duree d'enquete trop courte")

##duree d'enquete depasse 110mins
index    <-   pulluuid(data, data$duration_entretien > duration_max)
logbook  <-   make_log(data, logbook,"id02",index,"duration_entretien","Duree d'enquete trop longue")

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
logbook <- make_log(data, logbook, "id05", index,"consensus_note","pas de consentement pour enquete",action = "supprimez")

### Checks particulaires des indicateurs
# total_deces > 2 --> signalé dans le journal de nettoyage des données 
index   <- pulluuid(data, data$total_deces > 2)
logbook <- make_log(data, logbook, "id19", index, "total_deces", "Nombre de personnes decedees au cours du dernier mois eleve ")

# jr_consom_cereale < 5 jours par semaine
index   <- pulluuid(data, data$jr_consom_cereale < 5)
logbook <- make_log(data, logbook, "id20", index, "jr_consom_cereale", "Le menage consomme des cereales moins de 5 jours par semaine.")


