

# importez les capes de admin1 et admin2
admin1 <- sf::st_read("input/donnees_spatiales/admin1.geojson")

admin2 <- sf::st_read("input/donnees_spatiales/admin2.geojson")

admin3 <- sf::st_read("input/donnees_spatiales/admin3.geojson") %>% 
  dplyr::select(ADM1_FR, ADM2_FR, ADM3_FR)

# manipuler les noms des admin pour faire le join avec les donnees plus tard
admin3$ADM1_FR <- supp_accents(gsub("-| ", "_", tolower(admin3$ADM1_FR)))
admin3$ADM2_FR <- supp_accents(gsub("-| ", "_", tolower(admin3$ADM2_FR)))
admin3$ADM3_FR <- supp_accents(gsub("-| ", "_", tolower(admin3$ADM3_FR)))

# systeme de coordonees
WGS84 <- 4326

# importez le tableau des PCODES settlements de OCHA
ocha_villages <- read_excel("input/donnees_spatiales/SETTLEMENTS_3F_BFA_20200505.xlsx", sheet = "SETTLEMENTS_3F") %>%
  dplyr::select(pcode, POINT_X, POINT_Y) 

# selectionnez colonnes et convertir comme objet sf 
# ocha_villages_sf <- ocha_villages %>%
#   st_as_sf(
#     coords = c("POINT_X", "POINT_Y"), # choose lat and long columns in table
#     crs = WGS84)
# 
# ocha_villages_sf <- as.data.frame(ocha_villages_sf)

# filtrez les donnees pour entretiens face a face et selectionner seulement les colonnes necessaires
points_msna <- data %>% 
  filter(!is.na(gpsmenage_latitude), !is.na(gpsmenage_longitude)) %>% 
  dplyr::select(uuid, pcode, POINT_X2 = gpsmenage_longitude, POINT_Y2 = gpsmenage_latitude)


points_msna$POINT_X2 <-  as.numeric(points_msna$POINT_X2)
points_msna$POINT_Y2 <-  as.numeric(points_msna$POINT_Y2)

# joignez avec les villages OCHA pour determiner la distance entre les points
points_msna_join <- left_join(points_msna, ocha_villages, by = "pcode") %>% 
  filter(!is.na(POINT_X), !is.na(POINT_Y), !is.na(POINT_X2), !is.na(POINT_Y2))

# fonction pour convertir deux points a une ligne pour mesurer la distance
faire_lignes <- function(POINT_X1, POINT_X2, POINT_Y1, POINT_Y2) {
  st_linestring(matrix(c(POINT_X1, POINT_X2, POINT_Y1, POINT_Y2), 2, 2))
}

# faire les lignes
enquete_distance <- points_msna_join %>%
  dplyr::select(-uuid, -pcode) %>% 
  purrr::pmap(faire_lignes) %>% 
  st_as_sfc(crs = 4326) %>% 
  {tibble(uuid = points_msna_join$uuid, geometry = .)} %>% 
  st_sf() 

# mesurer la distance
enquete_distance$distance_m <- round(as.numeric(st_length(enquete_distance)),1)
st_geometry(enquete_distance) <- NULL

# join spatiale pour obtenir les admin vrai des points GPS
# convertir comme objet sf 
points_msna_sf <- points_msna %>%
  st_as_sf(
    coords = c("POINT_X2", "POINT_Y2"), # choose lat and long columns in table
    crs = WGS84)

# join spatiale pour obtenir les admin vrai des points GPS
gps_df <- st_join(points_msna_sf, admin3)%>% 
  dplyr::select(uuid, admin1_gps = ADM1_FR, admin2_gps = ADM2_FR, admin3_gps = ADM3_FR, pcode)

st_geometry(gps_df) <- NULL

# joignez avec les distances
gps_df <- left_join(gps_df, enquete_distance, by = "uuid") %>% 
  dplyr::select(uuid, distance_m, admin1_gps, admin2_gps, admin3_gps)

# joignez a les donnees
data <- left_join(data, gps_df, by = "uuid")

data <- data %>% 
  mutate(distance_m = case_when(is.na(distance_m) ~  NA_real_, TRUE ~ distance_m),
         distance_grande = case_when(distance_m > min_distance ~ 1, TRUE ~ 0) )

