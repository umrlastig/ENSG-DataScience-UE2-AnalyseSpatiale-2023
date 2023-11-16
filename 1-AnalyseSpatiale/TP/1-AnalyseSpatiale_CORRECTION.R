
#####
# TP : Analyse spatiale


#########
# 1 ) Analyse d'un semis de points : statistiques de synthèse
# Aires urbaines françaises (50 plus grandes) - evolution du centre de gravité - population (1831 - 2000)

#' Données: coordonnées et nom des villes (data/FRUrbanAreas/coords.csv), 
#' populations dans le temps (data/FRUrbanAreas/pop50.csv),
#' dates (data/FRUrbanAreas/dates.csv)
#'  * pas de header
#'  * utiliser de préférence dplyr et readr (tidyverse) 

library(dplyr)
library(readr)
library(ggplot2)

# 1.1) charger les données
# - Question : système de coordonnées?
# -> Lambert II hectometrique

coords <- read_csv("data/FRUrbanAreas/coords.csv",col_names = F)
colnames(coords)<-c("name","x","y")
  
populations <- read_csv("data/FRUrbanAreas/pop50.csv",col_names = F)
dates <- read_csv("data/FRUrbanAreas/dates.csv", col_names = F)

colnames(populations)<- as.character(dates$X1)

# -  coordonnees cohérentes ?
plot(coords$x,coords$y)


#  - population totale par annee?
totalPop = colSums(populations)
plot(dates$X1,totalPop, type='l')

# population de la plus grande ville (Paris)
points(dates$X1,populations[1,],type = 'l',add=T,col='red')


# 1.2) calculer point moyen, point moyen pondéré, distance-type pour chaque date

#  - point moyen 
meanx = mean(coords$x)
meany = mean(coords$y)

plot(coords$x,coords$y)
points(meanx,meany,col='red')


#  - point moyen pondéré  pour 1831
wmeanx = sum(populations$`1831` * coords$x)/sum(populations$`1831`)
wmeany = sum(populations$`1831` * coords$y)/sum(populations$`1831`)

plot(coords$x,coords$y);points(wmeanx,wmeany,col='red');points(meanx,meany,col='blue')



#  - boucle pour chaque année
wmean = list()
for(date in colnames(populations)){
  wmeanx = sum(populations[,date] * coords$x)/sum(populations[,date])
  wmeany = sum(populations[,date] * coords$y)/sum(populations[,date])
  wmean[[date]] = c(wmeanx,wmeany)
}

#  -  meme chose avec apply
wmean = apply(populations, MARGIN = 2, function(p){
  wmeanx = sum(p * coords$x)/sum(p)
  wmeany = sum(p * coords$y)/sum(p)
  return(c(wmeanx,wmeany))
})
wmean=data.frame(t(wmean))
colnames(wmean)<- c("x","y")
wmean$date = as.numeric(colnames(populations))

plot(coords$x,coords$y);points(wmean$x,wmean$y,col='blue')

plot(wmean$x,wmean$y,col='blue')

ggplot(wmean,aes(x=x,y=y,col=date))+geom_point()


# 1.3) cartographier les villes et l'évolution du point moyen
#  Question : quel package pour cartographier "simplement" ? -> ggplot (function geom_sf)
#  Données supplémentaires: limites des régions: data/regions/regions_2015_metropole_region.shp
#   ! systèmes de coordonnées à gérer

library(sf)

regions <- st_read(dsn='data/regions/',layer='regions_2015_metropole_region')


#  - voir les drivers disponibles (formats de fichiers)



# - plot point moyen uniquement



#  - "carte" avec point moyen et regions


# -  meme carte "zoomée"



#  - distance type ponderée




# 1.4) faire de même avec le point médian et point médian pondéré
#  package pour calculer le point median en 2d:

library(ICSNP)

#  - point median


#  (difficile) - point median "pondéré" ?
#  -> utiliser une technique type bootstrap en générant des points synthétiques en quantité proportionelle aux populations





#######
#  2 ) Analyse d'un semis de points: 


# 2.1) Charger les données d'OpenStreetMap:
#  * données gpkg
#  * fichiers disponibles: data/osmdata/
#   c("Architecte.gpkg","Courtier_immobilier.gpkg","Hôtels.gpkg",
#    "Auberge.gpkg","École_élémentaire.gpkg","Lycée.gpkg",
#    "Cabinet_avocats.gpkg","École_maternelle.gpkg","Motel.gpkg",
#    "Chambredhôte.gpkg","Ecole_primaire.gpkg","Notaires.gpkg",
#    "Collège.gpkg","Enseignement_Supérieur.gpkg","Salon_de_coiffure.gpkg",
#     "Comptable.gpkg","Géomètre.gpkg")
#   -> a regrouper par type d'activité: éducation, prof. libérales, logements, coiffeurs
#   (choisir une activité ou ne charger qu'un seul fichier pour l'instant)

library(sf)
coiffeurs = st_read("data/osmdata/Salon_de_coiffure.gpkg")
facs = st_read("data/osmdata/Enseignement_Supérieur.gpkg")

st_read(dsn = 'data/regions/',layer = 'regions_2015_metropole_region')

# - systeme de coordonnees?


#  - reprojection vers "EPSG:2154"



# 2.2) Calculer l'indice de plus proche voisin dans le cas d'un faible nombre de points (universités par exemple)



# 2.3) Cartographier la densité des points



# 2.4) Charger le recensement 2017 au niveau départemental (niveau d'agrégation pour l'analyse statistique)
#   * fichier csv population data/insee/Departements.csv
#   * fichier shapefile data/departements/DEPARTEMENT.shp
#  puis agréger les aménités au niveau départemental

library(readr)
library(dplyr)
library(ggplot2)

popdeps = read_delim('data/insee/Departements.csv', delim=";")
deps = read_sf(dsn='data/departements/',layer='DEPARTEMENT')

# - jointure


# 2.5) Corréler les effectifs à la population





# -  joindre les resultats au sf departements



# 2.6) Calculer des indices de concentration

#  - Construction des comptages pour l'ensemble des activites par departement
#  avec une boucle sur les noms de fichier, repeter l'operation precedente d'aggregation et jointure
activityfiles = c(archi="Architecte.gpkg",immo="Courtier_immobilier.gpkg",hotel="Hôtels.gpkg",
                  auberge="Auberge.gpkg",ecole="École_élémentaire.gpkg",lycee="Lycée.gpkg",avocats="Cabinet_avocats.gpkg",
                  maternelle="École_maternelle.gpkg",motel="Motel.gpkg",chambrehote="Chambredhôte.gpkg",
                  primaire="Ecole_primaire.gpkg",notaires="Notaires.gpkg",
                  college = "Collège.gpkg",enssup="Enseignement_Supérieur.gpkg",coiffeur="Salon_de_coiffure.gpkg",
                  comptable= "Comptable.gpkg",geometre="Géomètre.gpkg")




#  - specialisation en ens sup parmi education


# - prof liberales

# - cartographie

library(mapsf)


# 2.7) Calculer l'autocorrélation spatiale

# - a la main (produits de matrices)

#  - Moran avec le package spdep, fonction moran.test
library(spdep)

# - indice de geary


# - indice de Moran local (LISA)


