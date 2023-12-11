setwd('ENSG-DataScience-UE2-AnalyseSpatiale-2023/3-InteractionsSpatiales/TP/')

#####
# TP : Interaction spatiales
#

#########
# 1) Analyse du reseau Europeen de filiales d'entreprises
#    Raimbault, J., Zdanowska, N., & Arcaute, E. (2020). Modeling growth of urban firm networks. arXiv preprint arXiv:2009.05528.
#
#   -> Aires urbaines Fonctionelles Europeennes (base GHSL, Joint Research Center Commission Europeenne)
#   -> liens d'appartenance entre entreprises agreges (poids du lien: turnover pondere)
#      base ORBIS : Rozenblat, C. (2021). Intra and inter-city networks of multinational firms (2010-2019). Handbook on cities and networks, 511-556.
#   
#  Caractéristiques des aires urbaines:
#    * turnover des entreprises
#    * parts de differents secteurs d'activite (secteurs d'activité: https://ec.europa.eu/competition/mergers/cases/index/nace_all.html)
#    * pays
#    * population
#    * gdp
# 
#  Caractéristiques des liens:
#    * origine
#    * destination
#    * poids: turnover pondéré
#    * turnover a l'origine
#    * turnover a destination
#    * pays d'origine
#    * pays de destination
#    * distance geographique
#    * similarite entre structure industrielle
# 

library(readr)
library(dplyr)
library(sf)
library(mapsf)

# 1.1) Charger les donnees: data/firmsnetwork/{cities.csv,links.csv}
cities <- read_csv('data/firmsnetwork/cities.csv')
links <- read_csv('data/firmsnetwork/links.csv')

# 1.2) Cartographier la specialisation des aires urbaines

# cartographier la proportion de certains secteurs
# secteur C: manufacture; secteur K: finance
st_cities = st_as_sf(cities, coords = c("X","Y"), crs = 4326)

mf_base(st_cities,col = 'white')
mf_map(st_cities, type = 'prop_choro', var = c('turnover','sectorC'), leg_pos = c(1,1))

mf_base(st_cities,col = 'white')
mf_map(st_cities, type = 'prop_choro', var = c('turnover','sectorK'), leg_pos = c(1,1))


# - avec fond de carte pays https://www.naturalearthdata.com/downloads/110m-cultural-vectors/
#  -> fichier data/pays/ne_110m_admin_0_countries.shp
countries <- st_read('data/pays','ne_110m_admin_0_countries')

mf_base(countries, extent = st_bbox(st_cities))
mf_map(st_cities, type = 'prop_choro', var = c('turnover','sectorC'), leg_pos = c(1,1))


# - calculer la spécialisation
#  -> definir une fonction calculant la specialisation dans un secteur donné
#  spec_j = part_locale(activ_j) / part_globale(activ_j)
activites = c("sectorB","sectorC","sectorM","sectorK","sectorG","sectorD","sectorJ", "sectorH","sectorF", 
              "sectorI","sectorO","sectorN","sectorL","sectorS","sectorE","sectorA","sectorR","sectorQ",
              "sectorP","sectorT","sectorU")
specialisation <- function(sector){
  unlist(st_drop_geometry(st_cities[,sector])/
    (sum(st_drop_geometry(st_cities[,sector]))/ nrow(st_cities)))
}
specialisation('sectorC')

# cartographier avec mf_map
st_cities$specC = specialisation('sectorC')
mf_base(st_cities,col = 'white')
mf_map(st_cities, type = 'prop_choro', var = c('turnover','specC'), leg_pos = c(1,1))

st_cities$specK = specialisation('sectorK')

par(mfrow=c(2,1))
mf_base(st_cities,col = 'white')
mf_map(st_cities, type = 'prop_choro', var = c('turnover','sectorK'), leg_pos = c(1,1))

mf_base(st_cities,col = 'white')
mf_map(st_cities, type = 'prop_choro', var = c('turnover','specK'), leg_pos = c(1,1))
# -> specialisation est la proportion rescalee par une constante -> interet juste dans l'interpretation (% 1)


#########
# 1.3) Modeles d'interaction spatiale simples

# fitter un modele avec la distance uniquement pour expliquer le poids des liens (weight)
modele_simple_dist = lm(log(weight)~log(distance),data=links)
summary(modele_simple_dist)
# ! les liens nuls ont déjà été filtrés -> biais potentiel
# Interpretation : coef significatif pour la distance, effet attendu (négatif), mais modèle au R2 quasi nul 

# idem avec similarité, comparer



# essayer en ajoutant d'autres variables



# verifier la presence d'overfitting (fonction AIC)



# déterminer le meilleur modèle en termes d'AIC




# 1.4) Modeles contraints (origine et/ou destination)
#  -> utiliser des effets fixes

# origin



# destination 




# contrainte double



# effets fixes pays origin / pays destination



##########
# 1.5) Modeles de poisson
#  utiliser glm(...,family = poisson(link='log')) : generalized linear model
#  ! pour Poisson, les poids doivent être entiers





#########
# 2) Flux quotidiens en ile-de-france par mode de transport, issus de l'EGT 2010


# Table des flux




# Fitter des modèles simples (pour chaque mode, pour l'ensemble des modes)




# Matrices de temps de trajet




# jointure



# Fitter des modèles prenant en compte la distance-temps réseau


# donnees socio-economiques (a l'IRIS): raffiner les modèles



# charger dans une liste



# garder seulement la population en 2011, aggreger à la commune, ajouter a la table des flux (origine), idem destination


# idem avec income et employment


# Modele complet pour chaque mode



# Faire des modèles au niveau des IRIS





