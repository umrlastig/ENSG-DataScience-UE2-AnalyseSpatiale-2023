setwd('AnalyseSpatiale-UE2/2-InteractionsSpatiales/TP/')

#####
# TP : Interaction spatiales
#

#########
# 1) Analyse du reseau Europeen de filiales d'entreprises
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
#    * turnover a destination, 
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



# 1.2) Cartographier la specialisation des aires urbaines


# cartographier- la proportion de certains secteurs


# - avec fond de carte pays https://www.naturalearthdata.com/downloads/110m-cultural-vectors/
#  -> fichier data/pays/ne_110m_admin_0_countries.shp


# - calculer la spécialisation
#  -> definir une fonction calculant la specialisation dans un secteur donné
#  spec_j = part_locale(activ_j) / part_globale(activ_j)
activites = c("sectorB","sectorC","sectorM","sectorK","sectorG","sectorD","sectorJ", "sectorH","sectorF", 
              "sectorI","sectorO","sectorN","sectorL","sectorS","sectorE","sectorA","sectorR","sectorQ",
              "sectorP","sectorT","sectorU")



# cartographier avec mf_map




#########"
# 1.3) Modeles d'interaction spatiale simples

# fitter un modele avec la distance uniquement pour expliquer le poids des liens (weight)




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





