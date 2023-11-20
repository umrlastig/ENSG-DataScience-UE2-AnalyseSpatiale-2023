####
# TP : Statistiques spatiales


#########
#  1) Préparation des données


library(readr)
library(sf)
library(dplyr)
library(mapsf)

rawdvf2021 <- read_csv(file = 'https://files.data.gouv.fr/geo-dvf/latest/csv/2021/full.csv.gz')


# agrégation au niveau départemental (via code_departement)


# autres données

deps = read_sf(dsn='../../1-AnalyseSpatiale/TP/data/departements/',layer='DEPARTEMENT')

popdeps = read_delim('../../1-AnalyseSpatiale/TP/data/insee/Departements.csv', delim=";")

# Insee: Filosofi 2020
insee_filosofi <- read_delim(file = 'data/filosofi/cc_filosofi_2020_DEP.csv', delim = ";")

# jointure


# exploration des données: cartes



# exploration des données: PCA / corrélations


# modele lineaire non spatialise



# cartographier résidus du modèle linéaire




# tests autocorrelation spatiale








#########
#  2 ) Geographically weighted regression
#


# 2.1) Tester des modèles GWR à bandwidth fixe
# package GWmodel

library(GWmodel)



# GWR simple



# cartographier coefficients


# 2.2) Optimiser la bandwidth selon un critère d'AIC
# bandwidth adaptative en nombre de voisins



# 2.3) Selection de modèle (méthode de "forward selection")






#####
## 3 ) Auto-regression spatiales

library(spatialreg)

# modèle de Durbin spatial


# modèle avec erreur spatiale



# changer la matrice de poids, les spécifications des modèles



#####
##  4 ) Regressions multi-niveaux


library(lme4)

# modèle simple avec intercepts variables


# modèle simple avec coefficients variables


# comparer les modèles et en tester d'autres


# comparaison à GWR et interprétation

