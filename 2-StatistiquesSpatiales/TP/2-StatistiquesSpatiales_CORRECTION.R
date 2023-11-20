####
# TP : Statistiques spatiales


#########
#  1) Préparation des données


library(readr)
library(sf)
library(dplyr)
library(mapsf)

rawdvf2021 <- read_csv(file = 'https://files.data.gouv.fr/geo-dvf/latest/csv/2021/full.csv.gz')

toremove=unique(rawdvf2021[!is.na(rawdvf2021$valeur_fonciere)&
                    rawdvf2021$valeur_fonciere>200000000,c("id_mutation")])

# agrégation au niveau départemental (via code_departement)
depdvf = rawdvf2021 %>%
  filter(type_local%in%c('Appartement', 'Maison')) %>%
  filter(!id_mutation%in%unlist(toremove)) %>%
  group_by(code_departement) %>% 
  summarise(
    prix = median(valeur_fonciere, na.rm=T),
    surface_bati = median(surface_reelle_bati, na.rm=T),
    surface_terrain = median(surface_terrain, na.rm=T)
  )

summary(depdvf$prix)

  

# autres données

deps = read_sf(dsn='../../1-AnalyseSpatiale/TP/data/departements/',layer='DEPARTEMENT')

popdeps = read_delim('../../1-AnalyseSpatiale/TP/data/insee/Departements.csv', delim=";")

# Insee: Filosofi 2020
insee_filosofi <- read_delim(file = 'data/filosofi/cc_filosofi_2020_DEP.csv', delim = ";")

# jointure
deps = left_join(deps,popdeps[,c("CODDEP","PTOT")],
                 by=c("CODE_DEPT"="CODDEP"))
deps = left_join(deps,insee_filosofi[,c("CODGEO","MED20","PPEN20","PPAT20")],
                 by=c("CODE_DEPT"="CODGEO"))
deps = left_join(deps,depdvf,by=c("CODE_DEPT"="code_departement"))
deps = na.exclude(deps)

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

