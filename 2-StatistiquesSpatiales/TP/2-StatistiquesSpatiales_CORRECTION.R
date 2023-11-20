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

d = st_drop_geometry(deps)

# exploration des données: cartes

# exploration des données: PCA / corrélations


# modele lineaire non spatialise
lmglobal = lm(prix~MED20+PPEN20+PPAT20+surface_bati+surface_terrain,data=d)
summary(lmglobal)

# cartographier résidus du modèle linéaire
deps$global_residuals = lmglobal$residuals
mf_map(x = deps, var = "global_residuals", type = "choro")


# tests autocorrelation spatiale
library(spdep)
depsnb = poly2nb(deps)
w = nb2listw(depsnb)

lm.morantest(lmglobal, w, alternative="two.sided")

lm.LMtests(lmglobal, w, test=c("LMerr","LMlag"))

# fonction weightMatrix du tp 1
w=weightMatrix(deps, function(x){exp(-x/50000)})
w = mat2listw(w)
lm.morantest(lmglobal, w, alternative="two.sided")





#########
#  2 ) Geographically weighted regression
#


# 2.1) Tester des modèles GWR à bandwidth fixe
# package GWmodel

library(GWmodel)

# GWR simple
gwbasic <- gwr.basic(prix~PTOT+MED20+PPEN20+PPAT20+surface_bati+surface_terrain,
                     data=as(deps, "Spatial"),
                     bw=10,
                     kernel="bisquare",
                     adaptive=TRUE)
print(gwbasic)

# cartographier coefficients
coefs = gwbasic$SDF@data

deps$localR2=coefs$Local_R2
deps$residual = coefs$residual
deps$alpha_population = coefs$PTOT
deps$alpha_revenu = coefs$MED20
deps$alpha_retraite = coefs$PPEN20
deps$alpha_patrimoine = coefs$PPAT20
mf_map(x = deps, var = "localR2", type = "choro")
mf_map(x = deps, var = "residual", type = "choro")
mf_map(x = deps, var = "alpha_population", type = "choro")
mf_map(x = deps, var = "alpha_revenu", type = "choro")
mf_map(x = deps, var = "alpha_retraite", type = "choro")
mf_map(x = deps, var = "alpha_patrimoine", type = "choro")

# 2.2) Optimiser la bandwidth selon un critère d'AIC
# bandwidth adaptative en nombre de voisins
bwfullaic = bw.gwr(prix~PTOT+MED20+PPEN20+PPAT20+surface_bati+surface_terrain,
                   data=as(deps, "Spatial"),
                   approach="AIC",
                   kernel="bisquare",
                   adaptive=T)

gwopt <- gwr.basic(prix~PTOT+MED20+PPEN20+PPAT20+surface_bati+surface_terrain,
                     data=as(deps, "Spatial"),bw=bwfullaic,kernel="bisquare",
                     adaptive=TRUE)

# 2.3) Selection de modèle (méthode de "forward selection")
gwselec = gwr.model.selection(DeVar = "prix",
                              InDeVars = c("PTOT","MED20","PPEN20","PPAT20","surface_bati","surface_terrain"),
                              data=as(deps, "Spatial"),bw = bwfullaic,
                              approach="AIC", kernel="bisquare",adaptive=T)
print(gwselec)
aicc = gwselec[[2]][,3]
gwselec[[1]][which(aicc==min(aicc))]

gwopt <- gwr.basic(prix~PTOT+MED20+PPEN20+PPAT20+surface_terrain,
                   data=as(deps, "Spatial"),bw=bwfullaic,kernel="bisquare",
                   adaptive=TRUE)
coefs = gwopt$SDF@data

deps$alpha_population = coefs$PTOT
deps$alpha_revenu = coefs$MED20
mf_map(x = deps, var = "alpha_population", type = "choro")
mf_map(x = deps, var = "alpha_revenu", type = "choro")


#####
## 3 ) Auto-regression spatiales

library(spatialreg)

# modèle de Durbin spatial
depsnb = poly2nb(deps)
w = nb2listw(depsnb)
spatdurbin = lagsarlm(prix~PTOT+MED20+PPEN20+PPAT20+surface_bati+surface_terrain, data=d, w)
summary(spatdurbin)

# modèle avec erreur spatiale
spaterror = errorsarlm(prix~PTOT+MED20+PPEN20+PPAT20+surface_bati+surface_terrain, data=d, w)
summary(spaterror)

# changer la matrice de poids, les spécifications des modèles

bws=seq(from=1,to=100,by=5)
aics = c()
for(bw in 1000*bws){
  show(bw)
  w=mat2listw(weightMatrix(deps, function(x){exp(-x/bw)}))
  spatdurbin = lagsarlm(prix~PTOT+MED20+PPEN20+PPAT20+surface_bati+surface_terrain, data=d, w)
  aics = append(aics,AIC(spatdurbin))
}
bws[aics==min(aics)]

w=mat2listw(weightMatrix(deps, function(x){exp(-x/26000)}))
spatdurbin = lagsarlm(prix~PTOT+MED20+PPEN20+PPAT20+surface_bati+surface_terrain, data=d, w)
summary(spatdurbin)

#####
##  4 ) Regressions multi-niveaux

library(lme4)

# modèle simple avec intercepts variables
multiniv_intercept =
  lmer(prix~PTOT+MED20+PPEN20+PPAT20+surface_bati+surface_terrain + (1 | NOM_REG),
     data=d)
summary(multiniv_intercept)

# modèle simple avec coefficients variables
multiniv_slopes =
  lmer(prix~PTOT+MED20+PPEN20+PPAT20+surface_bati+surface_terrain +
         (PTOT+MED20+PPEN20+PPAT20+surface_bati+surface_terrain | NOM_REG),
       data=d)
summary(multiniv_slopes)

# comparer les modèles et en tester d'autres
AIC(multiniv_intercept)
AIC(multiniv_slopes)

# comparaison à GWR et interprétation

# TODO recuperer effets fixes


