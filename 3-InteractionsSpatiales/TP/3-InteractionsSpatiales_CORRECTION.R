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
modele_simple_sim = lm(log(weight)~log(sim),data=links)
summary(modele_simple_sim)

# essayer en ajoutant d'autres variables

# -> distance et sim
modele_simple_dsim = lm(log(weight)~log(sim)+log(distance),data=links)
summary(modele_simple_dsim)

# "vrai" modele gravitaire
modele_gravitaire = lm(log(weight)~log(sim)+log(distance)+log(from_turnover)+
                         log(to_turnover), data = links)
summary(modele_gravitaire)

# interaction distance-sim?
#modele_gravitaire_int = lm(log(weight)~log(sim)+log(distance)+log(from_turnover)+
#                         log(to_turnover)+log(sim)*log(distance), data = links)
#summary(modele_gravitaire_int)
# -> pas d'interaction

# formulation exponentielle du modele gravitaire
modele_gravitaire_exp = lm(log(weight)~sim+distance+log(from_turnover)+
                         log(to_turnover), data = links)
summary(modele_gravitaire_exp)


# verifier la presence d'overfitting (fonction AIC)
AIC(modele_gravitaire) - AIC(modele_simple_dsim)


# déterminer le meilleur modèle en termes d'AIC
sapply(list(modele_simple_dist, modele_simple_sim, modele_simple_dsim, 
            modele_gravitaire, modele_gravitaire_exp), AIC)



# 1.4) Modeles contraints (origine et/ou destination)
#  -> utiliser des effets fixes

# origin
modele_gravitaire_originconstrained = 
  lm(log(weight)~log(sim)+log(distance)+log(from_turnover)+
                         log(to_turnover)+as.character(from_fua), data = links)
summary(modele_gravitaire_originconstrained)


# destination 
modele_gravitaire_destinationconstrained = 
  lm(log(weight)~log(sim)+log(distance)+log(from_turnover)+
       log(to_turnover)+as.character(to_fua), data = links)
summary(modele_gravitaire_destinationconstrained)


# contrainte double
modele_gravitaire_ODconstrained = 
  lm(log(weight)~log(sim)+log(distance)+log(from_turnover)+
       log(to_turnover)+as.character(to_fua)+as.character(from_fua), data = links)
summary(modele_gravitaire_ODconstrained)



# effets fixes pays origin / pays destination -> "multi-niveau"
modele_gravitaire_pays = lm(log(weight)~log(distance)+log(sim)+log(from_turnover)+
                        log(to_turnover)+ from_country + to_country, data=links)
summary(modele_gravitaire_pays)

modele_gravitaire_pays_ODconstrained =
  lm(log(weight)~log(distance)+log(sim)+log(from_turnover)+
                log(to_turnover)+ from_country + to_country+
                as.character(from_fua)+as.character(to_fua), data=links)
summary(modele_gravitaire_pays_ODconstrained)

sapply(list(modele_simple_dist, modele_simple_sim, modele_simple_dsim, 
            modele_gravitaire, modele_gravitaire_exp,
            modele_gravitaire_originconstrained, modele_gravitaire_destinationconstrained,
            modele_gravitaire_ODconstrained, modele_gravitaire_pays,
            modele_gravitaire_pays_ODconstrained) , AIC)



##########
# 1.5) Modeles de poisson
#  utiliser glm(...,family = poisson(link='log')) : generalized linear model
#  ! pour Poisson, les poids doivent être entiers
links$weight_integer = round(links$weight)
gravity_poisson = glm(
  formula = weight_integer~log(distance)+log(sim)+
     log(from_turnover)+log(to_turnover)+ from_country + to_country,
  data = links,
  family = poisson(link='log')
)

summary(gravity_poisson)
AIC(gravity_poisson)
# ! aic lm et aic posson pas comparables

1 - sum((links$weight_integer - fitted(gravity_poisson))^2)/sum((links$weight_integer - mean(links$weight_integer))^2)


##
# 1.6) Carto effets fixes
fixed_from=coefficients(gravity_poisson)[grep(names(coefficients(gravity_poisson)),
                                   pattern='from_country')]
fixed_from_df = data.frame(fixed_from=fixed_from,
                           country=substring(names(fixed_from), first=13))
# noms pas forcément uniformisé avec la table sf countries (UK par exemple)
countries = left_join(countries,fixed_from_df,by=c('NAME'='country'))

mf_map(countries[!is.na(countries$fixed_from),], type="choro", var='fixed_from')


#########
# 2) Flux quotidiens en Ile-de-france par mode de transport, issus de l'EGT 2010
#  Récupérées depuis https://analytics.huma-num.fr/geographie-cites/aperitif/
# (pas de doc propre; agrégées uniquement - l'EGT n'est pas ouverte)

# Table des flux
idfflows = readRDS('data/mobility/tabflows.Rds')

# Fitter des modèles simples (pour chaque mode, pour l'ensemble des modes)
summary(lm(log(FLOW)~log(DIST), data=idfflows))

summary(lm(log(FLOW)~log(DIST), data=idfflows[idfflows$MODE=='NM',]))
# -0.68937

summary(lm(log(FLOW)~log(DIST), data=idfflows[idfflows$MODE=='TC',]))
# -0.607988

summary(lm(log(FLOW)~log(DIST), data=idfflows[idfflows$MODE=='VP',]))
# -0.681978


# Matrices de temps de trajet
times = readRDS('data/mobility/listtimes.Rds')

# jointure
idfflows = left_join(idfflows, times$TC, by=c('ORI'='ORI','DES'='DES'))
names(idfflows)[9] = "TIME_TC"
idfflows = left_join(idfflows, times$VPM, by=c('ORI'='ORI','DES'='DES'))
names(idfflows)[10] = "TIME_VPM"
idfflows = left_join(idfflows, times$VPS, by=c('ORI'='ORI','DES'='DES'))
names(idfflows)[11] = "TIME_VPS"
idfflows$TIME_VP = (idfflows$TIME_VPM + idfflows$TIME_VPS)/2

# Fitter des modèles prenant en compte la distance-temps réseau
summary(lm(log(FLOW)~log(TIME_TC), data=idfflows[idfflows$MODE=='TC',]))

summary(lm(log(FLOW)~log(TIME_VP), data=idfflows[idfflows$MODE=='VP',]))

# Modele avec distance et distance temps
summary(lm(log(FLOW)~log(TIME_TC)+log(DIST), data=idfflows[idfflows$MODE=='TC',]))

summary(lm(log(FLOW)~log(TIME_VP)+log(DIST), data=idfflows[idfflows$MODE=='VP',]))



# donnees socio-economiques (à l'IRIS): raffiner les modèles
# Ref :  Raimbault, J. (2017, November). Identification de causalités dans des données spatio-temporelles. In Spatial Analysis and GEOmatics 2017.
#   (données socio-eco uniquement)


# charger dans une liste
socioeco <- mget(
  load('data/mobility/socioeco.RData', envir=(temp <- new.env())), envir=temp)

# garder seulement la population en 2011, aggreger à la commune, ajouter a la table des flux (origine), idem destination
populations = socioeco$pops
populations = populations[populations$year=="11",]
populations$code_com = substr(populations$id,1, 5)
populations_aggr = as_tibble(populations) %>% 
  group_by(code_com) %>% summarise(population = sum(var,na.rm=T))

# ajouter a la table des flux (origine)
idfflows = left_join(idfflows,populations_aggr,by=c('ORI'='code_com'))
names(idfflows)[13] = "POP_ORI"
# idem destination
idfflows = left_join(idfflows,populations_aggr,by=c('DES'='code_com'))
names(idfflows)[14] = "POP_DES"

# idem avec income et employment
incomes = socioeco$incomes
incomes = incomes[incomes$year=="11",]
incomes$code_com = substr(incomes$id,1, 5)
incomes_aggr = as_tibble(incomes) %>% group_by(code_com) %>% summarise(income = median(var,na.rm=T)) # revenu median
idfflows = left_join(idfflows,incomes_aggr,by=c('ORI'='code_com'))
names(idfflows)[15] = "INC_ORI"
idfflows = left_join(idfflows,incomes_aggr,by=c('DES'='code_com'))
names(idfflows)[16] = "INC_DES"

employment = socioeco$employment
employment_aggr = as_tibble(employment) %>% group_by(id) %>% summarise(employment = sum(var,na.rm=T))
idfflows = left_join(idfflows,employment_aggr,by=c('ORI'='id'))
names(idfflows)[17] = "EMP_ORI"
idfflows = left_join(idfflows,employment_aggr,by=c('DES'='id'))
names(idfflows)[18] = "EMP_DES"


# Modele complet pour chaque mode
full_gravity_tc = lm(data=idfflows[idfflows$MODE=='TC',],
      log(FLOW)~log(TIME_TC)+log(POP_ORI)+log(POP_DES)+log(INC_ORI)+
        log(INC_DES)+log(EMP_ORI)+log(EMP_DES)
      )
summary(full_gravity_tc)

full_gravity_dist_tc = lm(data=idfflows[idfflows$MODE=='TC',],
        log(FLOW)~log(TIME_TC)+log(DIST)+log(POP_ORI)+log(POP_DES)+log(INC_ORI)+
          log(INC_DES)+log(EMP_ORI)+log(EMP_DES))
summary(full_gravity_dist_tc)

AIC(full_gravity_dist_tc) - AIC(full_gravity_tc)

# idem pour voiture

full_gravity_vp = lm(data=idfflows[idfflows$MODE=='VP',],
                     log(FLOW)~log(TIME_VP)+log(TIME_TC)+log(POP_ORI)+log(POP_DES)+log(INC_ORI)+
                       log(INC_DES)+log(EMP_ORI)+log(EMP_DES)
)
summary(full_gravity_vp)

full_gravity_dist_vp = lm(data=idfflows[idfflows$MODE=='VP',],
      log(FLOW)~log(TIME_VP)+log(TIME_TC)+log(DIST)+log(POP_ORI)+
        log(POP_DES)+log(INC_ORI)+log(INC_DES)+log(EMP_ORI)+log(EMP_DES))
summary(full_gravity_dist_vp)

AIC(full_gravity_dist_vp) - AIC(full_gravity_vp)

# modele avec fonction exponentielle
full_gravity_exp_vp = lm(data=idfflows[idfflows$MODE=='VP',],
        log(FLOW)~TIME_VP+TIME_TC+log(DIST)+log(POP_ORI)+
          log(POP_DES)+log(INC_ORI)+log(INC_DES)+log(EMP_ORI)+log(EMP_DES))
summary(full_gravity_exp_vp)

AIC(full_gravity_exp_vp) - AIC(full_gravity_vp)
# -> moins bien que fonction puissance

# Suggestions: 
#  - selection de modeles avec generation automatique
#  - application : scenarios sur
#    * emplois
#    * reseau de transport
#    * coefficients: par exemple prix de l'essence pour la voiture
#

