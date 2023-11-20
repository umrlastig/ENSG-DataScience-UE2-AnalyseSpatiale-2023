
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
st_drivers()


# - plot point moyen uniquement
g=ggplot(data=wmean,aes(x=x,y=y,col=date))
g+geom_point()


#  - "carte" avec point moyen et regions
g=ggplot(data=regions)
g+geom_sf(aes(fill=st_coordinates(st_centroid(regions))[,1]))+
  geom_point(data=wmean,aes(x=x*100,y=y*100,col=date))


# -  meme carte "zoomée"
g=ggplot(data=regions[regions$RégION%in%c("Bourgogne et Franche-Comté","Centre"),])
g+geom_sf()+geom_point(data=wmean,aes(x=x*100,y=y*100,col=date))



#  - distance type ponderée
wsigma = apply(populations,MARGIN = 2,FUN = function(pop){
  currenttotalpop = sum(pop)
  wmeanx = sum(pop*coords$x)/currenttotalpop
  wmeany = sum(pop*coords$y)/currenttotalpop
  sigma = sqrt(sum(pop*((coords$x - wmeanx)^2 + (coords$y - wmeany)^2))/currenttotalpop)
  return(sigma)
}
)

ggplot(data=data.frame(date=dates$X1,sigma=wsigma),aes(x=date,y=sigma))+geom_line()




# 1.4) faire de même avec le point médian et point médian pondéré
#  package pour calculer le point median en 2d:

library(ICSNP)

#  - point median
spmedian = spatial.median(coords[,c("x","y")])

ggplot()+geom_point(data=wmean,aes(x=x*100,y=y*100,col=date))+
  geom_point(data=data.frame(x=spmedian[1]*100,y=spmedian[2]*100),aes(x=x,y=y),color='red',shape=3)


#  (difficile) - point median "pondéré" ?
#  -> utiliser une technique type bootstrap en générant des points synthétiques
#      en quantité proportionelle aux populations
wspmedians=data.frame()
for(date in as.character(dates$X1)){
  synthpoints=coords[,c("x","y")]
  for(i in 1:nrow(coords)){
    addpoints=matrix(data=jitter(rep(unlist(c(coords[i,c("x","y")])), floor(populations[i,date]/100)),
                                 amount=100),ncol=2,byrow = T)
    colnames(addpoints)=c("x","y")
    synthpoints=rbind(synthpoints,addpoints)
  }
  show(dim(synthpoints))
  wspmedian = spatial.median(synthpoints)
  show(wspmedian)
  wspmedians=rbind(wspmedians,wspmedian)
}
wspmedians$date=dates$X1
names(wspmedians)<-c("x","y","date")

ggplot()+geom_point(data=wmean,aes(x=x*100,y=y*100,col=date))+
  geom_point(data=wspmedians,aes(x=x*100,y=y*100,col=date),shape=3)





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

regions = st_read(dsn = 'data/regions/',layer = 'regions_2015_metropole_region')

deps = read_sf(dsn='data/departements/',layer='DEPARTEMENT')

# - systeme de coordonnees?
st_crs(coiffeurs)

#  - reprojection vers "EPSG:2154"
coiffeurs = st_transform(coiffeurs,st_crs(deps))
facs = st_transform(facs,st_crs(deps))

regions = st_transform(regions,st_crs(deps))

# 2.2) Calculer l'indice de plus proche voisin dans le cas
#      d'un faible nombre de points (universités par exemple)
distancefacs = st_distance(facs)
nearestneighdists = apply(distancefacs,1,function(r){min(r[r>0])})
nndindex = 2*sqrt(nrow(facs)/sum(st_area(regions)))*mean(nearestneighdists)


# 2.3) Cartographier la densité des points
coiffeursmetro = st_filter(coiffeurs,regions)

g=ggplot(regions)
g+geom_sf()+geom_density2d_filled(
  data=data.frame(st_coordinates(coiffeursmetro)),
  mapping=aes(x=X,y=Y),alpha=0.5
  )



# 2.4) Charger le recensement 2017 au niveau départemental
#    (niveau d'agrégation pour l'analyse statistique)
#   * fichier csv population data/insee/Departements.csv
#   * fichier shapefile data/departements/DEPARTEMENT.shp
#  puis agréger les aménités au niveau départemental

library(readr)
library(dplyr)
library(ggplot2)


popdeps = read_delim('data/insee/Departements.csv', delim=";")
#deps = read_sf(dsn='data/departements/',layer='DEPARTEMENT')

# - jointure
deps = left_join(deps,popdeps[,c("CODDEP","PTOT")],
                 by=c("CODE_DEPT"="CODDEP"))


# 2.5) Corréler les effectifs à la population
joincoiffeurs = st_join(coiffeursmetro, deps)
aggrcoiffeurs = joincoiffeurs %>% group_by(CODE_DEPT) %>%
  summarise(numcoiffeur = n(), population = PTOT[1])

cor.test(aggrcoiffeurs$numcoiffeur,aggrcoiffeurs$population)
cor.test(aggrcoiffeurs$numcoiffeur,aggrcoiffeurs$population,method = "spearman")


ggplot(data.frame(logcoif=log(aggrcoiffeurs$numcoiffeur),logpop=log(aggrcoiffeurs$population)),aes(x=logpop,y=logcoif))+
  geom_point()+geom_smooth()

summary(lm(data=data.frame(logcoif=log(aggrcoiffeurs$numcoiffeur),logpop=log(aggrcoiffeurs$population)),
           formula = logcoif~logpop
))
summary(lm(data=data.frame(coif=aggrcoiffeurs$numcoiffeur,pop=aggrcoiffeurs$population),
           formula = coif~pop
))




# -  joindre les resultats au sf departements
deps=left_join(deps,as_tibble(aggrcoiffeurs[,c("CODE_DEPT","numcoiffeur")]),
               by=c("CODE_DEPT"="CODE_DEPT"))
aggrfacs = st_join(facs, deps) %>% group_by(CODE_DEPT) %>%
  summarise(numfacs = n(), population = PTOT[1])

summary(lm(data=data.frame(logfacs=log(aggrfacs$numfacs),logpop=log(aggrfacs$population)),
           formula = logfacs~logpop
))

deps=left_join(deps,as_tibble(aggrfacs[,c("CODE_DEPT","numfacs")]),
               by=c("CODE_DEPT"="CODE_DEPT"))

# - cartes: package mapsf 
library(mapsf)

mf_map(x = deps, var = "numcoiffeur", type = "choro")

mf_map(x = deps, var = "numfacs", type = "choro")



# 2.6) Calculer des indices de concentration

#  - Construction des comptages pour l'ensemble des activites par departement
#  avec une boucle sur les noms de fichier, repeter l'operation precedente d'aggregation et jointure
activityfiles = c(archi="Architecte.gpkg",immo="Courtier_immobilier.gpkg",hotel="Hôtels.gpkg",
                  auberge="Auberge.gpkg",ecole="École_élémentaire.gpkg",lycee="Lycée.gpkg",avocats="Cabinet_avocats.gpkg",
                  maternelle="École_maternelle.gpkg",motel="Motel.gpkg",chambrehote="Chambredhôte.gpkg",
                  primaire="Ecole_primaire.gpkg",notaires="Notaires.gpkg",
                  college = "Collège.gpkg",enssup="Enseignement_Supérieur.gpkg",coiffeur="Salon_de_coiffure.gpkg",
                  comptable= "Comptable.gpkg",geometre="Géomètre.gpkg")


aggregate_activity <- function(activitename,locfiles=activityfiles,locdeps=deps){
  activite = st_transform(st_read(paste0("data/osmdata/",locfiles[[activitename]])),st_crs(locdeps))
  aggractivite = st_join(activite, locdeps) %>% group_by(CODE_DEPT) %>% summarise(num = n())
  aggractivite[[activitename]] = aggractivite$num
  return(left_join(locdeps,as_tibble(aggractivite)[,c("CODE_DEPT",activitename)],by=c("CODE_DEPT"="CODE_DEPT")))
}


for(activitename in names(activityfiles)){
  show(activitename)
  deps = aggregate_activity(activitename)
}

specialisation <- function(departements,activites,activitespec){
  counts = as_tibble(departements)[,activites]
  counts[is.na(counts)]=0
  localshare = counts[,activitespec] / rowSums(counts)
  globalShare = sum(counts[,activitespec])/sum(counts)
  return(localshare/globalShare)
}


#  - specialisation en ens sup parmi education
deps$specfac = specialisation(deps,c("ecole","lycee","maternelle","primaire","college","enssup"),"enssup")[[1]]

# - avocats parmi prof liberales
deps$specavocat = specialisation(deps,c("archi","immo","avocats","notaires","comptable","geometre"),"avocats")[[1]]



# - cartographie
mf_map(x = deps, var = "specfac", type = "choro")

mf_map(x = deps, var = "specavocat", type = "choro")



# 2.7) Calculer l'autocorrélation spatiale

# - a la main (produits de matrices)
weightMatrix<-function(layer,FUN){
  d = units::drop_units(st_distance(st_centroid(layer)))
  #w = exp(-d)/decay)
  w = FUN(d)
  diag(w)<-0
  return(w)
}

spAutocorr<-function(x,w){
  n=length(x)
  cx = x - mean(x)
  cxvec=matrix(cx,nrow=n,ncol=1)
  normalization = n/(sum(w)*sum(cxvec*cxvec))
  return(sum(((
    matrix(data = rep(cx,n),ncol = n,nrow = n,byrow = FALSE)*w)%*%cxvec))*normalization)
}

w=weightMatrix(deps, function(x){exp(-x/10000)})

spAutocorr(deps$coiffeur,w)

# TODO debug

#  - Moran avec le package spdep, fonction moran.test
library(spdep)

depsnb = poly2nb(deps)
w = nb2listw(depsnb)

moran.test(deps$coiffeur,w,na.action = na.omit)

# TODO from manual weight matrix


# - indice de geary
geary.test(deps$coiffeur,w)

# - indice de Moran local (LISA)
loccoiff = localmoran(deps$coiffeur,w)

deps$loccoiff = loccoiff[,1]
mf_map(x = deps, var = "loccoiff", type = "choro")

# TODO carto categs
deps$categ = loccoiff@quadr


