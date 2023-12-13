
##
# TP Reseaux

library(igraph)
library(ggplot2)

#####
## Partie 1
# graphes aleatoires


# 1.1) tester la generation de graphe aléatoire: igraph::erdos.renyi.game()
n = 1000
g = erdos.renyi.game(n = n, p.or.m = 0.005)

gdir = erdos.renyi.game(n = n, p.or.m = 0.005, directed = T)

# calculer la densite
ecount(g)/(vcount(g)*(vcount(g)-1)/2)

# noeuds, liens, attributs
V(g)
E(g)
V(g)$id = 1:vcount(g)
V(g)$name = paste0("sommet_",V(g)$id)

# definir des poids pour les liens
# (aléatoires uniformes)
E(g)$weight = runif(ecount(g))


# matrice d'adjacence : application aux chemins d'une longueur donnée
A = as_adj(g)

A_full = as_adj(erdos.renyi.game(100,0.8))

A_square = A%*%A
sum(A_square!=0)/(n*n)

# nombre de composantes connexes?
composantes = components(g)
composantes

# taille de la plus grande composante en fonction de p (étude de percolation)
sizes=c()
numcomps = c()
all_p = seq(from=0.0001,to=0.01, by = 0.0001)
for(p in all_p){
  g=erdos.renyi.game(n,p)
  comps=components(g)
  sizes = append(sizes, max(comps$csize))
  numcomps = append(numcomps,comps$no)
}
plot(all_p, sizes)
plot(all_p, numcomps)


# extraire le sous-graphe correspondant à la plus grosse composante
g = erdos.renyi.game(n = n, p.or.m = 0.005)
composantes = components(g)
largestcomp = which(composantes$csize==max(composantes$csize))
v_in_largest = V(g)[composantes$membership==largestcomp]
subg = induced_subgraph(g, v_in_largest)

# diametre du graphe
diameter(g)
diameter(subg)

# diametre pondere
E(subg)$weight = runif(ecount(subg))
diameter(subg,weights = E(subg)$weights)

# diametre en fonction taille et proba du graphe aleatoire
res=data.frame()
for(n in seq(100,500,100)){
  show(n)
  for(logp in seq(-3,-0.5,0.1)){
    for(k in 1:10){
      g = erdos.renyi.game(n = n, p.or.m = 10^logp)
      d=diameter(g, unconnected = T)
      res = rbind(res,c(n,logp,k,d))
    }
  }
}
colnames(res)<-c("n","p","k","d")
ggplot(res,aes(x=p,y=d,col=n,group=n))+geom_smooth()



# plotter le graphe
g = erdos.renyi.game(n = 100, p.or.m = 0.1)
plot(g, vertex.size=0, vertex.label=NA)


# layouts: algorithme de spatialisation du graphe
# -> tester layout fruchterman reingold : layout_with_fr
coords = layout_with_fr(g)
V(g)$x = coords[,1]
V(g)$y = coords[,2]
plot(g, vertex.size=0, vertex.label=NA)



# 1.2) Generer et plotter un graphe en grille (lattice): igraph::make_lattice
g=make_lattice(dimvector = c(50,50))
coords = layout_on_grid(g)
V(g)$x = coords[,1]
V(g)$y = coords[,2]
plot(g, vertex.size=0, vertex.label=NA)


# Supprimer des liens aléatoirement dans le graphe en grille
p_garde=0.5
liens = sample(E(g),size = floor(p_garde*ecount(g)),replace = F)
gsub = subgraph.edges(g,liens,delete.vertices = T)
plot(gsub, vertex.size=0, vertex.label=NA)

#  étudier la taille de la plus grande composante connexe
#  en fonction de la proportion de liens gardés et de la taille du graphe

largest_comp_size <- function(g){
  comps = components(g)
  largestcomp = which(comps$csize==max(comps$csize))
  return(length(which(comps$membership==largestcomp)))
}

compsizes = data.frame()
for(n in seq(10,100,10)){
  show(n)
  for(p in seq(0.45,0.7,0.01)){
    for(k in 1:20){ # repetitions aleatoires
      g_rep=make_lattice(dimvector = c(n,n))
      liens_gardes = sample.int(n=ecount(g_rep),size=floor(p*ecount(g_rep)),replace = F)
      gsubrep = subgraph.edges(g_rep, liens_gardes, delete.vertices = T)
      compsizes=rbind(compsizes, c(n,p,k,largest_comp_size(gsubrep)/(n*n)))
    }
  }
}
colnames(compsizes)<-c("n","p","k","size")

ggplot(compsizes,aes(x=p,y=size,col=n,group=n))+geom_smooth()



# 1.3) perturber les coordonnées des noeuds de la grille pour obtenir
#  des plus courts chemins uniques;
# étudier le diametre en fonction des liens supprimes
# algos: shortest_paths()/ distances() : algorithme adapte au cas (voir doc)
V(gsub)$x = jitter(factor = 1.0,V(gsub)$x)
V(gsub)$y = jitter(factor = 1.0,V(gsub)$y)
plot(gsub, vertex.size=0, vertex.label=NA)


# ajouter la distance euclidienne comme poids des liens
v_ends = ends(gsub,1:ecount(gsub))
lengths = apply(v_ends, 1, function(e){
  sqrt( (V(gsub)$x[e[1]] - V(gsub)$x[e[2]])^2 + 
          (V(gsub)$y[e[1]] - V(gsub)$y[e[2]])^2 )
})
E(gsub)$weight = lengths


# tous les plus courts chemins: distances
d = distances(gsub)
min(d[d>0])
max(d)
diameter(gsub,unconnected = F)
diameter(gsub)

# certains plus courts chemins: shortest_paths
path = shortest_paths(gsub,
      from = sample.int(vcount(gsub),1),
      to = sample.int(vcount(gsub),1)
      )
plot(gsub, vertex.size=5, vertex.label=NA,
     vertex.color = ifelse(V(gsub)%in%path$vpath[[1]],'green','black')
)


# plus court chemin entre coins dans le reseau en grille
#   (sur la plus grande composante)
comps = components(gsub)
index_of_largest_component = which(comps$csize==max(comps$csize))
vertices_in_largest = comps$membership==index_of_largest_component
subgraph_largest = induced_subgraph(gsub, vertices_in_largest)

first_col = V(subgraph_largest)[V(subgraph_largest)$x < min(V(subgraph_largest)$x + 1)]
from = first_col[first_col$y==max(first_col$y)]

last_col = V(subgraph_largest)[V(subgraph_largest)$x > max(V(subgraph_largest)$x - 1)]
to = last_col[last_col$y==min(last_col$y)]

path = shortest_paths(subgraph_largest,from = from,to = to)$vpath[[1]]

plot(subgraph_largest,vertex.size=5,vertex.label=NA,
     vertex.color = ifelse(V(subgraph_largest)%in%path,'green','black')
)


#####
## Partie 2
# Analyse de reseau social
# Data : co-occurence des personnages de A Song of Ice and Fire
#  https://github.com/mathbeveridge/asoiaf

library(readr)
library(igraph)

# 2.1) charger les donnees
# Data available under a CC-BY-NC-SA Licence at https://github.com/mathbeveridge/asoiaf
nodes <- read_csv("https://raw.githubusercontent.com/mathbeveridge/asoiaf/master/data/asoiaf-all-nodes.csv")
edges <- read_csv("https://raw.githubusercontent.com/mathbeveridge/asoiaf/master/data/asoiaf-all-edges.csv")




# construire le graphe: graph_from_data_frame
g = graph_from_data_frame(edges, directed = F, vertices = nodes)
E(g)$occ = E(g)$weight
E(g)$weight = 1/E(g)$weight

# 2.2) ploter le graph avec un layout adapte
coords = layout_with_fr(g)
V(g)$x = coords[,1]
V(g)$y = coords[,2]
plot(g, vertex.size=1, vertex.label.cex=0.4)


# pour bien visualiser: gephi, par exemple apres export en gml
# https://gephi.org/
# 
#igraph::write_graph(g,file="",format="gml")

# Alternatives:
# package ggnetwork ~ compatible avec igraph
#  https://cran.r-project.org/web/packages/ggnetwork/vignettes/ggnetwork.html



# 2.3) distribution des degres
degree(g)
deg_pondere = strength(g)
deg_pondere[deg_pondere>quantile(deg_pondere,0.99)]

# histogramme
hist(deg_pondere, breaks=200)

# loi rang-taille : log(degre) ~ log(rang)
plot(log(1:length(V(g))), sort(log(strength(g)), decreasing = T))



# ajuster des power law avec plus de parametres, ou des distributions log-normale
# package poweRlaw
library(poweRlaw)
wdeg_estimator = poweRlaw::conpl$new(deg_pondere)
est = poweRlaw::estimate_xmin(wdeg_estimator,xmax = max(deg_pondere))
wdeg_estimator$setXmin(est)

wdeg_estimator_lnorm = poweRlaw::conlnorm$new(deg_pondere)
est_lnorm = poweRlaw::estimate_xmin(wdeg_estimator_lnorm,xmax = max(deg_pondere))
wdeg_estimator_lnorm$setXmin(est_lnorm)

plot(wdeg_estimator);lines(wdeg_estimator, col=2, lwd=2);lines(wdeg_estimator_lnorm, col=3, lwd=2)





# 2.4) centralites : closeness, betwenness, eigenvalue
clos = closeness(g)
clos[clos>quantile(clos,0.99)]

betw = betweenness(g)
betw[betw>quantile(betw,0.99)]

cor.test(clos,betw)
cor.test(deg_pondere,betw)

eig = eigen_centrality(g)$vector
eig[eig>quantile(eig,0.99)]

# role de la centralité pour expliquer le nombre d'occurences?
cor.test(betw, strength(g,weight=E(g)$occ))
cor.test(clos, strength(g,weight=E(g)$occ))
cor.test(eig, strength(g,weight=E(g)$occ))


# 2.5) detection de communautes : cluster_... 
# -> methode de Louvain cluster_louvain
coms = cluster_fast_greedy(g)
coms

coms = cluster_louvain(g)
coms

# 2.6) plotter avec multiples infos:
#   communaute, centralite, degre
# (export dans le fichier "./graph.png")
png('graph.png',width=20,height = 20, units='cm',res=300)
plot(
  g,
  vertex.size = 2+log(strength(g,weights = E(g)$occs))/2,
  vertex.frame.color = NA,
  vertex.color = coms$membership,
  vertex.label.cex = eig/2
)
dev.off()




#########
## Partie 3 : OSM et reseaux de transports



library(osmdata)
library(sf)
library(ggplot2)


# routes principales pour Paris
bb <- getbb('paris fr', format_out = 'polygon')
roads <- opq(bbox = bb, timeout = 200) %>%
  add_osm_feature(key='highway',value='primary') %>% osmdata_sf()

# visualiser
ggplot()+geom_sf(data=roads$osm_lines)

# exporter en shapefile : st_write

# restaurants pour Paris
restaurants <- opq(bbox = bb, timeout = 200) %>%
  add_osm_feature(key='amenity',value='restaurant') %>% osmdata_sf()
ggplot()+geom_sf(data=restaurants$osm_points)


# exporter les routes en sp (pour utilisation avec des packages non compatibles avec sf)
#  %>% osmdata_sp()



# transformer les donnees brutes en graphe igraph pour calculer des temps de parcours
#  -> fonctions disponible ici : https://github.com/JusteRaimbault/TransportationNetwork
#   (pas encore déployé en package)
source('https://raw.githubusercontent.com/JusteRaimbault/TransportationNetwork/master/NetworkAnalysis/network.R')

# -> les fonctions addTransportationLayer(), addPoints(), addPointsLayer(), addAdministrativeLayer()
#   permettent de construire itérativement un graphe multimodal

# Ajouter une seule couche de transport pour construire un réseau routier
#  (pour le snapping = aggregation des noeuds, ici les donnees ne sont pas projetees, on aggrege a 100m ~ 0.001)
g <- addTransportationLayer(link_layer = roads$osm_lines, snap = 0.0005)

# plot d'un plus court chemin aleatoire (vitesse constante = 1 -> a adapter a une vitesse reelle)

path = shortest_paths(g, from = sample.int(vcount(g),1), to = sample.int(vcount(g),1),weights = 1/E(g)$length)
plot(g, vertex.size=1, vertex.label=NA,
     vertex.color = ifelse(V(g)%in%path$vpath[[1]],'green', 'black')
)


# Tester avec le réseau de métro/train (utilisation de l'argument "station")
metro_data <- opq(bbox = bb, timeout = 200) %>%
  add_osm_feature(key='route',value='subway') %>% osmdata_sf()
stations_data <-opq(bbox = st_bbox(metro_data$osm_lines), timeout = 200) %>%
  add_osm_feature(key='station',value='subway') %>% osmdata_sf()

g_metro <- addTransportationLayer(link_layer = metro_data$osm_lines,
                                  stations_layer = stations_data$osm_points,
                                  snap = 0.0005)
plot(g_metro, vertex.size=ifelse(V(g_metro)$station,5,0)
     , vertex.label=NA, vertex.color=ifelse(V(g_metro)$station,'red','black'))




# Ajouter une couche administrative (connecte les centroides des zones au noeud le plus proche)
mairies_data <-opq(bbox = st_bbox(metro_data$osm_lines), timeout = 200) %>% add_osm_feature(key='amenity',value='townhall') %>% osmdata_sf()
plot(mairies_data$osm_points$geometry)
g_full <- addAdministrativeLayer(g_metro, admin_layer = mairies_data$osm_points, attributes=list('name'='name','building'='building'), empty_graph_heuristic=F)

plot(g_full, vertex.size=ifelse(V(g_full)$station,5,0)
     , vertex.label=NA, vertex.color=ifelse(!is.null(V(g_full)$name),'red','black'))


# Calculer des accessibilités en termes de temps de trajet
# -> utiliser distance(g_full)


# Pour aller plus loin :
# - calcul des accessibilités aux aménités
# - utilisation des paramètres du modèle gravitaire
# - scenario : taxe sur l'essence, impact sur l'accessibilité




