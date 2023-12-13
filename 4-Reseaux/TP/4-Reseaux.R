
##
# TP Reseaux

library(igraph)
library(ggplot2)

#####
## Partie 1
# graphes aleatoires


# 1.1) tester la generation de graphe aléatoire: igraph::erdos.renyi.game()



# calculer la densite


# noeuds, liens, attributs



# definir des poids pour les liens



# matrice d'adjacence : application aux chemins d'une longueur donnée



# nombre de composantes connexes?



# taille de la plus grande composante en fonction de p (étude de percolation)



# extraire le sous-graphe correspondant a la plus grosse composante


# diametre du graphe


# diametre pondere


# diametre en fonction taille et proba du graphe aleatoire



# plotter le graphe



# layouts: algorithme de spatialisation du graphe
# -> tester layout fruchterman reingold : layout_with_fr



# 1.2) Generer et plotter un graphe en grille (lattice): igraph::make_lattice


# Supprimer des liens aléatoirement dans le graphe en grille


#  étudier la taille de la plus grande composante connexe
#  en fonction de la proportion de liens gardés et de la taille du graphe





# 1.3) perturber les coordonnées des noeuds de la grille pour obtenir
#  des plus courts chemins uniques;
# étudier le diametre en fonction des liens supprimes
# algos: shortest_paths()/ distances() : algorithme adapte au cas (voir doc)




# ajouter la distance euclidienne comme poids des liens



# tous les plus courts chemins: distances



# certains plus courts chemins: shortest_paths



# plus court chemin entre coins dans le reseau en grille (sur la plus grande composante)





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



# 2.2) ploter le graph avec un layout adapte



# pour bien visualiser: gephi, par exemple apres export en gml
# https://gephi.org/
# 
#igraph::write_graph(g,file="",format="gml")

# Alternatives:
# package ggnetwork ~ compatible avec igraph
#  https://cran.r-project.org/web/packages/ggnetwork/vignettes/ggnetwork.html



# 2.3) distribution des degres


# histogramme


# loi rang-taille : log(degre) ~ log(rang)



# ajuster des power law avec plus de parametres, ou des distributions log-normale
# package poweRlaw




# 2.4) centralites : closeness, betwenness, eigenvalue



# role de la centralité pour expliquer le nombre d'occurences?



# 2.5) detection de communautes : cluster_... -> methode de Louvain cluster_louvain




# 2.6) plotter avec multiples infos: communaute, centralite, degre
# (export dans le fichier "./graph.png")






#########
## Partie 3 : OSM et reseaux de transports



library(osmdata)
library(sf)
library(ggplot2)


# routes principales pour Paris



# visualiser


# exporter en shapefile


# restaurants pour Paris



# exporter les routes en sp (pour utilisation avec des packages non compatibles avec sf)



# exporter les routes en sf



# transformer les donnees brutes en graphe igraph pour calculer des temps de parcours
#  -> fonctions disponible ici : https://github.com/JusteRaimbault/TransportationNetwork (pas encore déployé en package)
source('https://raw.githubusercontent.com/JusteRaimbault/TransportationNetwork/master/NetworkAnalysis/network.R')

# -> les fonctions addTransportationLayer(), addPoints(), addPointsLayer(), addAdministrativeLayer()
#   permettent de construire itérativement un graphe multimodal

# Ajouter une seule couche de transport pour construire un réseau routier
#  (pour le snapping = aggregation des noeuds, ici les donnees ne sont pas projetees, on aggrege a 100m ~ 0.001)




# plot d'un plus court chemin aleatoire (vitesse constante = 1 -> a adapter a une vitesse reelle)




# Tester avec le réseau de métro/train (utilisation de l'argument "station")




# Ajouter une couche administrative (connecte les centroides des zones au noeud le plus proche)


# Calculer des accessibilités en termes de temps de trajet



# Pour aller plus loin :
# - calcul des accessibilités aux aménités
# - utilisation des paramètres du modèle gravitaire
# - scenario : taxe sur l'essence, impact sur l'accessibilité




