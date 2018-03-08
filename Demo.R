#Packages
library(dplyr)
library(raster)
########################################################################################################################################################################################################################################################################################################################################################################################

# Jeu de simulation de rockfall (21.5 Go) : ~ 1 milliard de lignes et 3 colonnes 
# Premier colonne : Identifiant (ID) de la cellule raster que le bloc simulé à traversé
# Deuxième colonne : Energie de passage du bloc simulé dans la cellule 
# Troisième colonne : Volume du bloc simulé  
# Objectif : Connaître pour chaque ID, et chaque classe de volume (de 1 à 20 m3, organisés en 19 classes)
# le nombre de passage total et le dommage moyen 
# (chaque valeur d'énergie est traduite en un terme de dommage physique sur le bâti entre 0 et 1) 

# Idée : Matrice -> 1er layer : on enregistre le nombre de passage (QRA_3Dmatrix[row,column,1])
#                   2nd layer : on somme les énergies (QRA_3Dmatrix[row,column,2])
# (pour avoir la moyenne il suffira juste de faire QRA_3Dmatrix[,,2]/QRA_3Dmatrix[,,1])

# Problème 1: 
# Trop de lignes, donc temps pour "chercher les données" et les placer dans la matrice énorme
# (j'ai 11 subsamples, 1 subsample demande 5 jours de calcul)
# PC pro : 32 Go, mais les calculs en parralèles font exploser la mémoire 

# Problème 2: 
# Cette approche marche très bien si je veux faire une moyenne.   
# Cependant, je vais devoir faire des quantiles sur les distributions, donc je dois soit 
# (1) stocker toutes les informations d'énergie par classe de volume et ID  
# (2) développer un moyen qui me permet de travailler sur le fichier source sans que ça ne prenne trop 
# de temps ou que ça fasse exploser la mémoire


# Code R pour une approche type "Moyenne"

##Data
dataINDIVIDUAL           <- readRDS("SubSample.rds")
colnames(dataINDIVIDUAL) <- c("ID", "Energy", "Volume")
                       
##ID/Plan
rB    = raster("net_number.asc")
dataB = rasterToPoints(rB); colnames(dataB) = c("XBat","YBat","NetNumber")


QRA_3Dmatrix = array(0,dim = c(nrow(dataB),19,2))  

for (i in 1:nrow(dataINDIVIDUAL)) {
         row    <- as.numeric(dataINDIVIDUAL[i,1])                    # row corresponds to IDcell 
         column <- floor(as.numeric(dataINDIVIDUAL[i,3]))             # column corresponds to the volume class
         if (column == 20) {column = 19}
         QRA_3Dmatrix[row,column,1] <- QRA_3Dmatrix[row,column,1] + 1 # Nr passage, initially zero, then +1 
         QRA_3Dmatrix[row,column,2] <- QRA_3Dmatrix[row,column,2] + as.numeric(1 - (1.358/(1 + exp((((dataINDIVIDUAL[i,2]*10^3) - 129000)/120300)))))
                                                                      # Sum Damage 
}


