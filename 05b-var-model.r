
library(dplyr)
library(tidyr)
library(ggplot2)
library(vars)
library(boot)
library(rio)

#Importer les bases de données
# Définir le chemin du dossier contenant les fichiers CSV
folder_path <- "data_prod/dashboard/bertopic/data" #Modifier bertopic par lda si on souhaite faire les VAR sur LDA. Voir aussi la gestion des noms de fichier

# Lister tous les fichiers CSV dans le dossier
csv_files <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)
data_list <- lapply(csv_files, read.csv)
names(data_list) <- basename(csv_files) #Nommer chaque base 

#Il va falloir fusionner ces listes. Il faudra adapter les colonnes en conséquence
#But : avoir une base de la force : date/topic/party or group/prop. We will call it db 

#Conserver uniquement les topics politiques 
pol_issues <- c() #Insert numbers

db <- db %>%
  filter(topic %in% pol_issues)

#variables <- c("lr", "majority", "nupes", "rn", "publr", "pubmajority",, "pubnupes", "pubrn", "att_public", "gen_public", "media")
