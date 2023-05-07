# Exercice 1
1. # Importer la base “céréales”

library(haven)
datapath <- read_dta("D://ISEP2//Semestre_2//R//Cours_R_2023//Devoir1//cereales.dta")
View(datapath)
# dim(datapath)
# nrow(datapath)
# ncol(datapath)
names(datapath)
str(datapath)
2.# Rénomer l’ensemble des variables suivant le dictionnaire des variables, en des noms plus parlant et
# maximum 10 charactères;
## affiche le nom des variables
colnames(datapath)  
## Renomer avec select()
library(dplyr)

df_cereal<- select(datapath,key=interview__key,Interview_ID=interview__id,ID_Cereal=cereales__id,autre_cereal=s07Bq02_autre_cereales,quanite_cons=s07Bq03a_cereales,unites_cons=s07Bq03b_cereales,taille_cons=s07Bq03c_cereales,
                   provenance_auto=s07Bq04_cereales,provenance_other=s07Bq05_cereales,freq_achat=s07Bq06_cereales,
                   quatite_achat=s07Bq07a_cereales,unite_achat=s07Bq07b_cereales,taille_achat=s07Bq07c_cereales,
                   value_lastachat=s07Bq08_cereales)

View(df_cereal)
dim(df_cereal)
dim(datapath)
3. # Décrire les variables d’intérêt
#str(datapath_rename)
summary(df_cereal)
4. # identifier les valeurs manquantes et les imputer;
names(df_cereal)
# is.na(df$key)
# is.na(df$nterview_ID)
# is.na(df$ID_Cereal)  
# is.na(df$autre_cereal)
# is.na(df$quanite_cons)
# is.na(df$unites_cons)
# is.na(df$taille_cons)
# is.na(df$provenance_other)
# is.na(df$freq_achat)
# les variables suivantes sont les seules var contenant des valeurs manquantes
is.na(df_cereal$provenance_auto)
is.na(df_cereal$quatite_achat)
is.na(df_cereal$unite_achat)
is.na(df_cereal$taille_achat)
is.na(df_cereal$value_lastachat)

# Remplacer les valeurs manquantes par la moyenne de chaque colonne
df_cereal <- as.data.frame(lapply(df_cereal, function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x)))
# Afficher le nouveau dataframe
df_cereal

# Attacher la base de données
# attach(df_cereal)

5.# Importer la base table de conversion
# Table de conversion phase 2.xlsx
chemin="D:/ISEP2/Semestre_2/R/Cours_R_2023/Devoir1/Table_de_conversion_phase 2.csv"  # on peut aussi / alieu de \\
Table_Conversion=read.csv2(chemin)
View(Table_Conversion)
6. # Décrire la variable poids
typeof(Table_Conversion$poids) # output  character
## Vérifier les types de données des variables
str(Table_Conversion$poids)
# les modalites de la variables poids sont nobres ecrits avec des virgules
#et non des points comme reconnu par R.Je veux changer les virgules en points
## Charger le package dplyr
library(dplyr)
# # remplacer les virgules par des points et convertir en nombre
Table_Conversion$poids <- as.numeric(gsub(",", ".", Table_Conversion$poids))
# Afficher le type
typeof(Table_Conversion$poids)


# ## Convertir la variable poids en type numérique
# Base <- mutate(Base , poids = as.numeric(poids))
# # Vérifier les types de données des variables après la conversion
# # Remplacer les valeurs manquantes par la moyenne de chaque colonne
# summary(Base$poids,na.rm=TRUE)
# Base <- as.data.frame(lapply(Base, function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x)))
## Remplacer les valeurs manquantes de la variable tailleNom par la médiane
median(Table_Conversion$tailleNom, na.rm = TRUE)
Table_Conversion[c(38,1115,1116,1117,1118,1119,1120), "tailleNom"] <- "petit"

# Table_Conversion <- mutate(Table_Conversion, tailleNom = ifelse(is.na(tailleNom), median(tailleNom, na.rm = TRUE), tailleNom))
# Table_Conversion <- mutate(Table_Conversion, tailleNom = ifelse(tailleNom="Tr<e8>s petit" , median(tailleNom, na.rm = TRUE), tailleNom))
## aggreger la var "poids" par "tailleNom"
#Conversion des quantités en unités standards
for (i in 1:nrow(Table_Conversion)){
  if(Table_Conversion$tailleNom[i] == "Grand"){
    Table_Conversion$tailleNom[i] <- Table_Conversion$tailleNom[i]
  }
  else if(Table_Conversion$tailleNom[i] == "grand"){
    Table_Conversion$tailleNom[i] <- "Grand"
  }
}
for (i in 1:nrow(Table_Conversion)){
  if(Table_Conversion$tailleNom[i] == "moyen"){
    Table_Conversion$tailleNom[i] <- Table_Conversion$tailleNom[i]
  }
  else if(Table_Conversion$tailleNom[i] == "Moyen"){
    Table_Conversion$tailleNom[i] <- "moyen"
  }
}
for (i in 1:nrow(Table_Conversion)){
  if(Table_Conversion$tailleNom[i] == "petit"){
    Table_Conversion$tailleNom[i] <- Table_Conversion$tailleNom[i]
  }
  else if(Table_Conversion$tailleNom[i] == "Petit"){
    Table_Conversion$tailleNom[i] <- "petit"
  }
}

aggregate(Table_Conversion$poids, by = list(Table_Conversion$tailleNom), FUN = sum)

7.#  Créer une clé d’identification dans les deux bases : une concaténation

attach(Table_Conversion)
Table_Conversion$var_concatenate<-paste(produitID, uniteID,tailleID,sep="-")  
View(Table_Conversion)
8. # Fusionner les deux bases
### Q8

colnames(df_cereal)[3] <- "produitID"
str(Table_Conversion)
Table_Conversion$produitID<-as.numeric(Table_Conversion$produitID)
Table_Conversion$tailleID<-as.numeric(Table_Conversion$tailleID)
Table_Conversion$uniteID<-as.numeric(Table_Conversion$uniteID)
df_cereal$produitID<-as.numeric(df_cereal$produitID)
names(df_cereal)
names(Table_Conversion)
cereal_mg<- merge(df_cereal, Table_Conversion, by="produitID", all=TRUE)

View(cereal_mg)


# names(Table_Conversion)
# names(df_cereal)

9. #Convertir les quantités consommées en unités standards. Ind: Creer une nouvelle variable qui calcule
#le poids consommé durant les 7 derniers jours ensuite la raméné en année.
cereal_conv <- merge(df_cereal, Table_Conversion, by="produitID")
View(cereal_conv)
cereal_conv$standars <- ((cereal_conv$quanti_con*cereal_conv$poids/1000)/7)*365
10. # Générer une variable “taille_men” comprise entre 1 et 30. La moyenne nationale est de 9. La taille est
# un entier. Cette variable sera unique pour chaque ménage qui est identifié par interview_key. Chaque
# interview_key est un ménage et donc chaque interview_key devra avoir une valeur unique.

set.seed(123) # Pour la reproductibilité des résultats
# Générer un vecteur de 100 valeurs aléatoires entre 1 et 30
tm <- runif(106583, 1, 30)
tm <- tm + 9 - mean(tm)
tm_moy=mean(tm)
tm_moy
cereal_conv$taille_men<-tm
mean(cereal_conv$taille_men)

11. # Importer la base calories et fusionner celle avec la base. Cacluer la consommation calorie par tête.
# Nettoyer d’abord les valeurs manquantes et les valeurs aberrantes.








# Exercice 2

1. # Fonction d’importation dont l’argument sera le chemin et nom du fichier
import<- function(chemin,tour) {
  acces<-paste0(chemin,"\\",tour)
  extension <- tools::file_ext(acces)
  if (extension == "csv") {
    base <- read.csv(acces)
  } else if (extension == "xlsx" || extension == "xls") {
    base <- readxl::read_excel(acces)
  } else if (extension == "dta") {
    base <- haven::read_dta(acces)
  } else {
    stop("Le type de fichier n'est pas supporté.")
  }
  return(base)
  
  
}

2.#  Fonction qui renome les variables

renome_vars <- function(df, nomAncien, nvnom) {
  if(length(nomAncien) != length(nvnom)) {
    stop("Le nombre d'anciens noms et de nouveaux noms doit être identique.")
  }
  colnames(df)[match(nomAncien, colnames(df))] <- nvnom
  return(df)
}


3. # Fonction qui fusionne deux bases

merge_base <- function(base1, base2, by.vars) {
  # Vérification des variables à fusionner
  if (!is.list(by.vars)) {
    by.vars <- list(by.vars)
  }
  # Fusion des bases de données
  merged_base <- merge(base1, base2, by = by.vars, all = TRUE)
  # Retourne la base de données fusionnée
  return(merged_base)
}

4.#  Fonction qui détecte les valeurs manquates

detecte_valeurs_manquantes <- function(df) {
  nb_na <- sum(is.na(df))
  pourcentage_na <- round(mean(is.na(df)) * 100, 2)
  cat("Il y a", nb_na, "valeurs manquantes (soit", pourcentage_na, "% du total)\n")
}

5.#  Fonction qui impute les valeurs manquantes

impute_na <- function(data) {
  # Calculer la moyenne de chaque colonne
  means <- colMeans(data, na.rm = TRUE)
  # Remplacer les valeurs manquantes par la moyenne de la colonne
  data[is.na(data)] <- means[rep(1, length(data))]
  return(data)
}

6.# Fonction qui détecte les valeurs aberrantes 7. Fonction qui les corrige

# Fonction qui détecte et corrige les valeurs aberrantes
detecter_corriger_aberrations <- function(base, variables) {
  
  # Boucle sur les variables
  for (var in variables) {
    
    # Calcul de l'écart interquartile
    q1 <- quantile(base[[var]], 0.25)
    q3 <- quantile(base[[var]], 0.75)
    iqr <- q3 - q1
    
    # Détermination des bornes
    born_inf <- q1 - 1.5*iqr
    born_sup <- q3 + 1.5*iqr
    
    # Correction des valeurs aberrantes
    base[[var]][base[[var]] < born_inf] <- mean(base[[var]], na.rm = TRUE)
    base[[var]][base[[var]] > born_sup] <- mean(base[[var]], na.rm = TRUE)
    
  }
  
  # Retourne le jeu de données corrigé
  return(data)
  
}

