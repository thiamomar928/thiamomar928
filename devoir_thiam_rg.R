1.# Fais
2.# créer un data frame qui contient n lignes et m colonnes (n>100, m>10)
#  5. varibales quantitatives ;
# 5. variables qualitatives de type factor avec une variable binaire. > Indication : vous pouvez
# utiliser les focntions “rep”, “seq”, “rnorm”, dnorm, etc.
n <- 120
m <- 12

# Créer les variables quantitatives
Var_Quanti_1 <- rnorm(n, mean = 10, sd = 2)
Var_Quanti_2 <- rnorm(n, mean = 5, sd = 1)
Var_Quanti_3 <- rnorm(n, mean = 20, sd = 4)
Var_Quanti_4 <- rnorm(n, mean = 15, sd = 3)
Var_Quanti_5 <- rnorm(n, mean = 8, sd = 2)

# Créer les variables qualitatives de type factor avec une variable binaire
Var_Quali_1 <- factor(rep(c("A", "B", "C", "D"), length.out = n))
Var_Quali_2 <- factor(sample(c("F", "M"), size = n, replace = TRUE))
Var_Quali_3 <- factor(rep(seq(1, 3), length.out = n))
Var_Quali_4 <- factor(sample(c("X", "Y", "Z"), size = n, replace = TRUE))
Var_Binaire <- factor(sample(c("Yes", "No"), size = n, replace = TRUE))

# Creation des deux autres vecteurs pour avoir 12 colonnes
## generer les 120 nombres pairs
Nb_pair<-seq(2,240,by=2)
## generer les 120 nombres pairs
Nb_impair<-seq(1,240,by=2)

# Créer le data frame
df<- data.frame(Var_Quanti_1, Var_Quanti_2, Var_Quanti_3, Var_Quanti_4, Var_Quanti_5, Var_Quali_1, Var_Quali_2, Var_Quali_3, Var_Binaire, Var_Quali_4,Nb_pair,Nb_impair, stringsAsFactors = FALSE)
View(df)
3.# Enregistrer le dataframe dans un objet noommé df.votreNom
df_Thiam<-df
4. # Exporter au format “csv” dont sep=“;”
write.csv(df, "dataframe_Thiam.csv", row.names = FALSE)

5. # générer un rapport au format “html”













