
############# Pratique avec la base swissmunicipalities ###############

install.packages("sampling")
library(sampling)

data("swissmunicipalities")
View(swissmunicipalities)
data = data.frame(swissmunicipalities)

#Echantillon:
##On détermine la taille optimale de l'échantillon à partir des théories du sondage.
#Supposons que cette taille est n = 500
n = 500
N = 2896 #Nombre d'observations de la base de données

# Echantillon tiré à partir du SAS

##Plan sans remise
sampling_SAS_wor = srswor(n, N)
print(sampling_SAS_wor)
sampling_SAS_wor = getdata(data, sampling_SAS_wor)
View(sampling_SAS_wor)
## Plan avec remise
sampling_SAS_wr = srswr(n, N)
print(sampling_SAS_wr)
sampling_SAS_wr = getdata(data, sampling_SAS_wr)
View(sampling_SAS_wr)

# Echantillon tiré à partir du Sondage à probabilités inégales

#On choisit en premier la variable de stratification. Ici prenons la variable REG
REG = data$REG
pik_REG = inclusionprobabilities(REG, n)
# Tirage systématique
sampling_SPI_sys = UPsystematic(pik = pik_REG)
print(sampling_SPI_sys)
sampling_SPI_sys = getdata(data, sampling_SPI_sys)
View(sampling_SPI_sys)

# Echantillon tiré à partir du Sondage aléatoire stratifié

## On veut former des strates à partir des variables CT et POPTOT
POPTOT = data$POPTOT
POPTOT_Class = list()
for (i in 1:length(POPTOT)){
  if (POPTOT[i]<=1000){
    POPTOT_Class = append(POPTOT_Class,1)
  }else if (POPTOT[i]<=3000){
    POPTOT_Class = append(POPTOT_Class,2)
  }else {
    POPTOT_Class = append(POPTOT_Class,3)
  }
}
POPTOT_Class = as.data.frame(POPTOT_Class)
POPTOT_Class = t(POPTOT_Class)
View(POPTOT_Class)
data_strata = cbind(data,POPTOT_Class)
View(data_strata)

table(data_strata$REG,data_strata$POPTOT_Class)

sampling_SAStr = strata(data_strata, stratanames = c("REG","POPTOT_Class"), 
                        size = rep(15, times=21), method = "srswor")
sampling_SAStr = getdata(data_strata, sampling_SAStr)

sampling_SAStr1 = strata(data_strata, stratanames = c("REG","POPTOT_Class"), 
                         size = rep(15, times=21), method = "srswr")
sampling_SAStr1 = getdata(data_strata, sampling_SAStr1)

sampling_SAStr2 = strata(data_strata, stratanames = c("REG","POPTOT_Class"), 
                         size = rep(15, times=21), method = "poisson", pik = data_strata$CT)
sampling_SAStr2 = getdata(data_strata, sampling_SAStr2)

sampling_SAStr3 = strata(data_strata, stratanames = c("REG","POPTOT_Class"), 
                         size = rep(15, times=21), method = "systematic", pik = data_strata$CT)
sampling_SAStr3 = getdata(data_strata, sampling_SAStr3)

## Echantillon à  partir du sondage par grappe
# On veut effectuer un sondage par grappe sur la variable REG

cl = cluster(data, c("REG"), size = 7, method = "srswor")
cl1 = cluster(data, c("REG"), size = 7, method = "srswr")
cl2 = cluster(data, c("REG"), size = 7, 
              method = "poisson", pik = data$income)
cl3 = cluster(data, c("REG"), size = 7, 
              method = "systematic", pik = data$income)
getdata(data,cl)
getdata(data,cl1)
getdata(data,cl2)
getdata(data,cl3)

## Echantillon à partir du sondage à plusieurs degrés

## On veut effectuer un sondage à plusieurs dégrés sur les variables REG et CT avec des tailles de 500 et 250 suivant les degrés:

m = mstage(data, size =list(500, 250), method = list("srswor","srswor"))
getdata(data, m)

## Estimateurs de HT

# On considère l'échantillon tiré à partir sondage à probabilités inégales méthode systématique
# On souhaite estimer la population totale
y = rep(seq(from = 1100, to = 2000, by = 100),50)
length(y)
pik=inclusionprobabilities(data$POPTOT,n)
s = UPsystematic(pik = pik)
HTestimator(y,pik[s==1])

## Estimateurs par le calage
data_cal = cbind(data$CT,data$REG,data$COM,data$HApoly,data$Surfacesbois,data$Surfacescult,data$POPTOT)
colnames(data_cal) = c("CT", "REG","COM","HApoly","Surfacesbois","Surfacescult","POPTOT")

View(data_cal)
Xs = getdata(data_cal,s)
Xs = Xs[,-1]
Xs = as.matrix(Xs)
total = c(41202, 9248, 9934534, 3998831, 1270996, 987317, 7288010)
View(Xs)
g=calib(Xs,d=1/pik,total,method="truncated",
        bounds=c(0.75,1.2))
