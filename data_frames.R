# WORK DIRECTORY 
# LEITURA DE ARQUIVOS
# UNIFICAÇÃO EM UM DATAFRAME
# LEITURA DE DATAFRAMES
# COMEÇAREMOS EM ORDEM DESCENDENTE POR ANO (TEORICAMENTE TEM MAIS DADOS)
# VARIÃVEIS CATEGORICAS

setwd("C:/Users/1513 IRON/Desktop/Projetos/ProjetosGIT/r-worksheet/votacao") # SETING WORK DIRECTORY
getwd() # GET WORKING DIRECTORY

#acreElections = read.csv("votacao_candidato_munzona_2016_AC.csv", sep = ";") # import CSV

TSEFiles = list.files(path = ".", pattern = "*.csv|*.txt")  #GET ALL TXT AN CSV FILES

for (file in TSEFiles) {
  # if the merged dataset doesn't exist, create it
  if (!exists("dataset")){
    dataset <- read.csv(file, sep=";", header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
  }
  # if the merged dataset does exist, append to it
  else if (exists("dataset")){
    temp_dataset <- read.csv(file, sep=";", header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
    dataset<- rbind(dataset, temp_dataset)
    rm(temp_dataset)
  }
}

nrow(dataset) # number of rows of data frame
head(dataset[1,]) # first 5 rows of data frame
tail(dataset[1, 1:10]) # last 5 rows of data frame get only 10 columns

unique(dataset["SG_UF"]) # SHOW ALL UF
summary(dataset["QT_VOTOS_NOMINAIS"]) # SHOW SUMMARY OF VOTES

#SHOW SUM OF ALL VOTES
sum(dataset["QT_VOTOS_NOMINAIS"], na.rm=TRUE)

#SHOW MAX OF ALL VOTES
max(dataset["QT_VOTOS_NOMINAIS"], na.rm=TRUE)

#FILTERING A DATAFRAME
onlyAcre <- dataset$SG_UF == "AC" #CREATE A LOGICAL TYPE
datasetOnlyAcre = dataset[onlyAcre,] #DO THE FILTER

#FILTERING A DATAFRAME
onlyWithMoreThan70kVotes <- dataset$QT_VOTOS_NOMINAIS > 70000
datasetMoreThan70kVotes = dataset[onlyWithMoreThan70kVotes,]

#FILTERING A DATAFRAME USING AND (&)
onlyElectedWithMoreThan70kVotes <- dataset$QT_VOTOS_NOMINAIS > 70000 & dataset$DS_SIT_TOT_TURNO == 'ELEITO'
datasetElectedMoreThan70kVotes = dataset[onlyElectedWithMoreThan70kVotes,]

#FILTERING A DATAFRAME USING OR (|)
onlyPTandPMDBCandidates <- dataset$SG_PARTIDO == "PT" | dataset$SG_PARTIDO == 'PMDB'
datasetPTandPmdb = dataset[onlyPTandPMDBCandidates,]
unique(datasetPTandPmdb["SG_PARTIDO"])

#THE MOST VOTED 
mostVoted <- dataset$QT_VOTOS_NOMINAIS == max(dataset["QT_VOTOS_NOMINAIS"], na.rm=TRUE)
datasetMostVoted = dataset[mostVoted,]


#GROUPING AND SUM DATASET
sumVotes = aggregate(dataset$QT_VOTOS_NOMINAIS, by=list(dataset$NM_CANDIDATO), FUN=sum)
onlyDoria = sumVotes$Group.1 == "JOÃO AGRIPINO DA COSTA DORIA JUNIOR"
doria = sumVotes[onlyDoria,]

# EXCLUDE HIGH CORRELATIONS COLUMNS SG_UF, NM_MUNICIP IO, NM_PARTIDO
excludeColumns <- names(dataset) %in% c("SG_UF", "NM_MUNICIPIO", "NM_PARTIDO") 
newDataset <- dataset[!excludeColumns]


#GETTING A SAMPLE FROM DATAFRAME
install.packages("dplyr")
library(dplyr)

sampleSize = nrow(dataset) * 0.25 #GETTING 25% PERCENT FROM THE ORIGINAL DATASET
sampleDataset = sample_n(dataset, sampleSize)

print(nrow(dataset)) # full dataset
print(nrow(sampleDataset)) # 25% of dataset (random data)


#USIGN K MODES
install.packages("klaR")  
library(klaR)
categoricalColumns = excludeColumns <- names(dataset) %in% c("CD_TIPO_ELEICAO", "CD_MUNICIPIO", "DT_GERACAO", "SQ_CANDIDATO", "HH_GERACAO", "ANO_ELEICAO", "NR_TURNO", "CD_ELEICAO", "SG_UE", "NR_ZONA", "CD_CARGO", "CD_SITUACAO_CANDIDATURA", "CD_DETALHE_SITUACAO_CAND", "NR_PARTIDO", "SQ_COLIGACAO", "CD_SIT_TOT_TURNO", "QT_VOTOS_NOMINAIS") 
categoricalDataSet <- dataset[!categoricalColumns]
cl <- kmodes(categoricalDataSet, modes=3, iter.max = 10, weighted = FALSE, fast = TRUE)

plot(jitter(x), col = cl$cluster)
points(cl$modes, col = 1:5, pch = 8)

install.packages("fastDummies") # install dummies
library(fastDummies) # import dummies

categoricalDataFrame = read.csv("example_categorical.csv")
summary(categoricalDataFrame) #VERIFICAR A QUALIDADE DOS DOS DADOS
dummies = fastDummies::dummy_cols(categoricalDataFrame)

#dummie variables
print(dummies)

#

# ANÃLISE DE CORRESPONDÃŠNCIA MULTIPLA (MCA)
#install.packages(c("FactoMineR", "factoextra"), dependencies=TRUE)
#library("FactoMineR") #MCA
#library("factoextra") #PLOT

## install.packages(c('tibble', 'dplyr', 'tidyr'))
#library(tibble)
#library(dplyr)
#library(tidyr)
# APRENDIZADO SUPERVISIONADO OU NÃO, REGRAS DE ASSOCIAÇÃO!? 
# https://shapeofdata.wordpress.com/2014/03/04/k-modes/

