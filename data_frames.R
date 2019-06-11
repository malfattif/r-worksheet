# WORK DIRECTORY 
# LEITURA DE ARQUIVOS
# UNIFICAÇÃO EM UM DATAFRAME
# LEITURA DE DATAFRAMES
# COMEÇAREMOS EM ORDEM DESCENDENTE POR ANO (TEORICAMENTE TEM MAIS DADOS)
# VARIÁVEIS CATEGORICAS
# APRENDIZADO SUPERVISIONADO OU NÃO, REGRAS DE ASSOCIAÇÃO!? 
# https://shapeofdata.wordpress.com/2014/03/04/k-modes/

setwd(paste(getwd(), "/r-worksheet/votacao", sep = ""))  # SETING WORK DIRECTORY
getwd() # GET WORKING DIRECTORY

#acreElections = read.csv("votacao_candidato_munzona_2016_AC.csv", sep = ";") # import CSV
TSEFiles = list.files(path = ".", pattern = "*.csv|*.txt")  #GET ALL TXT AN CSV FILES
  

for (file in TSEFiles){
  # if the merged dataset doesn't exist, create it
  if (!exists("dataset")){
    dataset <- read.csv(file, sep=";", header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
  }
  
  # if the merged dataset does exist, append to it
  if (exists("dataset")){
    temp_dataset <- read.csv(file, sep=";", header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
    dataset<-rbind(dataset, temp_dataset)
    rm(temp_dataset)
  }
}

nrow(dataset) # number of rows of data frame
head(dataset[1, 1:10]) # first 5 rows of data frame
tail(dataset[1, 1:10]) # last 5 rows of data frame

unique(dataset["SG_UF"]) # SHOW ALL UF
summary(dataset["QT_VOTOS_NOMINAIS"]) # SHOW SUMMARY OF VOTES


# EXCLUDE HIGH CORRELATIONS COLUMNS SG_UF, NM_MUNICIP IO, NM_PARTIDO
excludeColumns <- names(dataset) %in% c("SG_UF", "NM_MUNICIPIO", "NM_PARTIDO") 
newDataset <- dataset[!excludeColumns]

# install.packages("klaR")
# library(klaR)
#categoricalColumns = excludeColumns <- names(dataset) %in% c("CD_TIPO_ELEICAO", "CD_MUNICIPIO", "DT_GERACAO", "SQ_CANDIDATO", "HH_GERACAO", "ANO_ELEICAO", "NR_TURNO", "CD_ELEICAO", "SG_UE", "NR_ZONA", "CD_CARGO", "CD_SITUACAO_CANDIDATURA", "CD_DETALHE_SITUACAO_CAND", "NR_PARTIDO", "SQ_COLIGACAO", "CD_SIT_TOT_TURNO", "QT_VOTOS_NOMINAIS") 
#categoricalDataSet  <- dataset[!categoricalColumns]
cl <- kmodes(categoricalDataSet, modes=3, iter.max = 10, weighted = FALSE, fast = TRUE)

install.packages("fastDummies") # install dummies
library(fastDummies) # import dummies

categoricalDataFrame = read.csv("example_categorical.csv")
summary(categoricalDataFrame) #VERIFICAR A QUALIDADE DOS DOS DADOS
dummies = fastDummies::dummy_cols(categoricalDataFrame)

#dummie variables
print(dummies)

#

# ANÁLISE DE CORRESPONDÊNCIA MULTIPLA (MCA)
#install.packages(c("FactoMineR", "factoextra"), dependencies=TRUE)
#library("FactoMineR") #MCA
#library("factoextra") #PLOT

## install.packages(c('tibble', 'dplyr', 'tidyr'))
#library(tibble)
#library(dplyr)
#library(tidyr)

