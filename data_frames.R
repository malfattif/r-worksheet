# LEITURA DE ARQUIVOS
# UNIFICAÇÃO EM UM DATAFRAME
# LEITURA DE DATAFRAMES
# COMEÇAREMOS EM ORDEM DESCENDENTE POR ANO (TEORICAMENTE TEM MAIS DADOS)
# VARIÁVEIS CATEGORICAS
# APRENDIZADO SUPERVISIONADO OU NÃO, REGRAS DE ASSOCIAÇÃO!? 

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

install.packages("fastDummies") # install dummies
library(fastDummies) # import dummies

categoricalDataFrame = read.csv("example_categorical.csv")
summary(categoricalDataFrame) #VERIFICAR A QUALIDADE DOS DOS DADOS
dummies = fastDummies::dummy_cols(categoricalDataFrame)

#dummie variables
print(dummies)

# ANÁLISE DE CORRESPONDÊNCIA MULTIPLA (MCA)
#install.packages(c("FactoMineR", "factoextra"), dependencies=TRUE)
#library("FactoMineR") #MCA
#library("factoextra") #PLOT

## install.packages(c('tibble', 'dplyr', 'tidyr'))
#library(tibble)
#library(dplyr)
#library(tidyr)

