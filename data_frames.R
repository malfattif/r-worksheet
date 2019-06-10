setwd(paste(getwd(), "/r-worksheet", sep = "")) # SETING WORK DIRECTORY
getwd() # GET WORKING DIRECTORY

TSEFiles = list.files(path = "votacao/", pattern = "*.csv|*.txt")  #GET ALL TXT AN CSV FILES

#acreElections = read.csv("votacao/votacao_candidato_munzona_2016_AC.csv", sep = ";") # import CSV

setwd(paste(getwd(), "/votacao", sep = "")) 
tables <- lapply(TSEFiles, read.csv, sep = ";", header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")

mydata1 = read.csv("example.csv") # import CSV
mydata2 = read.csv("example.csv")

head(mydata1) # first 5 rows of data frame
tail(mydata1) # last 5 rows of data frame
nrow(mydata1) # number of rows of data frame

myfulldata = rbind(mydata1, mydata2) # append files 
print(nrow(myfulldata))

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

