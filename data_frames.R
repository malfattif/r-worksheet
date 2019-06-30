# CRIANDO FUNÇÕESES
# ESTRUTURAS CONDICIONAIS
# LA??ES DE REPETI??O
# USANDO E IMPORTANDO BIBLIOTECAS
# WORK DIRECTORY 
# LEITURA DE ARQUIVOS
# UNIFICA??O EM UM DATAFRAME
# MANIPULA??O E FILTRAGEM DE DATAFRAMES
# COME?AREMOS EM ORDEM DESCENDENTE POR ANO (TEORICAMENTE TEM MAIS DADOS)
# VARI?VEIS CATEGORICAS

#acreElections = read.csv("votacao_candidato_munzona_2016_AC.csv", sep = ";") # import CSV

setwd("C:/Users/1513 IRON/Desktop/Projetos/ProjetosGIT/r-worksheet/votacao") # SETING WORK DIRECTORY
getwd() # GET WORKING DIRECTORY

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
sum(dataset$QT_VOTOS_NOMINAIS, na.rm=TRUE) #missing values

#SHOW MAX OF ALL VOTES
max(dataset$QT_VOTOS_NOMINAIS, na.rm=TRUE)

#FILTERING A DATAFRAME
onlyAcre <- dataset$SG_UF == "AC" #CREATE A LOGICAL TYPE
datasetOnlyAcre = dataset[onlyAcre,] #DO THE FILTER

#FILTERING A DATAFRAME
onlyWithMoreThan70kVotes <- dataset$QT_VOTOS_NOMINAIS > 70000
datasetMoreThan70kVotes = dataset[onlyWithMoreThan70kVotes,]

#FILTERING A DATAFRAME USING 'AND' (&)
onlyElectedWithMoreThan70kVotes <- dataset$QT_VOTOS_NOMINAIS > 70000 & dataset$DS_SIT_TOT_TURNO == 'ELEITO'
datasetElectedMoreThan70kVotes = dataset[onlyElectedWithMoreThan70kVotes,]

#FILTERING A DATAFRAME USING 'OR' (|)
onlyPTandPMDBCandidates <- dataset$SG_PARTIDO == "PT" | dataset$SG_PARTIDO == 'PMDB'
datasetPTandPmdb = dataset[onlyPTandPMDBCandidates,]
unique(datasetPTandPmdb$SG_PARTIDO)

#THE MOST VOTED 
mostVoted <- dataset$QT_VOTOS_NOMINAIS == max(dataset$QT_VOTOS_NOMINAIS, na.rm=TRUE)
datasetMostVoted = dataset[mostVoted,]

#GROUPING AND SUM DATASET
sumVotes = aggregate(dataset$QT_VOTOS_NOMINAIS, by=list(dataset$NM_CANDIDATO), FUN=sum)
onlyDoria = sumVotes$Group.1 == "JO?O AGRIPINO DA COSTA DORIA JUNIOR"
doria = sumVotes[onlyDoria,]

#DEAL WITH MISSING VALUES (NOT AVAILABLE)
namesVector <- c("felipe", "thais", NA)
names <- data.frame(namesVector)

is.na(names) #SHOW IF THERES ANY MISSING VALUES
sum(is.na(names)) #SUM MISSING VALUES
na.omit(names) #REMOVE ROWS WITH MISSING VALUES

isNA <- is.na(dataset$QT_VOTOS_NOMINAIS)
missingVotes = dataset[isNA,]

# EXCLUDE HIGH CORRELATIONS COLUMNS SG_UF, NM_MUNICIPIO, NM_PARTIDO
excludeColumns <- names(dataset) %in% c("SG_UF", "NM_MUNICIPIO", "NM_PARTIDO")
#SERA QUE PRECISAMOS DE UF?
newDataset <- dataset[!excludeColumns]

#GETTING A SAMPLE WITHOUT LIBRARY
sampleSize = nrow(dataset) * 0.25
sampleDataset = dataset[0 : sampleSize, ] #WE ALWAYS WILL TAKE THE SAME 25% DATA

print(nrow(dataset)) # full dataset
print(nrow(sampleDataset)) # 25% of dataset (random data)

#USIN DPLYR TO MANAGE DATAFRAMES
install.packages("dplyr")
library(dplyr)

#GETTING A RANDOM SAMPLE FROM DATAFRAME WITH LIBRARY
sample2000 = sample_n(dataset, 2000) #GETTING 25% PERCENT FROM THE ORIGINAL DATASET

#GETTING A RANDOM SAMPLE FROM DATAFRAME WITH LIBRARY
sampleDataset = sample_frac(dataset, 0.25) #GETTING 25% PERCENT FROM THE ORIGINAL DATASET

print(nrow(dataset)) # full dataset
print(nrow(sampleDataset)) # 25% of dataset (random data)

#glimpse(sampleDataset) #SHOW A PIECE OF DATAFRAME IN A BEUTY WAYp

#FILTER USING DPLYR
datasetMoreThan70kVotes <- filter(dataset, QT_VOTOS_NOMINAIS > 70000)

datasetElectedMoreThan70kVotes <- filter(dataset, QT_VOTOS_NOMINAIS > 70000, DS_SIT_TOT_TURNO == 'ELEITO')

datasetPTandPmdb <- filter(dataset, SG_PARTIDO == "PT" | SG_PARTIDO == 'PMDB')

datasetPTandPmdb <- filter(dataset, SG_PARTIDO %in% c("PT", 'PMDB'))

mostVoted <- filter(dataset, QT_VOTOS_NOMINAIS == max(dataset$QT_VOTOS_NOMINAIS, na.rm=TRUE))
mostVoted <- mostVoted[, c("NM_PARTIDO", "NM_MUNICIPIO", "QT_VOTOS_NOMINAIS")]

select(mostVoted, NM_PARTIDO, NM_MUNICIPIO)

select(mostVoted, contains("NM"), contains("QT"))

#CHAINING (SELECT + FILTER)

#ELECTED WITH MORE THAN 70K VOTES + SELECT A FEW FIELDS

dataset %>% head() 
dataset %>% head #if theres no argument we can omit the parenteses

dataset %>%
  select(NM_URNA_CANDIDATO, NM_PARTIDO, NM_MUNICIPIO, QT_VOTOS_NOMINAIS, DS_SIT_TOT_TURNO) %>%
  filter(QT_VOTOS_NOMINAIS > 70000, DS_SIT_TOT_TURNO == 'ELEITO')

#PARTY WITH MORE THAN 70K VOTES + SELECT A FEW FIELDS
dataset %>%
  select(NM_URNA_CANDIDATO, NM_PARTIDO, NM_MUNICIPIO, QT_VOTOS_NOMINAIS, DS_SIT_TOT_TURNO) %>%
  filter(QT_VOTOS_NOMINAIS > 70000, DS_SIT_TOT_TURNO == 'ELEITO') %>%
  distinct(NM_PARTIDO)

#MOST VOTED + SELECT A FEW FIELDS
dataset %>%
  select(NM_URNA_CANDIDATO, NM_PARTIDO, NM_MUNICIPIO, QT_VOTOS_NOMINAIS, DS_SIT_TOT_TURNO) %>%
  filter(QT_VOTOS_NOMINAIS == max(dataset$QT_VOTOS_NOMINAIS, na.rm=TRUE))

#ELECTED WITH MORE THAN 70K VOTES + SELECT A FEW FIELDS + SORTING BY VOTES
dataset %>%
  select(NM_URNA_CANDIDATO, NM_PARTIDO, NM_MUNICIPIO, QT_VOTOS_NOMINAIS, DS_SIT_TOT_TURNO) %>%
  filter(QT_VOTOS_NOMINAIS > 70000, DS_SIT_TOT_TURNO == 'ELEITO') %>%
  arrange(desc(QT_VOTOS_NOMINAIS))

#NUMBER OF PARTIES
distinctParties <- 
  dataset %>%
  select(SG_PARTIDO) %>%
  group_by(SG_PARTIDO) %>%
  n_groups()

#SUM ALL VOTES + GET ONLY WITH MORE THAN 1KK VOTES + STORTING BY VOTES
moreThan1kk <- 
dataset %>%
  select(NM_URNA_CANDIDATO, QT_VOTOS_NOMINAIS) %>%
  group_by(NM_URNA_CANDIDATO) %>%
  summarise(QT_TOTAL_VOTOS = sum(QT_VOTOS_NOMINAIS, na.rm = TRUE)) %>%
  filter(QT_TOTAL_VOTOS > 1000000) %>%
  arrange(desc(QT_TOTAL_VOTOS))


#PRINT DATAFRAMES
install.packages("knitr")
library(knitr)
knitr::kable(categoricalDataFrame)

#JOIN, LEFT JOIN AND CROSS-JOIN
a = read.csv("../example_categorical.csv", encoding="UTF-8") 
b = read.csv("../example_categorical_join.csv", encoding="UTF-8")

#INNER JOIN (Intersection)
a %>% inner_join(b, by = c("UF", "cidade"))

#LEFT JOIN (The entire left side and intersection)
a %>% left_join(b, by = c("UF", "cidade"))

#RIGHT JOIN (The entire right side and intersection)
a %>% right_join(b, by = c("UF", "cidade"))

#FULL JOIN (everything)
a %>% full_join(b, by = c("UF", "cidade"))

# PLOTING CHARTS
install.packages("ggplot2")
library(ggplot2)

#BARPLOT
barPlot <- ggplot(data=moreThan1kk[1:4,], aes(x=NM_URNA_CANDIDATO, y=QT_TOTAL_VOTOS)) 
barPlot <- barPlot + geom_col(fill="darkgreen")
barPlot <- barPlot + xlab("Candidato") + ylab("Votos")
barPlot <- barPlot + ggtitle("Mais de um milhão de votos")
barPlot <- barPlot + theme(axis.title.x = element_text(face="bold",colour="#3CB371",size=12))

#REMOVING MATH NOTATION 
#IMPORT SCALES LIBRARY
library(scales)
barPlot <- barPlot + scale_y_continuous(labels = comma)

# scatter plot
scatterPlot <- ggplot(data=moreThan1kk[1:4,], aes(x=NM_URNA_CANDIDATO, y=QT_TOTAL_VOTOS)) 
scatterPlot <- scatterPlot + geom_point()
scatterPlot <- scatterPlot + scale_y_continuous(labels = comma)

# DUMMIES VARIABLES (BINARY MATRIX)
install.packages("fastDummies") # install dummies
library(fastDummies) # import dummies

categoricalDataFrame = read.csv("../example_categorical.csv", encoding="UTF-8")
summary(categoricalDataFrame) #VERIFICAR A QUALIDADE DOS DOS DADOS
datasetBinaryMatrix = dummy_cols(categoricalDataFrame)

datasetBinaryMatrix

#DUMMIES SELECTED COLUMNS (EVEN NUMBER CATEGORICALS VARIABLES)
results <- dummy_cols(categoricalDataFrame, select_columns = c("codigo_regiao", "cidade"))


#USIGN K MODES
install.packages("klaR")  
library(klaR)

categoricalColumns = excludeColumns <- names(dataset) %in% c("CD_TIPO_ELEICAO", "CD_MUNICIPIO", "DT_GERACAO", "SQ_CANDIDATO", "HH_GERACAO", "ANO_ELEICAO", "NR_TURNO", "CD_ELEICAO", "SG_UE", "NR_ZONA", "CD_CARGO", "CD_SITUACAO_CANDIDATURA", "CD_DETALHE_SITUACAO_CAND", "NR_PARTIDO", "SQ_COLIGACAO", "CD_SIT_TOT_TURNO", "QT_VOTOS_NOMINAIS") 
categoricalDataSet <- dataset[!categoricalColumns]
cl <- kmodes(categoricalDataSet, modes=3, iter.max = 10, weighted = FALSE, fast = TRUE)

#plot(jitter(x), col = cl$cluster)
#points(cl$modes, col = 1:5, pch = 8)

# ANALISE DE CORRESPOND?NCIA MULTIPLA (MCA)
install.packages(c("FactoMineR", "factoextra"))
library("FactoMineR") #MCA
library("factoextra") #PLOT

## install.packages(c('tibble', 'dplyr', 'tidyr'))
#library(tibble)
#library(dplyr)
#library(tidyr)
# APRENDIZADO SUPERVISIONADO OU N?O, REGRAS DE ASSOCIA??O!? 
# https://shapeofdata.wordpress.com/2014/03/04/k-modes/

