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

library(dplyr)

dfWithTseCode <- localElections2016 %>%
  mutate(tse_code = as.character(as.numeric(SIGLA_UE)))

library(codesBR)
teste <- ibge_from_tse(dfWithTseCode, tse_code)

# 
# cods <- dplyr::select(.data = codigo_ibge_tse, .data$cod_tse, .data$cod_ibge)

#
#
#
#
#
#
lapply(c("esquisse", "ggplot2", "dplyr"), require, character.only = TRUE)
#
#
setwd("C:/Users/1513 IRON/Desktop/Projetos/ProjetosGIT/r-worksheet/votacao") # SETING WORK DIRECTORY
getwd() # GET WORKING DIRECTORY
#
TSEFiles = list.files(path = ".", pattern = "*.csv|*.txt")  #GET ALL TXT AN CSV FILES

rm(dataset)
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



teste <-dataset %>% filter(DS_CARGO == "Prefeito")

teste <- dataset[dataset$DS_CARGO == "Prefeito", ]

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
onlyDoria = sumVotes$Group.1 == "JOÃO AGRIPINO DA COSTA DORIA JUNIOR"
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

o <- filter(dataset, DS_CARGO == "Prefeito")


datasetPTandPmdb <- filter(dataset, SG_PARTIDO %in% c("PT", 'PMDB'))

mostVoted <- filter(dataset, QT_VOTOS_NOMINAIS == max(dataset$QT_VOTOS_NOMINAIS, na.rm=TRUE))
mostVoted <- mostVoted[, c("NM_PARTIDO", "NM_MUNICIPIO", "QT_VOTOS_NOMINAIS")]

select(mostVoted, NM_PARTIDO, NM_MUNICIPIO)

select(mostVoted, contains("NM"), contains("QT"))

#CHAINING (SELECT + FILTER)

#ELECTED WITH MORE THAN 70K VOTES + SELECT A FEW FIELDS

dataset %>% head()
dataset %>% head #if theres no argument we can omit the parenteses

filtrarCandidatosEleitosCoMaisDe70kVotos <- function(votos) {
  votos %>%
    select(NM_URNA_CANDIDATO, NM_PARTIDO, NM_MUNICIPIO, QT_VOTOS_NOMINAIS, DS_SIT_TOT_TURNO) %>%
    filter(QT_VOTOS_NOMINAIS > 70000, DS_SIT_TOT_TURNO == 'ELEITO')
}

library(dplyr)
teste <-  filtrarCandidatosEleitosCoMaisDe70kVotos(dataset)
teste2 <- filtrarCandidatosEleitosCoMaisDe70kVotos(dataset)


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

knitr::kable(categoricalDataFrame)
#
# #JOIN, LEFT JOIN AND CROSS-JOIN
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
#
# #BARPLOT
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

#MUTATE VS TRANSMUTE

votos <- read.csv("../votos.csv")

votosComPercentual <-
  votos %>%
   mutate(diferenca= as.numeric(gsub("\\.", "", votos.X)) - as.numeric(gsub("\\.", "", votos.Y))) %>%
   mutate(somaVotos= as.numeric(gsub("\\.", "", votos.X)) + as.numeric(gsub("\\.", "", votos.Y))) %>%
   mutate(percentual = diferenca / somaVotos * 100)

faixa_20 <- sum(votosComPercentual$percentual <= 20)
faixa_20_50 <- sum(votosComPercentual$percentual > 20 & votosComPercentual$percentual < 50)
faixa_50_80 <- sum(votosComPercentual$percentual >= 50 & votosComPercentual$percentual < 80)
faixa_maior_80 <- sum(votosComPercentual$percentual >= 80)

faixa_20 <- sum(votosComPercentual$percentual <= 20)
faixa_20_50 <- sum(votosComPercentual$percentual > 20 & votosComPercentual$percentual < 50)
faixa_50_80 <- sum(votosComPercentual$percentual >= 50 & votosComPercentual$percentual < 80)
faixa_maior_80 <- sum(votosComPercentual$percentual >= 80)


#calcular correlação- competitividade vs partidos na coligação
#PEARSON
cor(votosComPercentual$somaVotos, votosComPercentual$votos.X, method = "pearson")
#SPEARMAN
cor(votosComPercentual$somaVotos, votosComPercentual$votos.Y, method = "spearman")

cor.test(votosComPercentual$somaVotos, votosComPercentual$votos.X, method = "pearson", conf.level = 0.95)

install.packages("ggpubr")

ggplot(data=votosComPercentual, aes(x=votos.X, y=votos.Y))+ geom_point() + geom_smooth(method="lm")

#CORREÇÃO NÃO IMPLICA EM CAUSALIDADE!!


competitividade <- data.frame("quantidade"=c(faixa_20, faixa_20_50, faixa_50_80, faixa_maior_80),
                              "faixa"=c("< 20%", "20 ~ 50%", "50 ~ 80%", " > 80%"),
                               "teste"=c("1", "0", "0", "1"))

plotCompetitividade <- ggplot(data=competitividade, aes(x=faixa, y=quantidade))
plotCompetitividade <- plotCompetitividade + geom_col(fill="darkgreen")
plotCompetitividade <- plotCompetitividade + xlab("Competitividade") + ylab("Quantidade")
plotCompetitividade <- plotCompetitividade + ggtitle("Indice de competitividade")
plotCompetitividade

install.packages("esquisse")
#
esquisser(viewer = "browser", data=competitividade)

# # GOOGLE VISUALIZATION

install.packages("googleVis")
library(googleVis)

Area <- gvisAreaChart(competitividade,
                      xvar = "faixa",
                      yvar = "quantidade")
plot(Area)

Bubble <- gvisBubbleChart(competitividade, idvar="quantidade",
                          xvar="faixa", yvar="quantidade",
                          colorvar="faixa", sizevar="quantidade",
                           )
plot(Bubble)

ggplot(moreThan1kk) +
 aes(x = NM_URNA_CANDIDATO, fill = NM_URNA_CANDIDATO, weight = QT_TOTAL_VOTOS) +
 geom_bar() +
 scale_fill_viridis_d(option = "plasma") +
 labs(x = "Nome do Candidato", y = "Quantidade de Votos", title = "Votos X Candidato", subtitle = "Mais de 1 milhão de votos", fill = "Legenda") +
 theme_minimal() +
 theme(legend.position = "bottom")
esquisser(viewer = "browser", data= moreThan1kk)

#WRITE CSV
write.csv(moreThan1kk, file = "moreThan1kk.csv")

#DOCUMENTATION
#https://dreamrs.github.io/esquisse/index.html

fviz_mca_var(res.mca, col.var="cos2", repel=TRUE)

# # ANALISE DE CORRESPOND?NCIA MULTIPLA (MCA)
install.packages(c("FactoMineR", "factoextra"))
library("FactoMineR") #MCA
library("factoextra") #PLOT

reduced <-
datasetElectedMoreThan70kVotes %>%
  select(SG_PARTIDO, NM_COLIGACAO)

res.mca <- MCA(reduced, graph= FALSE)
fviz_mca_var(res.mca, repel=TRUE, col.var ="cos2")

# 1) Exibir a quantidade total de votos por estado da eleição de 2016 (usar as funções group_by e select, summarise)

votesByState <-
  dataset %>%
  select(SG_UF, QT_VOTOS_NOMINAIS, DS_CARGO) %>%
  group_by(SG_UF, DS_CARGO) %>%
  summarise(QT_TOTAL_VOTOS = sum(QT_VOTOS_NOMINAIS, na.rm = TRUE))

votesByState %>% filter(SG_UF == "SP")
votesByState %>% filter(SG_UF == "RJ")
votesByState %>% filter(SG_UF == "SC")

# 2) Descobrir o motivo de cassação mais frequente do Estado Rio grande do Sul - eleição de 2016
cassadosRS <- read.csv("../cassacao/motivo_cassacao_2016_RS.csv", sep=";")

#ROOT MapReduce
motivos <-
  cassadosRS %>%
  mutate(UM=1) %>%
  group_by(DS_MOTIVO_CASSACAO) %>%
  summarise(QUANTIDADE = sum(UM))

# #NUTELLA
motivos <- count(cassadosRS, DS_MOTIVO_CASSACAO) %>% arrange(desc(n))
#
# # 3) Exibir em um gráfico de barras os 3 principais motivos de cassação do Rio Grande do Sul - eleição de 2016
#
library(ggplot2)

ggplot(motivos[1:3, ]) +
 aes(x = DS_MOTIVO_CASSACAO, fill = DS_MOTIVO_CASSACAO, weight = n) +
 geom_bar() +
 scale_fill_hue() +
 labs(x = "Motivos", y = "Quantidade", title = "Maiores motivos de cassação RS", fill = "Motivos de cassação") +
 theme_minimal()

# 4) Criar um dataset com o nome do municipio onde candidatos com mais de 100 mil votos foram cassados (usar join)

cassadosFiles = list.files(path = "../cassacao", pattern = "*.csv|*.txt")  #GET ALL TXT AN CSV FILES

for (file in cassadosFiles) {

  if (!exists("cassados")){
    cassados <- read.csv(paste("../cassacao", file, sep="/"), sep=";", header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
  }
  else if (exists("cassados")){
    temp_dataset <- read.csv(paste("../cassacao", file, sep="/"), sep=";", header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
    cassados<- rbind(cassados, temp_dataset)
    rm(temp_dataset)
  }
}

moreThan50kVotes <-
  dataset %>%
  group_by(NM_URNA_CANDIDATO, NM_MUNICIPIO, SQ_CANDIDATO) %>%
  summarise(QT_TOTAL_VOTOS = sum(QT_VOTOS_NOMINAIS, na.rm = TRUE)) %>%
  filter(QT_TOTAL_VOTOS >= 50000) %>%
  arrange(desc(QT_TOTAL_VOTOS))

cassadosWithMoreThan50kVotes <- inner_join(moreThan50kVotes, cassados, by="SQ_CANDIDATO")
cassadosWithMoreThan50kVotes

motivos <- count(cassados, DS_MOTIVO_CASSACAO) %>% arrange(desc(n))

ggplot(motivos[1:3, ]) +
  aes(x = DS_MOTIVO_CASSACAO, fill = DS_MOTIVO_CASSACAO, weight = n) +
  geom_bar() +
  scale_fill_hue() +
  labs(x = "Motivos", y = "Quantidade", title = "Maiores motivos de cassação RS", fill = "Motivos de cassação") +
  theme_minimal()
#
#
# #2012
files2012 = list.files(path = "../votacao2012/", pattern = "*.csv|*.txt")  #GET ALL TXT AN CSV FILES

for (file in files2012) {

  if (!exists("dataset2012")){
    dataset2012 <- read.csv(paste("../votacao2012/", file, sep="/"), sep=";", header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
  }
  else if (exists("dataset2012")){
    temp_dataset <- read.csv(paste("../votacao2012/", file, sep="/"), sep=";", header=TRUE, stringsAsFactors=FALSE, fileEncoding="latin1")
    dataset2012<- rbind(dataset2012, temp_dataset)
    rm(temp_dataset)
  }
}


#ELECTIONS BR

install.packages("electionsBR")
library(electionsBR)

#PRÓS
#FACIL DE USAR
#BAIXA SEMPRE O ARQUIVO MAIS ATUALIZADO
#ECONOMIA DE TEMPO

#CONTRAS
#DEPENDE DE INTERNET
#DEPENDE DO TSE


#FEDERAL ELECTIONS
federal2002 <- candidate_fed(year = 2002)

#RS FEDERAL ELECTIONS
rsFederal2002 <- vote_mun_zone_fed(2002, uf = "RS")

#LOCAL ELECTIONS
localElections2012 <- vote_mun_zone_local(2012)

#LOCAL ELECTIONS
localElections2016 <- vote_mun_zone_local(2016)

#ELECTORAL COALITION
coalition <- legend_local(2016)

#ALL LOCAL ELECTIONS BETWEEN 1996 AND 2016
anos <- seq(1996, 2016, by = 4)

dadosEleicoes <- lapply(anos, vote_mun_zone_local)

allEleicoes <- bind_rows(dadosEleicoes)

unique(allEleicoes$ANO_ELEICAO)

group <-
  allEleicoes %>%
  group_by(NOME_URNA_CANDIDATO, NOME_MUNICIPIO) %>%
  summarise(QT_TOTAL_VOTOS = sum(TOTAL_VOTOS, na.rm = TRUE))

group %>% select(QT_TOTAL_VOTOS) %>% dplyr::filter(NOME_URNA_CANDIDATO == "JOÃO DORIA")


install.packages("devtools")
library(devtools)
devtools::install_github("xmarquez/democracyData")
library(democracyData)


#PARITICIPATION = (VOTING_POPULATION / TOTAL_POPULATION) * 100

#COMPETITION = 100 - PERCENTAGE_WON_LARGEST_PARTY

#PARLAMENTARY = 100 - PERCENTAGE_WON_LARGEST_PARTY
#PRESIDENCTIAL = 100 - PERCENTAGE_WON_LARGEST_PARTY

#COMPETITION = (PRESIDENCTIAL + PARLAMENTARY) / 2


#MÉDIA PONDERADA - ENTRE OS INDICES
# oposição 0,3  - 0 - 100% (TODOS TEM QUE ESTAR NA MESMA ESCALA)
# participação 0,1 -  
# competição 0,25 - 20%, 50%, 80%
# 
# INDICE DE COMPETITIVIDADE = 47%

#
install.packages("codesBR")
library(codesBR)
devtools::install_github("meirelesff/codesBR")
anos <- seq(1996, 2016, by = 4)
dadosEleicoes <- lapply(anos, vote_mun_zone_local)
x3 <- lapply(anos, elections_rda,  level = "local", archive = "candidate")

allEleicoes <- bind_rows(x3)
