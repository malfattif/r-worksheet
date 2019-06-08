mydata1 = read.csv(file.path("example.csv"), header=T) # import CSV
mydata2 = read.csv(file.path("example.csv"), header=T)

head(mydata1) # first 5 rows of data frame
tail(mydata1) # last 5 rows of data frame
nrow(mydata1) # number of rows of data frame

myfulldata = rbind(mydata1, mydata2) # append files
print(nrow(myfulldata))

install.packages("fastDummies") # install dummies
library(fastDummies) # import dummies

categoricalDataFrame = read.csv(file.path("~/r-worksheet/example_categorical.csv"), header=T)
summary(categoricalDataFrame) #VERIFICAR A QUALIDADE DOS DOS DADOS
dummies = fastDummies::dummy_cols(categoricalDataFrame)

#dummie variables
print(dummies)

# MAYBE THIS IS NOT A CLUSTERIZATION PROBLEM!?
# ANÁLISE DE CORRESPONDÊNCIA MULTIPLA (MCA)

install.packages(c("FactoMineR", "factoextra"), dependencies=TRUE)
library("FactoMineR") #MCA
library("factoextra") #PLOT


