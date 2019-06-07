
mydata1 = read.csv(file.path("example.csv"), header=T)

mydata2 = read.csv(file.path("example.csv"), header=T)

head(mydata1)
tail(mydata1)
nrow(mydata1)

myfulldata = rbind(mydata1, mydata2) # append files
print(nrow(myfulldata))

