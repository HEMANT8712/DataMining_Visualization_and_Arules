###############################################################3

# Question -3 : Bounus Implementation

#################################################################

#install arules package

##Setting Directory Path

drive="E:"
path.upto <- paste("STAT5703-HEMANT-101062246-Assignment1", sep="/" )
code.dir <- paste(drive, path.upto,"Code", sep="/")
data.dir <- paste(drive, path.upto,"Data", sep="/")
work.dir <- paste(drive, path.upto,"Work", sep="/")
setwd(work.dir)

## Reading Data from File
titanic.file <- paste(data.dir,"Titanic.csv", sep="/")
titanic.dat = read.csv(titanic.file,header=TRUE)

## Factorizing the data
titanic.dat$Index <- as.factor(titanic.dat$Index)
titanic.dat$SexCode <- as.factor(titanic.dat$SexCode)
titanic.dat$Survived <- as.factor(titanic.dat$Survived)

## Subsetting the data on the basis of people Survived and Not Survived 

titanic.dat.survived = (subset(titanic.dat, Survived == 1))
titanic.dat.dead = (subset(titanic.dat, Survived == 0))

## Removing Column Index, LastName and FirstName,SexCode, Survival
titanic.dat.survived <- titanic.dat.survived[,c(4,5,6)]
titanic.dat.dead = titanic.dat.dead[,c(4,5,6)]

## Removing Column Index, LastName and FirstName,SexCode
titanic.dat <- titanic.dat[,c(4,5,6,7)]

## Rules implementation on whole dataset
library(arules)
rules <- apriori(titanic.dat, parameter=list(support=0.05, confidence=0.5, minlen=2))
summary(rules)
inspect(rules)

## Rules implementation on dataset of survived people
rules.survived <- apriori(titanic.dat.survived, parameter=list(support=0.02, confidence=0.2, minlen=2))
summary(rules.survived)
inspect(rules.survived)

## Rules implementation on dataset of Dead people
rules.dead <- apriori(titanic.dat.dead, parameter=list(support=0.015, confidence=0.1, minlen=2))
summary(rules.dead)
inspect(rules.dead)
