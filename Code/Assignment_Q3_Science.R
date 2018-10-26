###############################################################3

# Question -3 : Bounus Implementation

#################################################################
##install arules package

##Setting Directory Path

drive="E:"
path.upto <- paste("STAT5703-HEMANT-101062246-Assignment1", sep="/" )
code.dir <- paste(drive, path.upto,"Code", sep="/")
data.dir <- paste(drive, path.upto,"Data", sep="/")
work.dir <- paste(drive, path.upto,"Work", sep="/")
setwd(work.dir)

## Reading Data from File
Science.file <- paste(data.dir,"Australian_School_Science_Survey.csv", sep="/")
Science.dat = read.csv(Science.file,header=TRUE)

str(Science.dat)
Science.dat <- Science.dat[,c(2,3,4,5,6,7)]

## Factorizing the data
Science.dat$school <- as.factor(Science.dat$school)
Science.dat$class <- as.factor(Science.dat$class)
Science.dat$like <- as.factor(Science.dat$like)


## Rules implementation on whole dataset
library(arules)
## Minimum Variable must be 2
rules <- apriori(Science.dat, parameter=list(support=0.3, confidence=0.5, minlen=2))
summary(rules)
inspect(rules)

## Minimum Variable must be 5
rules.min5 <- apriori(Science.dat, parameter=list(support=0.03, confidence=0.4, minlen=5))
summary(rules.min5)
inspect(rules.min5)
