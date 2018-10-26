############################################################

#                    Question 2

#############################################################
## Install readxl package
## Install arulesViz package
## Install arules package


## Setting up the directory path
drive="E:"
path.upto <- paste("STAT5703-HEMANT-101062246-Assignment1", sep="/" )
code.dir <- paste(drive, path.upto,"Code", sep="/")
data.dir <- paste(drive, path.upto,"Data","Online Retail.xlsx", sep="/")
work.dir <- paste(drive, path.upto,"Work", sep="/")
setwd(work.dir)

## Installing the library
library("readxl")

## reading the Data file
online_data <- read_excel(data.dir)

##Spliting the data on behalf of Description and Invoice No.

original_data <- split(online_data$Description,online_data$InvoiceNo)
head(original_data)

##Setting Up the arules
library(arules)
library(arulesViz)

##Applying Ariori Rules
rules <- apriori(original_data, parameter=list(support=0.01, confidence=0.1,minlen=2))

###Plotting Value of Support and Confidence to get new value to get best 10 rules
plot(rules, measure=c("support", "confidence"), shading="lift",main = "All Rules Original_Dataset")

##Getting Rules with new value of Support and Confidence from Graph
rules <- apriori(original_data, parameter=list(support=0.02131, confidence=0.5893))

summary(rules)
inspect(head(rules,n=10))

######Plotting rules for minmimum 2 items
plot(rules, method="graph", control=list(type="Description"),main = "10 Rules with 2 Min Items")

## Applying arules for relation between 3 items on original data 
rules.3.items <- apriori(original_data, parameter=list(support=0.015, confidence=0.5, minlen=3))
inspect(rules.3.items)

######Plotting rules for minmimum 3 items
plot(rules.3.items, method="graph", control=list(type="Description"),main = "10 Rules with 3 Min Items")



## Applying arules for relation between 4 items on original data 
rules.4.items <- apriori(original_data, parameter=list(support=0.01, confidence=0.1, minlen=4))

inspect(rules.4.items)

######Plotting rules for minmimum 3 items
plot(rules.4.items, method="graph", control=list(type="Description"),main = "10 Rules with 4 Min Items")



################## Removing Cancelled Transactions############


new_data <- (subset(online_data, Quantity > 0))

data_success_trnx <- split(new_data$Description,new_data$InvoiceNo)

library(arules)
## Applying arules
rules_success_trnx <- apriori(data_success_trnx, parameter=list(support=0.01, confidence=0.1))

###Plotting Value of Support and Confidence to get new value to get best 10 rules
plot(rules_success_trnx, measure=c("support", "confidence"), shading="lift", main = "All Rules Successful Transaction")

##Getting Rules with new value of Support and Confidence from Graph
rules_success_trnx <- apriori(data_success_trnx, parameter=list(support=0.025, confidence=0.66))

summary(rules_success_trnx)
inspect(head(rules_success_trnx, n=10))

######Plotting rules
plot(rules_success_trnx, method="graph", control=list(type="Description"),main = "10 Rules Successful Transaction")



#####################SUBSET of DATA WITH UK Customers############


table(online_data$Country == "United Kingdom")
## As as 495478 entries are from United Kingdom
## Therefore we take the subset of UK data to apply arules

OR.UK<- (subset(online_data, Country == "United Kingdom"))
data.UK <- split(OR.UK$Description,OR.UK$InvoiceNo)
head(data.UK)

#######Applying Arules###################
rules.UK <- apriori(data.UK, parameter=list(support=0.01, confidence=0.1))

###Plotting Value of Support and Confidence to get new value to get best 10 rules
plot(rules.UK, measure=c("support", "confidence"), shading="lift", main = "All Rules Only UK Customers")

##Getting Rules with new value of Support and Confidence from Graph
rules.UK <- apriori(data.UK, parameter=list(support=0.02131, confidence=0.5893, minlen = 2))

summary(rules.UK)
inspect(head(rules.UK,n=10))

######Plotting rules
plot(rules.UK, method="graph", control=list(type="Description"),main = "10 Rules Only UK Customers")

