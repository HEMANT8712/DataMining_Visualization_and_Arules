
##########################################################
#
#               Question 1
##########################################################
 #install rggobi package

## Setting the Path of Directory

drive="E:"
path.upto <- paste("STAT5703-HEMANT-101062246-Assignment1", sep="/" )
code.dir <- paste(drive, path.upto,"Code", sep="/")
data.dir <- paste(drive, path.upto,"Data","cars.OKC", sep="/")
work.dir <- paste(drive, path.upto,"Work", sep="/")
setwd(work.dir)

##Reading the CSV format of Cars Data

Cars.dat <- read.table(data.dir, header=TRUE)

## Factoring the Car Data ORigin Column
Cars.dat$Origin = as.factor(Cars.dat$Origin)

Cars.col <- Cars.dat$Origin


## Cleaning the data for Column Year
for (j in 1:length(Cars.dat$Year)){
  if(Cars.dat[j,6] > 100){
    Cars.dat[j,6] = Cars.dat[j,6]-100
  }
}


## Ggobi PLot : Scatterplot and Parallel COordinate gives overall picture of 
###              data but its hard to scale it.

library(rggobi)
g <- ggobi(Cars.dat)

display(g[1], "Scatterplot Matrix")

display(g[1], "Parallel Coordinates Display")

## Plotting the Cars.dat with respect to origin Pairs Plot as it also contain scale for each parameter

pairs(Cars.dat, col=Cars.col)


#Categorize data by origin

Cars.USA=Cars.dat[Cars.dat$Origin==1,-7]
Cars.JPN=Cars.dat[Cars.dat$Origin==2,-7]
Cars.EUR=Cars.dat[Cars.dat$Origin==3,-7]


par(mfrow=c(2,3))

##Setting X axis name
x.names=c("USA","JPN","EUR")

## PLotting the Box Plot of All 6 parameter based on Origin and keeping na.rm =TRUE to avoid NA values
for(i in 1:6){
  plot.title=colnames(Cars.dat)[i]
  boxplot(Cars.USA[,i],Cars.JPN[,i],Cars.EUR[,i],main=plot.title,xaxt="n", col= heat.colors(3), na.rm=TRUE)
  axis(1,at = 1:3,labels=x.names)
}

### CO-PLOT :-To get the concentation of points with yearly for each origin

coplot(Cylinders ~ Year| Origin, data = Cars.dat, col= Cars.dat$Origin)
coplot(MPG ~ Year| Origin, data= Cars.dat,col= Cars.dat$Origin)
coplot(Horsepower ~ Year| Origin, data= Cars.dat,col= Cars.dat$Origin)
coplot(Weight ~ Year| Origin, data= Cars.dat,col= Cars.dat$Origin)
coplot(Acceleration ~ Year | Origin, data= Cars.dat,col= Cars.dat$Origin)

#####We use Box plot to get the Aveage value of the data per year
#####Coplot shows the distribition of data point but clearly didnot mention
#####average avlue concentartion . This helps in deduction Accurately.

#Setting Data Of USA based on Year
Cars.USA.70=Cars.USA[Cars.USA$Year==70,-6]
Cars.USA.71=Cars.USA[Cars.USA$Year==71,-6]
Cars.USA.72=Cars.USA[Cars.USA$Year==72,-6]
Cars.USA.73=Cars.USA[Cars.USA$Year==73,-6]
Cars.USA.74=Cars.USA[Cars.USA$Year==74,-6]
Cars.USA.75=Cars.USA[Cars.USA$Year==75,-6]
Cars.USA.76=Cars.USA[Cars.USA$Year==76,-6]
Cars.USA.77=Cars.USA[Cars.USA$Year==77,-6]
Cars.USA.78=Cars.USA[Cars.USA$Year==78,-6]
Cars.USA.79=Cars.USA[Cars.USA$Year==79,-6]
Cars.USA.80=Cars.USA[Cars.USA$Year==80,-6]
Cars.USA.81=Cars.USA[Cars.USA$Year==81,-6]
Cars.USA.82=Cars.USA[Cars.USA$Year==82,-6]

##BoxPlot of Cars.USA data based on Yearly variation and keeping na.rm =TRUE to avoid NA values
par(mfrow=c(2,3))
x.names=c("1970","1971","1972","1973","1974","1975","1976","1977","1978","1979","1980","1981","1982")
for(i in 1:5){
   
  plot.title <- paste(colnames(Cars.USA)[i], "USA ",sep = '_')
  boxplot(Cars.USA.70[,i],Cars.USA.71[,i],Cars.USA.72[,i],Cars.USA.73[,i],Cars.USA.74[,i],Cars.USA.75[,i],Cars.USA.76[,i],Cars.USA.77[,i],Cars.USA.78[,i],Cars.USA.79[,i],Cars.USA.80[,i],Cars.USA.81[,i],Cars.USA.82[,i],main=plot.title, xaxt="n", col= heat.colors(13),na.rm=TRUE)
  axis(1,at = 1:13,labels=x.names)
}

#Setting Data Of Japan based on Year

Cars.JPN.70=Cars.JPN[Cars.JPN$Year==70,-6]
Cars.JPN.71=Cars.JPN[Cars.JPN$Year==71,-6]
Cars.JPN.72=Cars.JPN[Cars.JPN$Year==72,-6]
Cars.JPN.73=Cars.JPN[Cars.JPN$Year==73,-6]
Cars.JPN.74=Cars.JPN[Cars.JPN$Year==74,-6]
Cars.JPN.75=Cars.JPN[Cars.JPN$Year==75,-6]
Cars.JPN.76=Cars.JPN[Cars.JPN$Year==76,-6]
Cars.JPN.77=Cars.JPN[Cars.JPN$Year==77,-6]
Cars.JPN.78=Cars.JPN[Cars.JPN$Year==78,-6]
Cars.JPN.79=Cars.JPN[Cars.JPN$Year==79,-6]
Cars.JPN.80=Cars.JPN[Cars.JPN$Year==80,-6]
Cars.JPN.81=Cars.JPN[Cars.JPN$Year==81,-6]
Cars.JPN.82=Cars.JPN[Cars.JPN$Year==82,-6]

##BoxPlot of Cars.JPN data based on Yearly variation and keeping na.rm =TRUE to avoid NA values
par(mfrow=c(2,3))
x.names=c("1970","1971","1972","1973","1974","1975","1976","1977","1978","1979","1980","1981","1982")
for(i in 1:5){
  plot.title <- paste(colnames(Cars.USA)[i], "JPN ",sep = '_')
  boxplot(Cars.JPN.70[,i],Cars.JPN.71[,i],Cars.JPN.72[,i],Cars.JPN.73[,i],Cars.JPN.74[,i],Cars.JPN.75[,i],Cars.JPN.76[,i],Cars.JPN.77[,i],Cars.JPN.78[,i],Cars.JPN.79[,i],Cars.JPN.80[,i],Cars.JPN.81[,i],Cars.JPN.82[,i],main=plot.title,xaxt="n", col= heat.colors(13), na.rm=TRUE)
  axis(1,at = 1:13,labels=x.names)
}

#Setting Data Of Europe based on Year and keeping na.rm =TRUE to avoid NA values

Cars.EUR.70=Cars.EUR[Cars.EUR$Year==70,-6]
Cars.EUR.71=Cars.EUR[Cars.EUR$Year==71,-6]
Cars.EUR.72=Cars.EUR[Cars.EUR$Year==72,-6]
Cars.EUR.73=Cars.EUR[Cars.EUR$Year==73,-6]
Cars.EUR.74=Cars.EUR[Cars.EUR$Year==74,-6]
Cars.EUR.75=Cars.EUR[Cars.EUR$Year==75,-6]
Cars.EUR.76=Cars.EUR[Cars.EUR$Year==76,-6]
Cars.EUR.77=Cars.EUR[Cars.EUR$Year==77,-6]
Cars.EUR.78=Cars.EUR[Cars.EUR$Year==78,-6]
Cars.EUR.79=Cars.EUR[Cars.EUR$Year==79,-6]
Cars.EUR.80=Cars.EUR[Cars.EUR$Year==80,-6]
Cars.EUR.81=Cars.EUR[Cars.EUR$Year==81,-6]
Cars.EUR.82=Cars.EUR[Cars.EUR$Year==82,-6]

##BoxPlot of Cars.EUR data based on Yearly variation
par(mfrow=c(2,3))
x.names=c("1970","1971","1972","1973","1974","1975","1976","1977","1978","1979","1980","1981","1982")
for(i in 1:5){
  plot.title <- paste(colnames(Cars.USA)[i], "EUR",sep = '_')
  boxplot(Cars.EUR.70[,i],Cars.EUR.71[,i],Cars.EUR.72[,i],Cars.EUR.73[,i],Cars.EUR.74[,i],Cars.EUR.75[,i],Cars.EUR.76[,i],Cars.EUR.77[,i],Cars.EUR.78[,i],Cars.EUR.79[,i],Cars.EUR.80[,i],Cars.EUR.81[,i],Cars.EUR.82[,i],main=plot.title,xaxt="n", col= heat.colors(13),na.rm = TRUE)
  axis(1,at = 1:13,labels=x.names)
}


