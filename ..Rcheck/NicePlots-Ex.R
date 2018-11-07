pkgname <- "NicePlots"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('NicePlots')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
base::assign(".old_wd", base::getwd(), pos = 'CheckExEnv')
cleanEx()
nameEx("addNicePoints")
### * addNicePoints

flush(stderr()); flush(stdout())

### Name: addNicePoints
### Title: Add a datapoint overlay to a box or violin plot
### Aliases: addNicePoints

### ** Examples

#Add a beeswarm plot overlay to a boxplot in the iris dataset:
data(iris)
data<-list(data=iris$Sepal.Length)
boxplot(iris$Sepal.Length~iris$Species)
addNicePoints(data,by=iris$Species,pointMethod="beeswarm",plotAt=1:3)

#Add an outlier point to a boxplot:
boxplot(iris$Sepal.Length~iris$Species, outline=FALSE)
addNicePoints(data,by=iris$Species,pointMethod="linear",plotAt=1:3,
    drawPoints=FALSE,outliers=1.5)




cleanEx()
nameEx("calcStats")
### * calcStats

flush(stderr()); flush(stdout())

### Name: calcStats
### Title: calculate preliminary statistical significance analysis
### Aliases: calcStats

### ** Examples

data(iris)




cleanEx()
nameEx("drawBar")
### * drawBar

flush(stderr()); flush(stdout())

### Name: drawBar
### Title: drawBar
### Aliases: drawBar

### ** Examples

#ToDo




cleanEx()
nameEx("drawBoxPlot")
### * drawBoxPlot

flush(stderr()); flush(stdout())

### Name: drawBoxPlot
### Title: draw a custom box and whisker plot
### Aliases: drawBoxPlot

### ** Examples

library(dplyr)
data(iris)
iData<-iris %>% group_by(Species) %>%
   summarize(median=median(Sepal.Length),min=min(Sepal.Length),max=max(Sepal.Length),
   q1=quantile(Sepal.Length)[2],q3=quantile(Sepal.Length)[4]) %>%
   bind_cols(at=c(1:3),width=c(.2,.3,.4))
plot(1,1,type="n",xlim=c(0,4),ylim=c(0,9))



cleanEx()
nameEx("drawPoints")
### * drawPoints

flush(stderr()); flush(stdout())

### Name: drawPoints
### Title: draw dots for a dot plot
### Aliases: drawPoints

### ** Examples

library(dplyr)
data(iris)
boxplot(iris$Sepal.Length~iris$Species,ylab="Sepal Length")
iData<-data.frame(at=as.numeric(iris$Species),data=iris$Sepal.Length)
drawPoints(iData,type="jitter",col=c("red","blue","purple"))



cleanEx()
nameEx("errorBars")
### * errorBars

flush(stderr()); flush(stdout())

### Name: errorBars
### Title: draw custom error bars
### Aliases: errorBars

### ** Examples

library(dplyr)
data(iris)
iData<-iris %>% group_by(Species) %>%
   summarize(Average=mean(Sepal.Length),SD=sd(Sepal.Length))
barplot(iData$Average,ylim=c(0,10),names=levels(iris$Species),ylab="sepal length")
loc<-c(.7,1.9,3.1)
top<-iData$SD*2+iData$Average
bottom<-iData$SD*-2+iData$Average
errorBars(data.frame(at=loc,start=iData$Average,stop=top),capType="ball",capSize=2)
errorBars(data.frame(at=loc,start=iData$Average,stop=bottom),capType="ball",capSize=2)



cleanEx()
nameEx("facetSpacing")
### * facetSpacing

flush(stderr()); flush(stdout())

### Name: facetSpacing
### Title: Generate plotting locations for subgrouping data
### Aliases: facetSpacing

### ** Examples




cleanEx()
nameEx("formatPlotColors")
### * formatPlotColors

flush(stderr()); flush(stdout())

### Name: formatPlotColors
### Title: format a NicePlots color list
### Aliases: formatPlotColors

### ** Examples

myCols<-list(bg="lightgrey",fill=c("red","green","blue"),lines="darkgrey")
print(myCols)



cleanEx()
nameEx("makeColorMatrix")
### * makeColorMatrix

flush(stderr()); flush(stdout())

### Name: makeColorMatrix
### Title: Create a matrix of increasingly transparent colors
### Aliases: makeColorMatrix

### ** Examples

plot(1,1,col="white",xlim=c(0,10),ylim=c(0,10))
for(n in 1:6){rect(0:4,rep(8-n,5),1:5,rep(9-n,5),col=as.matrix(makeColorMatrix())[n,])}

#An example how it can be used in practice:
myData<-rnorm(600)
fact<-factor(c(rep("a",100),rep("b",100),rep("c",100),rep("d",100),rep("e",100),rep("f",100)))
plot(myData,col=makeColorMatrix()[fact,3])



cleanEx()
nameEx("makeLogTicks")
### * makeLogTicks

flush(stderr()); flush(stdout())

### Name: makeLogTicks
### Title: format a log scale axis
### Aliases: makeLogTicks

### ** Examples

plot(1:10,log(1:10,2),yaxt="n",ylab="")



cleanEx()
nameEx("niceBar")
### * niceBar

flush(stderr()); flush(stdout())

### Name: niceBar
### Title: draw a bar plot
### Aliases: niceBar

### ** Examples

data(iris)
mCols<-makeColorMatrix()
myCols<-list(fill=c(mCols[1,3],mCols[2,3],mCols[3,3]),lines="darkblue")
Lab<-"Sepal Length"
niceVio(iris$Sepal.Length,by=iris$Species,minorTick=4,showCalc=TRUE,
    calcType="anova",ylab=Lab,main="Sepal Length by Species",plotColors=myCols)


plot(density(iris$Petal.Length))
lengthFact<-factor(iris$Petal.Length>2.82,labels=c("short","long"))


Title<-"Sepal Length by Species and Petal Length"
factorFrame<-data.frame(Species=iris$Species,PetalLength=lengthFact)
niceVio(iris$Sepal.Length, by=factorFrame, minorTick=4,subGroup=TRUE,
    ylab=Lab,main=Title,plotColors=myCols)



cleanEx()
nameEx("niceBox")
### * niceBox

flush(stderr()); flush(stdout())

### Name: niceBox
### Title: draw a box plot
### Aliases: niceBox

### ** Examples

data(iris)
mCols<-makeColorMatrix()
myCols<-list(fill=c(mCols[1,3],mCols[2,3],mCols[3,3]),lines="darkblue")
Lab<-"Sepal Length"
niceBox(iris$Sepal.Length,iris$Species,minorTick=4,showCalc=TRUE,
    calcType="anova",ylab=Lab,main="Sepal Length by Species",plotColors=myCols)


plot(density(iris$Petal.Length))
lengthFact<-factor(iris$Petal.Length>2.82,labels=c("short","long"))


Title<-"Sepal Length by Species and Petal Length"
factorFrame<-data.frame(Species=iris$Species,PetalLength=lengthFact)
niceBox(iris$Sepal.Length, by=factorFrame, minorTick=4,subGroup=TRUE,
    ylab=Lab,main=Title,plotColors=myCols)



cleanEx()
nameEx("niceDots")
### * niceDots

flush(stderr()); flush(stdout())

### Name: niceDots
### Title: draw a dot plot
### Aliases: niceDots

### ** Examples

data(iris)
mCols<-makeColorMatrix()
myCols<-list(fill=mCols[1:3,3],lines="darkblue")
niceDots(iris$Sepal.Length,iris$Species,minorTick=4,showCalc=TRUE,calcType="anova",
    ylab="Sepal Length",main="Sepal Length by Species",plotColors=myCols)




cleanEx()
nameEx("niceVio")
### * niceVio

flush(stderr()); flush(stdout())

### Name: niceVio
### Title: draw a violin plot
### Aliases: niceVio

### ** Examples

data(iris)
mCols<-makeColorMatrix()
myCols<-list(fill=c(mCols[1,3],mCols[2,3],mCols[3,3]),lines="darkblue")
Lab<-"Sepal Length"
niceVio(iris$Sepal.Length,by=iris$Species,minorTick=4,showCalc=TRUE,
    calcType="anova",ylab=Lab,main="Sepal Length by Species",plotColors=myCols)


plot(density(iris$Petal.Length))
lengthFact<-factor(iris$Petal.Length>2.82,labels=c("short","long"))


Title<-"Sepal Length by Species and Petal Length"
factorFrame<-data.frame(Species=iris$Species,PetalLength=lengthFact)
niceVio(iris$Sepal.Length, by=factorFrame, minorTick=4,subGroup=TRUE,
    ylab=Lab,main=Title,plotColors=myCols)



cleanEx()
nameEx("prepCategoryWindow")
### * prepCategoryWindow

flush(stderr()); flush(stdout())

### Name: prepCategoryWindow
### Title: prepare a plotting environment for categorical data such as bar
###   plots or box plots
### Aliases: prepCategoryWindow

### ** Examples

todo<-1




cleanEx()
nameEx("prepNiceData")
### * prepNiceData

flush(stderr()); flush(stdout())

### Name: prepNiceData
### Title: Prepare and print basic statistics for niceBox and niceVio
### Aliases: prepNiceData

### ** Examples

data(iris)
filter<-rep(TRUE,length(iris$Species))
loc<-seq(1,length(levels(iris$Species)))
data<-list(data=iris[,1:4])




cleanEx()
nameEx("quantileTrim")
### * quantileTrim

flush(stderr()); flush(stdout())

### Name: quantileTrim
### Title: Filter data by interquartile range
### Aliases: quantileTrim

### ** Examples

x<-rnorm(1000)
paste0(mean(x)," (",range(x),")")
x<-quantileTrim(x,threshold=1.5)
paste0(mean(x)," (",range(x),")")

#Example using the filter function:
myData<-c(NA,rnorm(100),NA,NA,rnorm(100),NA,NA,NA,rnorm(300),NA,10000)
myIndex<-1:508
newData<-quantileTrim(myData,na.rm=TRUE,returnFilter=TRUE)
identical(newData$data,myData[newData$filter])



cleanEx()
nameEx("setAlpha")
### * setAlpha

flush(stderr()); flush(stdout())

### Name: setAlpha
### Title: add alpha transparency to a named color
### Aliases: setAlpha

### ** Examples

plot(1,1,col="white",xlim=c(0,10),ylim=c(0,10))
rect(1,1,7,7,col=setAlpha("darkblue"))
rect(3,3,9,9, col=setAlpha("red"))




### * <FOOTER>
###
cleanEx()
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
