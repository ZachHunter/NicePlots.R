#' @title Filter data by interquartile range
#' @description
#' \code{quantileTrim} takes a numeric vector and removes data points that fall more than \code{threshold} * the interquartile range outside of the interquartile range. If \code{returnFilter} is set to TRUE then the function returns a named list with the trimmed data and a logical vector
#'
#'@details
#'The interquartile range (IQR) also known as the H-spread, represents the range encompassing the middle 50% of the data.
#'This is is used to as a measure of dispersion around the median and more frequently to detect outlier data points.
#' Here data points are filtered if \eqn{x <  Q_{1} - threshold\times IQR}{x < Q1 - threshold * IQR} and \eqn{x > Q_{3} + threshold\times IQR}{x > Q3 + threshold * IQR} where \eqn{Q_{1}}{Q1} and \eqn{Q_{3}}{Q3} represent the cumulative 25% and 75% quartiles, respectively. Typical values for the \code{threshold} argument are 1.5 for outliers and 3 for extreme outliers.
#'
#' @param x a numeric vector or a object compatible with the \code{\link[stats]{quantile}} function
#' @param threshold numeric; the number of interquartile ranges out side of the inner 50\% range of the data to use as a cutoff from trimming. Typical values include 1.5 for outliers and 3 for extreme outliers.
#' @param na.rm logical; if true will remove all \code{\link{NA}} values from \code{x} before analyzing the data.
#' @param returnFilter logical; will cause the function to return a list including with both the trimmed data and a logical vector that can be used to filter objects of the same length as \code{x}.
#'
#' @return The trimmed numeric vector or a \code{returnFilter} is \code{\link{TRUE}} then a named list labeled data and filter is returned with the trimmed data and the logical filtering vector, respectively.
#' @examples
#' x<-rnorm(1000)
#' paste0(mean(x)," (",range(x),")")
#' x<-quantileTrim(x,threshold=1.5)
#' paste0(mean(x)," (",range(x),")")
#'
#' #Example using the filter function:
#' myData<-c(NA,rnorm(100),NA,NA,rnorm(100),NA,NA,NA,rnorm(300),NA,10000)
#' myIndex<-1:508
#' newData<-quantileTrim(myData,na.rm=TRUE,returnFilter=TRUE)
#' identical(newData$data,myData[newData$filter])
#' @export
#' @importFrom stats quantile
#' @seealso \code{\link[stats]{quantile}}.
quantileTrim<-function(x,threshold=3,na.rm=FALSE,returnFilter=FALSE){
  naFilter<-1:length(x)
  finalFilter<-rep(FALSE,length(x))
  if(na.rm){
    NAloc<-(is.na(x)==FALSE)
    x<-x[NAloc]
    naFilter<-naFilter[NAloc]
  }
  if(length(x)==0){return(NULL)}
  #Note: if x has less than seven elements outlier detection is probabaly not a good idea
  #May need to be increased even more.
  else if(length(x)<7){
    if(returnFilter) {
      return(list(data=x,filter=rep(1,length(x))))
    } else {
      return(x)
    }
  } else {
    iqr<-quantile(x)[c(2,4)]
    thresholdUpper<-(iqr[2]-iqr[1])*threshold +iqr[2]
    thresholdLower<-iqr[1] - (iqr[2]-iqr[1])*threshold
    filter<-which(x>thresholdLower & x<thresholdUpper)
    #naFilter<-naFilter[filter]
    finalFilter[naFilter[filter]]<-TRUE
    if(returnFilter) {
      return(list(data=x[filter],filter=finalFilter))
    }
    return(x[filter])
  }
}

#' @title Create a matrix of increasingly transparent colors
#' @description
#' \code{makeColorMatrix} is a convenience function for plotting with transparent colors.
#'
#' @details
#' This function take no arguments, but generates rows corresponding to red, blue, green, gray, purple and gold with increasing transparency moving from left to right across the columns.
#'
#' @return A \code{6 x 5} matrix of colors.
#' @examples
#' plot(1,1,col="white",xlim=c(0,10),ylim=c(0,10))
#' for(n in 1:6){rect(0:4,rep(8-n,5),1:5,rep(9-n,5),col=as.matrix(makeColorMatrix())[n,])}
#'
#' #An example how it can be used in practice:
#' myData<-rnorm(600)
#' fact<-factor(c(rep("a",100),rep("b",100),rep("c",100),rep("d",100),rep("e",100),rep("f",100)))
#' plot(myData,col=makeColorMatrix()[fact,3])
#' @export
#' @importFrom grDevices col2rgb rgb rainbow
#' @importFrom dplyr bind_cols
#' @import RColorBrewer
#' @seealso \code{\link[grDevices]{rainbow}}, \code{\link[grDevices]{col2rgb}}, \code{\link[grDevices]{rgb}}.
makeColorMatrix<-function(){
  myColors<-list(base=c("red","blue","green","black","purple","gold"))
  for(i in 1:4) {
    r<-col2rgb("red",alpha=(1-.2*i))
    b<-col2rgb("blue",alpha=(1-.2*i))
    g<-col2rgb("green",alpha=(1-.2*i))
    gr<-col2rgb("black",alpha=(1-.2*i))
    p<-col2rgb("purple",alpha=(1-.2*i))
    gl<-col2rgb("gold",alpha=(1-.2*i))
    r<-rgb(r[1]/255,r[2]/255,r[3]/255,alpha=(1-.2*i))
    b<-rgb(b[1]/255,b[2]/255,b[3]/255,alpha=(1-.2*i))
    g<-rgb(g[1]/255,g[2]/255,g[3]/255,alpha=(1-.2*i))
    gr<-rgb(gr[1]/255, gr[2]/255, gr[3]/255,alpha=(1-.2*i))
    p<-rgb(p[1]/255, p[2]/255, p[3]/255,alpha=(1-.2*i))
    gl<-rgb(gl[1]/255, gl[2]/255, gl[3]/255,alpha=(1-.2*i))
    myColors[[i+1]]<-c(r,b,g,gr,p,gl)
  }
  names(myColors)[2:5]<-paste0("alpha",seq(.2,.8,by=.2))
  as.matrix(dplyr::bind_cols(myColors))
}

#' @title add alpha transparency to a named color
#' @description
#' Takes a named color such as "red" or "darkgreen" and adds a level of transparency based on the alpha setting.
#'
#' @details
#' \code{setAlpha} is a convenience function that uses the \code{\link[grDevices]{col2rgb}} and \code{\link[grDevices]{rgb}} to add transparancy to named colors.
#'
#' @param x character string; a text string corresponding to an R color
#' @param alpha numeric [0-1]; sets the level of transparency.
#' @return An rbg color with transparancy \code{alpha}.
#' @examples
#' plot(1,1,col="white",xlim=c(0,10),ylim=c(0,10))
#' rect(1,1,7,7,col=setAlpha("darkblue"))
#' rect(3,3,9,9, col=setAlpha("red"))
#'
#' @export
#' @importFrom grDevices col2rgb rgb rainbow
#' @seealso \code{\link{makeColorMatrix}}, \code{\link[grDevices]{rainbow}}, \code{\link[grDevices]{col2rgb}}, \code{\link[grDevices]{rgb}}.
setAlpha<-function(x,alpha=.2){
  myCol<-col2rgb(x,alpha=alpha)
  myCol<-rgb(myCol[1]/255, myCol[2]/255, myCol[3]/255,alpha=alpha)
  myCol
}

#' @title calculate preliminary statistical significance analysis
#' @description
#' \code{calcStats} takes a numeric vector and a factor and runs a preliminary statistical analysis. Output is printed to the screen and the p-value is returned as a character string.
#'
#' @details
#' This is designed to be used in conjunction with data visualization plots to help with data exploration and should not be used for a robust statistical analysis. Normal distribution, variance and other data characteristics are not evaluated and there is no guarantee that the underling test assumptions are met. For two level factors \code{\link{wilcox.test}} or \code{\link{t.test}} is recommended. If the factor has more than two levels then \code{\link{pairwise.wilcox.test}} and \code{\link{pairwise.t.test}} are automatically selected. In this case \code{\link{anova}} and the optional follow-up \code{\link{TukeyHSD}} can also be used. All output it printed to the console and for the two level tests and \code{\link{anova}} the p-value is returned as a text string.
#'
#' @param x numeric; numeric vector of data points to analyze.
#' @param by factor; factor describing the groups within \code{x} to test.
#' @param type character; determines which statistical test should be used. Accepted values are 'wilcox', 't.test', 'ttest', 'anova' and 'tukey'. Values not matching a valid input will produce a warning.
#' @param verbose logical; will print statistical output to the screen if set \code{\link{TRUE}}. Calculations returned by the function either way.
#'
#' @return A three element list with the test type, test result object and the p-value as a text string (if available).
#' @importFrom stats t.test wilcox.test anova TukeyHSD pairwise.wilcox.test pairwise.t.test aov median
#' @examples
#' data(iris)
#' #calcStats is not exported but you can invoke it using the calcType options:
#' test<-niceBox(iris$Sepal.Length, by=iris$Species, calcType="anova", showCalc=TRUE)
#' test$stats
#'
#' @seealso \code{\link[stats]{wilcox.test}}, \code{\link[stats]{pairwise.wilcox.test}}, \code{\link[stats]{t.test}}, \code{\link[stats]{pairwise.t.test}}, \code{\link[stats]{anova}}, \code{\link[stats]{TukeyHSD}}
calcStats<-function(x,by,type=c("Wilcox","Tukey","T.Test","ANOVA"),verbose=FALSE){
  pvalue<-NA
  testType<-NA
  testData<-NA
  p<-NA
  if(length(levels(factor(by)))>2){
    if(grepl("^wil",ignore.case = TRUE,x = type[1])) {
      testData<-pairwise.wilcox.test(x,by,p.adjust.method="holm")
      testType<-"pairwise.wilcox.test"
      if(verbose){print(testData)}
    } else if(grepl("^t\\.?t",ignore.case = TRUE, x=type[1])) {
      testData<-pairwise.t.test(x,by,p.adjust.method="holm")
      testType<-"pairwise.t.test"
      if(verbose){print(testData)}
    } else if(grepl("^an",ignore.case = TRUE, x=type[1])) {
      testData<-aov(x~by)
      testType<-"anova"
      if(verbose){print(anova(testData))}
      pvalue <-"ANOVA p-value "
      p<-unlist(anova(testData)[5])
    } else if (grepl("^tuk",ignore.case = TRUE,x=type[1])) {
      m<-aov(x~by)
      if(verbose){print(anova(m))}
      testData<-TukeyHSD(m)
      testType<-"TukeyHSD"
      if(verbose){print(testData)}
      pvalue <-"ANOVA p-value "
      p<-unlist(anova(m)[5])
    } else {
      warning(paste0("Statistic type ",type," not recognized.\nPlease check spelling and/or documentation for more information."))
      #return("P=NA")
    }
  } else if(length(levels(factor(by)))==2){
    if(grepl("^an",ignore.case = TRUE, x=type[1])) {
      warning("Only two levels detected for analysis of variance (ANOVA).\nReccomend using wilcoxon rank sum instead.")
      testData<-aov(x~by)
      testType<-"anova"
      if(verbose){print(anova(testData))}
      pvalue <-"ANOVA p-value "
      p<-unlist(anova(testData)[5])
    } else if (grepl("^tuk",ignore.case = TRUE,x=type[1])) {
      warning("Only two levels detected for Tukey's honestly significant difference analysis.\nRecommend using wilcoxon rank sum instead.")
      m<-aov(x~by)
      if(verbose){print(anova(m))}
      testData<-TukeyHSD(m)
      testType<-"TukeyHSD"
      if(verbose){print(testData)}
      pvalue <-"ANOVA p-value "
      p<-unlist(anova(m)[5])
    } else if(grepl("^wil",ignore.case = TRUE,x = type[1])) {
      testData<-wilcox.test(x~by)
      testType<-"wilcox.test"
      if(verbose){print(testData)}
      p<-testData$p.value
      pvalue <-"Wilcoxon rank sum p-value "
    } else if(grepl("^t\\.?t",ignore.case = TRUE, x=type[1])) {
      testData<-t.test(x~by)
      testType<-"t.test"
      if(verbose){print(testData)}
      p<-testData$p.value
      pvalue <-"Welch Two Sample t-test p-value "
    } else {
      warning(paste0("Statistic type ",type[1]," not recognized.\nPlease check spelling and/or documentation for more information."))
      #return("P=NA")
    }
  } else {
    warning("Only one level detected in the factor.\nStatistics can not be calculated.")
  }
  if(!is.na(p[1]) & ! is.na(pvalue[1])) {
    if(as.numeric(p[1])<0.00001){
      pvalue<-paste0(pvalue[1],"< 0.00001")
    } else {
      pvalue<-paste0(pvalue[1],"= ",round(p,5))
    }
  }
  return(list(p.value=pvalue,test=testType,results=testData))
}

#' @title Generate plotting locations for subgrouping data
#' @description
#' \code{facetSpacing} generates a vector for the \code{at=} specification in functions for data sub-grouping
#'
#'@details
#' \code{facetSpacing} takes the number factor levels from the primary and secodary grouping factors to generate a vector of positions for plotting subgrouped data for the nicePlots package.
#'The spacing assumes that each primary factor levels is plot on positive integers 1, 2, 3 etc.
#' For a primary factor at position \code{i} with \code{f} subgroup levels, the subgrouping comes from generating equally spaced intervals starting at \eqn{i-\frac{1}{2}+\frac{1}{f+1}}{i-.5+1/(f+1)} and ending at \eqn{i+\frac{1}{2}-\frac{1}{f+1}}{i+.5-1/(f+1)}. Simply put: \deqn{Spacing = \frac{1}{NSubGroups-1}}
#'
#' @param subgroup positive integer; number of levels in the subgrouping factor
#' @param labels positive integer; number of levels in the primary factor
#'
#' @return a numeric vector of where to plot the subgrouped data. Can be supplied to that \code{at=} option in plotting functions
#' @examples
#' #\donttest{boxplot(CNA$BM~ CNA$Status,border="white")}
#' #\donttest{stripchart(CNA$BM~factor(paste0(CNA$Status,CNA$Sex)),add=T,at=facetSpacing(2,2))}
#' @seealso \code{\link{prepCategoryWindow}}
facetSpacing<-function(subgroup,labels) {
  subLabLoc<-NULL
  padding<-1/(subgroup+1)
  for (i in 1:labels){
    subLabLoc<-c(subLabLoc,seq(i-0.5+padding,i+0.5-padding,length.out=subgroup))
  }
  subLabLoc
}

#' @title Calculate the standard error of the mean
#' @description
#' \code{se} takes a numeric vector and returns the corresponding standard error of the mean
#'
#'@details
#' This is a convenience function internal to \code{NicePlots} and is not exported. If \code{x} is a numeric vector, \code{SD} is the standard
#' deviation of \code{x} and  \code{N} is the length of \code{x} then the standard error (se) of the mean may be caclulated as:
#' \deqn{se = \frac{SD}{\sqrt{N}}}
#'
#' @param x numeric vector
#'
#' @return a double corresponding to the standard error of the mean
#' @examples
#' data(iris)
#' #se(iris$Sepal.Length)
#' @seealso \code{\link[stats]{sd}}
#'@importFrom stats sd
se<-function(x) {
  if(length(x)>0) {
    sd(x)/sqrt(length(x))
  } else {
    0
  }
}

#' @title Calculate a 95\% confidence interval from the t-distribution
#' @description
#' \code{se} Calculates the 95\% confidence interval of the mean for a vector of data based on the t-distribution
#'
#'@details
#' This is a convenience function internal to \code{NicePlots} and is not exported. If \code{x} is a numeric vector, \code{SD} is the standard
#' deviation of \code{x}, \code{N} is the length of \code{x} and \code{TQ} is the t-distrubition quantile for \code{0.975} with \code{N-1}
#' degrees for freedom  then the 95\% confidence interval of the mean may be caclulated as:
#' \deqn{95\%ci = \frac{QT*SD}{\sqrt{N}}}
#'
#' @param x numeric vector
#'
#' @return a double corresponding to the length of one arm ofthe 95\% confidence interval
#' @examples
#' data(iris)
#' #t95ci(iris$Sepal.Length)
#' @seealso \code{\link[stats]{sd}}
#'@importFrom stats qt
t95ci<-function(x) {
  if(length(x) >0) {
    qt(0.975,df=length(x)-1)*sd(x)/sqrt(length(x))
  } else {
    0
  }
}

#' @title Confidence interval helper function
#' @description
#' Designed to be used by \code{\link{boot95ci}} to calculate a bootstrap model of an aggregator function given by \code{agg}.
#'
#'@details
#' This is a convenience function internal to \code{NicePlots} and is not exported. The variable \code{agg} should be a string corresponding
#' an aggregator function such as \code{\link[base]{mean}} or \code{\link[stats]{median}}. The indicies designed to passed from the \code{\link[boot]{boot}} function
#' from the package \code{boot} and are used to determine calculated the different bootstrap iterations of the \code{agg} functions on \code{x}.
#'
#' @param x numeric vector
#' @param indices numeric vector: Indicies are passed to calculated individual bootstaps of \code{x}
#' @param agg character: A string corresponding to the aggregate or central tendency function to be modeled (eg. \code{\link[stats]{median}}, \code{\link[base]{mean}}, etc.)
#'
#' @return a number corresponding to a bootstrap iteration of the aggregator function given by \code{agg}
#' @examples
#' #Calculates the median of Sepal.Length from the iris data set
#' data(iris)
#' #ci(iris$Sepal.Length,"median",seq(1,length(iris$Sepal.Length)))
#' @seealso \code{\link[boot]{boot}}, \code{\link{boot95ci}}
#' @importFrom purrr invoke
ci<-function(x,indices,agg) {
  purrr::invoke(agg,list(x=x[indices]))
}

#' @title Calculate a basic bootstrap 95\% confidence interval
#' @description
#' Calculates a basic 95\% boostrap confidence interval for an aggregator function
#'
#'@details
#' This function calculates a bootstrap 95\% confidence interval for a aggregator function determined by the \code{agg} variable.
#' The number of iterations is hard coded at 1000. The variable \code{upper} will cause to return the upper bound of the 95\%
#' confidence interval if set to \code{\link{TRUE}} or the lower bound if set to \code{\link{FALSE}}.
#'
#' @param x numeric vector
#' @param agg character: A string corresponding to the aggregate or central tendency function to be modeled (eg. \code{\link[stats]{median}}, \code{\link[base]{mean}}, etc.)
#' @param upper logical: Should the upper or lower confidence interval boundary be returned
#' @param nBoot numeric; An integer value indicating the number of boot strap resampling trails to run. Defaults to 1,000.
#'
#' @return a number corresponding to the upper or lower bootrap 95\% confidence interval of the aggregator function given by \code{agg}.
#' @examples
#' library(boot)
#' data(iris)
#' median(iris$Sepal.Width)
#' #boot95ci(iris$Sepal.Width,agg="median",upper=FALSE)
#' #boot95ci(iris$Sepal.Width,agg="median",upper=TRUE)
#' @seealso \code{\link{prepBarData}}, \code{\link{niceBar}}, \code{\link[boot]{boot}}, \code{\link[boot]{boot.ci}}
# @importFrom boot boot boot.ci
# @importFrom purrr invoke
boot95ci<-function(x,agg="mean",upper=FALSE,nBoot=1000L) {
  boot::boot.ci(boot::boot(x,ci,agg=agg,R=nBoot),type="perc")$percent[4+upper]
}

# This is just an extention of plot to handle the npData class for convenience.
#' @export
plot.npData <- function(x,plotType=NULL, ...) {
  clOptions<-list(...)
  for(opt in names(clOptions)) {
    if(is.null(x$options[opt])){
      append(x$options,list(opt=clOptions[[opt]]))
    }else{
      x$options[[opt]]<-clOptions[[opt]]
    }
  }
  if(!is.null(plotType)) {
    x$plotType<-plotType
  }
  niceFun<-NA
  if(!(grepl("den", x$plotType[1], ignore.case = TRUE) | grepl("scat", x$plotType[1], ignore.case = TRUE))){
    if(!is.null(x$options[["logScale"]])) {
      if(length(x$options[["logScale"]])>1){
        x$options[["logScale"]]<- x$options[["logScale"]][1]
      }
    }
  }
  if(grepl("bar", x$plotType[1], ignore.case = TRUE)){
    niceFun<-"niceBar"
  } else if (grepl("dot", x$plotType[1], ignore.case = TRUE)) {
    niceFun<-"niceDots"
  } else if (grepl("vio", x$plotType[1], ignore.case = TRUE)){
    niceFun<-"niceVio"
  } else if (grepl("den", x$plotType[1], ignore.case = TRUE)){
    niceFun<-"niceDensity"
  } else if (grepl("surf", x$plotType[1], ignore.case = TRUE)){
    x$options[[plotType]]<-"surface"
    niceFun<-"niceDensity"
  } else if (grepl("box", x$plotType[1], ignore.case = TRUE)){
    niceFun<-"niceBox"
  } else if (grepl("scat", x$plotType[1], ignore.case = TRUE)){
    niceFun<-"niceScatter"
  } else {
    warning("Invalid plotType options.\nValid options include,'bar', 'box', 'dot', 'violin', 'density', 'surface' and 'scatter'.\nProceeding with default...",call.=FALSE)
  }
  dataOut<-do.call(niceFun,x$options)
  invisible(dataOut)
}

#Extends the print function to give summary data on npData class objects
#' @export
print.npData <- function(x, ...) {
  l<-1
  cn<-1
  if(is.data.frame(x$options$x)) {
    l<-dim(x$options$x)[1]
    cn<-colnames(x$options$x)
  } else {
    l<-length(x$options$x)
  }
  fl<-0
  fn<-0
  if(is.data.frame(x$options$by)){
    fn <- colnames(x$options$by)
    fl<-length(fn)
    fn<-paste0(" (",paste0(fn,collapse = ", "),")")
  } else if(!is.null(x$options$by)) {
    fn<-NULL
    fl<-1
  }
  cat(paste0("A nicePlots data object with ",l," observations of ",length(cn), " variable"))
  if(length(cn)>1) {
    cat("s")
  }
  cat(".\n")
  cat(paste0("  Plot Type: ",x$plotType,"\n"))
  if(cn[1]>1){
    cat(paste0("  Variable names: ",paste0(cn,collapse = ", "),"\n"))
  }
  if(fl[1] > 0){
    cat(paste0("  Available plotting factors: ",fl,fn,"\n"))
  } else {
    cat("Available plotting factors: None")
  }
}
