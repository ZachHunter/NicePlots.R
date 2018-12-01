#' @title drawBar
#' @description Add a barplot with options error pars to the active ploting enviroment
#'
#' @details
#' This function draws a series of bars based on a dataframe. The expected columns include \code{yt} (locaion top of the bar),
#' \code{yb} or bottom of the bar, \code{at} indicating where the bar should be drawn, \code{Group} which is a unique ID per row, \code{fact} which contains an optional stacking factor
#' \code{UpperError} for the top of the error bar and \code{LowerError} for the location of the bottom of the error bar. The construction of the dataframe is handled automatically from input data
#' by \code{\link{niceBar}}.
#'
#' @param x dataframe; number of interquartile ranges (IQR) past the Q1 (25\%) and Q3 (75\%) cumulative distribution values. Outliers are often defined as \eqn{1.5 \times IQR}{1.5 * IQR} and extreme outliers are more than \eqn{3 \times IQR}{3 * IQR} away from the inner 50\% data range.
#' @param plotColors list; a named list of vectors of colors that set the color options for all NicePlot functions. Names left unspecified will be added and set to default values automatically.
#' @param errorBars Logical; Should error bars be drawn. Defaults to true but is ignored if \code{stack=\link{TRUE}}.
#' @param errorCap character; Determines the style for the ends of the error bars. Valid options are \code{ball}, \code{bar} or \code{none}.
#' @param errorLineType numeric; Sets \code{lty} line type for drawing the error bars.
#' @param width numeric; cex like scaling factor controlling the width of the bars.
#' @param sidePlot logical; Plots bar hight on the x axis if set to \code{\link{TRUE}}.
#' @param stacked logical; draws a stacked barplot if set to \code{\link{TRUE}}.
#' @param capSize numeric; cex like scaling value the controls the size of the caps on the error bars.
#' @param lineWidth numeric; Sets the \code{lwd} options for controling line plotting thickness for the bar plot.
#'
#' @examples
#' data(iris)
#' data<-iris  %>% group_by(Species) %>%
#'     summarize(yt=mean(Sepal.Length),yb=0,UpperError=sd(Sepal.Length),
#'     LowerError=sd(Sepal.Length)) %>%
#'     ungroup() %>% select(yt,yb,UpperError,LowerError,Group=Species) %>%
#'     bind_cols(at=1:3,fact=1:3)
#' plot(type="n",xlim=c(0,4),ylim=c(0,max(iris$Sepal.Length)),-1,xaxt="n")
#' \donttest{drawBar(data,plotColors=list())}
#'
#' @import tidyverse
#' @seealso \code{\link[graphics]{barplot}}, \code{\link{niceBar}}, \code{\link{errorBars}}
drawBar <- function(x, plotColors, errorBars=FALSE, errorCap="ball", errorLineType=1, width=.5, sidePlot=FALSE, stacked=FALSE,capSize=2,lineWidth=1) {
  colorOrder<-plotColors$fill
  #This section builds out the color list to match the number of factors for stacted barplots
  if(stacked){
    if(length(colorOrder)<length(levels(x$fact))) {
      warning("There are fewer colors in plotColors$fill than factor levels for stacking. Stacks will have repeated colors. Use plotColors=list(fill=c(...)) to make a custom color vector.")
      colorOrder<-as.character(rep(colorOrder,1+trunc(length(levels(x$fact))/length(colorOrder)))[1:length(levels(x$fact))])
    } else {
      colorOrder<-as.character(colorOrder[1:length(levels(x$fact))])
    }
    names(colorOrder)<-levels(x$fact)
  }
  if(sidePlot) {
    #IE plot with xy axis fliped
    if(stacked){
      #Build stacks by position using factor levels. Missing factor levels are skipped.
      for (at in unique(x$at)) {
        temp<-x[x$at==at,]
        hAdjust<-0
        for(i in unique(factor(temp$fact))) {
          rect(temp[temp$fact==i,"yb"]+hAdjust,at-width,temp[temp$fact==i,"yt"]+hAdjust,at+width,col=colorOrder[as.character(i)],border=plotColors$lines[1], lwd=lineWidth)
          hAdjust<-temp[temp$fact==i,"yt"]+hAdjust
        }
      }
    } else {
      rect(x$yb,x$at-width,x$yt,x$at+width,col=plotColors$fill,border=plotColors$lines, lwd=lineWidth)
      if(errorBars==TRUE){
        if(errorCap[1]=="bar") {
          x %>% mutate(stop=yt + UpperError,start=yt) %>% errorBars(capType="bar",capSize=width*.25,side=TRUE,col=plotColors$lines,width=lineWidth,lType=errorLineType)
          x %>% mutate(stop=yt - LowerError,start=yt) %>% errorBars(capType="bar",capSize=width*.25,side=TRUE,col=plotColors$lines,width=lineWidth,lType=errorLineType)
        } else if(errorCap[1]=="ball"){
          x %>% mutate(stop=yt + UpperError,start=yt) %>% errorBars(capType="ball",capSize=capSize,side=TRUE,col=plotColors$lines,width=lineWidth,lType=errorLineType)
          x %>% mutate(stop=yt - LowerError,start=yt) %>% errorBars(capType="ball",capSize=capSize,side=TRUE,col=plotColors$lines,width=lineWidth,lType=errorLineType)
        } else {
          x %>% mutate(stop=yt + UpperError,start=yt) %>% errorBars(capType="none",capSize=width*.25,side=TRUE,col=plotColors$lines,width=lineWidth,lType=errorLineType)
          x %>% mutate(stop=yt - LowerError,start=yt) %>% errorBars(capType="none",capSize=width*.25,side=TRUE,col=plotColors$lines,width=lineWidth,lType=errorLineType)
        }
      }
    }
  }else {
    #IE plot xy normally
    if(stacked){
      #Build stacks by position using factor levels. Missing factor levels are skipped.
      for (at in unique(x$at)) {
        temp<-x[x$at==at,]
        hAdjust<-0
        for(i in unique(factor(temp$fact))) {
          rect(at-width, temp[temp$fact==i,"yb"]+hAdjust,at+width,temp[temp$fact==i,"yt"]+hAdjust,col=colorOrder[as.character(i)],border=plotColors$lines[1], lwd=lineWidth)
          hAdjust<-temp[temp$fact==i,"yt"]+hAdjust
        }
      }
    } else {
      rect(x$at-width,x$yb,x$at+width,x$yt,col=plotColors$fill,border=plotColors$lines, lwd=lineWidth)
      if(errorBars==TRUE){
        if(errorCap[1]=="bar") {
          x %>% mutate(stop=yt + UpperError,start=yt) %>%
            errorBars(capType="bar",capSize=width*.25,side=FALSE,col=plotColors$lines,width=lineWidth,lType=errorLineType)
          x %>% mutate(stop=yt - LowerError,start=yt) %>% errorBars(capType="bar",capSize=width*.25,side=FALSE,col=plotColors$lines,width=lineWidth,lType=errorLineType)
        } else if(errorCap[1]=="ball"){
            x %>% mutate(stop=yt + UpperError,start=yt) %>%
            select(at,stop,start) %>%
            errorBars(capType="ball",capSize=capSize,side=FALSE,col=plotColors$lines,width=lineWidth,lType=errorLineType)
          x %>% mutate(stop=yt - LowerError,start=yt) %>%
            select(at,stop,start) %>%
            errorBars(capType="ball",capSize=capSize,side=FALSE,col=plotColors$lines,width=lineWidth,lType=errorLineType)
        } else {
          x %>% mutate(stop=yt + UpperError,start=yt) %>% errorBars(capType="none",capSize=width*.25,side=FALSE,col=plotColors$lines,width=lineWidth,lType=errorLineType)
          x %>% mutate(stop=yt - LowerError,start=yt) %>% errorBars(capType="none",capSize=width*.25,side=FALSE,col=plotColors$lines,width=lineWidth,lType=errorLineType)
        }
      }
    }
  }
}



#' @title draw a bar plot
#' @description Aggregates data from a numeric vector or dataframe using up to three factors to draw a barplot with optional error bars.
#'
#' @details
#' This bar plot function allows for standard barplot features but with error bars, the ability
#' summaryize dataframes into bar plots with median/mean values, sort by bar hight for waterfall plots,
#' color bars based on interquartile outlier detection and more. Barplots can be clustered by a secondary factor
#' or if a dataframe is passed to \code{x} the input values of multiple measurments (dataframe columns) can be
#' clustered together by the primary factor. As with \code{\link{niceBox}}, \code{\link{niceDots}}
#' and \code{\link{niceVio}}, \code{by} can be a factor or a dataframe factors for forming subgroups.
#'
#'For most data this would be nonsensical but if you data is say store profits by goods by region one could group by region (first)
#' @inheritParams prepCategoryWindow
#' @param aggFun character; Determines how the data is summarized by factor level. Valid options are \code{mean}, \code{median} or \code{none}.
#' @param errFun character; How the data spread is charactarized by the error bars. Valid options are \code{sd} (standard deviation), \code{se} (standard error of the mean) or \code{range}.
#' @param stack logical; Should one of the factors in \code{by} be used make a stacked bar plot. Note that this sort of analysis is nonsensical for many data sets.
#' @param theme list object; Themes are are an optional way of storing graphical preset options that are compatible with all nicePlot graphing functions.
#' @param width numeric; cex-like scaling factor controlling the width of the bars.
#' @param errorMultiple numeric; How many standard errors/deviations should be represented by the error bars.
#' @param errorBars Logical; Should error bars be drawn. Defaults to true but is ignored if \code{stack=\link{TRUE}}.
#' @param errorCap character; Determines the style for the ends of the error bars. Valid options are \code{ball}, \code{bar} or \code{none}.
#' @param errorLineType numeric; Sets \code{lty} line type for drawing the error bars.
#' @param capSize numeric; Controls the cex like scaling of the ball or width of the cap if they are drawn at the end of the error bars for the bar plot.
#' @param lWidth numeric; Line width (lwd) for drawing the bar plot.
#' @param add logical; causes plotting to be added to the existing plot rather the start a new one.
#' @param main character; title for the graph which is supplied to the \code{main} argument.
#' @param sub character; subtitle for the graph which is supplied to the \code{sub} argument. If \code{\link{NULL}} and \code{showCalc=\link{TRUE}} it will be used to display the output form \code{\link{calcStats}}.
#' @param ylab character; y-axis label.
#' @param outliers positive numeric; number of interquartile ranges (IQR) past the Q1 (25\%) and Q3 (75\%) cumulative distribution values. Outliers are often defined as \eqn{1.5 \times IQR}{1.5 * IQR} and extreme outliers are more than \eqn{3 \times IQR}{3 * IQR} away from the inner 50\% data range.
#' @param showCalc logical; if a p-value can be easily calculated for your data, it will be displayed using the \code{sub} annotation setting.
#' @param calcType character; should match one of 'none', 'wilcox', 'Tukey','t.test','anova' which will determine which, if any statistical test should be performed on the data.
#' @param flipFacts logical; When a dataframe of values is given, column names are used as a secondary grouping factor by default. Setting \code{flipFacts=\link{TRUE}} makes the column names the primary factor and \code{by} the secondary factor.
#' @param na.rm logical; Should \code{NA}s be removed from the data set? Both data input and the factor input from \code{by} with be checked.
#' @param verbose logical; Prints summary and p-value calculations to the screen. All data is silently by the function returned either way.
#' @param ... additional options for S3 method variants.
#' @examples
#' data(mtcars)
#' Groups<-data.frame(Cyl=factor(mtcars$cyl),Gear=factor(mtcars$gear))
#' niceBar(mtcars$mpg,by=Groups,subGroup=TRUE,yLim=c(0,45),main="MpG by Cylinders and Gear")
#'
#' @import tidyverse
#' @export
#' @seealso \code{\link[vioplot]{vioplot}}, \code{\link{boxplot}}, \code{\link{niceBox}}, \code{\link[beeswarm]{beeswarm}}, \code{\link{prepCategoryWindow}}
niceBar <- function(x, by=NULL, groupNames=NULL, aggFun=c("mean","median","none"),errFun=c("sd","se","range"), theme=basicTheme, stack=FALSE, main=NULL,sub=NULL, ylab=NULL, minorTick=FALSE, guides=TRUE, outliers=FALSE, width=1, errorMultiple=2, plotColors=list(bg="open",fill=setAlpha("grey",.8)), logScale=FALSE, trim=FALSE, axisText=c(NULL,NULL), showCalc=FALSE, calcType="none", yLim=NULL, rotateLabels=FALSE, rotateY=TRUE, add=FALSE, minorGuides=NULL, extendTicks=TRUE, subGroup=FALSE, subGroupLabels=NULL, expLabels=FALSE, sidePlot=FALSE, errorBars=TRUE, errorCap="ball", errorLineType=1,capSize=1.2, lWidth=1.5, na.rm=FALSE, flipFacts=FALSE, verbose=FALSE, ...) {UseMethod("niceBar",x)}

#' @import tidyverse
#' @export
niceBar.default <- function(x, by=NULL, groupNames=NULL, aggFun=c("mean","median","none"),errFun=c("sd","se","range"), theme=basicTheme, stack=FALSE, main=NULL,sub=NULL, ylab=NULL, minorTick=FALSE, guides=TRUE, outliers=FALSE, width=1, errorMultiple=2, plotColors=list(bg="open",fill=setAlpha("grey",.8)), logScale=FALSE, trim=FALSE, axisText=c(NULL,NULL), showCalc=FALSE, calcType="none", yLim=NULL, rotateLabels=FALSE, rotateY=TRUE, add=FALSE, minorGuides=NULL, extendTicks=TRUE, subGroup=FALSE, subGroupLabels=NULL, expLabels=FALSE, sidePlot=FALSE, errorBars=TRUE, errorCap="ball", errorLineType=1,capSize=1.2, lWidth=1.5, na.rm=FALSE, flipFacts=FALSE, verbose=FALSE, ...) {
  if(any(is.na(x))){warning("Warning: NAs detected in dataset")}
  prepedData<-NULL
  plotData<-NULL
  checked<-dataFlightCheck(x,by,na.rm=na.rm,flipFacts = flipFacts)
  x<-checked$d
  by<-checked$b
  rm(checked)
  #Here we check to see if the user specified any options so that they are left unaltered if present
  defaultFill<-FALSE
  defaultLines<-FALSE
  if(is.vector(plotColors,mode="list")){
    pcNames<-names(plotColors)
    if(!("lines" %in% pcNames)){defaultLines<-TRUE}
    if(!("fill" %in% pcNames)){defaultFill<-TRUE}
  }
  #Formating all options
  if(!is.list(theme)) {
    plotColors<-formatPlotColors(plotColors)
    if(is.null(minorTick)){minorTick<-FALSE}
    if(is.null(guides)){guides<-TRUE}
    if(is.null(width)){width<-1}
    if(is.null(lWidth)){lWidth<-1}
  } else {
    if(is.null(plotColors)){plotColors<-theme$plotColors}
    else (plotColors<-formatPlotColors(plotColors,theme$plotColors))
    if(is.null(minorTick)){
      if(logScale==FALSE){
        minorTick<-theme$minorTick
      } else {
        minorTick<-theme$minorTickLS
      }
    }
    if(is.null(guides)){guides<-theme$guides}
    if(is.null(width)){width<-theme$widthBar}
    if(is.null(lWidth)){lWidth<-theme$lWidthBar}
  }
  myLevels<-1
  #Calcuate the relevant factor levels formating the graph.
  if(is.data.frame(x)){
    myLevels<-dim(x)[2]
  } else if(subGroup==TRUE) {
    if(is.data.frame(by)){
      myLevels<-length(levels(factor(by[,2])))
    }
  } else if(is.data.frame(by)){
    myLevels<-length(levels(by[,1]))
  } else {
    myLevels<-length(levels(by))
  }
  #If left blank by the user, colors and shapes are adjust so that the repeat based on factor levels
  if(length(plotColors$fill)>1 & defaultFill==FALSE){plotColors$fill<-plotColors$fill[1:myLevels]}
  if(length(plotColors$lines)>1 & defaultLines==FALSE){plotColors$lines<-plotColors$lines[1:myLevels]}

  prepedData<-NULL
  if(is.data.frame(by)) {
    if(is.null(groupNames)){
      if(is.factor(by[,1])) {
        groupNames<-levels(by[,1])
      } else {
        groupNames<-levels(factor(by[,1]))
      }
    }
  } else {
    if(is.null(groupNames)) {
      if(is.factor(by)) {
        groupNames<-levels(by)
      } else {
        groupNames<-levels(factor(by))
      }
    }
  }
  if(add) {
    if(logScale>0) {
      prepedData<-list(data=log(x+1,logScale))
    } else {
      prepedData<-list(data=x)
    }
  } else {
    if(stack==T & is.null(yLim) & is.data.frame(by)){
      Stats<-c(0,0)
      if(is.data.frame(x)){
        if(ncol(by)>2 & subGroup==T){
          for(i in ncol(x)){
            cDat<-x[,i]
            cStats<-data.frame(cDat=cDat,fact=by[,1],subGroup=by[,2],Stack=by[,3]) %>%
              group_by(fact,subGroup,Stack) %>%
              summarize(Mean=mean(cDat),Median=median(cDat)) %>%
              ungroup() %>% group_by(fact, subGroup) %>%
              summarize(sumMean=sum(Mean),sumMedian=sum(Median)) %>%
              ungroup() %>% summarize(Min=min(sumMean,sumMedian),Max=max(sumMean,sumMedian))
            Stats[1]<-min(Stats[1],unlist(cStats[1,1]))
            Stats[2]<-max(Stats[2],unlist(cStats[1,2]))
          }
        } else if(ncol(by)>1 & subGroup==F) {
          for(i in ncol(x)){
            cDat<-x[,i]
            cStats<-data.frame(cDat=cDat,fact=by[,1],Stack=by[,2]) %>%
              group_by(fact,Stack) %>%
              summarize(Mean=mean(cDat),Median=median(cDat)) %>%
              ungroup() %>% group_by(fact) %>%
              summarize(sumMean=sum(Mean),sumMedian=sum(Median)) %>%
              ungroup() %>% summarize(Min=min(sumMean,sumMedian),Max=max(sumMean,sumMedian))
            Stats[1]<-min(Stats[1],unlist(cStats[1,1]))
            Stats[2]<-max(Stats[2],unlist(cStats[1,2]))
          }
        }
      } else {
        if(ncol(by)>2 & subGroup==T){
          cStats<-data.frame(cDat=x,fact=by[,1],subGroup=by[,2],Stack=by[,3]) %>%
            group_by(fact,subGroup,Stack) %>%
            summarize(Mean=mean(cDat),Median=median(cDat)) %>%
            ungroup() %>% group_by(fact, subGroup) %>%
            summarize(sumMean=sum(Mean),sumMedian=sum(Median)) %>%
            ungroup() %>% summarize(Min=min(c(sumMean,sumMedian)),Max=max(c(sumMean,sumMedian)))
          Stats[1]<-min(Stats[1],unlist(cStats[1,1]))
          Stats[2]<-max(Stats[2],unlist(cStats[1,2]))
        } else if(ncol(by)>1 & subGroup==F) {
           cStats<-data.frame(cDat=x,fact=by[,1],Stack=by[,2]) %>%
            group_by(fact,Stack) %>%
            summarize(Mean=mean(cDat),Median=median(cDat)) %>%
            ungroup() %>% group_by(fact) %>%
            summarize(sumMean=sum(Mean),sumMedian=sum(Median)) %>%
            ungroup() %>% summarize(Min=min(sumMean,sumMedian),Max=max(sumMean,sumMedian))
          Stats[1]<-min(Stats[1],unlist(cStats[1,1]))
          Stats[2]<-max(Stats[2],unlist(cStats[1,2]))
        }
      }
      #if(verbose){print(Stats)}
      if(Stats[1] != 0 | Stats[2] != 0) {
        if(logScale!=FALSE){
          if(Stats[1]>0){Stats[1]<-log(Stats[1]+1,logScale)}
          if(Stats[2]>0){Stats[2]<-log(Stats[2]+1,logScale)}
        }
        yLim<-c(Stats[1]*1.05,Stats[2]*1.05)
      }
    }
    if(is.null(minorGuides)){
      if(guides!=FALSE & logScale > 0) {
        minorGuides<-TRUE
      } else {
        minorGuides<-FALSE
      }
    }
    if(min(x)>=0){
      prepedData<-prepCategoryWindow(x,by=by, groupNames=groupNames, minorTick=minorTick, guides=guides, plotColors=plotColors, yLim=yLim, rotateLabels=rotateLabels, rotateY=rotateY, trim=trim, logScale=logScale, axisText=axisText, minorGuides=minorGuides, extendTicks=extendTicks, subGroup=subGroup, expLabels=expLabels,sidePlot=sidePlot,subGroupLabels=subGroupLabels,strictLimits=T)
    } else {
      prepedData<-prepCategoryWindow(x,by=by, groupNames=groupNames, minorTick=minorTick, guides=guides, plotColors=plotColors, yLim=yLim, rotateLabels=rotateLabels, rotateY=rotateY, trim=trim, logScale=logScale, axisText=axisText, minorGuides=minorGuides, extendTicks=extendTicks, subGroup=subGroup, expLabels=expLabels,sidePlot=sidePlot,subGroupLabels=subGroupLabels,strictLimits=F)
    }
  }
  pvalue<-NULL
  if(subGroup==TRUE){width<-width*2}
  filter<-rep(TRUE,length(x))
  if(trim>0){filter<-quantileTrim(x,trim,na.rm=T,returnFilter=T)[[2]]}
  bVal<-0
  if(!is.null(yLim)){bVal<-yLim[1]}
  if(is.numeric(prepedData[[1]])){
    #CASE: by is a factor data is a numeric vector
    if(is.factor(by)) {
      if(calcType[1]!="none"){pvalue<-calcStats(prepedData[[1]],by[filter],calcType[1],verbose=verbose)}
      plotLoc<-seq(1,length(groupNames),by=1)
      names(plotLoc)<-groupNames

      plotData<-bind_cols(data=prepedData[[1]],fact=by[filter]) %>%
        group_by(fact) %>%
          summarize(Mean=mean(data),Median=median(data),Max=max(data),Min=min(data),SD=sd(data)*errorMultiple,SE=errorMultiple*(sd(data)/(sqrt(length(data)))), N=n(),Q1=quantile(data,.25)) %>%
          bind_cols(at=plotLoc)
      printData<-plotData
      if(errorMultiple!=1){
        colnames(printData)[6]<-paste0("SDx",errorMultiple)
        colnames(printData)[7]<-paste0("SEx",errorMultiple)
      }
      width<-.25*width
      if(aggFun[1]=="mean") {
        if(errFun[1]=="sd"){
          if(verbose){print(select(printData,fact,N,Mean,starts_with("SD")))}
          plotData %>% mutate(yt=Mean,at=at,yb=bVal,UpperError=SD,LowerError=SD) %>%
            drawBar(plotColors=plotColors, errorBars=TRUE, errorCap=errorCap, errorLineType=errorLineType, width=width, sidePlot=sidePlot, stacked=stack, capSize=capSize, lineWidth=lWidth)
        } else if (errFun[1]=="se") {
          if(verbose){print(select(printData,fact,N,Mean,starts_with("SE")))}
          plotData %>% mutate(yt=Mean,at=at,yb=bVal,UpperError=SE,LowerError=SE) %>%
            drawBar(plotColors=plotColors, errorBars=TRUE, errorCap=errorCap, errorLineType=errorLineType, width=width, sidePlot=sidePlot, stacked=stack, capSize=capSize, lineWidth=lWidth)
        } else if (errFun[1]=="range") {
          if(verbose){print(select(printData,fact,N,Mean,Min,Max))}
          plotData %>% mutate(yt=Mean,at=at,yb=bVal,UpperError=Max-Mean,LowerError=Mean-Min) %>%
            drawBar(plotColors=plotColors, errorBars=TRUE, errorCap=errorCap, errorLineType=errorLineType, width=width, sidePlot=sidePlot, stacked=stack, capSize=capSize, lineWidth=lWidth)
        } else {
          if(verbose){print(select(printData,fact,N,Mean,starts_with("SE")))}
          plotData %>% mutate(yt=Mean,at=at,yb=bVal,UpperError=SE,LowerError=SE) %>%
            drawBar(plotColors=plotColors, errorBars=FALSE, errorCap=errorCap, errorLineType=errorLineType, width=width, sidePlot=sidePlot, stacked=stack, capSize=capSize, lineWidth=lWidth)
        }
      } else if(aggFun[1]=="median") {
        if(errFun[1]=="sd"){
          if(verbose){print(select(printData,fact,N,Median,starts_with("SD")))}
          plotData %>% mutate(yt=Median,at=at,yb=bVal,UpperError=SD,LowerError=SD) %>%
            drawBar(plotColors=plotColors, errorBars=TRUE, errorCap=errorCap, errorLineType=errorLineType, width=width, sidePlot=sidePlot, stacked=stack, capSize=capSize, lineWidth=lWidth)
        } else if (errFun[1]=="se") {
          if(verbose){print(select(printData,fact,N,Median,starts_with("SE")))}
          plotData %>% mutate(yt=Median,at=at,yb=bVal,UpperError=SE,LowerError=SE) %>%
            drawBar(plotColors=plotColors, errorBars=TRUE, errorCap=errorCap, errorLineType=errorLineType, width=width, sidePlot=sidePlot, stacked=stack, capSize=capSize, lineWidth=lWidth)
        } else if (errFun[1]=="range") {
          if(verbose){print(select(printData,fact,N,Median,Min,Max))}
          plotData %>% mutate(yt=Median,at=at,yb=bVal,UpperError=Max-Median,LowerError=Median-Min) %>%
            drawBar(plotColors=plotColors, errorBars=TRUE, errorCap=errorCap, errorLineType=errorLineType, width=width, sidePlot=sidePlot, stacked=stack, capSize=capSize, lineWidth=lWidth)
        } else {
          if(verbose){print(select(printData,fact,N,Median,Min,Max))}
          plotData %>% mutate(yt=Median,at=at,yb=bVal,UpperError=Max-Median,LowerError=Median-Min) %>%
            drawBar(plotColors=plotColors, errorBars=FALSE, errorCap=errorCap, errorLineType=errorLineType, width=width, sidePlot=sidePlot, stacked=stack, capSize=capSize, lineWidth=lWidth)
        }
      }
    } else {
      if(calcType[1]!="none"){pvalue<-calcStats(prepedData[[1]],by[filter,1],calcType[1],verbose=verbose)}
      #CASE: by is not a factor data is a numeric vector and subGroup is TRUE
      if(subGroup) {
        facetLoc<-facetSpacing(length(levels(by[,2])),length(groupNames))
        names(facetLoc)<-unlist(lapply(levels(by[,1]),FUN=function(x) paste(x,levels(by[,2]),sep=".")))
        if(stack==T & ncol(by)>2) {
          plotData<-bind_cols(data=prepedData[[1]],fact=by[filter,1],subGroup=by[filter,2],Stack=by[filter,3]) %>%
            group_by(fact,subGroup,Stack) %>%
            summarize(Mean=mean(data),Median=median(data),Max=max(data),Min=min(data),SD=sd(data)*errorMultiple,SE=errorMultiple*(sd(data)/(sqrt(length(data)))), N=n(),Q1=quantile(data,.25)) %>%
            mutate(facetLevel=paste(fact,subGroup,sep="."),at=facetLoc[facetLevel]) %>%
            ungroup()
        } else {
          plotData<-bind_cols(data=prepedData[[1]],fact=by[filter,1],subGroup=by[filter,2]) %>%
            group_by(fact,subGroup) %>%
            summarize(Mean=mean(data),Median=median(data),Max=max(data),Min=min(data),SD=sd(data)*errorMultiple,SE=errorMultiple*(sd(data)/(sqrt(length(data)))), N=n(),Q1=quantile(data,.25)) %>%
            mutate(facetLevel=paste(fact,subGroup,sep="."),at=facetLoc[facetLevel]) %>%
            ungroup()
        }
        printData<-plotData
        if(errorMultiple!=1){
          if(stack & ncol(by)>2) {
            colnames(printData)[8]<-paste0("SDx",errorMultiple)
            colnames(printData)[9]<-paste0("SEx",errorMultiple)
          } else {
            colnames(printData)[7]<-paste0("SDx",errorMultiple)
            colnames(printData)[8]<-paste0("SEx",errorMultiple)
          }
        }
        width<-width*(facetLoc[2]-facetLoc[1])/4
        if(stack==T & ncol(by)>2){
          plotData<-plotData %>% mutate(fact=Stack)
          printData<-printData %>% mutate(Group=paste(fact,subGroup,Stack,sep="."))
        } else {
          printData<-printData %>% mutate(Group=facetLevel)
        }
        if(aggFun[1]=="mean") {
          if(errFun[1]=="sd"){
            if(verbose){print(select(printData,Group,N,Mean,starts_with("SD")))}
            plotData %>% mutate(yt=Mean,yb=bVal,UpperError=SD,LowerError=SD) %>%
              drawBar(plotColors=plotColors, errorBars=TRUE, errorCap=errorCap, errorLineType=errorLineType, width=width, sidePlot=sidePlot, stacked=stack, capSize=capSize, lineWidth=lWidth)
          } else if (errFun[1]=="se") {
            if(verbose){print(select(printData,Group,N,Mean,starts_with("SE")))}
            plotData %>% mutate(yt=Mean,at=at,yb=bVal,UpperError=SE,LowerError=SE) %>%
              drawBar(plotColors=plotColors, errorBars=TRUE, errorCap=errorCap, errorLineType=errorLineType, width=width, sidePlot=sidePlot, stacked=stack, capSize=capSize, lineWidth=lWidth)
          } else if (errFun[1]=="range") {
            if(Verbose){print(select(printData,Group,N,Mean,Min,Max))}
            plotData %>% mutate(yt=Mean,at=at,yb=bVal,UpperError=Max-Mean,LowerError=Mean-Min) %>%
              drawBar(plotColors=plotColors, errorBars=TRUE, errorCap=errorCap, errorLineType=errorLineType, width=width, sidePlot=sidePlot, stacked=stack, capSize=capSize, lineWidth=lWidth)
          } else {
            if(verbose){print(select(printData,Group,N,Mean,starts_with("SE")))}
            plotData %>% mutate(yt=Mean,at=at,yb=bVal,UpperError=SE,LowerError=SE) %>%
              drawBar(plotColors=plotColors, errorBars=FALSE, errorCap=errorCap, errorLineType=errorLineType, width=width, sidePlot=sidePlot, stacked=stack, capSize=capSize, lineWidth=lWidth)
          }
        } else if(aggFun[1]=="median") {
          if(errFun[1]=="sd"){
            if(verbose){print(select(printData,Group,N,Median,starts_with("SD")))}
            plotData %>% mutate(yt=Median,at=at,yb=bVal,UpperError=SD,LowerError=SD) %>%
              drawBar(plotColors=plotColors, errorBars=TRUE, errorCap=errorCap, errorLineType=errorLineType, width=width, sidePlot=sidePlot, stacked=stack, capSize=capSize, lineWidth=lWidth)
          } else if (errFun[1]=="se") {
            if(Verbose){print(select(printData,Group,N,Median,starts_with("SE")))}
            plotData %>% mutate(yt=Median,at=at,yb=bVal,UpperError=SE,LowerError=SE) %>%
              drawBar(plotColors=plotColors, errorBars=TRUE, errorCap=errorCap, errorLineType=errorLineType, width=width, sidePlot=sidePlot, stacked=stack, capSize=capSize, lineWidth=lWidth)
          } else if (errFun[1]=="range") {
            if(verbose){print(select(printData,Group,N,Median,Min,Max))}
            plotData %>% mutate(yt=Median,at=at,yb=bVal,UpperError=Max-Median,LowerError=Median-Min) %>%
              drawBar(plotColors=plotColors, errorBars=TRUE, errorCap=errorCap, errorLineType=errorLineType, width=width, sidePlot=sidePlot, stacked=stack, capSize=capSize, lineWidth=lWidth)
          } else {
            if(verbose){print(select(printData,Group,N,Median,Min,Max))}
            plotData %>% mutate(yt=Median,at=at,yb=bVal,UpperError=Max-Median,LowerError=Median-Min) %>%
              drawBar(plotColors=plotColors, errorBars=FALSE, errorCap=errorCap, errorLineType=errorLineType, width=width, sidePlot=sidePlot, stacked=stack, capSize=capSize, lineWidth=lWidth)
          }
        }
      } else {
        #CASE: by is not a factor, data is a numeric vector and subGroup is FALSE
        facetLoc<-seq(1,length(groupNames))
        names(facetLoc)<-groupNames
        if(stack==T & ncol(by)>1) {
          plotData<-bind_cols(data=prepedData[[1]],fact=by[filter,1],Stack=by[filter,2]) %>%
            group_by(fact,Stack) %>%
            summarize(Mean=mean(data),Median=median(data),Max=max(data),Min=min(data),SD=sd(data)*errorMultiple,SE=errorMultiple*(sd(data)/(sqrt(length(data)))),N=n(),Q1=quantile(data,.25)) %>%
            mutate(facetLevel=fact,at=facetLoc[facetLevel]) %>%
            ungroup()
        } else {
          plotData<-bind_cols(data=prepedData[[1]],fact=by[filter,1]) %>%
            group_by(fact) %>%
            summarize(Mean=mean(data),Median=median(data),Max=max(data),Min=min(data),SD=sd(data)*errorMultiple,SE=errorMultiple*(sd(data)/(sqrt(length(data)))),N=n(),Q1=quantile(data,.25)) %>%
            mutate(facetLevel=fact,at=facetLoc[facetLevel]) %>%
            ungroup()
        }
        printData<-plotData
        if(errorMultiple!=1){
          if(stack & ncol(by)>1) {
            colnames(printData)[7]<-paste0("SDx",errorMultiple)
            colnames(printData)[8]<-paste0("SEx",errorMultiple)
          } else {
            colnames(printData)[6]<-paste0("SDx",errorMultiple)
            colnames(printData)[7]<-paste0("SEx",errorMultiple)
          }
        }
        width<-width*(facetLoc[2]-facetLoc[1])/4
        if(stack==T & ncol(by)>1){
          plotData<-plotData %>% mutate(fact=Stack)
          printData<-printData %>% mutate(Group=paste(fact,Stack,sep="."))
        } else {
          printData<-printData %>% mutate(Group=facetLevel)
        }
        if(aggFun[1]=="mean") {
          if(errFun[1]=="sd"){
            if(verbose){print(select(printData,Group,N,Mean,starts_with("SD")))}
            plotData %>% mutate(yt=Mean,yb=bVal,UpperError=SD,LowerError=SD) %>%
              drawBar(plotColors=plotColors, errorBars=TRUE, errorCap=errorCap, errorLineType=errorLineType, width=width, sidePlot=sidePlot, stacked=stack, capSize=capSize, lineWidth=lWidth)
          } else if (errFun[1]=="se") {
            if(verbose){print(select(printData,Group,N,Mean,starts_with("SE")))}
            plotData %>% mutate(yt=Mean,at=at,yb=bVal,UpperError=SE,LowerError=SE) %>%
              drawBar(plotColors=plotColors, errorBars=TRUE, errorCap=errorCap, errorLineType=errorLineType, width=width, sidePlot=sidePlot, stacked=stack, capSize=capSize, lineWidth=lWidth)
          } else if (errFun[1]=="range") {
            if(verbose){print(select(printData,Group,N,Mean,Min,Max))}
            plotData %>% mutate(yt=Mean,at=at,yb=bVal,UpperError=Max-Mean,LowerError=Mean-Min) %>%
              drawBar(plotColors=plotColors, errorBars=TRUE, errorCap=errorCap, errorLineType=errorLineType, width=width, sidePlot=sidePlot, stacked=stack, capSize=capSize, lineWidth=lWidth)
          } else {
            if(verbose){print(select(printData,Group,N,Mean,starts_with("SE")))}
            plotData %>% mutate(yt=Mean,at=at,yb=bVal,UpperError=SE,LowerError=SE) %>%
              drawBar(plotColors=plotColors, errorBars=FALSE, errorCap=errorCap, errorLineType=errorLineType, width=width, sidePlot=sidePlot, stacked=stack, capSize=capSize, lineWidth=lWidth)
          }
        } else if(aggFun[1]=="median") {
          if(errFun[1]=="sd"){
            if(verbose){print(select(printData,Group,N,Median,starts_with("SD")))}
            plotData %>% mutate(yt=Median,at=at,yb=bVal,UpperError=SD,LowerError=SD) %>%
              drawBar(plotColors=plotColors, errorBars=TRUE, errorCap=errorCap, errorLineType=errorLineType, width=width, sidePlot=sidePlot, stacked=stack, capSize=capSize, lineWidth=lWidth)
          } else if (errFun[1]=="se") {
            if(verbose){print(select(printData,Group,N,Median,starts_with("SE")))}
            plotData %>% mutate(yt=Median,at=at,yb=bVal,UpperError=SE,LowerError=SE) %>%
              drawBar(plotColors=plotColors, errorBars=TRUE, errorCap=errorCap, errorLineType=errorLineType, width=width, sidePlot=sidePlot, stacked=stack, capSize=capSize, lineWidth=lWidth)
          } else if (errFun[1]=="range") {
            if(verbose){print(select(printData,Group,N,Median,Min,Max))}
            plotData %>% mutate(yt=Median,at=at,yb=bVal,UpperError=Max-Median,LowerError=Median-Min) %>%
              drawBar(plotColors=plotColors, errorBars=TRUE, errorCap=errorCap, errorLineType=errorLineType, width=width, sidePlot=sidePlot, stacked=stack, capSize=capSize, lineWidth=lWidth)
          } else {
            if(verbose){print(select(printData,Group,N,Median,Min,Max))}
            plotData %>% mutate(yt=Median,at=at,yb=bVal,UpperError=Max-Median,LowerError=Median-Min) %>%
              drawBar(plotColors=plotColors, errorBars=FALSE, errorCap=errorCap, errorLineType=errorLineType, width=width, sidePlot=sidePlot, stacked=stack, capSize=capSize, lineWidth=lWidth)
          }
        }
      }
    }
  } else {
    #CASE: data is a dataframe, by is a factor, subGroup is ignored, Stack is ignored
    if(is.factor(by)) {
      facetLoc<-facetSpacing(length(prepedData[[1]]),length(levels(by)))
      names(facetLoc)<-unlist(lapply(levels(by),FUN=function(x) paste(x,colnames(prepedData[[1]]),sep=".")))
      plotData<-bind_cols(data=prepedData[[1]],fact=by[filter]) %>%
        gather(key=subGroup,value=data,-fact) %>%
        group_by(fact,subGroup) %>%
        summarize(Mean=mean(data),Median=median(data),Max=max(data),Min=min(data),SD=sd(data)*errorMultiple,SE=errorMultiple*(sd(data)/(sqrt(length(data)))), N=n(),Q1=quantile(data,.25)) %>%
        mutate(facetLevel=paste(fact,subGroup,sep="."),at=facetLoc[facetLevel]) %>%
        ungroup()
      printData<-plotData
      if(errorMultiple!=1){
        colnames(printData)[7]<-paste0("SDx",errorMultiple)
        colnames(printData)[8]<-paste0("SEx",errorMultiple)
      }
      width<-width*(facetLoc[2]-facetLoc[1])/4
      printData<-printData %>% mutate(Group=facetLevel)
      #print(plotData)
      if(aggFun[1]=="mean") {
        if(errFun[1]=="sd"){
          if(verbose){print(select(printData,Group,N,Mean,starts_with("SD")))}
          plotData %>% mutate(yt=Mean,yb=bVal,UpperError=SD,LowerError=SD) %>%
            drawBar(plotColors=plotColors, errorBars=TRUE, errorCap=errorCap, errorLineType=errorLineType, width=width, sidePlot=sidePlot, stacked=stack, capSize=capSize, lineWidth=lWidth)
        } else if (errFun[1]=="se") {
          if(verbose){print(select(printData,Group,N,Mean,starts_with("SE")))}
          plotData %>% mutate(yt=Mean,at=at,yb=bVal,UpperError=SE,LowerError=SE) %>%
            drawBar(plotColors=plotColors, errorBars=TRUE, errorCap=errorCap, errorLineType=errorLineType, width=width, sidePlot=sidePlot, stacked=stack, capSize=capSize, lineWidth=lWidth)
        } else if (errFun[1]=="range") {
          if(verbose){print(select(printData,Group,N,Mean,Min,Max))}
          plotData %>% mutate(yt=Mean,at=at,yb=bVal,UpperError=Max-Mean,LowerError=Mean-Min) %>%
            drawBar(plotColors=plotColors, errorBars=TRUE, errorCap=errorCap, errorLineType=errorLineType, width=width, sidePlot=sidePlot, stacked=stack, capSize=capSize, lineWidth=lWidth)
        } else {
          if(verbose){print(select(printData,Group,N,Mean,starts_with("SE")))}
          plotData %>% mutate(yt=Mean,at=at,yb=bVal,UpperError=SE,LowerError=SE) %>%
            drawBar(plotColors=plotColors, errorBars=FALSE, errorCap=errorCap, errorLineType=errorLineType, width=width, sidePlot=sidePlot, stacked=stack, capSize=capSize, lineWidth=lWidth)
        }
      } else if(aggFun[1]=="median") {
        if(errFun[1]=="sd"){
          if(verbose){print(select(printData,Group,N,Median,starts_with("SD")))}
          plotData %>% mutate(yt=Median,at=at,yb=bVal,UpperError=SD,LowerError=SD) %>%
            drawBar(plotColors=plotColors, errorBars=TRUE, errorCap=errorCap, errorLineType=errorLineType, width=width, sidePlot=sidePlot, stacked=stack, capSize=capSize, lineWidth=lWidth)
        } else if (errFun[1]=="se") {
          if(verbose){print(select(printData,Group,N,Median,starts_with("SE")))}
          plotData %>% mutate(yt=Median,at=at,yb=bVal,UpperError=SE,LowerError=SE) %>%
            drawBar(plotColors=plotColors, errorBars=TRUE, errorCap=errorCap, errorLineType=errorLineType, width=width, sidePlot=sidePlot, stacked=stack, capSize=capSize, lineWidth=lWidth)
        } else if (errFun[1]=="range") {
          if(verbose){print(select(printData,Group,N,Median,Min,Max))}
          plotData %>% mutate(yt=Median,at=at,yb=bVal,UpperError=Max-Median,LowerError=Median-Min) %>%
            drawBar(plotColors=plotColors, errorBars=TRUE, errorCap=errorCap, errorLineType=errorLineType, width=width, sidePlot=sidePlot, stacked=stack, capSize=capSize, lineWidth=lWidth)
        } else {
          if(verbose){print(select(printData,Group,N,Median,Min,Max))}
          plotData %>% mutate(yt=Median,at=at,yb=bVal,UpperError=Max-Median,LowerError=Median-Min) %>%
            drawBar(plotColors=plotColors, errorBars=FALSE, errorCap=errorCap, errorLineType=errorLineType, width=width, sidePlot=sidePlot, stacked=stack, capSize=capSize, lineWidth=lWidth)
        }
      }
    } else {
      #CASE: data is a dataframe, by is a dataframe, subGroup is ignored
      facetLoc<-facetSpacing(length(prepedData[[1]]),length(levels(by[,1])))
      names(facetLoc)<-unlist(lapply(levels(by[,1]),FUN=function(x) paste(x,colnames(prepedData[[1]]),sep=".")))
      if(stack==T & ncol(by)>1) {
        plotData<-bind_cols(data=prepedData[[1]],fact=by[filter,1],Stack=by[filter,2]) %>%
          gather(key=subGroup,value=data,-fact,-Stack) %>%
          group_by(fact,subGroup,Stack) %>%
          summarize(Mean=mean(data),Median=median(data),Max=max(data),Min=min(data),SD=sd(data)*errorMultiple,SE=errorMultiple*(sd(data)/(sqrt(length(data)))), N=n(),Q1=quantile(data,.25)) %>%
          mutate(facetLevel=paste(fact,subGroup,sep="."),at=facetLoc[facetLevel]) %>%
          ungroup()
      } else {
        plotData<-bind_cols(data=prepedData[[1]],fact=by[filter,1]) %>%
          gather(key=subGroup,value=data,-fact) %>%
          group_by(fact,subGroup) %>%
          summarize(Mean=mean(data),Median=median(data),Max=max(data),Min=min(data),SD=sd(data)*errorMultiple,SE=errorMultiple*(sd(data)/(sqrt(length(data)))), N=n(),Q1=quantile(data,.25)) %>%
          mutate(facetLevel=paste(fact,subGroup,sep="."),at=facetLoc[facetLevel]) %>%
          ungroup()
      }
      printData<-plotData
      if(errorMultiple!=1){
        if(stack & ncol(by)>1) {
          colnames(printData)[8]<-paste0("SDx",errorMultiple)
          colnames(printData)[9]<-paste0("SEx",errorMultiple)
        } else {
          colnames(printData)[7]<-paste0("SDx",errorMultiple)
          colnames(printData)[8]<-paste0("SEx",errorMultiple)
        }
      }
      width<-width*(facetLoc[2]-facetLoc[1])/4
      if(stack==T & ncol(by)>1){
        plotData<-plotData %>% mutate(fact=Stack)
        printData<-printData %>% mutate(Group=paste(fact,subGroup,Stack,sep="."))
      } else {
        printData<-printData %>% mutate(Group=facetLevel)
      }
      if(aggFun[1]=="mean") {
        if(errFun[1]=="sd"){
          if(verbose){print(select(printData,Group,N,Mean,starts_with("SD")))}
          plotData %>% mutate(yt=Mean,yb=bVal,UpperError=SD,LowerError=SD) %>%
            drawBar(plotColors=plotColors, errorBars=TRUE, errorCap=errorCap, errorLineType=errorLineType, width=width, sidePlot=sidePlot, stacked=stack, capSize=capSize, lineWidth=lWidth)
        } else if (errFun[1]=="se") {
          if(verbose){print(select(printData,Group,N,Mean,starts_with("SE")))}
          plotData %>% mutate(yt=Mean,at=at,yb=bVal,UpperError=SE,LowerError=SE) %>%
            drawBar(plotColors=plotColors, errorBars=TRUE, errorCap=errorCap, errorLineType=errorLineType, width=width, sidePlot=sidePlot, stacked=stack, capSize=capSize, lineWidth=lWidth)
        } else if (errFun[1]=="range") {
          if(verbose){print(select(printData,Group,N,Mean,Min,Max))}
          plotData %>% mutate(yt=Mean,at=at,yb=bVal,UpperError=Max-Mean,LowerError=Mean-Min) %>%
            drawBar(plotColors=plotColors, errorBars=TRUE, errorCap=errorCap, errorLineType=errorLineType, width=width, sidePlot=sidePlot, stacked=stack, capSize=capSize, lineWidth=lWidth)
        } else {
          if(verbose){print(select(printData,Group,N,Mean,starts_with("SE")))}
          plotData %>% mutate(yt=Mean,at=at,yb=bVal,UpperError=SE,LowerError=SE) %>%
            drawBar(plotColors=plotColors, errorBars=FALSE, errorCap=errorCap, errorLineType=errorLineType, width=width, sidePlot=sidePlot, stacked=stack, capSize=capSize, lineWidth=lWidth)
        }
      } else if(aggFun[1]=="median") {
        if(errFun[1]=="sd"){
          if(verbose){print(select(printData,Group,N,Median,starts_with("SD")))}
          plotData %>% mutate(yt=Median,at=at,yb=bVal,UpperError=SD,LowerError=SD) %>%
            drawBar(plotColors=plotColors, errorBars=TRUE, errorCap=errorCap, errorLineType=errorLineType, width=width, sidePlot=sidePlot, stacked=stack, capSize=capSize, lineWidth=lWidth)
        } else if (errFun[1]=="se") {
          if(verbose){print(select(printData,Group,N,Median,starts_with("SE")))}
          plotData %>% mutate(yt=Median,at=at,yb=bVal,UpperError=SE,LowerError=SE) %>%
            drawBar(plotColors=plotColors, errorBars=TRUE, errorCap=errorCap, errorLineType=errorLineType, width=width, sidePlot=sidePlot, stacked=stack, capSize=capSize, lineWidth=lWidth)
        } else if (errFun[1]=="range") {
          if(verbose){print(select(printData,Group,N,Median,Min,Max))}
          plotData %>% mutate(yt=Median,at=at,yb=bVal,UpperError=Max-Median,LowerError=Median-Min) %>%
            drawBar(plotColors=plotColors, errorBars=TRUE, errorCap=errorCap, errorLineType=errorLineType, width=width, sidePlot=sidePlot, stacked=stack, capSize=capSize, lineWidth=lWidth)
        } else {
          if(verbose){print(select(printData,Group,N,Median,Min,Max))}
          plotData %>% mutate(yt=Median,at=at,yb=bVal,UpperError=Max-Median,LowerError=Median-Min) %>%
            drawBar(plotColors=plotColors, errorBars=FALSE, errorCap=errorCap, errorLineType=errorLineType, width=width, sidePlot=sidePlot, stacked=stack, capSize=capSize, lineWidth=lWidth)
        }
      }
    }
  }
  if(add==FALSE) {
    if(is.null(sub) & showCalc==T & is.null(pvalue)==FALSE){
      sub<-pvalue
    }
    if(sidePlot) {
      title(main=main,xlab=ylab,sub=sub)
    } else {
      title(main=main,sub=sub,ylab=ylab)
    }
  }
  dataOut<-list(data=data.frame(prepedData$data,by),summary=plotData,stats=pvalue)
  invisible(dataOut)
}
