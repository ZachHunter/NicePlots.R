#' @include np_options_processing.R niceThemes.R np_categorical_plot_setup.R np_plotting_functions.R np_utility.R np_data_processing.R
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
#' @param capWidth numeric; Controls the cex like scaling of the ball or width of the cap if they are drawn at the end of the error bars for the bar plot.
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
#' @param legend logical/character; if not equal to \code{\link{FALSE}} with cause a legend to be drawn in the margins. If set to a character string instead of a logical value, the string will be used as the legend title insteas of the factor column name from \code{by}.
#' @param verbose logical; Prints summary and p-value calculations to the screen. All data is silently by the function returned either way.
#' @param ... additional options for S3 method variants.
#' @examples
#' data(mtcars)
#' Groups<-data.frame(Cyl=factor(mtcars$cyl),Gear=factor(mtcars$gear))
#' niceBar(mtcars$mpg,by=Groups,subGroup=TRUE,yLim=c(0,45),main="MpG by Cylinders and Gear")
#'
#' @import dplyr
#' @import tidyr
#' @export
#' @seealso \code{\link[vioplot]{vioplot}}, \code{\link{boxplot}}, \code{\link{niceBox}}, \code{\link[beeswarm]{beeswarm}}, \code{\link{prepCategoryWindow}}
niceBar <- function(x, by=NULL, groupNames=NULL, aggFun=c("mean","median","none"),errFun=c("sd","se","range"), theme=basicTheme, legend=FALSE, stack=FALSE, main=NULL,sub=NULL, ylab=NULL, minorTick=FALSE, guides=TRUE, outliers=FALSE, width=NULL, errorMultiple=2, plotColors=list(bg="open",fill=setAlpha("grey",.8)), logScale=FALSE, trim=FALSE, axisText=c(NULL,NULL), showCalc=FALSE, calcType="none", yLim=NULL, rotateLabels=FALSE, rotateY=TRUE, add=FALSE, minorGuides=NULL, extendTicks=TRUE, subGroup=FALSE, subGroupLabels=NULL, expLabels=FALSE, sidePlot=FALSE, errorBars=TRUE, errorCap="ball", errorLineType=1,capWidth=1.2, lWidth=1.5, na.rm=FALSE, flipFacts=FALSE, verbose=FALSE, ...) {UseMethod("niceBar",x)}

#' @import dplyr
#' @import tidyr
#' @export
niceBar.default <- function(x, by=NULL, groupNames=NULL, aggFun=c("mean","median","none"),errFun=c("sd","se","range"), theme=basicTheme, legend=FALSE, stack=FALSE, main=NULL,sub=NULL, ylab=NULL, minorTick=FALSE, guides=NULL, outliers=FALSE, width=NULL, errorMultiple=2, plotColors=NULL, logScale=FALSE, trim=FALSE, axisText=c(NULL,NULL), showCalc=FALSE, calcType="none", yLim=NULL, rotateLabels=FALSE, rotateY=TRUE, add=FALSE, minorGuides=NULL, extendTicks=TRUE, subGroup=FALSE, subGroupLabels=NULL, expLabels=FALSE, sidePlot=FALSE, errorBars=TRUE, errorCap=NULL, errorLineType=NULL,capWidth=NULL, lWidth=NULL, na.rm=FALSE, flipFacts=FALSE, verbose=FALSE, ...) {
  if(any(is.na(x))){warning("Warning: NAs detected in dataset")}
  prepedData<-NULL
  plotData<-NULL
  lWidth<-NULL
  whiskerLineType<-NULL
  capWidth<-NULL
  checked<-dataFlightCheck(x,by,na.rm=na.rm,flipFacts = flipFacts)
  x<-checked$d
  by<-checked$b
  rm(checked)

  #Here we check to see if the user specified any options so that they are left unaltered if present
  finalOptions<-procNiceOptions(x=x,by=by,minorTick=minorTick,pointShape=1,whiskerLineType=errorLineType,lWidth=lWidth,capWidth=capWidth,pointLaneWidth=1,width=width,guides=guides,pointSize=1,subGroup=subGroup,stack=stack,pointHighlights=FALSE,type="Bar",theme=theme,plotColors=plotColors,logScale=logScale,pointMethod="jitter",drawPoints=FALSE,groupNames=groupNames,swarmOverflow="random" ,errorCap=errorCap)
  minorTick<-finalOptions$minorTick
  pointShape<-finalOptions$pointShape
  errorLineType<-finalOptions$whiskerLineType
  lWidth<-finalOptions$lWidth
  capWidth<-finalOptions$capWidth
  width<-finalOptions$width
  guides<-finalOptions$guides
  theme<-finalOptions$theme
  plotColors<-finalOptions$plotColors
  groupNames<-finalOptions$groupNames
  errorCap<-finalOptions$errorCap

  #Capturing default group names
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
      prepedData<-prepCategoryWindow(x,by=by, groupNames=groupNames, minorTick=minorTick, guides=guides, plotColors=plotColors, yLim=yLim, rotateLabels=rotateLabels, rotateY=rotateY, trim=trim, logScale=logScale, axisText=axisText, minorGuides=minorGuides, extendTicks=extendTicks, subGroup=subGroup, expLabels=expLabels,sidePlot=sidePlot,subGroupLabels=subGroupLabels,strictLimits=T,theme=theme,legend=legend)
    } else {
      prepedData<-prepCategoryWindow(x,by=by, groupNames=groupNames, minorTick=minorTick, guides=guides, plotColors=plotColors, yLim=yLim, rotateLabels=rotateLabels, rotateY=rotateY, trim=trim, logScale=logScale, axisText=axisText, minorGuides=minorGuides, extendTicks=extendTicks, subGroup=subGroup, expLabels=expLabels,sidePlot=sidePlot,subGroupLabels=subGroupLabels,strictLimits=F)
    }
  }
  pvalue<-NULL
  if(subGroup==TRUE){width<-width*2}
  #Initialize legend variables so we can update based on options
  legendTitle<-"Legend"
  legendLabels<-NULL
  legendColors<-plotColors$fill

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
      legend<-FALSE
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
            drawBar(plotColors=plotColors, errorBars=TRUE, errorCap=errorCap, errorLineType=errorLineType, width=width, sidePlot=sidePlot, stacked=stack, capSize=capWidth, lineWidth=lWidth)
        } else if (errFun[1]=="se") {
          if(verbose){print(select(printData,fact,N,Mean,starts_with("SE")))}
          plotData %>% mutate(yt=Mean,at=at,yb=bVal,UpperError=SE,LowerError=SE) %>%
            drawBar(plotColors=plotColors, errorBars=TRUE, errorCap=errorCap, errorLineType=errorLineType, width=width, sidePlot=sidePlot, stacked=stack, capSize=capWidth, lineWidth=lWidth)
        } else if (errFun[1]=="range") {
          if(verbose){print(select(printData,fact,N,Mean,Min,Max))}
          plotData %>% mutate(yt=Mean,at=at,yb=bVal,UpperError=Max-Mean,LowerError=Mean-Min) %>%
            drawBar(plotColors=plotColors, errorBars=TRUE, errorCap=errorCap, errorLineType=errorLineType, width=width, sidePlot=sidePlot, stacked=stack, capSize=capWidth, lineWidth=lWidth)
        } else {
          if(verbose){print(select(printData,fact,N,Mean,starts_with("SE")))}
          plotData %>% mutate(yt=Mean,at=at,yb=bVal,UpperError=SE,LowerError=SE) %>%
            drawBar(plotColors=plotColors, errorBars=FALSE, errorCap=errorCap, errorLineType=errorLineType, width=width, sidePlot=sidePlot, stacked=stack, capSize=capWidth, lineWidth=lWidth)
        }
      } else if(aggFun[1]=="median") {
        if(errFun[1]=="sd"){
          if(verbose){print(select(printData,fact,N,Median,starts_with("SD")))}
          plotData %>% mutate(yt=Median,at=at,yb=bVal,UpperError=SD,LowerError=SD) %>%
            drawBar(plotColors=plotColors, errorBars=TRUE, errorCap=errorCap, errorLineType=errorLineType, width=width, sidePlot=sidePlot, stacked=stack, capSize=capWidth, lineWidth=lWidth)
        } else if (errFun[1]=="se") {
          if(verbose){print(select(printData,fact,N,Median,starts_with("SE")))}
          plotData %>% mutate(yt=Median,at=at,yb=bVal,UpperError=SE,LowerError=SE) %>%
            drawBar(plotColors=plotColors, errorBars=TRUE, errorCap=errorCap, errorLineType=errorLineType, width=width, sidePlot=sidePlot, stacked=stack, capSize=capWidth, lineWidth=lWidth)
        } else if (errFun[1]=="range") {
          if(verbose){print(select(printData,fact,N,Median,Min,Max))}
          plotData %>% mutate(yt=Median,at=at,yb=bVal,UpperError=Max-Median,LowerError=Median-Min) %>%
            drawBar(plotColors=plotColors, errorBars=TRUE, errorCap=errorCap, errorLineType=errorLineType, width=width, sidePlot=sidePlot, stacked=stack, capSize=capWidth, lineWidth=lWidth)
        } else {
          if(verbose){print(select(printData,fact,N,Median,Min,Max))}
          plotData %>% mutate(yt=Median,at=at,yb=bVal,UpperError=Max-Median,LowerError=Median-Min) %>%
            drawBar(plotColors=plotColors, errorBars=FALSE, errorCap=errorCap, errorLineType=errorLineType, width=width, sidePlot=sidePlot, stacked=stack, capSize=capWidth, lineWidth=lWidth)
        }
      }
      if(legend!=FALSE) {
        if(stack==TRUE){
          if(legend==TRUE){
            legendTitle<-colnames(by)[2]
          }
          legendLabels<-levels(by[,2])
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
              drawBar(plotColors=plotColors, errorBars=TRUE, errorCap=errorCap, errorLineType=errorLineType, width=width, sidePlot=sidePlot, stacked=stack, capSize=capWidth, lineWidth=lWidth)
          } else if (errFun[1]=="se") {
            if(verbose){print(select(printData,Group,N,Mean,starts_with("SE")))}
            plotData %>% mutate(yt=Mean,at=at,yb=bVal,UpperError=SE,LowerError=SE) %>%
              drawBar(plotColors=plotColors, errorBars=TRUE, errorCap=errorCap, errorLineType=errorLineType, width=width, sidePlot=sidePlot, stacked=stack, capSize=capWidth, lineWidth=lWidth)
          } else if (errFun[1]=="range") {
            if(Verbose){print(select(printData,Group,N,Mean,Min,Max))}
            plotData %>% mutate(yt=Mean,at=at,yb=bVal,UpperError=Max-Mean,LowerError=Mean-Min) %>%
              drawBar(plotColors=plotColors, errorBars=TRUE, errorCap=errorCap, errorLineType=errorLineType, width=width, sidePlot=sidePlot, stacked=stack, capSize=capWidth, lineWidth=lWidth)
          } else {
            if(verbose){print(select(printData,Group,N,Mean,starts_with("SE")))}
            plotData %>% mutate(yt=Mean,at=at,yb=bVal,UpperError=SE,LowerError=SE) %>%
              drawBar(plotColors=plotColors, errorBars=FALSE, errorCap=errorCap, errorLineType=errorLineType, width=width, sidePlot=sidePlot, stacked=stack, capSize=capWidth, lineWidth=lWidth)
          }
        } else if(aggFun[1]=="median") {
          if(errFun[1]=="sd"){
            if(verbose){print(select(printData,Group,N,Median,starts_with("SD")))}
            plotData %>% mutate(yt=Median,at=at,yb=bVal,UpperError=SD,LowerError=SD) %>%
              drawBar(plotColors=plotColors, errorBars=TRUE, errorCap=errorCap, errorLineType=errorLineType, width=width, sidePlot=sidePlot, stacked=stack, capSize=capWidth, lineWidth=lWidth)
          } else if (errFun[1]=="se") {
            if(Verbose){print(select(printData,Group,N,Median,starts_with("SE")))}
            plotData %>% mutate(yt=Median,at=at,yb=bVal,UpperError=SE,LowerError=SE) %>%
              drawBar(plotColors=plotColors, errorBars=TRUE, errorCap=errorCap, errorLineType=errorLineType, width=width, sidePlot=sidePlot, stacked=stack, capSize=capWidth, lineWidth=lWidth)
          } else if (errFun[1]=="range") {
            if(verbose){print(select(printData,Group,N,Median,Min,Max))}
            plotData %>% mutate(yt=Median,at=at,yb=bVal,UpperError=Max-Median,LowerError=Median-Min) %>%
              drawBar(plotColors=plotColors, errorBars=TRUE, errorCap=errorCap, errorLineType=errorLineType, width=width, sidePlot=sidePlot, stacked=stack, capSize=capWidth, lineWidth=lWidth)
          } else {
            if(verbose){print(select(printData,Group,N,Median,Min,Max))}
            plotData %>% mutate(yt=Median,at=at,yb=bVal,UpperError=Max-Median,LowerError=Median-Min) %>%
              drawBar(plotColors=plotColors, errorBars=FALSE, errorCap=errorCap, errorLineType=errorLineType, width=width, sidePlot=sidePlot, stacked=stack, capSize=capWidth, lineWidth=lWidth)
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
              drawBar(plotColors=plotColors, errorBars=TRUE, errorCap=errorCap, errorLineType=errorLineType, width=width, sidePlot=sidePlot, stacked=stack, capSize=capWidth, lineWidth=lWidth)
          } else if (errFun[1]=="se") {
            if(verbose){print(select(printData,Group,N,Mean,starts_with("SE")))}
            plotData %>% mutate(yt=Mean,at=at,yb=bVal,UpperError=SE,LowerError=SE) %>%
              drawBar(plotColors=plotColors, errorBars=TRUE, errorCap=errorCap, errorLineType=errorLineType, width=width, sidePlot=sidePlot, stacked=stack, capSize=capWidth, lineWidth=lWidth)
          } else if (errFun[1]=="range") {
            if(verbose){print(select(printData,Group,N,Mean,Min,Max))}
            plotData %>% mutate(yt=Mean,at=at,yb=bVal,UpperError=Max-Mean,LowerError=Mean-Min) %>%
              drawBar(plotColors=plotColors, errorBars=TRUE, errorCap=errorCap, errorLineType=errorLineType, width=width, sidePlot=sidePlot, stacked=stack, capSize=capWidth, lineWidth=lWidth)
          } else {
            if(verbose){print(select(printData,Group,N,Mean,starts_with("SE")))}
            plotData %>% mutate(yt=Mean,at=at,yb=bVal,UpperError=SE,LowerError=SE) %>%
              drawBar(plotColors=plotColors, errorBars=FALSE, errorCap=errorCap, errorLineType=errorLineType, width=width, sidePlot=sidePlot, stacked=stack, capSize=capWidth, lineWidth=lWidth)
          }
        } else if(aggFun[1]=="median") {
          if(errFun[1]=="sd"){
            if(verbose){print(select(printData,Group,N,Median,starts_with("SD")))}
            plotData %>% mutate(yt=Median,at=at,yb=bVal,UpperError=SD,LowerError=SD) %>%
              drawBar(plotColors=plotColors, errorBars=TRUE, errorCap=errorCap, errorLineType=errorLineType, width=width, sidePlot=sidePlot, stacked=stack, capSize=capWidth, lineWidth=lWidth)
          } else if (errFun[1]=="se") {
            if(verbose){print(select(printData,Group,N,Median,starts_with("SE")))}
            plotData %>% mutate(yt=Median,at=at,yb=bVal,UpperError=SE,LowerError=SE) %>%
              drawBar(plotColors=plotColors, errorBars=TRUE, errorCap=errorCap, errorLineType=errorLineType, width=width, sidePlot=sidePlot, stacked=stack, capSize=capWidth, lineWidth=lWidth)
          } else if (errFun[1]=="range") {
            if(verbose){print(select(printData,Group,N,Median,Min,Max))}
            plotData %>% mutate(yt=Median,at=at,yb=bVal,UpperError=Max-Median,LowerError=Median-Min) %>%
              drawBar(plotColors=plotColors, errorBars=TRUE, errorCap=errorCap, errorLineType=errorLineType, width=width, sidePlot=sidePlot, stacked=stack, capSize=capWidth, lineWidth=lWidth)
          } else {
            if(verbose){print(select(printData,Group,N,Median,Min,Max))}
            plotData %>% mutate(yt=Median,at=at,yb=bVal,UpperError=Max-Median,LowerError=Median-Min) %>%
              drawBar(plotColors=plotColors, errorBars=FALSE, errorCap=errorCap, errorLineType=errorLineType, width=width, sidePlot=sidePlot, stacked=stack, capSize=capWidth, lineWidth=lWidth)
          }
        }
      }
      if(legend!=FALSE) {
        if(stack==TRUE){
          if(legend==TRUE){
            legendTitle<-colnames(by)[3]
          }
          legendLabels<-levels(by[,3])
        } else {
          if(legend==TRUE){
            legendTitle<-colnames(by)[2]
          }
          legendLabels<-levels(by[,2])
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
            drawBar(plotColors=plotColors, errorBars=TRUE, errorCap=errorCap, errorLineType=errorLineType, width=width, sidePlot=sidePlot, stacked=stack, capSize=capWidth, lineWidth=lWidth)
        } else if (errFun[1]=="se") {
          if(verbose){print(select(printData,Group,N,Mean,starts_with("SE")))}
          plotData %>% mutate(yt=Mean,at=at,yb=bVal,UpperError=SE,LowerError=SE) %>%
            drawBar(plotColors=plotColors, errorBars=TRUE, errorCap=errorCap, errorLineType=errorLineType, width=width, sidePlot=sidePlot, stacked=stack, capSize=capWidth, lineWidth=lWidth)
        } else if (errFun[1]=="range") {
          if(verbose){print(select(printData,Group,N,Mean,Min,Max))}
          plotData %>% mutate(yt=Mean,at=at,yb=bVal,UpperError=Max-Mean,LowerError=Mean-Min) %>%
            drawBar(plotColors=plotColors, errorBars=TRUE, errorCap=errorCap, errorLineType=errorLineType, width=width, sidePlot=sidePlot, stacked=stack, capSize=capWidth, lineWidth=lWidth)
        } else {
          if(verbose){print(select(printData,Group,N,Mean,starts_with("SE")))}
          plotData %>% mutate(yt=Mean,at=at,yb=bVal,UpperError=SE,LowerError=SE) %>%
            drawBar(plotColors=plotColors, errorBars=FALSE, errorCap=errorCap, errorLineType=errorLineType, width=width, sidePlot=sidePlot, stacked=stack, capSize=capWidth, lineWidth=lWidth)
        }
      } else if(aggFun[1]=="median") {
        if(errFun[1]=="sd"){
          if(verbose){print(select(printData,Group,N,Median,starts_with("SD")))}
          plotData %>% mutate(yt=Median,at=at,yb=bVal,UpperError=SD,LowerError=SD) %>%
            drawBar(plotColors=plotColors, errorBars=TRUE, errorCap=errorCap, errorLineType=errorLineType, width=width, sidePlot=sidePlot, stacked=stack, capSize=capWidth, lineWidth=lWidth)
        } else if (errFun[1]=="se") {
          if(verbose){print(select(printData,Group,N,Median,starts_with("SE")))}
          plotData %>% mutate(yt=Median,at=at,yb=bVal,UpperError=SE,LowerError=SE) %>%
            drawBar(plotColors=plotColors, errorBars=TRUE, errorCap=errorCap, errorLineType=errorLineType, width=width, sidePlot=sidePlot, stacked=stack, capSize=capWidth, lineWidth=lWidth)
        } else if (errFun[1]=="range") {
          if(verbose){print(select(printData,Group,N,Median,Min,Max))}
          plotData %>% mutate(yt=Median,at=at,yb=bVal,UpperError=Max-Median,LowerError=Median-Min) %>%
            drawBar(plotColors=plotColors, errorBars=TRUE, errorCap=errorCap, errorLineType=errorLineType, width=width, sidePlot=sidePlot, stacked=stack, capSize=capWidth, lineWidth=lWidth)
        } else {
          if(verbose){print(select(printData,Group,N,Median,Min,Max))}
          plotData %>% mutate(yt=Median,at=at,yb=bVal,UpperError=Max-Median,LowerError=Median-Min) %>%
            drawBar(plotColors=plotColors, errorBars=FALSE, errorCap=errorCap, errorLineType=errorLineType, width=width, sidePlot=sidePlot, stacked=stack, capSize=capWidth, lineWidth=lWidth)
        }
      }
      if(legend!=FALSE) {
        if(flipFacts) {
          if(legend==TRUE){
            legendTitle<-"Legend"
          }
          legendLabels<-levels(by)
        } else {
          if(legend==TRUE){
            legendTitle<-"Legend"
          }
          legendLabels<-colnames(prepedData[[1]])
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
            drawBar(plotColors=plotColors, errorBars=TRUE, errorCap=errorCap, errorLineType=errorLineType, width=width, sidePlot=sidePlot, stacked=stack, capSize=capWidth, lineWidth=lWidth)
        } else if (errFun[1]=="se") {
          if(verbose){print(select(printData,Group,N,Mean,starts_with("SE")))}
          plotData %>% mutate(yt=Mean,at=at,yb=bVal,UpperError=SE,LowerError=SE) %>%
            drawBar(plotColors=plotColors, errorBars=TRUE, errorCap=errorCap, errorLineType=errorLineType, width=width, sidePlot=sidePlot, stacked=stack, capSize=capWidth, lineWidth=lWidth)
        } else if (errFun[1]=="range") {
          if(verbose){print(select(printData,Group,N,Mean,Min,Max))}
          plotData %>% mutate(yt=Mean,at=at,yb=bVal,UpperError=Max-Mean,LowerError=Mean-Min) %>%
            drawBar(plotColors=plotColors, errorBars=TRUE, errorCap=errorCap, errorLineType=errorLineType, width=width, sidePlot=sidePlot, stacked=stack, capSize=capWidth, lineWidth=lWidth)
        } else {
          if(verbose){print(select(printData,Group,N,Mean,starts_with("SE")))}
          plotData %>% mutate(yt=Mean,at=at,yb=bVal,UpperError=SE,LowerError=SE) %>%
            drawBar(plotColors=plotColors, errorBars=FALSE, errorCap=errorCap, errorLineType=errorLineType, width=width, sidePlot=sidePlot, stacked=stack, capSize=capWidth, lineWidth=lWidth)
        }
      } else if(aggFun[1]=="median") {
        if(errFun[1]=="sd"){
          if(verbose){print(select(printData,Group,N,Median,starts_with("SD")))}
          plotData %>% mutate(yt=Median,at=at,yb=bVal,UpperError=SD,LowerError=SD) %>%
            drawBar(plotColors=plotColors, errorBars=TRUE, errorCap=errorCap, errorLineType=errorLineType, width=width, sidePlot=sidePlot, stacked=stack, capSize=capWidth, lineWidth=lWidth)
        } else if (errFun[1]=="se") {
          if(verbose){print(select(printData,Group,N,Median,starts_with("SE")))}
          plotData %>% mutate(yt=Median,at=at,yb=bVal,UpperError=SE,LowerError=SE) %>%
            drawBar(plotColors=plotColors, errorBars=TRUE, errorCap=errorCap, errorLineType=errorLineType, width=width, sidePlot=sidePlot, stacked=stack, capSize=capWidth, lineWidth=lWidth)
        } else if (errFun[1]=="range") {
          if(verbose){print(select(printData,Group,N,Median,Min,Max))}
          plotData %>% mutate(yt=Median,at=at,yb=bVal,UpperError=Max-Median,LowerError=Median-Min) %>%
            drawBar(plotColors=plotColors, errorBars=TRUE, errorCap=errorCap, errorLineType=errorLineType, width=width, sidePlot=sidePlot, stacked=stack, capSize=capWidth, lineWidth=lWidth)
        } else {
          if(verbose){print(select(printData,Group,N,Median,Min,Max))}
          plotData %>% mutate(yt=Median,at=at,yb=bVal,UpperError=Max-Median,LowerError=Median-Min) %>%
            drawBar(plotColors=plotColors, errorBars=FALSE, errorCap=errorCap, errorLineType=errorLineType, width=width, sidePlot=sidePlot, stacked=stack, capSize=capWidth, lineWidth=lWidth)
        }
      }
      if(legend!=FALSE) {
        if(stack){
          if(legend==TRUE){
            legendTitle<-colnames(by)[2]
          }
          legendLabels<-levels(by[,2])
        } else {
          if(flipFacts) {
            if(legend==TRUE){
              legendTitle<-"Legend"
            }
            legendLabels<-levels(by[,1])
          } else {
            if(legend==TRUE){
              legendTitle<-"Legend"
            }
            legendLabels<-colnames(prepedData[[1]])
          }
        }
      }
    }
  }
  if(length(legendColors)<length(legendLabels) & legend!=FALSE){
    legend<-FALSE
    warning("Not enough point colors to uniquely color subGroups levels\nPlease update plotColors point options to use legend options with this subgroup.")
  }

  oFont<-par()$family
  oCexMain<-par()$cex.main
  oCexlab<-par()$cex.lab
  oCexSub<-par()$cex.sub
  if(!is.na(theme[1]) & !is.null(theme[1])){
    par(cex.main=theme$titleSize, cex.lab=theme$axisLabelSize, cex.sub=theme$subSize, family=theme$fontFamily)
  }
  if(legend!=FALSE) {
    if(is.na(legendTitle) | legendTitle=="factTwo") {
      legendTitle<="Legend"
    }
    makeNiceLegend(labels=legendLabels, title=legendTitle, fontCol=plotColors$labels, border=theme$LegendBorder, lineCol=theme$LegendLineCol, bg=theme$LegendBG, col=legendColors, shape="rect",size=theme$LegendSize,spacing=theme$LegendSpacing)
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
  par(cex.main=oCexMain, cex.lab=oCexlab, cex.sub=oCexSub,family=oFont)
  dataOut<-list(data=data.frame(prepedData$data,by),summary=plotData,stats=pvalue)
  invisible(dataOut)
}
