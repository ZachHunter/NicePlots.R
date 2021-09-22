#' @include np_options_processing.R niceThemes.R np_categorical_plot_setup.R np_plotting_functions.R np_utility.R np_data_processing.R
#' @title draw a box plot
#' @description draws a box plot with optional scatter plot overlays, subgrouping options and log scale support.
#'
#' @details
#' This box plot function offers extensive log scale support, outlier detection, data point overlay options, data subsetting with a secondary factor, and data point highlighting with a tertiary factor.
#' The complicated part of using this function is handling its many options. A wrapper function to set up and run it with preset options may be a good idea if you are using it along. The function \code{\link{niceDots}} is an example of this.
#' Briefly put, the \code{by} argument can be a data frame of factors and the function will  work through the columns in order as needed.
#' If \code{x} is a numeric vector, then \code{by} should be a factor to group it into categories. If \code{by} is a data frame of factors and \code{subgroup=\link{TRUE}}, then the first column for \code{by}
#' is used as the grouping factor and the second column is used as the sub-grouping factor. If \code{pointHighlights==\link{TRUE}}, and \code{subgroup=\link{TRUE}}, the the third column of \code{by}
#' is used to highlight points data point overlay (assuming \code{drawPoints=\link{TRUE}}). If \code{subgroup=\link{FALSE}} and \code{subgroup=\link{TRUE}}, then the second column of \code{by} is used to control
#' the point highlighting. If \code{x} itself is a data frame of numeric vectors, \code{subgroup} is automatically set to false and each column of \code{x} is plotted like a sub-group and grouped
#' by the first column of \code{by}. Data point highlighting with \code{pointHighlights=\link{TRUE}} can still be used when \code{x} is a data frame and the highlighting factor will be drawn from the second column of \code{by}.
#' Please note that the p-values can not always be calculated and are for general exploratory use only. More careful analysis is necessary to determine statistical significance.
#' This function is as S3 generic and can be extended to provide class specific functionality. To further facilitate data exploration, outputs from statistical testing and data set summaries
#' are printed to the console.
#'
#' @inheritParams prepCategoryWindow
#' @param theme list object; Themes are are an optional way of storing graphical preset options that are compatible with all nicePlot graphing functions.
#' @param outliers positive numeric; number of interquartile ranges (IQR) past the Q1 (25\%) and Q3 (75\%) cumulative distribution values. Outliers are often defined as \eqn{1.5 \times IQR}{1.5 * IQR} and extreme outliers are more than \eqn{3 \times IQR}{3 * IQR} away from the inner 50\% data range.
#' @param pointSize positive integer; sets the cex multiplier for point size.
#' @param pointMethod character; method to be used for ploting dots. Can be set to "jitter", "linear", "beeswarm" or "distribution".
#' @param width numeric; scaling factor controlling the width of the boxes.
#' @param pointShape positive integer; sets pty for plotting data points. Can be a vector to support additional graphical customization.
#' @param showCalc logical; if a p-value can be easily calculated for your data, it will be displayed using the \code{sub} annotation setting.
#' @param calcType character; should match one of 'none', 'wilcox', 'Tukey','t.test','anova' which will determine which, if any statistical test should be performed on the data.
#' @param drawBox logical; should the boxes be drawn. The median bar will be drawn regardless.
#' @param add logical; causes plotting to be added to the existing plot rather the start a new one.
#' @param main character; title for the graph which is supplied to the \code{main} argument.
#' @param sub character; subtitle for the graph which is supplied to the \code{sub} argument. If \code{\link{NULL}} and \code{showCalc=\link{TRUE}} it will be used to display the output form \code{\link{calcStats}}.
#' @param ylab character; y-axis label.
#' @param drawPoints logical; draws a dot plot overlay of the data for each box.
#' @param pointHighlights logical; will use additional factors in \code{by} to highlight points in the dot plot
#' @param pointLaneWidth numeric; This controls how far data point dots can move along the categorical axis when plotting. Used for \code{pointMethod} options 'jitter', 'beeswarm', and 'distribution'.
#' @param flipFacts logical; When a dataframe of values is given, column names are used as a secondary grouping factor by default. Setting \code{flipFacts=\link{TRUE}} makes the column names the primary factor and \code{by} the secondary factor.
#' @param na.rm logical; Should \code{NA}s be removed from the data set? Both data input and the factor input from \code{by} with be checked.
#' @param verbose logical; Prints summary and p-value calculations to the screen. All data is silently by the function returned either way.
#' @param ... additional options for S3 method variants
#'
#' @examples
#' data(iris)
#' mCols<-makeColorMatrix()
#' myCols<-list(fill=c(mCols[1,3],mCols[2,3],mCols[3,3]),lines="darkblue")
#' Lab<-"Sepal Length"
#' niceBox(iris$Sepal.Length,iris$Species,minorTick=4,showCalc=TRUE,
#'     calcType="anova",ylab=Lab,main="Sepal Length by Species",plotColors=myCols)
#'
#'
#' plot(density(iris$Petal.Length))
#' lengthFact<-factor(iris$Petal.Length>2.82,labels=c("short","long"))
#'
#'
#' Title<-"Sepal Length by Species and Petal Length"
#' factorFrame<-data.frame(Species=iris$Species,PetalLength=lengthFact)
#' niceBox(iris$Sepal.Length, by=factorFrame, minorTick=4,subgroup=TRUE,
#'     ylab=Lab,main=Title,plotColors=myCols)
#' @importFrom magrittr %>%
#' @importFrom dplyr bind_cols
#' @importFrom purrr reduce map_chr
#' @export
#' @seealso \code{\link{boxplot}}, \code{\link[beeswarm]{beeswarm}}, \code{\link{quantileTrim}}, \code{\link{prepCategoryWindow}}
niceBox <- function(x, by=NULL, groupNames=NULL, main=NULL,sub=NULL, ylab=NULL, theme=basicTheme, minorTick=FALSE, guides=TRUE, outliers=1.5, pointSize=1, width=1, pointShape=16, plotColors=list(bg="open"), logScale=FALSE, trim=FALSE, pointMethod="jitter", axisText=c(NULL,NULL), showCalc=FALSE, calcType="none", drawBox=TRUE, yLim=NULL, rotateLabels=FALSE, rotateY=FALSE, add=FALSE, minorGuides=NULL, extendTicks=TRUE, subgroup=FALSE, subgroupLabels=NULL, expLabels=TRUE, sidePlot=FALSE, drawPoints=TRUE, pointHighlights=FALSE, pointLaneWidth=.7, flipFacts=FALSE, na.rm=FALSE, verbose=FALSE, legend=FALSE, logAdjustment=1,...) {UseMethod("niceBox",x)}

#' @importFrom magrittr %>%
#' @importFrom dplyr bind_cols
#' @importFrom purrr reduce map_chr
#' @export
niceBox.default <- function(x, by=NULL, groupNames=NULL, main=NULL,sub=NULL, ylab=NULL, theme=basicTheme, minorTick=NULL, guides=NULL, outliers=1.5, pointSize=NULL, width=NULL, pointShape=NULL, plotColors=NULL, logScale=FALSE, trim=FALSE, pointMethod=NULL, axisText=c(NULL,NULL), showCalc=FALSE, calcType="none", drawBox=TRUE, yLim=NULL, rotateLabels=FALSE, rotateY=TRUE, add=FALSE, minorGuides=NULL, extendTicks=TRUE, subgroup=FALSE, subgroupLabels=NULL, expLabels=FALSE, sidePlot=FALSE, drawPoints=TRUE, pointHighlights=FALSE, pointLaneWidth=NULL, flipFacts=FALSE, na.rm=FALSE, verbose=FALSE, legend=FALSE,logAdjustment=1, ...) {
  if(any(is.na(x)) | any(is.na(by))){warning("Warning: NAs detected in dataset", call.=FALSE)}
  prepedData<-NULL
  plotData<-NULL
  lWidth<-NULL
  whiskerLineType<-NULL
  capWidth<-NULL
  capType<-NULL

  #documenting all the data and plotting options to attach to the output so the graph can be replotted if desired.
  moreOptions<-list(...)
  ActiveOptions<-list(x=x, by=by, groupNames=groupNames, main=main,sub=sub, ylab=ylab, theme=theme, minorTick=minorTick, guides=guides, outliers=outliers, pointSize=pointSize, width=width, pointShape=pointShape, plotColors=plotColors, logScale=logScale, trim=trim, pointMethod=pointMethod, axisText=axisText, showCalc=showCalc, calcType=calcType, drawBox=drawBox, yLim=yLim, rotateLabels=rotateLabels, rotateY=rotateY, add=add, minorGuides=minorGuides, extendTicks=extendTicks, subgroup=subgroup, subgroupLabels=subgroupLabels, expLabels=expLabels, sidePlot=sidePlot, drawPoints=drawPoints, pointHighlights=pointHighlights, pointLaneWidth=pointLaneWidth, flipFacts=flipFacts, na.rm=na.rm, verbose=verbose, legend=legend,logAdjustment=logAdjustment)
  ActiveOptions<-append(ActiveOptions,moreOptions)


  #This is to make sure multivariate input to X is ploted as a primary factor if subgroup==FALSE
  if(is.data.frame(x) | is.matrix(x)) {
    if(dim(x)[2]>1 & subgroup==FALSE) {flipFacts<-TRUE}
  }
  checked<-dataFlightCheck(x,by,na.rm=na.rm,flipFacts = flipFacts)
  x<-checked$d
  by<-checked$b
  rm(checked)
  swarmOverflow<-NULL

  #Here we check to see if the user specified any options so that they are left unaltered if present
  finalOptions<-procNiceOptions(x=x,by=by,minorTick=minorTick,pointShape=pointShape,whiskerLineType=whiskerLineType,lWidth=lWidth,capWidth=capWidth,pointLaneWidth=pointLaneWidth,width=width,guides=guides,pointSize=pointSize,subgroup=subgroup,stack=F,pointHighlights=pointHighlights,type="BP",theme=theme,plotColors=plotColors,logScale=logScale,pointMethod=pointMethod,drawPoints=drawPoints,groupNames=groupNames,swarmOverflow = swarmOverflow,CLOptions=moreOptions)
  minorTick<-finalOptions$minorTick
  pointShape<-finalOptions$pointShape
  whiskerLineType<-finalOptions$whiskerLineType
  lWidth<-finalOptions$lWidth
  capWidth<-finalOptions$capWidth
  pointLaneWidth<-finalOptions$pointLaneWidth
  width<-finalOptions$width
  guides<-finalOptions$guides
  pointSize<-finalOptions$pointSize
  theme<-finalOptions$theme
  plotColors<-finalOptions$plotColors
  groupNames<-finalOptions$groupNames
  pointMethod<-finalOptions$pointMethod
  swarmOverflow<-finalOptions$swarmOverflow
  capType<-finalOptions$errorCap
  if(is.null(capType)){capType<-"bar"}
  if(capType=="ball") {
    capWidth<-capWidth*6
  }

  #if(flipFacts & is.data.frame(x)){subgroup<-TRUE}
  #Handling adding plots to existing graph
  if(add==TRUE) {
    if(logScale>0) {
      prepedData<-list(data=log(x+logAdjustment,logScale))
    } else {
      prepedData<-list(data=x)
    }
    #Make a new graph with a new plotting environment
  } else {
    if(is.null(minorGuides)){
      if(guides!=FALSE & logScale > 0) {
        minorGuides<-TRUE
      } else {
        minorGuides<-FALSE
      }
    }
    #RStudio seems not to update the graphics devices properly
    if(Sys.getenv("RSTUDIO") == "1" & is.null(moreOptions[["RSOveride"]])) {graphics.off()}

    prepedData<-prepCategoryWindow(x,by=by, groupNames=groupNames, minorTick=minorTick, guides=guides, plotColors=plotColors, yLim=yLim, rotateLabels=rotateLabels, rotateY=rotateY, trim=trim, logScale=logScale, axisText=axisText, minorGuides=minorGuides, extendTicks=extendTicks, subgroup=subgroup, expLabels=expLabels,sidePlot=sidePlot,subgroupLabels=subgroupLabels, theme=theme, legend=legend, pointHighlights=pointHighlights,logAdjustment=logAdjustment)
  }
  pvalue<-NULL
  filter<-rep(TRUE,length(x))
  if(trim>0){filter<-quantileTrim(x,trim,na.rm=T,returnFilter=T)[[2]]}
  if(is.data.frame(by)) {
    by<-by[filter,]
  } else {
    by<-by[filter]
  }
  #Initialize legend variables so we can update based on options
  legendTitle<-"Legend"
  legendLabels<-NULL
  legendColors<-plotColors$points
  #Handles cases where users want the points overlay to be consistant and the fill to change.
  if(length(legendColors)<=1 & length(plotColors$fill)>1){
    legendColors<-plotColors$fill
  }

  #Data is set and ready to go. Plotting is handled based on cases handling if 'x' and 'by' are vectors or dataframes
  xypos<-c(1,1)
  if(is.numeric(prepedData[[1]])){
    #CASE: by is a factor and data is a numeric vector
    if(is.factor(by)) {
      if(calcType[1]!="none"){pvalue<-calcStats(prepedData[[1]],by,calcType[1],verbose=verbose)}
      plotLoc<-seq(1,length(groupNames),by=1)
      names(plotLoc)<-groupNames
      legend<-FALSE
      plotData<-prepNiceData(prepedData=prepedData,by=by, subgroup=subgroup, outliers=outliers, filter=filter, groupNames=groupNames, plotLoc=plotLoc, width=width,verbose=verbose)
      plotData %>% drawBoxPlot(side=sidePlot,col=plotColors$lines,fill=plotColors$fill,drawDot=F,drawBox=drawBox, lWidth=lWidth,whiskerLty=whiskerLineType,capWidth=capWidth, capType=capType)
      xypos<-addNicePoints(prepedData=prepedData, by=by, filter=filter, sidePlot=sidePlot, subgroup=subgroup, plotAt=plotLoc,pointHighlights=pointHighlights, pointMethod=pointMethod, pointShape=pointShape, pointSize=pointSize, width=width, pointLaneWidth=pointLaneWidth, plotColors=plotColors, drawPoints=drawPoints, outliers=outliers,swarmOverflow = swarmOverflow)
      xyid<-1
      xFilter<-1
      byFilter<-1
      if(is.vector(ActiveOptions$x) | is.factor(ActiveOptions$x)) {
        xyid<-seq(length(ActiveOptions$x))
        xFilter<-!is.na(ActiveOptions$x)
      } else {
        if(flipFacts==TRUE) {
          xyid<-rep(seq(dim(as.data.frame(ActiveOptions$x))[1]),ncol(ActiveOptions$x))
          xFilter<-rep(rowSums(is.na(as.data.frame(ActiveOptions$x)))==0,ncol(ActiveOptions$x))
        } else {
          xyid<-seq(dim(as.data.frame(ActiveOptions$x))[1])
          xFilter<-rowSums(is.na(as.data.frame(ActiveOptions$x)))==0
        }
      }
      if(is.vector(ActiveOptions$by) | is.factor(ActiveOptions$by)){
        byFilter<-!is.na(ActiveOptions$by)
      } else {
        byFilter<-rowSums(is.na(as.data.frame(ActiveOptions$by)))==0
      }
      xyid<-xyid[xFilter ==TRUE & byFilter ==TRUE]
      xyid<-xyid[filter]
      if(length(xyid)<nrow(xypos)){
        xyid<-rep(xyid,nrow(xypos)/length(xyid))
      }
      xypos<-data.frame(xypos,ID=xyid)

    } else {
      if(calcType[1]!="none"){pvalue<-calcStats(prepedData[[1]],by[,1],calcType[1])}
      #CASE: by is not a factor, data is a numeric vector and subgroup is TRUE
      if(subgroup) {
        facetLoc<-facetSpacing(length(levels(by[,2])),length(groupNames))
        names(facetLoc)<-unlist(lapply(levels(by[,1]),FUN=function(x) paste0(x,levels(by[,2]),sep=".")))
        plotData<-prepNiceData(prepedData=prepedData,by=by, subgroup=subgroup, outliers=outliers, filter=filter, groupNames=groupNames, plotLoc=plotLoc, width=width,verbose=verbose)
        cLoc<-facetLoc[plotData$facetLevel]
        plotData %>% bind_cols(at=cLoc,width=rep(.25*width/length(levels(by[,2])),length(cLoc))) %>%
          drawBoxPlot(side=sidePlot,col=plotColors$lines,fill=plotColors$fill,drawDot=F,drawBox=drawBox,lWidth = lWidth,whiskerLty=whiskerLineType,capWidth=capWidth, capType=capType)
        xypos<-addNicePoints(prepedData=prepedData, by=by, filter=filter, sidePlot=sidePlot, subgroup=subgroup, plotAt=facetLoc,pointHighlights=pointHighlights, pointMethod=pointMethod, pointShape=pointShape, pointSize=pointSize, width=width, pointLaneWidth=pointLaneWidth, plotColors=plotColors, drawPoints=drawPoints, outliers=outliers,swarmOverflow = swarmOverflow)
        xyid<-1
        xFilter<-1
        byFilter<-1
        if(is.vector(ActiveOptions$x) | is.factor(ActiveOptions$x)) {
          xyid<-seq(length(ActiveOptions$x))
          xFilter<-!is.na(ActiveOptions$x)
        } else {
          if(flipFacts==TRUE) {
            xyid<-rep(seq(dim(as.data.frame(ActiveOptions$x))[1]),ncol(ActiveOptions$x))
            xFilter<-rep(rowSums(is.na(as.data.frame(ActiveOptions$x)))==0,ncol(ActiveOptions$x))
          } else {
            xyid<-seq(dim(as.data.frame(ActiveOptions$x))[1])
            xFilter<-rowSums(is.na(as.data.frame(ActiveOptions$x)))==0
          }
        }
        if(is.vector(ActiveOptions$by) | is.factor(ActiveOptions$by)){
          byFilter<-!is.na(ActiveOptions$by)
        } else {
          byFilter<-rowSums(is.na(as.data.frame(ActiveOptions$by)))==0
        }
        xyid<-xyid[xFilter ==TRUE & byFilter ==TRUE]
        xyid<-xyid[filter]
        if(length(xyid)<nrow(xypos)){
          xyid<-rep(xyid,nrow(xypos)/length(xyid))
        }
        xypos<-data.frame(xypos,ID=xyid)

        if(legend!=FALSE) {
          if(pointHighlights){
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
      } else {
        #CASE: by is not a factor, data is a numeric vector and subgroup is FALSE
        plotLoc<-seq(1,length(groupNames),by=1)
        names(plotLoc)<-groupNames
        plotData<-prepNiceData(prepedData=prepedData,by=by, subgroup=subgroup, outliers=outliers, filter=filter, groupNames=groupNames, plotLoc=plotLoc, width=width,verbose=verbose)
        plotData %>% drawBoxPlot(side=sidePlot,col=plotColors$lines,fill=plotColors$fill,drawDot=F,drawBox=drawBox,lWidth=lWidth,whiskerLty=whiskerLineType,capWidth=capWidth, capType=capType)
        xypos<-addNicePoints(prepedData=prepedData, by=by, filter=filter, sidePlot=sidePlot, subgroup=subgroup, plotAt=plotLoc,pointHighlights=pointHighlights, pointMethod=pointMethod, pointShape=pointShape, pointSize=pointSize, width=width, pointLaneWidth=pointLaneWidth, plotColors=plotColors, drawPoints=drawPoints, outliers=outliers,swarmOverflow = swarmOverflow)
        xyid<-1
        xFilter<-1
        byFilter<-1
        if(is.vector(ActiveOptions$x) | is.factor(ActiveOptions$x)) {
          xyid<-seq(length(ActiveOptions$x))
          xFilter<-!is.na(ActiveOptions$x)
        } else {
          if(flipFacts==TRUE) {
            xyid<-rep(seq(dim(as.data.frame(ActiveOptions$x))[1]),ncol(ActiveOptions$x))
            xFilter<-rep(rowSums(is.na(as.data.frame(ActiveOptions$x)))==0,ncol(ActiveOptions$x))
          } else {
            xyid<-seq(dim(as.data.frame(ActiveOptions$x))[1])
            xFilter<-rowSums(is.na(as.data.frame(ActiveOptions$x)))==0
          }
        }
        if(is.vector(ActiveOptions$by) | is.factor(ActiveOptions$by)){
          byFilter<-!is.na(ActiveOptions$by)
        } else {
          byFilter<-rowSums(is.na(as.data.frame(ActiveOptions$by)))==0
        }
        xyid<-xyid[xFilter ==TRUE & byFilter ==TRUE]
        xyid<-xyid[filter]
        if(length(xyid)<nrow(xypos)){
          xyid<-rep(xyid,nrow(xypos)/length(xyid))
        }
        xypos<-data.frame(xypos,ID=xyid)

        facetLoc<-plotLoc
        if(legend!=FALSE) {
          if(pointHighlights==TRUE){
            if(legend==TRUE){
              legendTitle<-colnames(by)[2]
            }
            legendLabels<-levels(by[,2])
          }
        }
      }
    }
  } else {
    #CASE: data is a dataframe, by is a factor, subgroup is ignored
    if(is.factor(by)) {
      if(calcType[1]!="none"){pvalue<-calcStats(prepedData[[1]][,1],by,calcType[1],verbose=verbose)}
      facetLoc<-facetSpacing(length(prepedData[[1]]),length(groupNames))
      names(facetLoc)<-unlist(lapply(levels(by),FUN=function(y) paste0(y,names(x),sep=".")))
      plotData<-prepNiceData(prepedData=prepedData,by=by, subgroup=subgroup, outliers=outliers, filter=filter, groupNames=groupNames, plotLoc=plotLoc, width=width,flipFacts=flipFacts,verbose=verbose)
      cLoc<-facetLoc[plotData$facetLevel]
      plotData %>% bind_cols(at=cLoc,width=rep(.25*width/length(x),length(cLoc))) %>%
        drawBoxPlot(side=sidePlot,col=plotColors$lines,fill=plotColors$fill,drawDot=F,drawBox=drawBox, lWidth=lWidth,whiskerLty=whiskerLineType,capWidth=capWidth, capType=capType)
      xypos<-addNicePoints(prepedData=prepedData, by=by, filter=filter, sidePlot=sidePlot, subgroup=subgroup, plotAt=facetLoc,pointHighlights=pointHighlights, pointMethod=pointMethod, pointShape=pointShape, pointSize=pointSize, width=width, pointLaneWidth=pointLaneWidth, plotColors=plotColors, drawPoints=drawPoints, outliers=outliers, dataCols=length(x),swarmOverflow = swarmOverflow)
      xyid<-1
      xFilter<-1
      byFilter<-1
      if(is.vector(ActiveOptions$x) | is.factor(ActiveOptions$x)) {
        xyid<-seq(length(ActiveOptions$x))
        xFilter<-!is.na(ActiveOptions$x)
      } else {
        if(flipFacts==TRUE) {
          xyid<-rep(seq(dim(as.data.frame(ActiveOptions$x))[1]),ncol(ActiveOptions$x))
          xFilter<-rep(rowSums(is.na(as.data.frame(ActiveOptions$x)))==0,ncol(ActiveOptions$x))
        } else {
          xyid<-seq(dim(as.data.frame(ActiveOptions$x))[1])
          xFilter<-rowSums(is.na(as.data.frame(ActiveOptions$x)))==0
        }
      }
      if(is.vector(ActiveOptions$by) | is.factor(ActiveOptions$by)){
        byFilter<-!is.na(ActiveOptions$by)
      } else {
        byFilter<-rowSums(is.na(as.data.frame(ActiveOptions$by)))==0
      }
      xyid<-xyid[xFilter ==TRUE & byFilter ==TRUE]
      xyid<-xyid[filter]
      if(length(xyid)<nrow(xypos)){
        xyid<-rep(xyid,nrow(xypos)/length(xyid))
      }
      xypos<-data.frame(xypos,ID=xyid)

      #Note we are ignoring pointHighlights here as by is a factor
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
      #CASE: data is a data.frame, by is a dataframe, subgroup is ignored
      facetLoc<-facetSpacing(length(prepedData[[1]]),length(groupNames))
      names(facetLoc)<-unlist(lapply(levels(by[,1]),FUN=function(y) paste0(y,names(x),sep=".")))
      if(calcType[1]!="none"){pvalue<-calcStats(prepedData[[1]][,1],by[,1],calcType[1])}
      plotData<-prepNiceData(prepedData=prepedData,by=by, subgroup=subgroup, outliers=outliers, filter=filter, groupNames=groupNames, plotLoc=plotLoc, width=width,flipFacts=flipFacts,verbose=verbose)
      cLoc<-facetLoc[plotData$facetLevel]
      plotData %>% bind_cols(at=cLoc,width=rep(.25*width/length(x),length(cLoc))) %>%
        drawBoxPlot(side=sidePlot,col=plotColors$lines,fill=plotColors$fill,drawDot=F,drawBox=drawBox,lWidth=lWidth,whiskerLty=whiskerLineType,capWidth=capWidth, capType=capType)
      xypos<-addNicePoints(prepedData=prepedData, by=by, filter=filter, sidePlot=sidePlot, subgroup=subgroup, plotAt=facetLoc,pointHighlights=pointHighlights, pointMethod=pointMethod, pointShape=pointShape, pointSize=pointSize, width=width, pointLaneWidth=pointLaneWidth, plotColors=plotColors, drawPoints=drawPoints, outliers=outliers, dataCols=length(x),swarmOverflow = swarmOverflow)
      xyid<-1
      xFilter<-1
      byFilter<-1
      if(is.vector(ActiveOptions$x) | is.factor(ActiveOptions$x)) {
        xyid<-seq(length(ActiveOptions$x))
        xFilter<-!is.na(ActiveOptions$x)
      } else {
        if(flipFacts==TRUE) {
          xyid<-rep(seq(dim(as.data.frame(ActiveOptions$x))[1]),ncol(ActiveOptions$x))
          xFilter<-rep(rowSums(is.na(as.data.frame(ActiveOptions$x)))==0,ncol(ActiveOptions$x))
        } else {
          xyid<-seq(dim(as.data.frame(ActiveOptions$x))[1])
          xFilter<-rowSums(is.na(as.data.frame(ActiveOptions$x)))==0
        }
      }
      if(is.vector(ActiveOptions$by) | is.factor(ActiveOptions$by)){
        byFilter<-!is.na(ActiveOptions$by)
      } else {
        byFilter<-rowSums(is.na(as.data.frame(ActiveOptions$by)))==0
      }
      xyid<-xyid[xFilter ==TRUE & byFilter ==TRUE]
      xyid<-xyid[filter]
      if(length(xyid)<nrow(xypos)){
        xyid<-rep(xyid,nrow(xypos)/length(xyid))
      }
      xypos<-data.frame(xypos,ID=xyid)

      if(legend!=FALSE) {
        if(pointHighlights){
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
    warning("Not enough point colors to uniquely color subgroups levels\nPlease update plotColors point options to use legend options with this subgroup.", call.=FALSE)
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
    makeNiceLegend(labels=legendLabels, title=legendTitle, fontCol=plotColors$labels, border=theme$legendBorder, lineCol=plotColors$legendLineCol, bg=plotColors$legendBG, col=legendColors, shape="rect",size=theme$legendSize,spacing=theme$legendSpacing)
  }
  if(add==FALSE) {
    if(is.null(sub) & showCalc==T & is.null(pvalue)==FALSE){
      sub<-pvalue
    }
    if(sidePlot) {
      title(main=main,col.main=plotColors$title,sub=sub,col.sub=plotColors$subtext,xlab=ylab,col.lab=plotColors$axisLabels)
    } else {
      title(main=main,col.main=plotColors$title,sub=sub,col.sub=plotColors$subtext,ylab=ylab,col.lab=plotColors$axisLabels)
    }
  }
  par(cex.main=oCexMain, cex.lab=oCexlab, cex.sub=oCexSub,family=oFont)
  ActiveOptions$xypos<-xypos
  #formatting the output list and setting class int npData
  dataOut<-list(summary=plotData,stats=pvalue,plotType="box",options=ActiveOptions)
  class(dataOut)<-c("npData","list")

  invisible(dataOut)
}

#' @export
niceBox.npData <- function(x,  ...) {
  clOptions<-list(...)
  for(opt in names(clOptions)) {
    if(is.null(x$options[opt])){
      append(x$options,list(opt=clOptions[[opt]]))
    }else{
      x$options[[opt]]<-clOptions[[opt]]
    }
  }
  dataOut<-do.call("niceBox",x$options)
  invisible(dataOut)
}

