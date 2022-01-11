#' @include np_options_processing.R niceThemes.R np_categorical_plot_setup.R np_plotting_functions.R np_utility.R np_data_processing.R
#'
#' @title draw a dot plot
#' @description draws a categorical dot plot with optional data highlighting, log scale support and optional mean/median/distribution overlays.
#'
#' @details
#' This is really two different plotting functions merged together. First, the data points can be plotted individually using a \code{distribution} waterfall, \code{jitter}, \code{beeswarm}, just \code{linear} or not plotted at all.
#' A signle data vector can be subset (eg using multiple factors with \code{by} and optionally \code{subgroup==\link{TRUE}}) using up to two factors. If a multi-column tibble, matrix or dataframe is used for data input, then can be grouped by a single factor from \code{by} with the column names used for factor subgroups.
#' The option \code{flipFacts} can be used in this case to make the data columns the primary grouping factor and the first factor in \code{by} used for subgroups.
#' On top of this, the \code{\link[base]{mean}}/\code{\link[stats]{median}} values can be overplotted using \code{errorBars==\link{TRUE}} and error or distribution (eg. \code{\link[stats]{sd}}, \code{\link{se}} \code{\link[base]{range}}, etc.) can be also be shown as errorbars. The error bars can be multiplied by \code{errorMultiple} and supressed if \code{errorMultiple=0}.
#'
#' The complicated part of using this function is handling its many options. A wrapper function to set up and run it with preset options may be a good idea if you are using it alot.
#' Briefly put, the \code{by} argument can be a data frame of factors and the function will  work through the columns in order as needed.
#' If \code{x} is a numeric vector, then \code{by} should be a factor to group it into categories. If \code{by} is a data frame of factors and \code{subgroup=\link{TRUE}}, then the first column for \code{by}
#' is used as the grouping factor and the second column is used as the sub-grouping factor. If \code{pointHighlights=\link{TRUE}}, and \code{subgroup=\link{TRUE}}, the the third column of \code{by}
#' is used to highlight points data point overlay (assuming \code{drawPoints=\link{TRUE}}). If \code{subgroup=\link{FALSE}} and \code{subgroup=\link{TRUE}}, then the second column of \code{by} is used to control
#' the point highlighting. If \code{x} itself is a data frame of numeric vectors, \code{subgroup} is automatically set to false and each column of \code{x} is plotted like a sub-group and grouped
#' by the first column of \code{by}. Data point highlighting with \code{pointHighlights=\link{TRUE}} can still be used when \code{x} is a data frame and the highlighting factor will be drawn from the second column of \code{by}.
#' Please note that the p-values can not always be calculated and are for general exploratory use only. More careful analysis is necessary to determine statistical significance.
#' This function is as S3 generic and can be extended to provide class specific functionality.
#' To further facilitate data exploration, outputs from statistical testing and data set summaries
#' are printed to the console if \code{verbose=\link{TRUE}}.
#'
#' @inheritParams prepCategoryWindow
#' @param aggFun character; Determines how the data is summarized by factor level. Valid options are \code{\link[base]{mean}}, \code{\link[stats]{median}}.
#' @param errFun character; How the data spread is charactarized by the error bars. Valid options are \code{\link[stats]{sd}} (standard deviation), \code{\link{se}} (standard error of the mean), \code{t95ci} (t-distribution), \code{boot95ci} (\code{\link[boot]{boot}} strap confidence interval) or \code{\link[base]{range}}.
#' @param theme list object; Themes are are an optional way of storing graphical preset options that are compatible with all nicePlot graphing functions.
#' @param width numeric; cex-like scaling factor controlling the width of the width of each category lane.
#' @param errorMultiple numeric; How many standard errors/deviations should be represented by the error bars. Set to zero to supress error bars.
#' @param barWidth numeric; cex like scaling factor for percentage of the column width the \code{mean}/\code{median} bar will span if drawn.
#' @param barType character; Indicates the style of the \code{mean}/\code{median} bar. Should be 'dot', 'bar' or 'none'.
#' @param barThickness numeric; a cex like multiple for the thickness (\code{lwd}) of the aggregate bar relative to the line width \code{lWidth}.
#' @param errorCap character; Determines the style for the ends of the error bars. Valid options are '\code{ball}', '\code{bar}' or '\code{none}'.
#' @param errorLineType numeric; Sets \code{lty} line type for drawing the error bars.
#' @param capWidth numeric; Controls the cex like scaling of the ball or width of the cap if they are drawn at the end of the error bars for the bar plot.
#' @param lWidth numeric; Line width (\code{lwd}) for drawing the \code{mean}/\code{median} bars and errorbars.
#' @param add logical; causes plotting to be added to the existing plot rather the start a new one.
#' @param main character; title for the graph which is supplied to the \code{main} argument.
#' @param sub character; subtitle for the graph which is supplied to the \code{sub} argument. If \code{\link{NULL}} and \code{showCalc=\link{TRUE}} it will be used to display the output form \code{\link{calcStats}}.
#' @param ylab character; y-axis label.
#' @param showCalc logical; if a p-value can be easily calculated for your data, it will be displayed using the \code{sub} annotation setting.
#' @param calcType character; should match one of 'none', 'wilcox', 'Tukey','t.test','anova' which will determine which, if any statistical test should be performed on the data.
#' @param flipFacts logical; When a dataframe of values is given, column names are used as a secondary grouping factor by default. Setting \code{flipFacts=\link{TRUE}} makes the column names the primary factor and \code{by} the secondary factor.
#' @param na.rm logical; Should \code{NA}s be removed from the data set? Both data input and the factor input from \code{by} with be checked.
#' @param legend logical/character; if not equal to \code{\link{FALSE}} with cause a legend to be drawn in the margins. If set to a character string instead of a logical value, the string will be used as the legend title insteas of the factor column name from \code{by}.
#' @param verbose logical; Prints summary and p-value calculations to the screen. All data is silently by the function returned either way.
#' @param errorBars logical; Determins if the aggregate data and error (if any) is displayed
#' @param outliers positive numeric; number of interquartile ranges (IQR) past the Q1 (25\%) and Q3 (75\%) cumulative distribution values. Outliers are often defined as \eqn{1.5 \times IQR}{1.5 * IQR} and extreme outliers are more than \eqn{3 \times IQR}{3 * IQR} away from the inner 50\% data range.
#' @param pointSize positive integer; sets the cex multiplier for point size.
#' @param pointMethod character; method to be used for ploting dots. Can be set to "jitter", "linear", "beeswarm" or "distribution".
#' @param pointShape positive integer; sets pty for plotting data points. Can be a vector to support additional graphical customization.
#' @param drawPoints logical; draws a dot plot overlay of the data.
#' @param pointHighlights logical; will use additional factors in \code{by} to highlight points in the dot plot
#' @param highlightLabels character; An optional character vector to override the factor labels associated with point highlights if active.
#' @param pointLaneWidth numeric; This controls how far data point dots can spread along the categorical axis when plotting. Used for \code{pointMethod} options 'jitter', 'beeswarm', and 'distribution'.
#' @param ... additional options for S3 method variants.

#' @examples
#' data(iris)
#' mCols<-makeColorMatrix()
#' myCols<-list(fill=mCols[1:3,3],lines="darkblue")
#' niceDots(iris$Sepal.Length,iris$Species,minorTick=4,showCalc=TRUE,calcType="anova",
#'     ylab="Sepal Length",main="Sepal Length by Species",plotColors=myCols)
#'
#' @importFrom dplyr mutate
#' @importFrom magrittr %>%
#' @importFrom purrr reduce
#' @export
#' @seealso \code{\link[graphics]{stripchart}}, \code{\link[beeswarm]{beeswarm}}, \code{\link{quantileTrim}}, \code{\link{prepCategoryWindow}}, \code{\link[base]{jitter}}
niceDots <- function(x, by=NULL, groupNames=NULL, drawPoints=TRUE, errorBars=TRUE,barWidth=.33, barType=c("bar","dot"), barThickness=2, aggFun=c("mean","median","none"),errFun=c("se","sd","range"), errorMultiple=2, main=NULL,sub=NULL, ylab=NULL, minorTick=FALSE, theme=basicTheme, guides=TRUE, outliers=1.5, pointSize=1, width=NULL, pointShape=NULL, plotColors=NULL, logScale=FALSE, trim=FALSE, pointMethod=NULL, axisText=c(NULL,NULL), showCalc=FALSE, calcType="wilcox", yLim=NULL, rotateLabels=FALSE, rotateY=FALSE, add=FALSE, minorGuides=NULL, extendTicks=TRUE, subgroup=FALSE, subgroupLabels=NULL, highlightLabels=NULL, expLabels=TRUE, sidePlot=FALSE, pointHighlights=FALSE, pointLaneWidth=NULL, na.rm=FALSE, flipFacts=FALSE, verbose=FALSE, legend=FALSE,logAdjustment=1,errorCap=NULL, errorLineType=NULL,capWidth=NULL, lWidth=NULL, ...) {UseMethod("niceDots",x)}

#' @importFrom dplyr mutate
#' @importFrom magrittr %>%
#' @importFrom purrr reduce
#' @export
#'@author Zachary Hunter
niceDots.default <- function(x, by=NULL, groupNames=NULL, drawPoints=TRUE, errorBars=TRUE,barWidth=.33, barType=c("bar","dot"), barThickness=2, aggFun=c("mean","median","none"),errFun=c("se","sd","range"), errorMultiple=2, main=NULL,sub=NULL, ylab=NULL, minorTick=FALSE, theme=basicTheme, guides=TRUE, outliers=1.5, pointSize=1, width=NULL, pointShape=NULL, plotColors=NULL, logScale=FALSE, trim=FALSE, pointMethod=NULL, axisText=c(NULL,NULL), showCalc=FALSE, calcType="wilcox", yLim=NULL, rotateLabels=FALSE, rotateY=FALSE, add=FALSE, minorGuides=NULL, extendTicks=TRUE, subgroup=FALSE, subgroupLabels=NULL, highlightLabels=NULL, expLabels=TRUE, sidePlot=FALSE, pointHighlights=FALSE, pointLaneWidth=NULL, na.rm=FALSE, flipFacts=FALSE, verbose=FALSE, legend=FALSE,logAdjustment=1,errorCap=NULL, errorLineType=NULL,capWidth=NULL, lWidth=NULL, ...) {
  if(any(is.na(x)) | any(is.na(by))){warning("Warning: NAs detected in dataset", call.=FALSE)}
  prepedData<-NULL
  plotData<-NULL

  if(is.data.frame(x) | is.matrix(x)) {
    if(dim(x)[2]>1 & subgroup==FALSE) {flipFacts<-TRUE}
  }

  #documenting all the data and plotting options to attach to the output so the graph can be replotted if desired.
  moreOptions<-list(...)
  ActiveOptions<-list(x=x, by=by, groupNames=groupNames, drawPoints=drawPoints, errorBars=errorBars,barWidth=barWidth, barType=barType, barThickness=barThickness, aggFun=aggFun,errFun=errFun, errorMultiple=errorMultiple, main=main,sub=sub, ylab=ylab, minorTick=minorTick, theme=theme, guides=guides, outliers=outliers, pointSize=pointSize, width=width, pointShape=pointShape, plotColors=plotColors, logScale=logScale, trim=trim, pointMethod=pointMethod, axisText=axisText, showCalc=showCalc, calcType=calcType, yLim=yLim, rotateLabels=rotateLabels, rotateY=rotateY, add=add, minorGuides=minorGuides, extendTicks=extendTicks, subgroup=subgroup, subgroupLabels=subgroupLabels, expLabels=expLabels, sidePlot=sidePlot, pointHighlights=pointHighlights, pointLaneWidth=pointLaneWidth, na.rm=na.rm, flipFacts=flipFacts, verbose=verbose, legend=legend,logAdjustment=logAdjustment,errorCap=errorCap, errorLineType=errorLineType,capWidth=capWidth, lWidth=lWidth,highlightLabels=highlightLabels)

  #Flight check and prep data. Removes NAs.
  checked<-dataFlightCheck(x,by,na.rm=na.rm,flipFacts = flipFacts)
  x<-checked$d
  by<-checked$b
  rm(checked)
  swarmOverflow<-NULL

  ActiveOptions<-append(ActiveOptions,moreOptions)

  #Here we check to see if the user specified any options so that they not overwritten by the designated theme
  finalOptions<-procNiceOptions(x=x,by=by,minorTick=minorTick,pointShape=pointShape,whiskerLineType=errorLineType,lWidth=lWidth,capWidth=capWidth,pointLaneWidth=pointLaneWidth,width=width,guides=guides,pointSize=pointSize,subgroup=subgroup,stack=FALSE,pointHighlights=pointHighlights,type="DP",theme=theme,plotColors=plotColors,logScale=logScale,pointMethod=pointMethod,drawPoints=drawPoints,groupNames=groupNames,swarmOverflow=swarmOverflow ,errorCap=errorCap,CLOptions=moreOptions,subgroupLabels=subgroupLabels,highlightLabels=highlightLabels)
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
  subgroupLabels<-finalOptions$subgroupLabels
  highlightLabels<-finalOptions$highlightLabels
  errorCap<-finalOptions$errorCap
  pointMethod<-finalOptions$pointMethod
  swarmOverflow<-finalOptions$swarmOverflow
  pointLaneWidth<-finalOptions$pointLaneWidth

  if((!is.null(ActiveOptions$groupNames) | !is.null(ActiveOptions$subgroupLabels) | !is.null(ActiveOptions$highlightLabels)) & !is.null(groupNames)) {
    if(is.data.frame(by)) {
      by[,1]<-factor(by[,1], labels=groupNames)
    } else {
      by<-factor(by, labels=groupNames)
    }
  }
  if(!is.null(ActiveOptions$subgroupLabels) & !is.null(subgroupLabels)) {
    if(is.data.frame(by) & subgroup==TRUE & !is.data.frame(x)) {
      if(ncol(by) >= 2) {
        by[,2]<-factor(by[,2], labels=subgroupLabels)
      }
    }
  }
  if(!is.null(ActiveOptions$highlightLabels) & !is.null(highlightLabels)) {
    if(is.data.frame(by) & pointHighlights==TRUE) {
      if(ncol(by)>=2) {
        if(is.data.frame(x)) {
          by[,2]<-factor(by[,2], labels=highlightLabels)
        } else {
          if(subgroup==FALSE) {
            by[,2]<-factor(by[,2], labels=highlightLabels)
          } else if(ncol(by)>=3) {
            by[,3]<-factor(by[,3], labels=highlightLabels)
          }
        }
      }
    }
  }
  statsData<-list(p.value=NA,test="none",results=NA)

  if(grepl("ball",errorCap,ignore.case = TRUE)) {
    capWidth<-capWidth*6
  }

  #If fill colors are needed to distinguish groups but are of length 1, point colors will be used if it has more levels.
  if(length(plotColors$fill)<=1 & length(plotColors$lines)<=1 & length(plotColors$points)>1 & subgroup==T) {
    plotColors$fill<-plotColors$points
  }

  #To handle the fact the range is actually two different functions, upper and lower error bars are assigned separately
  upperErrorFun<-errFun[1]
  lowerErrorFun<-errFun[1]
  aggFun<-aggFun[1]
  if(upperErrorFun[1]=="range"){
    upperErrorFun<-"max"
    lowerErrorFun<-"min"
  }

  #Checking to make sure that the error and aggregator functions are valid
  if(!(aggFun[1] %in% c("mean", "median"))) {
    stop(paste0("The aggFun option needs to be equal to either 'mean' or 'median'.\nCurrently aggFun = ",aggFun,"."))
  }
  if(!(errFun[1] %in% c("sd", "se", "range", "t95ci", "boot95ci"))) {
    stop(paste0("The errFun option needs to be equal to either 'se', 'se', 'range' or 'boot95ci'.\nCurrently errFun = ",aggFun,".\nSee documentation for details."))
  }

  #If we are adding this to an existing plot then we can't count on prepCategoryWindow to log transform the data
  if(add==TRUE) {
    if(logScale>1) {
      prepedData<-list(data=log(x+logAdjustment,logScale))
    } else {
      prepedData<-list(data=x)
    }
  } else {
    prepedData<-x
    #in order to know how to set the window size, we need to preprocess the data
    #if(logScale>1) {prepedData<-log(prepedData+logAdjustment,logScale)}
    if(errFun[1]=="range"){errorMultiple<-1}
    pData<-prepBarData(x=prepedData,by=by,errorMultiple=errorMultiple,upperErrorFun=upperErrorFun,lowerErrorFun=lowerErrorFun,aggFunction=aggFun,stack=FALSE,subgroup=subgroup)
    dRange<-1
    if(errorBars[1]==TRUE) {
      dRange<-c(min(c(min(pData$plot$AData-pData$plot$lowerError),min(x))),max(c(max(pData$plot$AData+pData$plot$upperError),max(x))))
      if(logScale>0 & dRange[1]<.04*(abs(dRange[2]-dRange[1]))) {
        dRange[1]<-.04*(abs(dRange[2]-dRange[1]))
      }
    } else {
      dRange<-c(min(x),max(x))
    }
    dRange[1]<-dRange[1]-.04*(abs(dRange[2]-dRange[1]))
    dRange[2]<-dRange[2]+.04*(abs(dRange[2]-dRange[1]))
    if(!is.null(yLim)){
      dRange<-yLim
    }

    if(is.null(minorGuides)){
      if(guides!=FALSE & logScale > 0) {
        minorGuides<-TRUE
      } else {
        minorGuides<-FALSE
      }
    }

    #RStudio seems not to update the graphics devices properly
    if(Sys.getenv("RSTUDIO") == "1" & is.null(moreOptions[["RSOveride"]])) {graphics.off()}
    prepedData<-prepCategoryWindow(x,by=by, groupNames=groupNames, minorTick=minorTick, guides=guides, plotColors=plotColors, yLim=dRange, rotateLabels=rotateLabels, rotateY=rotateY, trim=trim, logScale=logScale, axisText=axisText, minorGuides=minorGuides, extendTicks=extendTicks, subgroup=subgroup, expLabels=expLabels,sidePlot=sidePlot,subgroupLabels=subgroupLabels,strictLimits=FALSE,theme=theme,legend=legend,logAdjustment=logAdjustment, pointHighlights=pointHighlights)
  }
  pvalue<-NULL
  if(subgroup==TRUE){width<-width*2}
  facetLoc<-NULL

  #Initialize legend variables so we can update based on options
  legendTitle<-"Legend"
  legendColors<-plotColors$points
  legendLabels<-NULL
  if(length(legendColors)<=1 & length(plotColors$fill)>1){
    legendColors<-plotColors$fill
  }

  filter<-rep(TRUE,length(x))
  if(trim>0){
    filter<-quantileTrim(x,trim,na.rm=T,returnFilter=T)[[2]]
    if(is.data.frame(by)) {
      by<-by[filter,]
    } else {
      by<-by[filter]
    }
  }

  #Here we calculated all the data to print
  pData<-prepBarData(x=prepedData[[1]],by=by,errorMultiple=errorMultiple,upperErrorFun=upperErrorFun,lowerErrorFun=lowerErrorFun,aggFunction=aggFun,stack=FALSE,subgroup=subgroup)

  #Data is set and ready to go. Plotting is handled based on cases handling if 'x' and 'by' are vectors or dataframes
  xypos<-c(1,1)

  #Now we just need to perform some slight customization to legend and width options based on inputs.
  if(is.numeric(prepedData[[1]])){
    #CASE: by is a factor data is a numeric vector
    if(is.factor(by)) {
      if(calcType[1]!="none"){
        statsData<-calcStats(prepedData[[1]],by,calcType[1],verbose=verbose)
        pvalue<-statsData$p.value
      }
      legend<-FALSE
      facetLoc<-seq(1,length(groupNames),by=1)
      names(facetLoc)<-groupNames
      width<-.25*width
    } else {
      if(calcType[1]!="none"){
        statsData<-calcStats(prepedData[[1]],by[,1],calcType[1],verbose=verbose)
        pvalue<-statsData$p.value
      }
      #CASE: by is not a factor data is a numeric vector and subgroup is TRUE
      if(subgroup) {
        facetLoc<-facetSpacing(length(levels(by[,2])),length(groupNames))
        names(facetLoc)<-unlist(lapply(levels(by[,1]),FUN=function(x) paste0(x,levels(by[,2]),sep=".")))
        width<-width*(facetLoc[2]-facetLoc[1])/4
      } else {
        #CASE: by is not a factor, data is a numeric vector and subgroup is FALSE
        facetLoc<-seq(1,length(groupNames),by=1)
        names(facetLoc)<-groupNames
        if (length(groupNames)>1) {
          width<-width*(facetLoc[2]-facetLoc[1])/4
        } else {
          width<-width/4
        }
      }
      if(legend!=FALSE) {
        if (subgroup==TRUE & dim(by)[2]>1) {
          if (pointHighlights==TRUE & dim(by)[2]>2) {
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
        } else {
          if(pointHighlights==TRUE) {
            if(legend==TRUE){
              legendTitle<-colnames(by)[2]
            }
            legendLabels<-levels(by[,2])
          } else {
            legend<-FALSE
          }
        }
      }
    }
  } else {
    #CASE: data is a dataframe, by is a factor, subgroup is ignored
    if(is.factor(by)) {
      facetLoc<-facetSpacing(length(prepedData[[1]]),length(groupNames))
      names(facetLoc)<-unlist(lapply(levels(by),FUN=function(y) paste0(y,names(x),sep=".")))
      width<-width*(facetLoc[2]-facetLoc[1])/4
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
      #CASE: data is a dataframe, by is a dataframe, subgroup is ignored
      facetLoc<-facetSpacing(length(prepedData[[1]]),length(groupNames))
      names(facetLoc)<-unlist(lapply(levels(by[,1]),FUN=function(y) paste0(y,names(x),sep=".")))
      width<-width*(facetLoc[2]-facetLoc[1])/4
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
      longX<-purrr::reduce(x,c)
    }
  }
  #Print summary data if indicated
  if(verbose){
    print(pData[[2]])
  }

  #updating preping the plot data from pData to be compatible with errorBars
  if(drawPoints[1]==TRUE) {
    xypos <- addNicePoints(prepedData=prepedData, by=by, filter=filter, sidePlot=sidePlot, subgroup=subgroup, plotAt=facetLoc,pointHighlights=pointHighlights, pointMethod=pointMethod, pointShape=pointShape, pointSize=pointSize, width=width, pointLaneWidth=pointLaneWidth, plotColors=plotColors, drawPoints=drawPoints, outliers=outliers,swarmOverflow = swarmOverflow)
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
  }
  plotThis<-pData[[1]] %>%
    mutate(barHight=.data$AData,width1=.data$at-width*barWidth,width2=.data$at + width*barWidth)
  if(sidePlot[1]==TRUE){
    if(grepl("dot",barType[1],ignore.case = T)) {
      points(x=plotThis$barHight,y=plotThis$at,pch=16,col=plotColors$lines, cex=lWidth*barThickness)
    } else if (grepl("bar",barType[1],ignore.case = T)) {
      segments(x0=plotThis$barHight,y0 = plotThis$width1,x1 = plotThis$barHight,y1 = plotThis$width2,col = plotColors$lines, lwd=lWidth*barThickness)
    }
  } else {
    if(grepl("dot",barType[1],ignore.case = T)) {
      points(x=plotThis$at,y=plotThis$barHight,pch=16,col=plotColors$lines, cex=lWidth*barThickness)
    } else if (grepl("bar",barType[1],ignore.case = T)) {
      segments(x0=plotThis$width1,y0 =plotThis$barHight ,x1 =plotThis$width2, y1 =plotThis$barHight,col = plotColors$lines, lwd=lWidth*barThickness)
    }
  }
  if(errorBars[1]==TRUE) {
    if(errorMultiple>0) {
      errorBars(data.frame(at=plotThis$at,start=plotThis$barHight,stop=plotThis$barHight+plotThis$upperError),capType = errorCap,lType = errorLineType,side=sidePlot,capSize=barWidth*capWidth,col=plotColors$lines,width=lWidth)
      errorBars(data.frame(at=plotThis$at,start=plotThis$barHight,stop=plotThis$barHight-plotThis$lowerError),capType = errorCap,lType = errorLineType,side=sidePlot,capSize=barWidth*capWidth,col=plotColors$lines, width=lWidth)
    }
  }
  #Draw legend and set associated options if indicated
  if(length(legendColors)<length(legendLabels) & legend!=FALSE){
    legend<-FALSE
    warning("Not enough point colors to uniquely color factor levels\nPlease update plotColors point options to use legend options.", call.=FALSE)
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
    makeNiceLegend(labels=legendLabels, title=legendTitle, fontCol=plotColors$labels, border=theme$legendBorder, lineCol=plotColors$LegendLineCol, bg=plotColors$LegendBG, col=legendColors, shape="rect",size=theme$legendSize,spacing=theme$legendSpacing)
  }

  #Add titles, sub and ylab
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
  #formating the output list and setting class int npData
  dataOut<-list(summary=pData[[2]],stats=statsData,plotType="dot",options=ActiveOptions)
  class(dataOut)<-c("npData","list")

  invisible(dataOut)
}

#' @export
niceDots.npData <- function(x,  ...) {
  clOptions<-list(...)
  for(opt in names(clOptions)) {
    if(is.null(x$options[opt])){
      append(x$options,list(opt=clOptions[[opt]]))
    }else{
      x$options[[opt]]<-clOptions[[opt]]
    }
  }
  dataOut<-do.call("niceDots",x$options)
  invisible(dataOut)
}


