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
#' @export
#' @seealso \code{\link[vioplot]{vioplot}}, \code{\link{boxplot}}, \code{\link{niceBox}}, \code{\link[beeswarm]{beeswarm}}, \code{\link{prepCategoryWindow}}
niceBar <- function(x, by=NULL, groupNames=NULL, aggFun=c("mean","median","none"),errFun=c("sd","se","range"), theme=basicTheme, legend=FALSE, stack=FALSE, main=NULL,sub=NULL, ylab=NULL, minorTick=FALSE, guides=TRUE, outliers=FALSE, width=NULL, errorMultiple=1, plotColors=list(bg="open",fill=setAlpha("grey",.8)), logScale=FALSE, trim=FALSE, axisText=c(NULL,NULL), showCalc=FALSE, calcType="none", yLim=NULL, rotateLabels=FALSE, rotateY=TRUE, add=FALSE, minorGuides=NULL, extendTicks=TRUE, subGroup=FALSE, subGroupLabels=NULL, expLabels=FALSE, sidePlot=FALSE, errorBars=TRUE, errorCap="ball", errorLineType=1,capWidth=1.2, lWidth=1.5, na.rm=FALSE, flipFacts=FALSE, verbose=FALSE,logAdjustment=1, ...) {UseMethod("niceBar",x)}

#' @import dplyr
#' @export

niceBar.default <- function(x, by=NULL, groupNames=NULL, aggFun=c("mean","median"),errFun=c("sd","se","range", "t95ci", "boot95ci"), theme=basicTheme, legend=FALSE, stack=FALSE, main=NULL,sub=NULL, ylab=NULL, minorTick=FALSE, guides=NULL, outliers=FALSE, width=NULL, errorMultiple=1, plotColors=NULL, logScale=FALSE, trim=FALSE, axisText=c(NULL,NULL), showCalc=FALSE, calcType="none", yLim=NULL, rotateLabels=FALSE, rotateY=TRUE, add=FALSE, minorGuides=NULL, extendTicks=TRUE, subGroup=FALSE, subGroupLabels=NULL, expLabels=FALSE, sidePlot=FALSE, errorBars=TRUE, errorCap=NULL, errorLineType=NULL,capWidth=NULL, lWidth=NULL, na.rm=FALSE, flipFacts=FALSE, verbose=FALSE,logAdjustment=1, ...) {
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

  #To handle the fact the range is actually two different functions, upper and lower error bars are assined separately
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
    if(logScale>1) {prepedData<-log(prepedData+logAdjustment,logScale)}
    pData<-prepBarData(x=prepedData,by=by,errorMultiple=errorMultiple,upperErrorFun=upperErrorFun,lowerErrorFun=lowerErrorFun,aggFunction=aggFun,stack=stack,subGroup=subGroup)

    #If all aggregated values are >= 0 then we want to interect the y-axis exactly at zero
    dmin<-min(pData$plot$AData)
    dRange<-c(min(pData$plot$AData-pData$plot$lowerError),max(pData$plot$AData+pData$plot$upperError))
    bVal<-0
    if(!is.null(yLim)){
      dRange<-yLim
      bVal<-yLim[1]
    } else {
      if(dmin >=0){
        dRange[2]<-dRange[2]*1.04 #this is to give a little padding on the top
        dRange[1]<-bVal
      }
    }

    if(is.null(minorGuides)){
      if(guides!=FALSE & logScale > 0) {
        minorGuides<-TRUE
      } else {
        minorGuides<-FALSE
      }
    }

    #bVal is the base of the rectangles
    if(!is.null(yLim)){bVal<-yLim[1]}

    #Again, if all aggregated values are above 0 we want to intersect the y-axis at zero with no padding
    strictBase<-TRUE
    if(dmin<0){
      strictBase<-FALSE
    }
    prepedData<-prepCategoryWindow(x,by=by, groupNames=groupNames, minorTick=minorTick, guides=guides, plotColors=plotColors, yLim=dRange, rotateLabels=rotateLabels, rotateY=rotateY, trim=trim, logScale=logScale, axisText=axisText, minorGuides=minorGuides, extendTicks=extendTicks, subGroup=subGroup, expLabels=expLabels,sidePlot=sidePlot,subGroupLabels=subGroupLabels,strictLimits=strictBase,theme=theme,legend=legend,logAdjustment=logAdjustment)
  }
  pvalue<-NULL
  if(subGroup==TRUE){width<-width*2}

  #Initialize legend variables so we can update based on options
  legendTitle<-"Legend"
  legendLabels<-NULL
  legendColors<-plotColors$fill

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
  pData<-prepBarData(x=prepedData[[1]],by=by,errorMultiple=errorMultiple,upperErrorFun=upperErrorFun,lowerErrorFun=lowerErrorFun,aggFunction=aggFun,stack=stack,subGroup=subGroup)

  #Now we just need to perform some slight customizations to legend and width options based on inputs.
  if(is.numeric(prepedData[[1]])){
    #CASE: by is a factor data is a numeric vector
    if(is.factor(by)) {
      if(calcType[1]!="none"){pvalue<-calcStats(prepedData[[1]],by,calcType[1],verbose=verbose)}
      legend<-FALSE
      width<-.25*width
    } else {
      if(calcType[1]!="none"){pvalue<-calcStats(prepedData[[1]],by[,1],calcType[1],verbose=verbose)}
      #CASE: by is not a factor data is a numeric vector and subGroup is TRUE
      if(subGroup) {
        facetLoc<-facetSpacing(length(levels(by[,2])),length(groupNames))
        width<-width*(facetLoc[2]-facetLoc[1])/4
      } else {
        #CASE: by is not a factor, data is a numeric vector and subGroup is FALSE
        facetLoc<-seq(1,length(groupNames))
        width<-width*(facetLoc[2]-facetLoc[1])/4
      }
      if(legend!=FALSE) {
        if(stack==TRUE){
          if(legend==TRUE){
            if(subGroup==TRUE) {
              legendTitle<-colnames(by)[3]
            } else {
              legendTitle<-colnames(by)[2]
            }
          }
          if(subGroup==TRUE) {
            legendLabels<-levels(by[,3])
          } else {
            legendLabels<-levels(by[,2])
          }
        } else if (subGroup==TRUE) {
          if(legend==TRUE){
            legendTitle<-colnames(by)[2]
          }
          legendLabels<-levels(by[,2])
        } else {
          legend<-FALSE
        }
      }
    }
  } else {
    #CASE: data is a dataframe, by is a factor, subGroup is ignored, Stack is ignored
    if(is.factor(by)) {
      facetLoc<-facetSpacing(length(prepedData[[1]]),length(levels(by)))
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
      #CASE: data is a dataframe, by is a dataframe, subGroup is ignored
      facetLoc<-facetSpacing(length(prepedData[[1]]),length(levels(by[,1])))
      width<-width*(facetLoc[2]-facetLoc[1])/4
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
  #Print summary data if indicated
  if(verbose){
    print(pData[[2]])
  }
  #updating preping the plot data fromp pData to be compatible with drawBar
  pData[[1]] %>%
    mutate(yb=bVal,UpperError=.data$upperError, LowerError=.data$lowerError,yt=.data$AData) %>%
    drawBar(plotColors=plotColors, errorBars=errorBars, errorCap=errorCap, errorLineType=errorLineType, width=width, sidePlot=sidePlot, stacked=stack, capSize=capWidth, lineWidth=lWidth)

  #Draw legend and set associated options if indicated
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

  #Add titles, sub and ylab
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
  dataOut<-list(data=data.frame(prepedData$data,by),summary=pData[[2]],stats=pvalue)
  invisible(dataOut)
}
