#' @include np_options_processing.R np_utility.R
#' @title format a log scale axis
#' @description
#' Generates the location and labels for the major tick marks for a given log base transformation along with optional minor tick mark location.
#'
#' @details
#' Base R does not have great visual queues to indicate when data is being plotted in log scale. This is a simple function takes the min and max of the untransformed data and  uses \code{\link[grDevices]{axisTicks}} from base R to determine the location of the major tick marks in the new scale. To better indicate that the graph is on a log scale, the major tick-marks are labeled in the untransformed values or expressed in as \eqn{logScale^{x}}{logScale^x} when \code{expLabels=\link{TRUE}}. The minor tick marks are drawn equidistant from each other between the major tick marks in the untransformed scale giving them shrinking appearance when rendered in log scale coordinates. This can help helps with the interpretation of data within the log scale and adds another visual indication that the data has been transformed. The value of \code{minorCount} gives number of minor ticks to be drawn between each pair of major tick-marks. \code{axisText} allows for symbols or units such as '%' to be prepend or appended to the labels (eg. \code{axisText=c("","%")}).
#'It is worth stressing again that the input values to dataRange are assumed to be raw values prior to log transformation. If log transformed values are given, the axis will be drawn correctly.
#'
#' @param dataRange numeric; a numeric vector with the min and max values for the data set prior to log transformation.
#' @param minorCount positive integer; the number of minor tick marks to be drawn between each major tick.
#' @param logScale numeric; the logarithm base to use for the log scale transformation.
#' @param axisText character; a length two character vector containing text to be prepend or append to the major tick labels, respectively.
#' @param expLabels logical; if set to \code{\link{TRUE}}, the major labels will written as \eqn{logbase^{x}}{logbase^x}. Otherwise the labels will correspond to the non-transformed values at that point.
#' @param logAdjustment numeric; a number added to each value prior to log trasformation. Defaults value is 1.
#'
#' @return a list with the following elements: major tick marks locations [[1]], major tick labels [[2]], minor tick mark locations [[3]].
#' @examples
#' plot(1:10,log(1:10,2),yaxt="n",ylab="")
#' \donttest{majorTicks<-makeLogTicks(c(0,10),minorCount= 4,logScale=2, axisText=c("","mg"), expLabels=TRUE)}
#' \donttest{axis(side=2,lab=majorTicks[[2]],at=majorTicks[[1]],las=2)}
#' \donttest{axis(side = 2, at = majorTicks[[3]], labels = FALSE, tcl = -0.2)}
#' @importFrom grDevices axisTicks
#' @seealso \code{\link[grDevices]{axisTicks}}, \code{\link[graphics]{axis}}, \code{\link{prepCategoryWindow}}
makeLogTicks<-function(dataRange,minorCount=10,logScale=2,axisText=c(NULL,NULL), expLabels=TRUE, logAdjustment=1) {
  majorLoc<-axisTicks(log(dataRange+logAdjustment, logScale),log=F,nint=5)
  preText<-axisText[1]
  postText<-axisText[2]
  transformed<-format(vapply(majorLoc,FUN=function(x) logScale^x,FUN.VALUE=numeric(1)), scientific=F, digits=2)
  majorLabels<-paste0(axisText[1],transformed, axisText[2])
  if(expLabels){
    #note: potentially problematic but no vapply available for class expression()
    majorLabels<-sapply(majorLoc,FUN=function(x) as.expression(bquote(.(preText)*.(logScale)^.(x)*.(postText))))
  }
  minorLoc<-NULL
  if(minorCount>0) {
    for(i in 1:(length(majorLoc)-1)){
      scale<-c(logScale^(majorLoc[i]),logScale^(majorLoc[i+1]))
      minorLoc<-c(minorLoc,seq(scale[1],scale[2],by=(scale[2]-scale[1])/(minorCount+1)))
    }
    minorLoc<-log(minorLoc, logScale)
  }
  list(majorLoc, majorLabels,minorLoc)
}


#' @title Set Legend Margins
#' @description
#' Calculate margin needed to plot a NicePlots legend
#'
#' @details
#' This is a utility function used by the window setup routines to calculate how much space the legend will take up in the margin and setting the \code{par(mai)} setting accordingly.
#'
#' @param x numeric; a numeric vector with the min and max values for the data set prior to log transformation.
#' @param by positive integer; the number of minor tick marks to be drawn between each major tick.
#' @param theme numeric; the logarithm base to use for the log scale transformation.
#' @param pointHighlights character; a length two character vector containing text to be prepend or append to the major tick labels, respectively.
#' @param subGroup logical; if set to \code{\link{TRUE}}, the major labels will written as \eqn{logbase^{x}}{logbase^x}. Otherwise the labels will correspond to the non-transformed values at that point.
#' @param legend character; Title for the legend collumn. Set to \code{\link{TRUE}} if no header is desired.
#'
#' @return Does not return a value but changes the global \code{par(mai)} settings.
#' @examples
#' TODO<-1
#' @import dplyr
#' @importFrom purrr map_dbl
prepLegendMarigins<-function(x,by,theme,legend,pointHighlights=FALSE,subGroup=TRUE){
  legendIndex<-NA
  legendTitle<-""
  legendLevels<-NULL
  oMai<-par()$mai
  oFont<-par()$family
  if(!is.na(theme[1]) & !is.null(theme[1])){
    par(family=theme$fontFamily)
  }
  if(!is.na(theme[1])){
    legendSize<-theme$LegendSize
  }
  if(legend!=FALSE) {
    maxLabelW<-0
    maxLabelH<-0
    if(pointHighlights==FALSE & subGroup==TRUE) {
      legendIndex<-2
    } else if(pointHighlights==TRUE & subGroup==TRUE) {
      legendIndex<-3
    } else if(pointHighlights==TRUE & subGroup==FALSE) {
      legendIndex<-2
    }
    if(is.data.frame(x)){
      if(is.data.frame(by) & pointHighlights==TRUE){
        legendTitle<-colnames(by)[2]
        legendLevels<-levels(by[,2])
      } else {
        legendTitle<-"Legend"
        legendLevels<-colnames(x)
      }
    } else {
      if(is.data.frame(by)){
        if(dim(by)[2]>=legendIndex) {
          legendTitle<-colnames(by)[legendIndex]
          legendLevels<-levels(by[,legendIndex])
        } else {
          warning(paste0("Warning: Unable to determine level which factor to use for legend.\nExpected ",legendIndex," columns for by but only found ",dim(by)[2],".\nProceeding  using the 2nd column of by."), call.=FALSE)
          legendTitle<-colnames(by)[2]
          legendLevels<-levels(by[,2])
        }
      } else {
        legendTitle<-"Legend"
        legendLevels<-levels(by)
      }
    }
    if(!(is.na(legend) | is.null(legend) | legend==TRUE)) {
      legendTitle<-legend
    }
    maxLabelW<-purrr::map_dbl(legendLevels,strwidth,cex=legendSize,units="in") %>% max()
    titleW<-strwidth(legendTitle,font=2,cex=legendSize,units="in")
    if(titleW>maxLabelW){maxLabelW<-titleW}
    maxLabelH<-purrr::map_dbl(legendLevels, strheight,cex=legendSize,units="in") %>% max()
    titleH<-strheight(legendTitle,font=2,cex=legendSize,units="in")
    nMai<-oMai
    nMai[4]<-nMai[4]+maxLabelW
    par(mai=nMai,family=oFont)
  }
}

#' @title prepare a plotting environment for categorical data such as bar plots or box plots
#' @description
#' takes untransformed data and draws the x and y axis with support for subgrouping data within factors, log transformation and outlier trimming.
#'
#' @details
#' This function does all the hard work of setting up the x and y axis for plotting as well as optionally log transforming and/or trimming the data of outliers. In particular, it adds much more robust support for plotting of log transformed data and subgrouping of primary vectors. Other features include the addition of both major and minor guidelines, support for horizontal plotting and improved label formatting options.
#'
#' @inheritParams formatPlotColors
#' @param x numeric vector or data frame; The input to \code{prepCategoryWindow} can be a numeric vector a  data frame of numeric vectors.
#' @param by factor or data frame of factors; used as the primary grouping factor and the factor levels will be used as group names if \code{groupNames} is not specified. If \code{by} is a data frame and \code{subGroup=\link{TRUE}}, the second column is assumed to be a secondary grouping factor, breaking out the data into sub-categories within each major group determined by the levels of the first column.
#' @param groupNames character vector; overrides the factor levels of \code{by} to label the groups
#' @param minorTick positive integer; number of minor tick-marks to draw between each pair of major ticks-marks.
#' @param guides logical; will draw guidelines at the major tick-marks if set to \code{\link{TRUE}}. Color of the guidelines is determined by \code{plotColors$guides}.
#' @param yLim numeric vector; manually set the limits of the plotting area (eg. \code{yLim=c(min,max)}). Used to format the y-axis by default but will modify the x-axis if \code{side=\link{TRUE}}.
#' @param rotateLabels logical; sets \code{las=2} for the x-axis category labels. Will affect y-axis if \code{side=\link{TRUE}}. Note that this may not work well if long names or with subgrouped data.
#' @param rotateY logical; sets \code{las=2} for the y-axis major tick-mark labels. Will affect x-axis if \code{side=\link{TRUE}}.
#' @param trim positive numeric; passed to \code{threshold} argument of \code{\link{quantileTrim}} if any data points are so extreme that they should be removed before plotting and downstream analysis. Set to \code{\link{FALSE}} to disable.
#' @param logScale positive numeric; the base for the for log scale data transformation calculated as \code{log(x+1,logScale)}.
#' @param axisText character; a length two character vector containing text to be prepended or appended to the major tick labels, respectively.
#' @param minorGuides logical; draws guidelines at minor tick-marks
#' @param extendTicks logical; extends minor tick-marks past the first and last major tick to the edge of the graph provided there is enough room. Works for both log-scale and regular settings.
#' @param subGroup logical; use additional column in \code{by} to group the data within each level of the major factor.
#' @param expLabels logical; prints the major tick labels is \eqn{logScale^{x}}{logScale^x} instead of the raw value
#' @param sidePlot logical; switches the axis to plot horizontally instead of vertically.
#' @param subGroupLabels character vector; sets the labels used for the \code{subGroup} factor. Defaults to the levels of the factor.
#' @param strictLimits logical; eliminates padding on the value axis so 0 can be flush with the x-axis. Defaults to \code{\link{FALSE}}.
#' @param legend logical/character; Draw a legend in the plot margins. If a character string is given it will overide the factor name default for the legend title.
#' @param pointHighlights logical; Is pointHightlights turned on? This is used to determin with column of \code{by} should be used for legend factor levels.
#' @param logAdjustment = numeric; This number is added to the input data prior to log transformation. Default value is 1.
#'
#' @return formats the plotting area and returns a named list with 'data' and 'labels' corresponding to the trimmed and/or transformed data and the labels for the primary factors, respectively.
#' @examples
#'	todo<-1
#'
#' @import graphics
#' @import grDevices
#' @import dplyr
#' @importFrom purrr map_dbl
#' @importFrom utils data str
#'
#' @seealso \code{\link[grDevices]{axisTicks}}, \code{\link[graphics]{axis}}, \code{\link{makeLogTicks}}, \code{\link{facetSpacing}}
prepCategoryWindow<-function(x,by=NULL, groupNames=levels(by), minorTick=FALSE, guides=TRUE, yLim=NULL, rotateLabels=FALSE, rotateY=TRUE, theme=NA, plotColors=if(is.na(theme)){list(bg="open",guides="black",lines="gray22",points="darkgrey",fill="white")}else{theme$plotColors}, trim=FALSE, logScale=FALSE, axisText=c(NULL,NULL), minorGuides=FALSE, extendTicks=F,subGroup=FALSE, expLabels=TRUE,sidePlot=FALSE,subGroupLabels=NULL,strictLimits=F, legend=FALSE, pointHighlights=FALSE, logAdjustment=1) {
  levelCount<-1
  tData<-x
  tBy<-by
  plotColors<-formatPlotColors(plotColors)
  oMai<-par()$mai
  cFont<-par()$family
  if(!is.na(theme[1])) {
    par(family=theme$fontFamily)
  }

  #Set margins for legends now
  prepLegendMarigins(x=x,by=by,theme=theme,legend=legend,pointHighlights=pointHighlights,subGroup=subGroup)

  #capture data range for plot formating
  dataRange<-NULL
  if(is.null(yLim)==FALSE) {
    dataRange<-yLim
  } else {
    dataRange<-range(x)
    if(strictLimits){
      dataRange[1]<-0
      dataRange[2]<-dataRange[2]*1.05
    }
  }
  if(trim>0) {
    if(is.numeric(x)){
      tData<-quantileTrim(x,threshold=trim,na.rm=T)
    } else if(is.data.frame(x)){
      tData<-apply(x,2,quantileTrim,trim,T)
    } else {
      stop(paste0("Non-numeric input passed to function.\nData structure:\n",str(x)))
    }
    if(is.null(yLim)==FALSE) {
      dataRange<-yLim
    } else {
      dataRange<-range(tData)
      if(strictLimits){
        dataRange[1]<-0
        dataRange[2]<-dataRange[2]*1.05
      }
    }
  }
  majorTicks<-NULL
  if(logScale>1){
    if(dataRange[1]<0){
      stop(paste0("Error: you can not log scale numbers less than or equal to zero\nLowest number detected: ",dataRange[1]))
    }
    majorTicks<-makeLogTicks(dataRange,minorCount= minorTick,logScale=logScale, axisText=axisText, expLabels=expLabels, logAdjustment=logAdjustment)
    tData <-log(x +logAdjustment,logScale)
    if(is.null(yLim)==FALSE) {
      dataRange<-log(yLim+logAdjustment,logScale)
    } else {
      dataRange<-range(tData)
      if(strictLimits){
        dataRange[1]<-0
        dataRange[2]<-dataRange[2]*1.05
      }
    }

    if(trim>0) {
      if(is.numeric(x)){
        tData<-log(quantileTrim(x,threshold=trim,na.rm=T)+logAdjustment,logScale)
      } else if(is.data.frame(x)){
        tData<-apply(x,2,function(y) {log(quantileTrim(y,threshold=trim,na.rm=T)+logAdjustment,logScale)})
      } else {
        stop(paste0("Non-numeric input passed to function.\nData structure:\n",str(x)))
      }
      if(is.null(yLim)==FALSE) {dataRange<-log(yLim+logAdjustment,logScale)}
      else {
        dataRange<-range(tData)
        if(strictLimits){
          dataRange[1]<-0
          dataRange[2]<-dataRange[2]*1.05
        }
        majorTicks<-makeLogTicks(c(logScale^dataRange[1] -1,logScale^dataRange[2] -1),minorCount= minorTick,logScale=logScale, axisText=axisText, expLabels=expLabels, logAdjustment=logAdjustment)
      }
    }
  }
  if(!is.null(by)){
    if(is.data.frame(by)) {
      levelCount<-length(levels(factor(by[,1])))
      if(is.null(groupNames)){groupNames<-levels(factor(by[,1]))}
    } else {
      levelCount<-length(levels(factor(by)))
      if(is.null(groupNames)){groupNames<-levels(factor(by))}
    }
  }
  if (is.null(groupNames)) {groupNames<-seq(1:levelCount)}
  oBg<-par("bg")
  par(bg=plotColors$marginBg)
  plot.new()
  if(sidePlot) {
    if(strictLimits) {
      plot.window(ylim=c(.5,levelCount+0.5),xlim=dataRange, xaxs="i")
    } else {
      plot.window(ylim=c(.5,levelCount+0.5),xlim=dataRange)
    }
  } else {
    if(strictLimits) {
      plot.window(xlim=c(.5,levelCount+0.5),ylim=dataRange,yaxs="i")
    } else {
      plot.window(xlim=c(.5,levelCount+0.5),ylim=dataRange)
    }
  }
  if(plotColors$bg=="open" | plotColors$bg=="Open") {
    abline(v=par("usr")[1],lwd=2.5,col=plotColors$axis)
    abline(h=par("usr")[3],lwd=2.5,col=plotColors$axis)
  } else {
    rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col=plotColors$bg, lwd=2.5,border=plotColors$axis)
  }
  par(bg=oBg)
  if(rotateLabels!=0){rotateLabels<-2}
  if(rotateY!=0){rotateY <-2}
  myLabels<-NULL
  whichSide<-1
  groupCex<-1
  subGroupCex<-.66
  groupLine<-.9
  subGroupLine<-.25
  if(!is.na(theme[1])){
    groupCex<-theme$groupLabSize
    subGroupCex<-theme$subGroupLabSize
    groupLine<-theme$groupLabelSpacing
    subGroupLine<-theme$subgroupLabelSpacing
  }
  sideGroupLine<-groupLine+.33
  sideSubGroupLine<-subGroupLine+.33
  axisNumCol<-plotColors$label
  if(!is.na(theme[1])){
    if(is.numeric(theme$groupLabSize)){
      groupCex<-theme$groupLabSize
    }
    if(is.numeric(theme$subGroupLabSize)) {
      subGroupCex<-theme$subGroupLabSize
    }
  }
  if(is.data.frame(x)) {
    subLabLoc<-facetSpacing(length(x),length(groupNames))
    if(is.null(subGroupLabels)){subGroupLabels<-names(x)}
    if(sidePlot) {
      if(legend==FALSE | pointHighlights==TRUE) {
        axis(side=2,at=seq(1:levelCount),labels=F,las=rotateLabels,lwd=0,col=plotColors$axis,col.ticks=plotColors$majorTick,cex.axis=groupCex)
        mtext(side=2, at=seq(1:levelCount), text=groupNames,las=rotateLabels, col=plotColors$labels,line=sideGroupLine ,cex=groupCex)
        axis(side=2,at=subLabLoc,labels=F,lwd=0,lwd.ticks=1,cex.axis=subGroupCex,col=plotColors$axis,col.ticks=plotColors$majorTick)
        mtext(side=2,at=subLabLoc,text=rep(subGroupLabels,length(groupNames)),cex=subGroupCex, line=sideSubGroupLine, col=plotColors$subGroupLabels)
      } else {
        axis(side=2,at=seq(1:levelCount),labels=F,las=rotateLabels,lwd=0,lwd.ticks=1,col=plotColors$axis,col.ticks=plotColors$majorTick,cex.axis=groupCex)
        mtext(side=2,at=seq(1:levelCount),line=groupLine, text=groupNames,las=rotateLabels, col=plotColors$labels,cex=theme$groupLabSize)
      }
    } else {
      if(legend==FALSE | pointHighlights==TRUE) {
        axis(side=1,at=seq(1:levelCount),labels=FALSE,las=rotateLabels,lwd=0,col=plotColors$axis,col.ticks=plotColors$majorTick,cex.axis=groupCex)
        mtext(side=1,at=seq(1:levelCount),text = groupNames,las=rotateLabels, col=plotColors$labels,line=groupLine ,cex=groupCex)
        axis(side=1,at=subLabLoc,labels=F,lwd=0,lwd.ticks=1,cex.axis=subGroupCex,col=plotColors$axis,col.ticks=plotColors$majorTick,cex.axis=theme$subGroupLabSize)
        mtext(text=rep(subGroupLabels,length(groupNames)),at=subLabLoc,side=1,line=subGroupLine,col=plotColors$subGroupLabels, cex=subGroupCex)
      } else {
        axis(side=1,at=seq(1:levelCount),labels=F,las=rotateLabels,lwd=0,lwd.ticks=1,col=plotColors$axis,col.ticks=plotColors$majorTick,cex.axis=groupCex)
        mtext(side=1,at=seq(1:levelCount),line=groupLine-.15,text=groupNames,cex=theme$groupLabSize,col=plotColors$labels,las=rotateLabels)
      }
      whichSide<-2
    }
  } else if(subGroup==TRUE & is.data.frame(by)) {
    subLabLoc<-facetSpacing(length(levels(by[,2])),length(groupNames))
    if(is.null(subGroupLabels)){subGroupLabels<-levels(by[,2])}
    if(sidePlot) {
      if(legend==FALSE | (legend!=FALSE & pointHighlights==TRUE)) {
        axis(side=2,at=seq(1:levelCount),labels=F,las=rotateLabels,lwd=0,col=plotColors$axis,col.ticks=plotColors$majorTick,cex.axis=groupCex)
        mtext(side=2,at=seq(1:levelCount),text=groupNames,las=rotateLabels,line=sideGroupLine, col=plotColors$labels,cex=groupCex)
        axis(side=2,at=subLabLoc,labels=F,lwd=0,lwd.ticks=1,col=plotColors$axis,col.ticks=plotColors$majorTick,cex.axis=subGroupCex)
        mtext(side=2,at=subLabLoc,text=rep(subGroupLabels,length(groupNames)),line=sideSubGroupLine, cex=subGroupCex,col=plotColors$subGroupLabels)
      } else {
        axis(side=2,at=seq(1:levelCount),,labels=F,las=rotateLabels,lwd=0,lwd.ticks=1,col=plotColors$axis,col.ticks=plotColors$majorTick,cex.axis=groupCex)
        mtext(side=2,at=seq(1:levelCount),line=groupLine,text=groupNames,las=rotateLabels,cex=groupCex,col=plotColors$labels)
      }
    } else {
      if(legend==FALSE | (legend!=FALSE & pointHighlights==TRUE)) {
        axis(side=1,at=seq(1:levelCount),labels=F,las=rotateLabels,lwd=0,col=plotColors$axis,col.ticks=plotColors$majorTicks,cex.axis=groupCex)
        mtext(side=1,at=seq(1:levelCount),text=groupNames,las=rotateLabels,line=groupLine,col=plotColors$labels, cex=groupCex)
        axis(side=1,at=subLabLoc,labels=F,lwd=0,lwd.ticks=1,cex.axis=theme$subGroupLabSize,col=plotColors$axis,col.ticks=plotColors$majorTick,cex.axis=subGroupCex)
        mtext(side=1,at=subLabLoc,text=rep(subGroupLabels,length(groupNames)),line=subGroupLine,cex=subGroupCex,col=plotColors$subGroupLabels)
      } else {
        axis(side=1,at=seq(1:levelCount),labels=F,las=rotateLabels,lwd=0,lwd.ticks=1,col=plotColors$axis,col.ticks=plotColors$majorTick,cex.axis=groupCex)
        mtext(side=1,line=groupLine-.15,at=seq(1:levelCount),text=groupNames,las=rotateLabels,col=plotColors$labels,cex=groupCex)
      }
      whichSide<-2
    }
  } else {
    if(sidePlot) {
      axis(side=2,at=seq(1:levelCount),labels=F,las=rotateLabels,lwd=0,lwd.ticks=1,col=plotColors$axis,col.ticks=plotColors$majorTick,cex.axis=groupCex)
      mtext(side=2,at=seq(1:levelCount),line=groupLine,text=groupNames,las=rotateLabels,col=plotColors$labels,cex=groupCex)
    } else {
      axis(side=1,at=seq(1:levelCount),labels=F,las=rotateLabels,lwd=0,lwd.ticks=1,col=plotColors$axis,col.ticks=plotColors$majorTick,cex.axis=groupCex)
      mtext(side=1,at=seq(1:levelCount),line=groupLine-.15,text=groupNames,las=rotateLabels,col=plotColors$labels,cex=groupCex)
      whichSide<-2
    }
  }
  #Formating the numeric axis and making sure it fits withing the margins.
  #If it is too long, the axis cex is lowered untill .6 at which point it switches to scientific notation
  labelCex<-.9
  if(!is.na(theme[1])){
    if(is.numeric(theme$yAxisLabSize)){
      labelCex<-theme$yAxisLabSize
    }
  }
  myMajorTicks<-axTicks(side=whichSide)
  myLabels<-paste0(axisText[1],axTicks(side=whichSide), axisText[2])
  if(logScale>0){
    myLabels<-majorTicks[[2]]
    myMajorTicks<-majorTicks[[1]]
  }
  maxLabelLength<-purrr::map_dbl(myLabels,strwidth,units="in",cex=labelCex) %>% max()
  if(par("mai")[whichSide]*.6< maxLabelLength){
    if(par("mai")[whichSide]*.6/maxLabelLength<.6){
      if(logScale>0){
        myLabels<-paste0(axisText[1],format(as.numeric(majorTicks[[2]]),scientific = TRUE,digits = 3), axisText[2])
      } else {
        myLabels<-paste0(axisText[1],format(as.numeric(axTicks(side=whichSide)),scientific = TRUE,digits = 3), axisText[2])
      }
    } else {
      if(par("mai")[whichSide]*.6/maxLabelLength <labelCex) {
        labelCex<-par("mai")[whichSide]*.6/maxLabelLength
      }
    }
  }

  if (minorTick > 0) {
    lowerLim<-par("usr")[3]
    upperLim<-par("usr")[4]
    if(sidePlot){
      lowerLim<-par("usr")[1]
      upperLim<-par("usr")[2]
    }
    minorLoc<-NULL
    if(logScale>0){
      minorLoc<-majorTicks[[3]]
      if(extendTicks) {
        delta<-myMajorTicks[2]-myMajorTicks[1]
        cBound<-logScale^(min(myMajorTicks)-delta)
        cBy<-(logScale^min(myMajorTicks)-cBound)/(minorTick+1)
        tempTick<-seq(cBound,logScale^min(myMajorTicks),by=cBy)
        minorLoc <-c(minorLoc,log(tempTick[which(tempTick>1 & tempTick>logScale^(lowerLim))],logScale))
        cBound<-logScale^(max(myMajorTicks)+delta)
        cBy<-(cBound-logScale^max(myMajorTicks))/(minorTick+1)
        tempTick<-seq(logScale^max(myMajorTicks),cBound,by=cBy)
        minorLoc <-c(minorLoc,log(tempTick[which(tempTick>1 & tempTick<logScale^(upperLim))],logScale))
      }
    } else {
      for(i in 1:(length(myMajorTicks)-1)){
        minorLoc<-c(minorLoc,seq(myMajorTicks[i], myMajorTicks[i+1],length.out= minorTick + 2)[2:(minorTick+1)])
      }
      if (extendTicks) {
        minorLoc<-c(minorLoc,seq(min(myMajorTicks), lowerLim,by=(minorLoc[1]-minorLoc[2])))
        minorLoc<-c(minorLoc,seq(max(myMajorTicks), upperLim,by=(minorLoc[2]-minorLoc[1])))
      }
    }
    if(minorGuides != FALSE){
      if(sidePlot) {
        abline(v= minorLoc[minorLoc!=lowerLim],lwd=.33,col=plotColors$minorGuides)
      } else {
        abline(h= minorLoc[minorLoc!=lowerLim],lwd=.33,col=plotColors$minorGuides)
      }
    }
    if(sidePlot) {
      axis(side = 1, at = minorLoc, labels = FALSE, tcl = -0.2,col=plotColors$axis ,col.ticks=plotColors$minorTick)
    } else {
      axis(side = 2, at = minorLoc, labels = FALSE, tcl = -0.2,col=plotColors$axis,col.ticks=plotColors$minorTick)
    }
  }
  clCex<-par()$cex.axis
  par(cex.axis=labelCex)
  if (sidePlot) {
    axis(side=1,labels=myLabels,at=myMajorTicks,las=rotateY,col=plotColors$axis,col.axis=axisNumCol,col.ticks=plotColors$majorTick)
  } else {
    axis(side=2,labels=myLabels,at= myMajorTicks,las=rotateY,col=plotColors$axis,col.axis=axisNumCol,col.ticks=plotColors$majorTick)
  }
  par(cex.axis=clCex)
  if(guides[1]!=FALSE){
    if(sidePlot) {
      abline(v=myMajorTicks[myMajorTicks!=par("usr")[1]],col=plotColors$guides,lwd=1)
    } else {
      abline(h=myMajorTicks[myMajorTicks!=par("usr")[3]],col=plotColors$guides,lwd=1)
    }
  }
  par(mai=oMai,family=cFont)
  return(list(data=tData,labels=groupNames))
}

#' @title prepare a plotting environment for non-categorical data such as bar plots or box plots
#' @description
#' takes untransformed data and draws the x and y axis with support for log transformation and outlier trimming.
#'
#' @details
#' This function does all the hard work of setting up the x and y axis for plotting as well as optionally log transforming and/or trimming the data of outliers. In particular, it adds much more robust support for plotting of log transformed data. Other features include the addition of both major and minor guidelines, support for horizontal plotting and improved label formatting options.
#'
#' @inheritParams formatPlotColors
#' @param x numeric vector or data frame; The input to \code{prepCategoryWindow} can be a numeric vector a  data frame of numeric vectors.
#' @param by factor or data frame of factors; used as the primary grouping factor and the factor levels will be used as group names if \code{groupNames} is not specified. If \code{by} is a data frame and \code{subGroup=\link{TRUE}}, the second column is assumed to be a secondary grouping factor, breaking out the data into sub-categories within each major group determined by the levels of the first column.
#' @param minorTick positive integer; number of minor tick-marks to draw between each pair of major ticks-marks.
#' @param guides logical; will draw guidelines at the major tick-marks if set to \code{\link{TRUE}}. Color of the guidelines is determined by \code{plotColors$guides}.
#' @param yLim numeric vector; manually set the limits of the plotting area (eg. \code{yLim=c(min,max)}). Used to format the y-axis.
#' @param xLim numeric vector; manually set the limits of the plotting area (eg. \code{xLim=c(min,max)}). Used to format the x-axis. by default but will modify the x-axis if \code{side=\link{TRUE}}.
#' @param rotateLabels logical; sets \code{las=2} for the x-axis category labels. Will affect y-axis if \code{side=\link{TRUE}}. Note that this may not work well if long names or with subgrouped data.
#' @param logScaleX positive numeric; the base for the for log scale data transformation for the x-axis calculated after the \code{logAdjustment} factor is added to the values.
#' @param logScaleY positive numeric; the base for the for log scale data transformation for the y-axis calculated after the \code{logAdjustment} factor is added to the values.
#' @param axisText list; A named list, "x" and "y" with two optional character values to be placed before and/or after the numeric axis label, respectively.
#' @param minorGuides logical; draws guidelines at minor tick-marks
#' @param extendTicks logical; extends minor tick-marks past the first and last major tick to the edge of the graph provided there is enough room. Works for both log-scale and regular settings.
#' @param subGroup logical; use additional column in \code{by} to group the data within each level of the major factor.
#' @param expLabels logical; prints the major tick labels is \eqn{logScale^{x}}{logScale^x} instead of the raw value
#' @param strictLimits logical; eliminates padding on the value axis so 0 can be flush with the x-axis. Defaults to \code{\link{FALSE}}.
#' @param legend logical/character; Draw a legend in the plot margins. If a character string is given it will overide the factor name default for the legend title.
#' @param logAdjustment = numeric; This number is added to the input data prior to log transformation. Default value is 1.
#'
#' @return formats the plotting area and returns a named list with 'data' and 'labels' corresponding to the trimmed and/or transformed data and the labels for the primary factors, respectively.
#' @examples
#'	todo<-1
#'
#' @import graphics
#' @import grDevices
#' @import dplyr
#' @importFrom purrr map_dbl
#' @importFrom utils data str
#'
#' @seealso \code{\link[grDevices]{axisTicks}}, \code{\link[graphics]{axis}}, \code{\link{makeLogTicks}}, \code{\link{facetSpacing}}
prepNiceWindow<-function(x,by=NULL, minorTick=FALSE, guides=TRUE, yLim=NULL, xLim=NULL,rotateLabels=FALSE, theme=NA, plotColors=if(is.na(theme)){list(bg="open",guides="black",lines="gray22",points="darkgrey",fill="white")}else{theme$plotColors}, logScaleX=FALSE,logScaleY=FALSE, axisText=list(x=c(NULL,NULL),y=c(NULL,NULL)), minorGuides=FALSE, extendTicks=F,subGroup=FALSE, expLabels=TRUE,strictLimits=F, legend=FALSE, logAdjustment=1) {
  levelCount<-1
  xData<-x[,1]
  yData<-x[,2]
  tBy<-by
  plotColors<-formatPlotColors(plotColors)
  oMai<-par()$mai
  cFont<-par()$family
  if(!is.na(theme[1])) {
    par(family=theme$fontFamily)
  }

  #Set margins for legends now
  prepLegendMarigins(x=yData,by=by,theme=theme,legend=legend,pointHighlights=FALSE,subGroup=TRUE)

  #Calculate the data ranges for x and y
  dataRange<-list(x=c(NULL,NULL),y=c(NULL,NULL))
  if(is.null(yLim)==FALSE) {
    if(is.numeric(yLim) & length(yLim==2)) {
      dataRange$y<-yLim
    } else {
      stop(paste0("yLim values be two consecutive numbers (eg. c(1,10)\nCurrent values: ",yLim))
    }
  } else {
    dataRange$y<-range(yData)
  }
  if(is.null(xLim)==FALSE) {
    if(is.numeric(xLim) & length(xLim==2)) {
      dataRange$x<-xLim
    } else {
      stop(paste0("xLim values be two consecutive numbers (eg. c(1,10)\nCurrent values: ",xLim))
    }
  } else {
    dataRange$x<-range(xData)
  }

  #Calculate major tick marks if the data is log scaled
  majorTicksX<-NULL
  if(logScaleX>1){
    if(dataRange$x[1]<0){
      stop(paste0("Error: you can not log scale numbers less than or equal to zero\nLowest number detected: ",dataRange$x[1]))
    }
    majorTicksX<-makeLogTicks(dataRange$x,minorCount= minorTick,logScale=logScaleX, axisText=axisText$x, expLabels=expLabels, logAdjustment=logAdjustment)
    xData <-log(xData +logAdjustment,logScaleX)
    if(is.null(yLim)==FALSE) {
      dataRange$x<-log(xLim+logAdjustment,logScaleX)
    } else {
      dataRange$x<-range(xData)
    }
  }

  majorTicksY<-NULL
  if(logScaleY>1){
    if(dataRange$y[1]<0){
      stop(paste0("Error: you can not log scale numbers less than or equal to zero\nLowest number detected: ",dataRange$y[1]))
    }
    majorTicksY<-makeLogTicks(dataRange$y,minorCount= minorTick,logScale=logScaleY, axisText=axisText$y, expLabels=expLabels, logAdjustment=logAdjustment)
    yData <-log(yData +logAdjustment,logScaleY)
    if(is.null(yLim)==FALSE) {
      dataRange$y<-log(xLim+logAdjustment,logScaleY)
    } else {
      dataRange$y<-range(yData)
    }
  }

  #Make the plotting window while setting backgroud and canvass colors
  oBg<-par("bg")
  par(bg=plotColors$marginBg)
  plot.new()
  if(strictLimits) {
    plot.window(xlim=dataRange$x, ylim=dataRange$y, xaxs="i", yaxs="i")
  } else {
    plot.window(xlim=dataRange$x, ylim=dataRange$y)
  }
  par(xpd=FALSE)
  if(plotColors$bg=="open" | plotColors$bg=="Open") {
    abline(v=par("usr")[1],lwd=2.5,col=plotColors$axis)
    abline(h=par("usr")[3],lwd=2.5,col=plotColors$axis)
  } else {
    rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col=plotColors$bg, lwd=2.5,border=plotColors$axis)
  }
  par(bg=oBg)

  #Formating the axis labels
  labelCex<-.9
  if(!is.na(theme[1])){
    if(is.numeric(theme$yAxisLabSize)){
      labelCex<-theme$yAxisLabSize
    }
  }
  myMajorTicksX<-axTicks(side=1)
  myLabelsX<-paste0(axisText$x[1],axTicks(side=1), axisText$x[2])
  if(logScaleX>0){
    myLabelsX<-majorTicksX[[2]]
    myMajorTicksX<-majorTicksX[[1]]
  }
  myMajorTicksY<-axTicks(side=2)
  myLabelsY<-paste0(axisText$y[1],axTicks(side=2), axisText$y[2])
  if(logScaleY>0){
    myLabelsY<-majorTicksY[[2]]
    myMajorTicksY<-majorTicksY[[1]]
  }

  #Scale labels if y-axis margins if necessary
  #If it is too long, the axis cex is lowered untill .6 at which point it switches to scientific notation
  maxLabelLength<-purrr::map_dbl(c(myLabelsY),strwidth,units="in",cex=labelCex) %>% max()
  if(par("mai")[2]*.6< maxLabelLength){
    if(par("mai")[2]*.6/maxLabelLength<.6){
      if(logScaleY>0){
        myLabelsY<-paste0(axisText$y[1],format(as.numeric(majorTicksY[[2]]),scientific = TRUE,digits = 3), axisText$y[2])
        myLabelsX<-paste0(axisText$x[1],format(as.numeric(majorTicksX[[2]]),scientific = TRUE,digits = 3), axisText$x[2])
      } else {
        myLabelsX<-paste0(axisText$x[1],format(as.numeric(axTicks(side=1)),scientific = TRUE,digits = 3), axisText$x[2])
        myLabelsY<-paste0(axisText$y[1],format(as.numeric(axTicks(side=2)),scientific = TRUE,digits = 3), axisText$y[2])
      }
    } else {
      if(par("mai")[2]*.6/maxLabelLength <labelCex) {
        labelCex<-par("mai")[2]*.6/maxLabelLength
      }
    }
  }

  minorLocY<-NULL
  minorLocX<-NULL
  if (minorTick > 0) {
    lowerLimY<-par("usr")[3]
    upperLimY<-par("usr")[4]
    lowerLimX<-par("usr")[1]
    upperLimX<-par("usr")[2]

    #Calculate minor ticks for x axis
    if(logScaleX>0){
      minorLocX<-majorTicksX[[3]]
      if(extendTicks) {
        delta<-myMajorTicksX[2]-myMajorTicksX[1]
        cBound<-logScaleX^(min(myMajorTicksX)-delta)
        cBy<-(logScaleX^min(myMajorTicksX)-cBound)/(minorTick+1)
        tempTick<-seq(cBound,logScaleX^min(myMajorTicksX),by=cBy)
        minorLocX <-c(minorLocX,log(tempTick[which(tempTick>1 & tempTick>logScaleX^(lowerLimX))],logScaleX))
        cBound<-logScaleX^(max(myMajorTicksX)+delta)
        cBy<-(cBound-logScaleX^max(myMajorTicksX))/(minorTick+1)
        tempTick<-seq(logScaleX^max(myMajorTicksX),cBound,by=cBy)
        minorLocX <-c(minorLocX,log(tempTick[which(tempTick>1 & tempTick<logScaleX^(upperLimX))],logScaleX))
      }
    } else {
      for(i in 1:(length(myMajorTicksX)-1)){
        minorLocX<-c(minorLocX,seq(myMajorTicksX[i], myMajorTicksX[i+1],length.out= minorTick + 2)[2:(minorTick+1)])
      }
      if (extendTicks) {
        minorLocX<-c(minorLocX,seq(min(myMajorTicksX), lowerLimX,by=(minorLocX[1]-minorLocX[2])))
        minorLocX<-c(minorLocX,seq(max(myMajorTicksX), upperLimX,by=(minorLocX[2]-minorLocX[1])))
      }
    }

    #Calculate minor ticks for Y axis
    if(logScaleY>0){
      minorLocY<-majorTicksY[[3]]
      if(extendTicks) {
        delta<-myMajorTicksY[2]-myMajorTicksY[1]
        cBound<-logScaleY^(min(myMajorTicksY)-delta)
        cBy<-(logScaleY^min(myMajorTicksY)-cBound)/(minorTick+1)
        tempTick<-seq(cBound,logScaleY^min(myMajorTicksY),by=cBy)
        minorLocY <-c(minorLocY,log(tempTick[which(tempTick>1 & tempTick>logScaleY^(lowerLimY))],logScaleY))
        cBound<-logScaleY^(max(myMajorTicksY)+delta)
        cBy<-(cBound-logScaleY^max(myMajorTicksY))/(minorTick+1)
        tempTick<-seq(logScaleY^max(myMajorTicksY),cBound,by=cBy)
        minorLocY <-c(minorLocY,log(tempTick[which(tempTick>1 & tempTick<logScaleY^(upperLimY))],logScaleY))
      }
    } else {
      for(i in 1:(length(myMajorTicksY)-1)){
        minorLocY<-c(minorLocY,seq(myMajorTicksY[i], myMajorTicksY[i+1],length.out= minorTick + 2)[2:(minorTick+1)])
      }
      if (extendTicks) {
        minorLocY<-c(minorLocY,seq(min(myMajorTicksY), lowerLimY,by=(minorLocY[1]-minorLocY[2])))
        minorLocY<-c(minorLocY,seq(max(myMajorTicksY), upperLimY,by=(minorLocY[2]-minorLocY[1])))
      }
    }

    #Draw minor guides and tick marks for x and y axis
    if(minorGuides != FALSE){
      abline(h= minorLocY[minorLocY!=lowerLimY],lwd=.33,col=plotColors$minorGuides)
      abline(v= minorLocX[minorLocX!=lowerLimX],lwd=.33,col=plotColors$minorGuides)
    }
    axis(side = 1, at = minorLocX, labels = FALSE, tcl = -0.2,col=plotColors$axis,col.ticks=plotColors$minorTicks)
    axis(side = 2, at = minorLocY, labels = FALSE, tcl = -0.2,col=plotColors$axis,col.ticks=plotColors$minorTicks)
  }

  #Draw major ticks and labels for back x and y
  clCex<-par()$cex.axis
  par(cex.axis=labelCex)
  axis(side=1,labels=myLabelsX,at= myMajorTicksX,las=rotateLabels,col=plotColors$axis,col.ticks=plotColors$majorTicks)
  axis(side=2,labels=myLabelsY,at= myMajorTicksY,las=rotateLabels,col=plotColors$axis,col.ticks=plotColors$majorTicks)
  par(cex.axis=clCex)

  if(guides[1]!=FALSE){
    abline(v=myMajorTicksX[myMajorTicksX!=par("usr")[1]],col=plotColors$guides,lwd=1)
    abline(h=myMajorTicksY[myMajorTicksY!=par("usr")[2]],col=plotColors$guides,lwd=1)
  }
  par(mai=oMai,family=cFont)
  return(data.frame(x=xData,y=yData))
}

#' @title Draw a nice plot legened
#' @description Draws a customizable legend in the margins based on factor levels.
#' @details This functions works with plot enviroment initializing functions such as \code{\link{prepCategoryWindow}}
#' to expand the right margin to accomodate a figure legend.
#'
#' @examples
#' ToDo<-1
#'
#' @param labels character vector; The names of the levels decribed in the legend. Typically factor levels.
#' @param title character; The title of the legend. This defaults to "Legend" if unspecificed.
#' @param fontCol R color; Color of the legend text.
#' @param border R color; The color of the rectanglar border surrounding the legend. Defaults to \code{\link{NULL}} which supresses this feature
#' @param lineCol R color; The color of the line colors for the color key. Optional. Defaults to \code{\link{NA}}.
#' @param bg R color; Sets the background color for the legend aread. Note that this can be distinct the the margin background.
#' @param col R color vector; A vector of colors determining the color of the color code boxes.
#' @param shape character; Determins if the color code is rectangles or circles. Valid ptions are "rect", "rectangle", "circ", or "circle". Not there is no funcitonal difference between the synonyms.
#' @param size numeric; Sets the legend font cex sizing.
#' @param spacing numeric; Determins the total amount of padding (sum of upper and lower padding) surrounding each line. in the legend in units of font line hight.
#'
#' @import dplyr
#' @importFrom graphics rect text
#' @importFrom purrr map_dbl
#' @seealso \code{\link{legend}}, \code{\link{prepCategoryWindow}}, \code{\link{niceBox}}, \code{\link{niceDots}}, \code{\link{niceBar}}, \code{\link{niceVio}}
makeNiceLegend<-function(labels, title="Legend", fontCol="black", border=NULL, lineCol=NA, bg=NA, col=makeColorMatrix()[,3], shape="rect",size=.66,spacing=.2) {
  maxLabelW<-map_dbl(c(labels),strwidth,cex=size,units="in") %>% max()
  titleW<-strwidth(title,font=2,cex=size,units="in")
  if(titleW>maxLabelW){maxLabelW<-titleW}
  maxLabelH<-map_dbl(labels, strheight,cex=size,units="in") %>% max()
  titleH<-strheight(title,font=2,cex=size,units="in")
  oMai<-par("mai")
  nMai<-oMai
  nMai[4]<-nMai[4]+maxLabelW #+maxLabelH
  par(mai=nMai)

  par(xpd=NA)
  iRange<-par("pin")[1]
  uRange<-par("usr")[2]-par("usr")[1]
  ConvertW<-iRange/uRange
  iRange<-par("pin")[2]
  uRange<-par("usr")[4]-par("usr")[3]
  ConvertH<-iRange/uRange
  LegendCo<-oMai[4]/3/ConvertW +par("usr")[2]

  totalLegendH<-maxLabelH*1.2*length(labels)+titleH
  startH<-0
  if(totalLegendH/2>iRange/3){
    startH<-par("usr")[4]
  } else {
    startH<-par("usr")[4]-iRange/3/ConvertH + totalLegendH/2/ConvertH
  }
  if(!is.null(border)){
    rect(LegendCo-oMai[4]/9/ConvertW,startH-totalLegendH/ConvertH-oMai[4]/9/ConvertH*1.1,LegendCo+maxLabelH/ConvertW+maxLabelW/ConvertW+oMai[4]/9/ConvertW*2, startH+oMai[4]/9/ConvertH*2,col=bg,border=border)
  }
  text(LegendCo, startH, label=title,cex=size, font=2, offset=0, pos=4, col=fontCol)
  for(i in 1:length(labels)){
    cH<-startH-titleH/ConvertH*(1+ spacing/2)-.5*maxLabelH/ConvertH-(i-1)*maxLabelH/ConvertH*(1+ spacing)
    rect(LegendCo, cH-.3*maxLabelH/ConvertH,LegendCo+maxLabelH/ConvertW, cH +.7* maxLabelH/ConvertH, border=lineCol,col=col[i])
    text(LegendCo+maxLabelH/ConvertW, cH,labels=labels[i],cex=size,pos=4,offset=.2,col=fontCol)
  }

  par(xpd=F,mai=oMai)

}
