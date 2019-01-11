#' @include np_options_processing.R niceThemes.R np_categorical_plot_setup.R np_plotting_functions.R np_utility.R np_data_processing.R
#' @title draw a dot plot
#' @description draws a categorical dot plot with optional data highlighting and log scale support.
#'
#'
#' @details
#' This is a wrapper function for \code{\link{niceBox}} that just plots the points with no box distribution data. data point overlay options, data subsetting with a secondary factor, and data point highlighting with a tertiary factor.
#' The complicated part of using this function is handling its many options. A wrapper function to set up and run it with preset options may be a good idea if you are using it along. The function \code{\link{niceDots}} is an example of this.
#' Briefly put, the \code{by} argument can be a data frame of factors and the function will  work through the columns in order as needed.
#' If \code{x} is a numeric vector, then \code{by} should be a factor to group it into categories. If \code{by} is a data frame of factors and \code{subGroup=\link{TRUE}}, then the first column for \code{by}
#' is used as the grouping factor and the second column is used as the sub-grouping factor. If \code{pointHighlights==\link{TRUE}}, and \code{subGroup=\link{TRUE}}, the the third column of \code{by}
#' is used to highlight points data point overlay (assuming \code{drawPoints=\link{TRUE}}). If \code{subGroup=\link{FALSE}} and \code{subGroup=\link{TRUE}}, then the second column of \code{by} is used to control
#' the point highlighting. If \code{x} itself is a data frame of numeric vectors, \code{subGroup} is automatically set to false and each column of \code{x} is plotted like a sub-group and grouped
#' by the first column of \code{by}. Data point highlighting with \code{pointHighlights=\link{TRUE}} can still be used when \code{x} is a data frame and the highlighting factor will be drawn from the second column of \code{by}.
#' Please note that the p-values can not always be calculated and are for general exploratory use only. More careful analysis is necessary to determine statistical significance.
#' This function is as S3 generic and can be extended to provide class specific functionality.
#' To further facilitate data exploration, outputs from statistical testing and data set summaries
#' are printed to the console.
#'
#' @inheritParams niceBox
#'
#' @examples
#' data(iris)
#' mCols<-makeColorMatrix()
#' myCols<-list(fill=mCols[1:3,3],lines="darkblue")
#' niceDots(iris$Sepal.Length,iris$Species,minorTick=4,showCalc=TRUE,calcType="anova",
#'     ylab="Sepal Length",main="Sepal Length by Species",plotColors=myCols)
#'
#' @export
#' @seealso \code{\link[graphics]{stripchart}}, \code{\link[beeswarm]{beeswarm}}, \code{\link{quantileTrim}}, \code{\link{prepCategoryWindow}}, \code{\link{niceBox}}
niceDots <- function(x, by=NULL, groupNames=NULL, main=NULL,sub=NULL, ylab=NULL, minorTick=FALSE, theme=basicTheme, guides=TRUE, outliers=1.5, pointSize=1, width=1, pointShape=1, plotColors=list(bg="open"), logScale=FALSE, trim=FALSE, pointMethod=NULL, axisText=c(NULL,NULL), showCalc=FALSE, calcType="none", yLim=NULL, rotateLabels=FALSE, rotateY=FALSE, add=FALSE, minorGuides=NULL, extendTicks=TRUE, subGroup=FALSE, subGroupLabels=NULL, expLabels=TRUE, sidePlot=FALSE, pointHighlights=FALSE, pointLaneWidth=1, na.rm=FALSE, flipFacts=FALSE, verbose=FALSE, legend=FALSE,logAdjustment=1, ...) {UseMethod("niceDots",x)}

#' @export
niceDots.default<-function(x, by=NULL, groupNames=NULL, main=NULL,sub=NULL, ylab=NULL, minorTick=FALSE, theme=basicTheme, guides=TRUE, outliers=1.5, pointSize=NULL, width=NULL, pointShape=NULL, plotColors=NULL, logScale=FALSE, trim=FALSE, pointMethod=NULL, axisText=c(NULL,NULL), showCalc=FALSE, calcType="none", yLim=NULL, rotateLabels=FALSE, rotateY=FALSE, add=FALSE, minorGuides=NULL, extendTicks=TRUE, subGroup=FALSE, subGroupLabels=NULL, expLabels=TRUE, sidePlot=FALSE, pointHighlights=FALSE, pointLaneWidth=NULL, na.rm=FALSE, flipFacts=FALSE, verbose=FALSE, legend=FALSE, logAdjustment=1, ...) {
  #Here we check to see if the user specified any options so that they are left unaltered if present
  lWidth<-NULL
  checked<-dataFlightCheck(x,by,na.rm=na.rm,flipFacts = flipFacts)
  finalOptions<-procNiceOptions(x=checked$d,by=checked$b,minorTick=minorTick,pointShape=pointShape,whiskerLineType=1,lWidth=lWidth,capWidth=1,pointLaneWidth=pointLaneWidth,width=width,guides=guides,pointSize=pointSize,subGroup=subGroup,stack=F,pointHighlights=pointHighlights,type="DP",theme=theme,plotColors=plotColors,logScale=logScale,pointMethod=pointMethod,drawPoints=TRUE,groupNames=groupNames,swarmOverflow=NULL)
  minorTick<-finalOptions$minorTick
  pointShape<-finalOptions$pointShape
  lWidth<-finalOptions$lWidth
  pointLaneWidth<-finalOptions$pointLaneWidth
  width<-finalOptions$width
  guides<-finalOptions$guides
  pointSize<-finalOptions$pointSize
  theme<-finalOptions$theme
  plotColors<-finalOptions$plotColors
  groupNames<-finalOptions$groupNames
  pointMethod<-finalOptions$pointMethod

  niceBox(x=x,by=by,groupNames=groupNames,theme=theme,main=main,ylab=ylab,minorTick=minorTick,guides=guides,outliers=outliers,pointSize=pointSize,width=width,pointShape=pointShape,plotColors=plotColors,logScale=logScale,trim=trim,pointMethod=pointMethod, axisText=axisText, showCalc=showCalc, calcType=calcType, yLim=yLim, rotateLabels=rotateLabels, rotateY=rotateY, add=add, minorGuides=minorGuides, extendTicks=extendTicks,subGroup=subGroup,subGroupLabels=subGroupLabels,expLabels=expLabels,sidePlot=sidePlot, pointHighlights=pointHighlights, pointLaneWidth=pointLaneWidth, drawBox=FALSE, drawPoints=TRUE,na.rm=na.rm, flipFacts=flipFacts, verbose=verbose,logAdjustment=logAdjustment, legend=legend)
}
