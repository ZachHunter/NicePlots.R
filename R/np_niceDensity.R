#' @include np_options_processing.R niceThemes.R np_categorical_plot_setup.R np_plotting_functions.R np_utility.R np_data_processing.R
#' @title draw a kernal density plot
#' @description Draws a kernal density plot of one or two variables.
#' @details
#' TODO
#'
#' @inheritParams prepNiceWindow
#' @param groupNames character vector; overrides the factor levels of \code{by} to label the groups
#' @param drawPoints logical; draws a dot plot overlay for contour plots
#' @param subGroup logical; Will use the factor levels in \code{by} to plot a series of distributions from the data in \code{x}.
#' @param useRgl logical; Should the library \code{\link[rgl]{rgl}} be used to make 3D surface plots.
#' @param plotType character; Can be set to \code{contour} or \code{surface} to control the type of 2D plot.
#' @param theme list object; Themes are are an optional way of storing graphical preset options that are compatible with all nicePlot graphing functions.
#' @param main character; title for the graph which is supplied to the \code{main} argument.
#' @param sub character; subtitle for the graph which is supplied to the \code{sub} argument. If \code{\link{NULL}} and \code{showCalc=\link{TRUE}} it will be used to display the output form \code{\link{calcStats}}.
#' @param ylab character; y-axis label.
#' @param xlab character; x-axis label.
#' @param na.rm logical; Should \code{NA}s be removed from the data set? Both data input and the factor input from \code{by} with be checked.
#' @param verbose logical; Prints summary and p-value calculations to the screen. All data is silently by the function returned either way.
#' @param showCalc logical; TODO Not sure if there are calcs worth doing here... maybe normality testing?
#' @param calcType charactar; TODO Not sure if there are calcs worth doing here...maybe normality testing?
#' @param add logical; Adds the plot to the existing window rather the generating a new plotting enviroment
#' @param lWidth numeric; Line width, equivelent to the cex \code{lwd} option.
#' @param logScale numeric; A length two numeric vector indicating the log scale the x and y axis, respectively. If only one value is given, it is applied to boxth axis. Set to \code{\link{FALSE}} to disable (Default).
#' @param ... additional options for S3 method variants
#'
#' @examples
#' TODO<-1
#' @seealso \code{\link[stats]{density}}, \code{\link[graphics]{contour}}, \code{\link[graphics]{persp}}, \code{\link[rgl]{persp3d}}, \code{\link[KernSmooth]{bkde}}
#' @import dplyr
#' @importFrom KernSmooth bkde bkde2D dpih
#' @importFrom purrr walk2 map_dbl
#' @importFrom graphics polygon contour lines persp
#' @export
niceDensity<-function(x, by=NULL, drawPoints=TRUE, groupNames=NULL, subGroup=FALSE, useRgl=TRUE, plotType=c("contour","surface"),theme=basicTheme, main=NULL,sub=NULL, ylab=NULL, xlab=NULL, minorTick=FALSE, guides=NULL, plotColors=NULL, logScale=FALSE, axisText=c(NULL,NULL), showCalc=FALSE, calcType="none", rotateLabels=FALSE, add=FALSE, minorGuides=NULL, extendTicks=TRUE, expLabels=FALSE, lWidth=NULL, na.rm=FALSE, verbose=FALSE,logAdjustment=1,xLim=NULL,yLim=NULL, strictLimits=FALSE, legend=FALSE, ...) {UseMethod("niceDensity",x)}

#' @import dplyr
#' @importFrom KernSmooth bkde bkde2D dpih
#' @importFrom purrr walk2 map_dbl
#' @importFrom graphics polygon contour lines persp
#' @export
niceDensity.default<-function(x, by=NULL, drawPoints=TRUE, groupNames=NULL,subGroup=FALSE, useRgl=TRUE, plotType=c("contour","surface"),theme=basicTheme, main=NULL,sub=NULL, ylab=NULL, xlab=NULL, minorTick=FALSE, guides=NULL, plotColors=NULL, logScale=FALSE, axisText=list(x=c(NULL,NULL),y=c(NULL,NULL)), showCalc=FALSE, calcType="none", rotateLabels=TRUE, add=FALSE, minorGuides=NULL, extendTicks=TRUE, expLabels=FALSE, lWidth=NULL, na.rm=FALSE, verbose=FALSE,logAdjustment=1,xLim=NULL,yLim=NULL, strictLimits=FALSE, legend=FALSE, ...)  {
  if(any(is.na(x)) | any(is.na(by))){warning("Warning: NAs detected in dataset")}
  prepedData<-NULL
  plotData<-NULL
  checked<-dataFlightCheck(x,by,na.rm=na.rm,flipFacts = FALSE)
  x<-checked$d
  by<-checked$b
  rm(checked)
  if(is.data.frame(by)){
    by<-factor(by[,1])
  }
  #Here we check to see if the user specified any options so that they are left unaltered if present
  finalOptions<-procNiceOptions(x=rep(1,length(by)),by=by,minorTick=minorTick,pointShape=NULL,whiskerLineType=NULL,lWidth=lWidth,capWidth=NULL,pointLaneWidth=FALSE,width=NULL,guides=guides,pointSize=NULL,subGroup=subGroup,stack=F,pointHighlights=FALSE,type="VP",theme=theme,plotColors=plotColors,logScale=logScale,pointMethod=NULL,drawPoints=drawPoints,groupNames=groupNames,swarmOverflow=NULL, errorCap = NULL)
  minorTick<-finalOptions$minorTick
  pointShape<-finalOptions$pointShape
  lWidth<-finalOptions$lWidth
  guides<-finalOptions$guides
  pointSize<-finalOptions$pointSize
  theme<-finalOptions$theme
  plotColors<-finalOptions$plotColors
  groupNames<-finalOptions$groupNames
  pointMethod<-finalOptions$pointMethod
  theme$plotColors<-plotColors
  curvePoints<-500
  if(!is.null(theme$curvePoints)){
    curvePoints<-theme$curvePoints
  }

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


  #Initialize legend variables so we can update based on options
  legendLabels<-groupNames
  legendTitle<-"Legend"
  if(!is.null(by) & subGroup==TRUE){
    if(legend[1]==FALSE | is.null(legend[1])) {
      legend<-TRUE
    } else if (legend[1]!=TRUE) {
      legendTitle<-legend
    }
  }
  legendColors<-plotColors$points


  logScaleX<-FALSE
  logScaleY<-FALSE
  if(is.null(logScale[1])){logScale[1]<-FALSE}
  if(length(logScale)>1 & is.null(logScale[2])) {logScale[2]<-FALSE}
  if((length(logScale)>1 & sum(logScale[1:2])>0) | logScale[1]!=FALSE) {
    if(length(logScale)==1) {
      if(logScale[1]!=FALSE){
        logScaleX<-logScale
        logScaleY<-logScale
      }
    } else {
      if(logScale[1]!=FALSE) {
        logScaleX<-logScale[1]
      }
      if(logScale[2]!=FALSE) {
        logScaleY<-logScale[2]
      }
    }
  }
  #Handles cases where users want the points overlay to be consistant and the fill to change.
  if(length(legendColors)<=1 & length(plotColors$fill)>1){
    legendColors<-plotColors$fill
  }

  oFont<-par()$family
  oCexMain<-par()$cex.main
  oCexlab<-par()$cex.lab
  oCexSub<-par()$cex.sub
  oLabCol<-par()$col.lab
  oMainCol<-par()$col.main
  oSubCol<-par()$col.sub
  if(!is.na(theme[1]) & !is.null(theme[1])){
    par(col.sub=plotColors$labels, col.lab=plotColors$labels,col.main=plotColors$labels,cex.main=theme$titleSize, cex.lab=theme$axisLabelSize, cex.sub=theme$subSize, family=theme$fontFamily)
  }

  if(is.data.frame(x)){
    if(is.null(xlab)) {xlab<-colnames(x)[1]}
    if(is.null(ylab)) {ylab<-colnames(x)[2]}
    if(add[1]==FALSE) {
      x<-prepNiceWindow(x, by, minorTick=minorTick, guides=guides, yLim=yLim, xLim=xLim, rotateLabels=rotateLabels, theme=theme, plotColors=plotColors, logScaleX=logScaleX, logScaleY=logScaleY, axisText=axisText, minorGuides=minorGuides, extendTicks=extendTicks, expLabels=expLabels, legend=legend, logAdjustment=logAdjustment)
      title(main=main,sub=sub,ylab=ylab,xlab=xlab)
    }
    den2D<-bkde2D(as.matrix(x[,1:2]),bandwidth=c(dpih(x[,1],gridsize=curvePoints),dpih(x[,2],gridsize=curvePoints),gridsize=c(curvePoints,curvePoints)))
    if(plotType[1]=="contour") {
      contour(den2D$x1, den2D$x2, den2D$fhat,col=theme$plotColors$lines[1],main=main,sub=sub,ylab=ylab, add=TRUE)
      if(drawPoints==TRUE) {
        if(is.null(by)){
          n_groups<-1
          groups<-factor(rep("Group",length(x[,1])))
        } else if(is.factor(by)) {
          n_groups<-length(levels(by))
          groups<-factor(by)
        } else {
          n_groups<-length(levels(factor(by[,1])))
          groups<-factor(by[,1])
        }
        print(theme$plotColors$points)
        if(length(pointShape) < n_groups) {pointShape<-rep(pointShape,n_groups)}
        if(length(plotColors$points) < n_groups) {plotColors$points<-rep(plotColors$points,n_groups)}
        for(i in 1:n_groups){
          points(x[groups==levels(groups)[i],1],x[groups==levels(groups)[i],2],col=plotColors$points[i],cex=pointSize,pch=pointShape[i])
        }
      }
    } else if(plotType=="surface") {
      legend<-FALSE
      persp(den2D$x1, den2D$x2, den2D$fhat,col=theme$plotColors$fill[1],main = main,xlab = xlab,ylab=ylab,zlab = "Density")
    } else {
      stop("Error: plotType should be set to either \'contour\' or \'surface\' for 2D desntsity plots")
    }
  } else {
    if(logScaleX+logScaleY!=0) {
      x<-log(x + logAdjustment, max(logScaleX+logScaleY))
    }
    if(is.null(ylab)) {ylab<-"Density"}
    if(!is.null(by) & subGroup==TRUE){
      if(length(plotColors$fill)==1) {plotColors$fill<-plotColors$points}
      n_groups<-length(levels(by))
      densities<-vector(mode="list",length=n_groups)
      for(i in 1:n_groups){
        densities[[i]]<-bkde(x[by==levels(by)[i]],gridsize=curvePoints)
      }
      maxx<-max(map_dbl(densities, function(z) max(z$x)))
      minx<-min(map_dbl(densities, function(z) min(z$x)))
      maxy<-max(map_dbl(densities, function(z) max(z$y)))
      if(add[1]==FALSE) {
        test<-prepNiceWindow(data.frame(x=x,y=x), by, minorTick=minorTick, guides=guides, yLim=c(0,maxy), xLim=c(minx,maxx), rotateLabels=rotateLabels, theme=theme, plotColors=plotColors, logScaleX=logScaleX, logScaleY=logScaleY, axisText=axisText, minorGuides=minorGuides, extendTicks=extendTicks, expLabels=expLabels, legend=legend, logAdjustment=logAdjustment)
        title(main=main,sub=sub,ylab=ylab,xlab=xlab)
      }
      #plot(-1,-1,type="n",xlim=c(minx,maxx),ylim=c(0,maxy),main=main,sub=sub,ylab=ylab)
      walk2(.x=densities,.y=plotColors$fill[1:n_groups],~polygon(.x,col=.y,border=0))
      walk2(.x=densities,.y=plotColors$lines[1:n_groups],~lines(.x,col=.y,lwd=lWidth))
    } else {
      densities<-bkde(x,gridsize=curvePoints)
      maxx<-max(densities$x)
      minx<-min(densities$x)
      maxy<-max(densities$y)
      if(add[1]==FALSE) {
        test<-prepNiceWindow(data.frame(x=x,y=x), by, minorTick=minorTick, guides=guides, yLim=c(0,maxy), xLim=c(minx,maxx), rotateLabels=rotateLabels, theme=theme, plotColors=plotColors, logScaleX=logScaleX, logScaleY=logScaleY, axisText=axisText, minorGuides=minorGuides, extendTicks=extendTicks, expLabels=expLabels, legend=legend, logAdjustment=logAdjustment)
        title(main=main,sub=sub,ylab=ylab,xlab=xlab)
      }
      #plot(-1,-1,type="n",xlim=c(minx,maxx),ylim=c(0,maxy),main=main,sub=sub,ylab=ylab)
      polygon(densities,col=theme$plotColors$fill[1],border=0)
      lines(densities,col=theme$plotColors$lines[1])
    }
  }
  #Draw legend and set associated options if indicated
  if(length(legendColors)<length(legendLabels) & legend!=FALSE){
    legend<-FALSE
    warning("Not enough point colors to uniquely color subGroups levels\nPlease update plotColors point options to use legend options with this subgroup.")
  }

  if(legend!=FALSE) {
    makeNiceLegend(labels=legendLabels, title=legendTitle, fontCol=plotColors$labels, border=theme$LegendBorder, lineCol=theme$LegendLineCol, bg=theme$LegendBG, col=legendColors, shape="rect",size=theme$LegendSize,spacing=theme$LegendSpacing)
  }
  par(col.sub=oSubCol, col.lab=oLabCol,col.main=oMainCol,family=oFont,cex.main=oCexMain,cex.lab=oCexlab,cex.sub=oCexSub)
}


# open3d()
# bg3d("white")
# material3d(col = "black")
# persp3d(test$x1, test$x2, test$fhat,col="lightblue")

# vizz <- function(x, y){
#   if (! requireNamespace("lattice", quietly = TRUE)) {
#     stop("Please install lattice: install.packages('lattice')")
#     lattice::xyplot(y ~ x)
#   }
# }
