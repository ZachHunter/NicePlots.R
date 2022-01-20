#' @include np_options_processing.R niceThemes.R np_categorical_plot_setup.R np_plotting_functions.R np_utility.R np_data_processing.R
#' @title draw a scatter plot
#' @description Draws a 2D or 3D scatter plot with optional regression lines.
#' @details
#' The niceScatter function operates a little differently from the rest of the functions in this package.
#' While it can accept a \code{\link{data.frame}} or a \code{\link{vector}} for \code{x} and \code{by} values,
#' it is also possible to construct the \code{by} \code{\link{data.frame}} implicitly by supplying an actual \code{\link{factor}}/numeric vector to
#' to the options like \code{color}, \code{shape}, and \code{size} instead of \code{\link{TRUE}} or \code{\link{FALSE}}. These approaches can not be mixed and matched.
#' If a value is given to \code{by} directly then the other active options are just set to \code{\link{TRUE}}.
#' If a numeric \code{\link{vector}} or single column \code{\link{data.frame}} is given to \code{x}, the x-axis is automatically assigned to the row numbers of the data.
#' The standard options for \code{type} in the base R \code{\link{plot}} function can be given here to determine the plotting method.
#' If three or more columns of data are supplied to \code{x}, \code{niceScatter} will switch to 3D plotting mode with \code{\link[scatterplot3d]{scatterplot3d}}.
#' The options \code{color}, \code{size} and \code{shape} can be used in this mode. The \code{\link[scatterplot3d]{scatterplot3d}} options \code{grid}, \code{angle} and \code{box} can be given as
#' arguments to be passed to \code{\link[scatterplot3d]{scatterplot3d}}. If \code{useRgl} is \code{\link{TRUE}}, then it will attempt to load the \code{rgl} package and use \code{rgl} interactive graphics instead.
#' Note that the \code{shape} option does not work in with \code{rgl} \code{\link[rgl]{plot3d}}. However, the \code{size} option will scale the radius of the spheres in \code{rgl} correctly.
#' When \code{useRgl} is active, the \code{\link[rgl]{plot3d}} options \code{axes} and \code{box} can be endtered as arguments to niceScatter to format the graph.
#' Finally, in 2D mode, linear trend lines with 95\% confidence intervals can be drawn for the data or for each factor level of an option such as \code{color} or \code{shape}.
#' @inheritParams prepNiceWindow
#' @param color factor or logical; if by is NULL, a factor suppolied to this option will populate by to color points by the factor levels. If by is defined then color should be set to TRUE or FALSE.
#' @param shape factor or logical; if by is NULL, a factor suppolied to this option will populate by to control the shape of the points points by the factor levels. If by is defined then shape should be set to TRUE or FALSE.
#' @param size factor or logical; if by is NULL, a factor suppolied to this option will populate by to control the size of the points by the factor levels. If by is defined then color should be set to TRUE or FALSE.
#' @param trendline To do.
#' @param groupLabels character vector; overrides the factor levels of \code{by} to label the groups
#' @param bandwidth numeric; Manually sets the bandwith for the kernal density estimation overiding default calculations. For 2D plots \code{bandwidth} should be a vector of length 2. Set to \code{\link{NULL}} or \code{\link{NA}} to enable default calculations.
#' @param useRgl logical; Should the library \code{\link[rgl]{rgl}} be used to make 3D surface plots.
#' @param type character;
#' @param sizeScale numeric; How much larger should points be scaled based on factor level than the default.
#' @param sizeLevels How many levels should be used to make the size scaling effect if the value given to size is numeric.
#' @param theme list object; Themes are are an optional way of storing graphical preset options that are compatible with all nicePlot graphing functions.
#' @param main character; title for the graph which is supplied to the \code{main} argument.
#' @param sub character; subtitle for the graph which is supplied to the \code{sub} argument.
#' @param xlab character; x-axis label.
#' @param ylab character; y-axis label.
#' @param zlab character; z-axis label.
#' @param zLim numeric; optional limits on the z-axis for 3D plotting.
#' @param na.rm logical; Should \code{NA}s be removed from the data set? Both data input and the factor input from \code{by} with be checked.
#' @param verbose logical; Prints summary and p-value calculations to the screen. All data is silently by the function returned either way.
#' @param add logical; Adds the plot to the existing window rather the generating a new plotting enviroment
#' @param lWidth numeric; Line width, equivelent to the cex \code{lwd} option.
#' @param logScale numeric; A length two numeric vector indicating the log scale the x and y axis, respectively. If only one value is given, it is applied to boxth axis. Set to \code{\link{FALSE}} to disable (Default).
#' @param trimTrendLines logical; Limits trend line to the min and max data points if set to \code{\link{TRUE}}.
#' @param showTrendConfidence logical; Shows the 95\% CI of the model tend line if set to \code{\link{TRUE}}.
#' @param drawPoints logical; Should the points be drawn. Defaults to \code{\link{TRUE}}. Only useful for drawing an empty graph or only the trend lines.
#' @param corMethod character; This should be a valid argument to \code{method} from \code{\link[stats]{cor.test}}. Calculated only when trendlines are requested, the values be seen using \code{verbose} and in the \code{stats} section of the output list.
#' @param ... additional options for S3 method variants
#'
#' @examples
#' data(iris)
#' #Construct the by data.frame implicitly by supplying values to the options
#' niceScatter(iris[,1:2], color=iris$Species, shape=iris$Species,
#'    size=iris[,3], trendline = "color",theme=npColorTheme)
#'
#' #Same thing but defining the by input explicitly and makeing a single trandlline for the group.
#' #Note that seting trendline to color as before would produce a exact replica of the first graph.
#' niceScatter(iris[,1:2],by=data.frame(iris$Species,iris$Species,iris[,3]),
#'    color=TRUE, shape=TRUE, size=TRUE, theme=npColorTheme, trendline = TRUE)
#'
#' #Using 3D
#' niceScatter(iris[,1:3], color=iris$Species, size=iris[,4], shape=iris$Species, angle=140)
#'
#' #3D with rgl
#' niceScatter(iris[,1:3], color=iris$Species, size=iris[,4], useRgl=TRUE)
#'
#' @seealso \code{\link[scatterplot3d]{scatterplot3d}}, \code{\link[rgl]{plot3d}}, \code{\link{plot}}, \code{\link{niceDensity}}
#' @importFrom magrittr %>%
#' @importFrom purrr map_lgl map_dbl map walk
#' @importFrom graphics points
#' @importFrom tibble is_tibble
#' @importFrom broom tidy
#' @importFrom stats lm predict cor.test
#' @importFrom scatterplot3d scatterplot3d
#' @export
niceScatter<-function(x, by=NULL, color=NULL, shape=NULL, size=NULL,trendline=FALSE, sizeScale=3, sizeLevels=6, groupLabels=NULL, subgroup=FALSE, bandwidth=NULL, useRgl=FALSE, type="p",theme=basicTheme, main=NULL,sub=NULL, ylab=NULL, xlab=NULL, zlab=NULL,  minorTick=FALSE, guides=NULL, plotColors=NULL, logScale=FALSE, axisText=c(NULL,NULL), rotateLabels=FALSE, add=FALSE, minorGuides=FALSE, extendTicks=TRUE, expLabels=FALSE, lWidth=NULL, na.rm=TRUE, verbose=FALSE,logAdjustment=1,xLim=NULL,yLim=NULL,zLim=NULL, strictLimits=FALSE, legend=FALSE ,trimTrendLines=TRUE, showTrendConfidence=TRUE, drawPoints=TRUE, corMethod="pearson", ...) {UseMethod("niceScatter",x)}

#' @importFrom magrittr %>%
#' @importFrom purrr map_lgl map_dbl map walk
#' @importFrom graphics points
#' @importFrom tibble is_tibble
#' @importFrom broom tidy
#' @importFrom stats lm predict cor.test
#' @importFrom scatterplot3d scatterplot3d
#' @export
niceScatter.default <-function(x, by=NULL, color=NULL, shape=NULL, size=NULL,trendline=FALSE, sizeScale=3, sizeLevels=6, groupLabels=NULL, subgroup=FALSE, bandwidth=NULL, useRgl=FALSE, type="p",theme=basicTheme, main=NULL,sub=NULL, ylab=NULL, xlab=NULL, zlab=NULL, minorTick=FALSE, guides=NULL, plotColors=NULL, logScale=FALSE, axisText=c(NULL,NULL), rotateLabels=FALSE, add=FALSE, minorGuides=FALSE, extendTicks=TRUE, expLabels=FALSE, lWidth=NULL, na.rm=TRUE, verbose=FALSE,logAdjustment=1,xLim=NULL,yLim=NULL, zLim=NULL, strictLimits=FALSE, legend=FALSE, trimTrendLines=TRUE, showTrendConfidence=TRUE, drawPoints=TRUE, corMethod="pearson", ...) {
  if(any(is.na(x)) | any(is.na(by))){warning("Warning: NAs detected in dataset",call.=FALSE)}
  prepedData<-NULL
  plotData<-NULL

  #by works a little differently here. It can be a factor/data.frame of factors with the options like color set to T/F
  #Or by will default to NULL and be populated by the factors assigned to color, shape, and size.
  if(is.null(by)){
    byList<-list(color=color,shape=shape,size=size)
    byListFilter<-!map_lgl(byList, is.null)
    color<-byListFilter[1]
    shape<-byListFilter[2]
    size<-byListFilter[3]

    if(sum(byListFilter)>0){
      byList<-byList[byListFilter]
      if(sum(byListFilter)>1) {
        if(sum(map_dbl(byList,length)!=length(byList[[1]]))>0) {
          stop("The values of 'color', 'size', and 'shape' must either be NULL or of the same length as the observations in x.\n",call. = FALSE)
        } else {
          by<-data.frame(byList)
        }
      } else {
        by<-factor(byList[[1]])
      }
    }
  } else {
    if(is.null(color)) {
      color<-FALSE
    } else if (color[1]==FALSE | is.na(color[1])) {
      color<-FALSE
    } else {
      color<-TRUE
    }
    if(is.null(shape)) {
      shape<-FALSE
    } else if (shape[1]==FALSE | is.na(shape[1])) {
      shape<-FALSE
    } else {
      shape<-TRUE
    }
    if(is.null(size)) {
      size<-FALSE
    } else if (size[1]==FALSE | is.na(size[1])) {
      size<-FALSE
    } else {
      size<-TRUE
    }
    if(is.data.frame(by) | is_tibble(by)) {
      if(color[1]==TRUE) {
        colnames(by)[1]<-"color"
        if(shape[1]==TRUE) {
          if (dim(by)[2]>=2) {
            colnames(by)[2]<-"shape"
          } else {
            by<-data.frame(by,shape=by[,1])
          }
          if(size[1]==TRUE) {
            if (dim(by)[2]>=3) {
              colnames(by)[3]<-"size"
            } else {
              by<-data.frame(by,size=by[,1])
            }
          }
        } else {
          if(size[1]==TRUE) {
            if (dim(by)[2]>=2) {
              colnames(by)[2]<-"size"
            } else {
              by<-data.frame(by,size=by[,1])
            }
          }
        }
      } else if (shape[1]==TRUE) {
        colnames(by)[1]<-"shape"
        if(size[1]==TRUE) {
          if (dim(by)[2]>=2) {
            colnames(by)[2]<-"size"
          } else {
            by<-data.frame(by,size=by[,1])
          }
        }
      } else if (size[1]==TRUE) {
        colnames(by)[1]<-"size"
      }
    }
  }

  #documenting all the data and plotting options to attach to the output so the graph can be replotted if desired.
  moreOptions<-list(...)
  ActiveOptions<-list(x=x, by=by, groupLabels=groupLabels,subgroup=subgroup,color=color,shape=shape,size=size, useRgl=useRgl, type=type,theme=theme, main=main,sub=sub, ylab=ylab, xlab=xlab, minorTick=minorTick,trendline=trendline, guides=guides, plotColors=plotColors, logScale=logScale, axisText=axisText, showCalc=FALSE, calcType="none", rotateLabels=rotateLabels, add=add, minorGuides=minorGuides, extendTicks=extendTicks, expLabels=expLabels, lWidth=lWidth, na.rm=na.rm, verbose=verbose,logAdjustment=logAdjustment,xLim=xLim,yLim=yLim, strictLimits=strictLimits, legend=legend,trimTrendLines=trimTrendLines, showTrendConfidence=showTrendConfidence, drawPoints=drawPoints, corMethod=corMethod)
  ActiveOptions<-append(ActiveOptions,moreOptions)

  preFlightBy<-by
  checked<-dataFlightCheck(x,by,na.rm=na.rm,flipFacts = FALSE)
  x<-checked$d
  by<-checked$b
  rm(checked)
  finalStats<-NULL
  finalSummary<-NULL



  #Here we check to see if the user specified any options so that they not overwritten by the designated theme
  finalOptions<-procNiceOptions(x=rep(1,length(by)),by=by,minorTick=minorTick,pointShape=NULL,whiskerLineType=NULL,lWidth=lWidth,capWidth=NULL,pointLaneWidth=FALSE,width=NULL,guides=guides,pointSize=NULL,subgroup=subgroup,stack=F,pointHighlights=FALSE,type="2D",theme=theme,plotColors=plotColors,logScale=logScale,pointMethod=NULL,drawPoints=TRUE,groupLabels=groupLabels,swarmOverflow=NULL, errorCap = NULL, CLOptions=moreOptions)
  minorTick<-finalOptions$minorTick
  pointShape<-finalOptions$pointShape
  lWidth<-finalOptions$lWidth
  guides<-finalOptions$guides
  pointSize<-finalOptions$pointSize
  theme<-finalOptions$theme
  plotColors<-finalOptions$plotColors
  groupLabels<-finalOptions$groupLabels
  pointMethod<-finalOptions$pointMethod
  lineType<-theme$errorBarLineType2D
  theme$plotColors<-plotColors
  curvePoints<-50
  if(!is.null(moreOptions[["curvePoints"]])) {curvePoints<-moreOptions[["curvePoints"]]}
  else if(!is.null(theme$curvePoints)){
    curvePoints<-theme$curvePoints
  }

  if(is.data.frame(by)) {
    if(is.null(groupLabels)){
      if(is.factor(by[,1])) {
        groupLabels<-levels(by[,1])
      } else {
        groupLabels<-levels(factor(by[,1]))
      }
    }
  } else {
    if(is.null(groupLabels)) {
      if(is.factor(by)) {
        groupLabels<-levels(by)
      } else {
        groupLabels<-levels(factor(by))
      }
    }
  }


  #Initialize legend variables so we can update based on options
  legendLabels<-groupLabels
  legendTitle<-"Legend"
  if(!is.null(by)){
    if(legend[1]==FALSE | is.null(legend[1])) {
      legend<-TRUE
    } else if (legend[1]!=TRUE) {
      legendTitle<-legend
    }
  }
  legendColors<-plotColors$points

  #Handling log transformations for x and y
  logScaleX<-FALSE
  logScaleY<-FALSE
  logScaleZ<-NULL
  if(is.null(logScale[1])){logScale[1]<-FALSE}
  if(length(logScale)>1 & is.null(logScale[2])) {logScale[2]<-FALSE}
  if((length(logScale)>1 & sum(logScale[1:3])>0) | logScale[1]!=FALSE) {
    if(length(logScale)==1) {
      if(logScale[1]!=FALSE){
        logScaleX<-logScale
        logScaleY<-logScale
        logScaleZ<-logScale
      }
    } else {
      if(logScale[1]!=FALSE) {
        logScaleX<-logScale[1]
      }
      if(logScale[2]!=FALSE) {
        logScaleY<-logScale[2]
      }
    }
    #special case of 3D plots
    if(length(logScale)>=3 & is.null(logScaleZ)) {
      if(is.null(logScale[3])) {
        logScaleZ<-FALSE
      } else if (is.na(logScale[3]) | logScale[3]==TRUE) {
        logScaleZ<-FALSE
      } else if(is.numeric(logScale[3]) & logScale[3] > 1) {
        logScaleZ<-logScale[3]
      }
    }
  }
  if(is.null(logScaleZ)) {logScaleZ<-FALSE}

  #Handles cases where users want the points overlay to be consistent and the fill to change.
  if(length(legendColors)<=1 & length(plotColors$fill)>1){
    legendColors<-plotColors$fill
  }

  #saving current parameters so we can restore them later
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

  #initialize custom formating vectors and determine plot details for the input
  myColors<-NULL
  myShapes<-NULL
  mySize<-NULL
  if(is.data.frame(x)){

    if(is.null(xlab)) {xlab<-colnames(x)[1]}
    if(is.null(ylab)) {ylab<-colnames(x)[2]}
    if(is.null(zlab) & dim(x)[2]>2) {zlab<-colnames(x)[3]}

    myShapes<-pointShape[1]
    myColors<-plotColors$points[1]
    mySize<-pointSize[1]
    if(is.data.frame(by)) {
      #The strategy here is to calculate the shape, color, and size vectors in advance and then call points()
      if(shape[1]==TRUE) {
        if (length(pointShape)<length(levels(by$shape))) {
          warning("Not enough shape values to uniquely define all levels!\nUse the 'pointShape' argument to define a longer vector of shape values.\n", call. = FALSE)
          pointShape<-rep(pointShape,trunc(length(levels(by$shape))/length(pointShape))+1)
        }
        myShapes<-pointShape[by$shape]
      }
      if(color[1]==TRUE) {
        if (length(plotColors$points)<length(levels(by$color))) {
          warning("Not enough color values to uniquely define all levels!\nUse 'plotColors=list(points=c('color1','color2',...)) to define an alternate point color vector.\nSee the help section on themes and formating for more information.\n", call. = FALSE)
          plotColors$points<-rep(plotColors$points,trunc(length(levels(by$color))/length(plotColors$points))+1)
        }
        myColors<-plotColors$points[by$color]
      }
      if(size[1]==TRUE) {
        #Size needs to be scaled to go from the default to sizeScale times larger
        #If the input vector is a numeric or a factor determines how it gets handled
        #I'm sure there is a better way to do this. Revisit later?
        if(is.numeric(preFlightBy$size)) {
          sizes<-cut(as.numeric(as.character(by$size)),breaks=sizeLevels,labels=FALSE)
          mySize<-((sizes-1)/(sizeLevels-1)*(sizeScale-1))*pointSize[1]+pointSize[1]
        } else {
          sizes<-as.numeric(by$size)
          mySize<-((sizes-1)/(length(levels(by$size))-1))*(sizeScale-1)*pointSize[1]+pointSize[1]

        }
      }
    } else if(!is.null(by)) {
      #If by is just a factor.
      if(shape[1]==TRUE) {
        if (length(pointShape)<length(levels(by))) {
          warning("Not enough shape values to uniquely define all levels!\nUse the 'pointShape' argument to define a longer vector of shape values.\n", call. = FALSE)
          pointShape<-rep(pointShape,trunc(length(levels(by))/length(pointShape))+1)
        }
        myShapes<-pointShape[by]
      }
      if(color[1]==TRUE) {
        if (length(plotColors$points)<length(levels(by))) {
          warning("Not enough color values to uniquely define all levels!\nUse 'plotColors=list(points=c('color1','color2',...)) to define an alternate point color vector.\nSee the help section on themes and formating for more information.\n", call. = FALSE)
          plotColors$points<-rep(plotColors$points,trunc(length(levels(by))/length(plotColors$points))+1)
        }
        myColors<-plotColors$points[by]
      }
      if(size[1]==TRUE) {
        sizes<-as.numeric(by)
        mySize<-((sizes-1)/(length(levels(by))-1)*(sizeScale-1))*pointSize[1]+pointSize[1]
      }
    }
    if (dim(x)[2]>2){
      x_temp<-prepNiceWindow(x, by, minorTick=minorTick, guides=guides, yLim=yLim, xLim=xLim, rotateLabels=rotateLabels, theme=theme, plotColors=plotColors, logScaleX=logScaleX, logScaleY=logScaleY, axisText=axisText, minorGuides=minorGuides, extendTicks=extendTicks, expLabels=expLabels, legend=legend, logAdjustment=logAdjustment, strictLimits=strictLimits, makePlot=FALSE)
      if(logScaleZ!=FALSE) {
        x[,3]<-log(x[,3] + logAdjustment,logScaleZ)
      }
      x[,1:2]<-x_temp
      #3D plot handling
      if(useRgl==TRUE) {
        if (! requireNamespace("rgl", quietly = TRUE)) {
          useRgl<-FALSE
          warning("Unable to load package rgl, proceeding with scatterplot3d plotting options.\nPlease install rgl: install.packages('rgl') or set useRgl to FALSE", call.=FALSE)
        } else {
          box<-TRUE
          if(!is.null(moreOptions[["box"]])){box<-moreOptions[["box"]]}
          axes<-TRUE
          if(!is.null(moreOptions[["axes"]])){axes<-moreOptions[["axes"]]}
          rgl::plot3d(x[,1:3],col=myColors,type="s", main=main,sub=sub,ylab=ylab,xlab=xlab,zlab=zlab, col.main=plotColors$title,col.sub=plotColors$subtext,col.lab=plotColors$numbers,ylim=yLim, xlim=xLim,zlim=zLim, box=box, axes=axes,size=mySize)
          #rgl::plot3d(x[,1:3],col=myColors,type="s", main=main,sub=sub,ylab=ylab,xlab=xlab,zlab=zlab, col.main=plotColors$title,col.sub=plotColors$subtext,col.lab=plotColors$numbers,ylim=yLim, xlim=xLim,zlim=zLim,radius=mySize, box=box, axes=axes,size=pointSize[1])
        }
      }
      if(useRgl==FALSE) {
        #check to see if angle, box, or grid options were set for scatterplot3d
        angle<-40
        if(!is.null(moreOptions[["angle"]])){angle<-moreOptions[["angle"]]}
        box<-TRUE
        if(!is.null(moreOptions[["box"]])){box<-moreOptions[["box"]]}
        grid<-TRUE
        if(!is.null(moreOptions[["grid"]])){grid<-moreOptions[["grid"]]}
        scatterplot3d(x[,1:3],pch=myShapes,color=myColors, main=main,sub=sub,ylab=ylab,xlab=xlab,zlab=zlab, col.main=plotColors$title,col.sub=plotColors$subtext,col.lab=plotColors$numbers,yLim=yLim, xLim=xLim,zlim=zLim,cex.symbols=mySize,angle=angle,grid=grid,box=box)
      }

    } else {
      #2D plot handling
      if(add[1]==FALSE) {
        #RStudio seems not to update the graphics devices properly
        if(Sys.getenv("RSTUDIO") == "1" & is.null(moreOptions[["RSOverride"]])) {graphics.off()}

        x<-prepNiceWindow(x, by, minorTick=minorTick, guides=guides, yLim=yLim, xLim=xLim, rotateLabels=rotateLabels, theme=theme, plotColors=plotColors, logScaleX=logScaleX, logScaleY=logScaleY, axisText=axisText, minorGuides=minorGuides, extendTicks=extendTicks, expLabels=expLabels, legend=legend, logAdjustment=logAdjustment, strictLimits=strictLimits)
        title(main=main,sub=sub,ylab=ylab,xlab=xlab, col.main=plotColors$title,col.sub=plotColors$subtext,col.lab=plotColors$axisLabels)
      }
    }
  } else {
    #Handle if x is a vector
    yData<-x
    x<-data.frame(x=seq(length(x)),y=yData)
    if(is.null(xlab)) {xlab<-colnames(x)[1]}
    if(is.null(ylab)) {ylab<-colnames(x)[2]}
    myShapes<-pointShape[1]
    mySize<-pointSize[1]
    myColors<-plotColors$points[1]
    if(add[1]==FALSE) {
      #RStudio seems not to update the graphics devices properly
      if(Sys.getenv("RSTUDIO") == "1" & is.null(moreOptions[["RSOverride"]])) {graphics.off()}

      x<-prepNiceWindow(x, by, minorTick=minorTick, guides=guides, yLim=yLim, xLim=xLim, rotateLabels=rotateLabels, theme=theme, plotColors=plotColors, logScaleX=logScaleX, logScaleY=logScaleY, axisText=axisText, minorGuides=minorGuides, extendTicks=extendTicks, expLabels=expLabels, legend=legend, logAdjustment=logAdjustment)
      title(main=main,sub=sub,ylab=ylab,xlab=xlab, col.main=plotColors$title,col.sub=plotColors$subtext,col.lab=plotColors$axisLabels)
      if(is.data.frame(by)) {
        #The strategy here is to calculate the shape, color, and size vectors in advance and then call points()
        #Here we assume by is a data.frame
        if(shape[1]==TRUE) {
          if (length(pointShape)<length(levels(by$shape))) {
            warning("Not enough shape values to uniquely define all levels!\nUse the 'pointShape' argument to define a longer vector of shape values.\n", call. = FALSE)
            pointShape<-rep(pointShape,trunc(length(levels(by$shape))/length(pointShape))+1)
          }
          myShapes<-pointShape[by$shape]
        }
        if(color[1]==TRUE) {
          if (length(plotColors$points)<length(levels(by$color))) {
            warning("Not enough color values to uniquely define all levels!\nUse 'plotColors=list(points=c('color1','color2',...)) to define an alternate point color vector.\nSee the help section on themes and formating for more information.\n", call. = FALSE)
            plotColors$points<-rep(plotColors$points,trunc(length(levels(by$color))/length(plotColors$points))+1)
          }
          myColors<-plotColors$points[by$color]
        }
        if(size[1]==TRUE) {
          #Size needs to be scaled to go from the default to sizeScale times larger
          #If the input vector is a numeric or a factor determines how it gets handled
          #I'm sure there is a better way to do this. Revisit later?
          if(is.numeric(preFlightBy$size)) {
            sizes<-cut(as.numeric(as.character(by$size)),breaks=sizeLevels,labels=FALSE)
            mySize<-((sizes-1)/(sizeLevels-1)*(sizeScale-1))*pointSize[1]+pointSize[1]
          } else {
            sizes<-as.numeric(by$size)
            mySize<-((sizes-1)/(length(levels(by$size))-1)*(sizeScale-1))*pointSize[1]+pointSize[1]
          }
        }
      } else if(!is.null(by)) {
        #If by is just a factor.
        if(shape[1]==TRUE) {
          if (length(pointShape)<length(levels(by))) {
            warning("Not enough shape values to uniquely define all levels!\nUse the 'pointShape' argument to define a longer vector of shape values.\n", call. = FALSE)
            pointShape<-rep(pointShape,trunc(length(levels(by))/length(pointShape))+1)
          }
          myShapes<-pointShape[by]
        }
        if(color[1]==TRUE) {
          if (length(plotColors$points)<length(levels(by))) {
            warning("Not enough color values to uniquely define all levels!\nUse 'plotColors=list(points=c('color1','color2',...)) to define an alternate point color vector.\nSee the help section on themes and formating for more information.\n", call. = FALSE)
            plotColors$points<-rep(plotColors$points,trunc(length(levels(by))/length(plotColors$points))+1)
          }
          myColors<-plotColors$points[by]
        }
        if(size[1]==TRUE) {
          sizes<-as.numeric(by)
          mySize<-((sizes-1)/(length(levels(by))-1)*(sizeScale-1))*pointSize[1]+pointSize[1]
        }
      }
    }
  }

  #Add line fitting
  modelStats<-list()
  myFill<-plotColors$fill
  myLineType<-lineType
  myLineCol<-plotColors$lines
  myModels<-NULL
  groupLevels<-NULL
  if(trendline!=FALSE & !is.null(trendline) & ! is.na(trendline)) {
    #OkToDraw is here to make sure the options all check out before plotting (for instance calling 'shape' or 'group' when no associated data is available.)
    OkToDraw<-FALSE
    if(grepl("col", trendline, ignore.case = T) & color[1]==TRUE) {
      OkToDraw<-TRUE
      if(!is.data.frame(by)) {
        by<-data.frame(color=by)
      }
      groupLevels<-length(unique(myColors))
      myModels<-map(levels(by$color), function(l) lm(y~x,data=data.frame(x=x[by$color==l,1],y=x[by$color==l,2])))
      myCor<-map(levels(by$color), function(l) cor.test(x[by$color==l,1],x[by$color==l,2], method=corMethod))
      newX<-map(levels(by$color), function(l) seq(min(x[by$color==l,1]), max(x[by$color==l,1]),length.out = curvePoints))
      confModels<-map(seq(length(newX)), function(i) predict(myModels[[i]], newdata=data.frame(x=newX[[i]]), interval = "confidence", level=.95))
      modelStats<-map(seq(length(myModels)), function(n) list(lm=tidy(myModels[[n]]),cor=myCor[[n]]))
      names(modelStats)<-levels(by$color)
      if(verbose==TRUE){
        print(modelStats)
        #walk(seq(groupLevels), function(p) print(paste0(levels(by$color)[p],":\n",myModels[[p]])))
      }
      #draw trend lines based on groupings that determine point shape
    } else if (grepl("shape", trendline, ignore.case = T) & shape[1]==TRUE) {
      OkToDraw<-TRUE
      groupLevels<-length(unique(myShapes))
      if(!is.data.frame(by)) {
        by<-data.frame(shape=by)
      }
      myModels<-map(levels(by$shape), function(l) lm(y~x,data=data.frame(x=x[by$shape==l,1],y=x[by$shape==l,2])))
      myCor<-map(levels(by$shape), function(l) cor.test(x[by$shape==l,1],x[by$shape==l,2], method=corMethod))
      newX<-map(levels(by$shape), function(l) seq(min(x[by$shape==l,1]), max(x[by$shape==l,1]),length.out = curvePoints))
      confModels<-map(seq(length(newX)), function(i) predict(myModels[[i]], newdata=data.frame(x=newX[[i]]), interval = "confidence", level=.95))
      modelStats<-map(seq(length(myModels)), function(n) list(lm=tidy(myModels[[n]]),cor=myCor[[n]]))
      names(modelStats)<-levels(by$shape)
      if(verbose==TRUE){
        print(modelStats)
        #walk(seq(groupLevels), function(p) print(paste0(levels(by$shape)[p],":\n",myModels[[p]])))
      }
      #If trendline == 'group' then the first column of by is taken as a grouping factor
      #This alows for grouped trend lines independent of color, shape, etc.
    } else if(grepl("group", trendline, ignore.case = T) & !is.null(by)){
      OkToDraw<-TRUE
      if(!is.data.frame(by)){
        by<-data.frame(group=by)
      }
      groupLevels<-length(levels(factor(by[,1])))
      myModels<-map(levels(factor(by[,1])), function(l) lm(y~x,data=data.frame(x=x[by[,1]==l,1],y=x[by[,1]==l,2])))
      myCor<-map(levels(by[,1]), function(l) cor.test(x[by[,1]==l,1],x[by[,1]==l,2], method=corMethod))
      newX<-map(levels(by[,1]), function(l) seq(min(x[by[,1]==l,1]), max(x[by[,1]==l,1]),length.out = curvePoints))
      confModels<-map(seq(length(newX)), function(i) predict(myModels[[i]], newdata=data.frame(x=newX[[i]]), interval = "confidence", level=.95))
      modelStats<-map(seq(length(myModels)), function(n) list(lm=tidy(myModels[[n]]),cor=myCor[[n]]))
      names(modelStats)<-levels(by[,1])
      if(verbose==TRUE){
        print(modelStats)
        #walk(seq(groupLevels), function(p) cat(paste0(levels(by[,1])[p],":\n",summary(myModels[[p]]))))
      }
    }
    else if(trendline==TRUE){
      OkToDraw<-TRUE
      groupLevels<-1
      myModels<-list(m=lm(y~x,data=data.frame(x=x[[1]], y=x[[2]])))
      myCor<-list(c=cor.test(x[[1]], x[[2]],method=corMethod))
      newX<-list(x=seq(min(x[[1]]),max(x[[1]]),length.out = curvePoints))
      confModels <- list(m=predict(myModels[[1]], newdata=data.frame(x=newX), interval = "confidence", level=.95))
      modelStats<-list(lm=tidy(myModels[[1]]),cor=myCor[[1]])
      if(verbose==TRUE){
        print(myModels)
      }
    } else {
      warning("Unable to interpret the trendline options.\nTrendlines will be suppressed.\nSee help for more information.\n", call. = FALSE)
    }
    if(OkToDraw==TRUE) {
      if (length(myFill)<groupLevels) {
        warning("Not enough fill values to uniquely define all levels!\nUse 'plotColors=list(fill=c('color1','color2',...)) to define an alternate fill color vector.\nSee the help section on themes and formating for more information.\n", call. = FALSE)
        myFill<-rep(plotColors$fill,trunc(groupLevels/length(plotColors$fill))+1)
      }
      if (length(myLineCol)<groupLevels) {
        warning("Not enough line color values to uniquely define all levels!\nUse 'plotColors=list(fill=c('color1','color2',...)) to define an alternate fill color vector.\nSee the help section on themes and formating for more information.\n", call. = FALSE)
        myLineCol<-rep(plotColors$lines,trunc(groupLevels/length(plotColors$lines))+1)
      }
      if (length(myLineType)<groupLevels) {
        #warning("Not enough line type values to uniquely define all levels!\nUse 'plotColors=list(fill=c('color1','color2',...)) to define an alternate fill color vector.\nSee the help section on themes and formating for more information.\n", call. = FALSE)
        myLineType<-rep(myLineType,trunc(groupLevels/length(myLineType))+1)
      }
      if(showTrendConfidence==TRUE) {
        walk(seq(length(newX)), function(i) polygon(c(newX[[i]],rev(newX[[i]])),c(confModels[[i]][,2],rev(confModels[[i]][,3])),border = NA,col=myFill[i]))
        walk(seq(length(newX)), function(i) {lines(newX[[i]], confModels[[i]][,2], col=myLineCol[i], lty=myLineType[i]); lines(newX[[i]], confModels[[i]][,3], col=myLineCol[i], lty=myLineType[i])})
      }
      if(trimTrendLines==TRUE) {
        walk(seq(length(myModels)), function(m) segments(min(newX[[m]]),predict(myModels[[m]],newdata=data.frame(x=min(newX[[m]]))),max(newX[[m]]),predict(myModels[[m]],newdata=data.frame(x=max(newX[[m]]))),lwd=lWidth*1.5,col=myLineCol[m]))
      } else {
        walk(seq(length(myModels)), function(m) abline(myModels[[m]],lwd=lWidth*1.5,col=myLineCol[m],lty=myLineType[m]))
      }
    }
  }
  if(drawPoints[1]==TRUE) {
    points(x[[1]],x[[2]],
      col=myColors,
      pch=myShapes,
      cex=mySize,
      type=type)
    xfilter<-1
    byfilter<-1
    IDs<-1
    if(is.vector(ActiveOptions$x)) {
      xfilter<-!is.na(ActiveOptions$x)
      IDs<-seq(length(ActiveOptions$x))
    } else {
      xfilter<-rowSums(is.na(ActiveOptions$x))==0
      IDs<-seq(nrow(ActiveOptions$x))
    }
    if(is.null(preFlightBy)) {
      byfilter<-rep(TRUE,length(xfilter))
    } else {
      if(is.vector(preFlightBy) | is.factor(preFlightBy)) {
        byfilter<-!is.na(preFlightBy)
      } else {
        byfilter<-rowSums(is.na(preFlightBy))==0
      }
    }
    if(na.rm==TRUE) {
      ActiveOptions$xypos<-data.frame(x=x[[1]],y=x[[2]],ID=IDs[xfilter & byfilter])
    } else {
      ActiveOptions$xypos<-data.frame(x=x[[1]],y=x[[2]],ID=IDs)
    }
  }

  #Note from niceDensity, needs review
  if(length(legendColors)<length(legendLabels) & legend!=FALSE){
    legend<-FALSE
    warning("Not enough point colors to uniquely color subgroups levels\nPlease update plotColors point options to use legend options with this subgroup.", call.=FALSE)
  }

  if(legend!=FALSE) {
    makeNiceLegend(labels=legendLabels, title=legendTitle, fontCol=plotColors$labels, border=plotColors$legendBorder, lineCol=plotColors$legendLineCol, bg=plotColors$legendBG, col=legendColors, shape="rect",size=theme$legendSize,spacing=theme$legendSpacing)
  }
  par(col.sub=oSubCol, col.lab=oLabCol,col.main=oMainCol,family=oFont,cex.main=oCexMain,cex.lab=oCexlab,cex.sub=oCexSub)

  #formatting the output list and setting class int npData
  dataOut<-list(summary=finalSummary,stats=modelStats,plotType="scatter",options=ActiveOptions)
  class(dataOut)<-c("npData","list")

  invisible(dataOut)
}

#' @export
niceScatter.npData <- function(x,  ...) {
  clOptions<-list(...)
  for(opt in names(clOptions)) {
    if(is.null(x$options[opt])){
      append(x$options,list(opt=clOptions[[opt]]))
    }else{
      x$options[[opt]]<-clOptions[[opt]]
    }
  }

  dataOut<-do.call("niceScatter",x$options)
  invisible(dataOut)
}

