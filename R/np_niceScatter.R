#' @include np_options_processing.R niceThemes.R np_categorical_plot_setup.R np_plotting_functions.R np_utility.R np_data_processing.R
#' @title draw a scatter plot
#' @description Draws a 2D or 3D scatter plot with optional regression lines.
#' @details
#' Something goes here
#'
#'
#'
#' @inheritParams prepNiceWindow
#' @param color factor or logical; if by is NULL, a factor suppolied to this option will populate by to color points by the factor levels. If by is defined then color should be set to TRUE or FALSE.
#' @param shape factor or logical; if by is NULL, a factor suppolied to this option will populate by to control the shape of the points points by the factor levels. If by is defined then shape should be set to TRUE or FALSE.
#' @param size factor or logical; if by is NULL, a factor suppolied to this option will populate by to control the size of the points by the factor levels. If by is defined then color should be set to TRUE or FALSE.
#' @param trendline To do.
#' @param groupNames character vector; overrides the factor levels of \code{by} to label the groups
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
#' @param trimCurves logical; Should the density curves be restricted to the min and max of the data.
#' @param ... additional options for S3 method variants
#'
#' @examples
#' ToDo<-1
#'
#' @seealso
#' @import dplyr
#' @importFrom purrr map_lgl map_dbl map
#' @importFrom graphics points
#' @importFrom tibble is.tibble
#' @impartFrom broom tidy
#' @export
#' @seealso \code{\link{niceDensity}}
niceScatter<-function(x, by=NULL, color=NULL, shape=NULL, size=NULL,trendline=FALSE, sizeScale=3, sizeLevels=6, groupNames=NULL, subGroup=FALSE, bandwidth=NULL, useRgl=FALSE, type="p",theme=basicTheme, main=NULL,sub=NULL, ylab=NULL, xlab=NULL, zlab=NULL,  minorTick=FALSE, guides=NULL, plotColors=NULL, logScale=FALSE, axisText=c(NULL,NULL), rotateLabels=FALSE, add=FALSE, minorGuides=FALSE, extendTicks=TRUE, expLabels=FALSE, lWidth=NULL, na.rm=FALSE, verbose=FALSE,logAdjustment=1,xLim=NULL,yLim=NULL,zLim=NULL, strictLimits=FALSE, legend=FALSE,trimCurves=TRUE, ...) {UseMethod("niceScatter",x)}

#' @import dplyr
#' @importFrom purrr map_lgl map_dbl map
#' @importFrom graphics points
#' @importFrom tibble is.tibble
#' @importFrom scatterplot3d scatterplot3d
#' @export
niceScatter.default <-function(x, by=NULL, color=NULL, shape=NULL, size=NULL,trendline=FALSE, sizeScale=3, sizeLevels=6, groupNames=NULL, subGroup=FALSE, bandwidth=NULL, useRgl=FALSE, type="p",theme=basicTheme, main=NULL,sub=NULL, ylab=NULL, xlab=NULL, zlab=NULL, minorTick=FALSE, guides=NULL, plotColors=NULL, logScale=FALSE, axisText=c(NULL,NULL), rotateLabels=FALSE, add=FALSE, minorGuides=FALSE, extendTicks=TRUE, expLabels=FALSE, lWidth=NULL, na.rm=FALSE, verbose=FALSE,logAdjustment=1,xLim=NULL,yLim=NULL, zLim=NULL, strictLimits=FALSE, legend=FALSE,trimCurves=TRUE, ...) {
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
    if(is.data.frame(by) | is.tibble(by)) {
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
  preFlightBy<-by
  checked<-dataFlightCheck(x,by,na.rm=na.rm,flipFacts = FALSE)
  x<-checked$d
  by<-checked$b
  rm(checked)
  finalStats<-NULL
  finalSummary<-NULL

  #documenting all the data and plotting options to attach to the output so the graph can be replotted if desired.
  moreOptions<-list(...)
  ActiveOptions<-list(x=x, by=by, drawPoints=TRUE, groupNames=groupNames,subGroup=subGroup, useRgl=useRgl, type=type,theme=theme, main=main,sub=sub, ylab=ylab, xlab=xlab, minorTick=minorTick, guides=guides, plotColors=plotColors, logScale=logScale, axisText=axisText, showCalc=FALSE, calcType="none", rotateLabels=rotateLabels, add=add, minorGuides=minorGuides, extendTicks=extendTicks, expLabels=expLabels, lWidth=lWidth, na.rm=na.rm, verbose=verbose,logAdjustment=logAdjustment,xLim=xLim,yLim=yLim, strictLimits=strictLimits, legend=legend,trimCurves=trimCurves)
  ActiveOptions<-append(ActiveOptions,moreOptions)

  #Here we check to see if the user specified any options so that they not overwritten by the designated theme
  finalOptions<-procNiceOptions(x=rep(1,length(by)),by=by,minorTick=minorTick,pointShape=NULL,whiskerLineType=NULL,lWidth=lWidth,capWidth=NULL,pointLaneWidth=FALSE,width=NULL,guides=guides,pointSize=NULL,subGroup=subGroup,stack=F,pointHighlights=FALSE,type="2D",theme=theme,plotColors=plotColors,logScale=logScale,pointMethod=NULL,drawPoints=TRUE,groupNames=groupNames,swarmOverflow=NULL, errorCap = NULL, CLOptions=moreOptions)
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
  curvePoints<-50
  if(!is.null(moreOptions[["curvePoints"]])) {curvePoints<-moreOptions[["curvePoints"]]}
  else if(!is.null(theme$curvePoints)){
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
  if(!is.null(by) & (subGroup==TRUE | is.data.frame(x))){
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
          rgl::plot3d(x[,1:3],pch=myShapes,col=myColors,type=type, main=main,sub=sub,ylab=ylab,xlab=xlab,zlab=zlab, col.main=plotColors$title,col.sub=plotColors$subtext,col.lab=plotColors$numbers,yLim=yLim, xLim=xLim,zlim=zLim,radius=mySize/10, box=box, axes=axes,size=pointSize[1])
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
        if(Sys.getenv("RSTUDIO") == "1") {graphics.off()}
        x<-prepNiceWindow(x, by, minorTick=minorTick, guides=guides, yLim=yLim, xLim=xLim, rotateLabels=rotateLabels, theme=theme, plotColors=plotColors, logScaleX=logScaleX, logScaleY=logScaleY, axisText=axisText, minorGuides=minorGuides, extendTicks=extendTicks, expLabels=expLabels, legend=legend, logAdjustment=logAdjustment)
        title(main=main,sub=sub,ylab=ylab,xlab=xlab, col.main=plotColors$title,col.sub=plotColors$subtext,col.lab=plotColors$numbers)
      }
     # points(x[[1]],x[[2]],
      #       col=myColors,
       #      pch=myShapes,
        #     cex=mySize)
    }
  } else {
    #Handle if x is a vector
    x<-data.frame(x=seq(length(x)),y=x)
    if(is.null(xlab)) {xlab<-colnames(x)[1]}
    if(is.null(ylab)) {ylab<-colnames(x)[2]}
    if(add[1]==FALSE) {
      #RStudio seems not to update the graphics devices properly
      if(Sys.getenv("RSTUDIO") == "1") {graphics.off()}
      x<-prepNiceWindow(x, by, minorTick=minorTick, guides=guides, yLim=yLim, xLim=xLim, rotateLabels=rotateLabels, theme=theme, plotColors=plotColors, logScaleX=logScaleX, logScaleY=logScaleY, axisText=axisText, minorGuides=minorGuides, extendTicks=extendTicks, expLabels=expLabels, legend=legend, logAdjustment=logAdjustment)
      title(main=main,sub=sub,ylab=ylab,xlab=xlab, col.main=plotColors$title,col.sub=plotColors$subtext,col.lab=plotColors$numbers)
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
      #points(x[[1]],x[[2]],
       #      col=myColors,
        #     pch=myShapes,
         #    cex=mySize,
          #   type=type)
    }
  }

  #Add line fitting
  modelStats<-list()
  if(trendline!=FALSE & !is.null(trendline) & ! in.na(trendline)) {
    if(grepl("col", trendline, ignore.case = T & color[1]==TRUE)) {
      myModels<-map(levels(by$color), function(l) lm(y~x,data=data.frame(x=x[by$color==l,1],y=x[by$color==l,2])))
      newX<-map(levels(by$color), function(l) seq(min(x[by$color==l,1]), max(x[by$color==l,1]),length.out = curvePoints))
      confModels<-map(seq(length(newX)), function(i) predict(myModels[[i]], newdata=data.frame(x=newX[[i]]), interval = "confidence", level=.95))
      walk(seq(length(newX)), function(i) polygon(c(newX[[i]],rev(newX[[i]])),c(confModels[[1]][,2],rev(confModels[[i]][,3])),border = NA,col=myFill[i],lty=myLineType[i]))
      walk(seq(length(newX)), function(i) {lines(newX[[i]], confModels[[i]][,2], col=myLineCol[i], lty=myLineType[i]); lines(newX[[i]], confModels[[i]][,3], col=myLineCol[i], lty=myLineType[i])})
      modelStats<-map(myModels,tidy)
      names(modelStats)<-levels(by$color)
    } else if (grepl("shape", trendline, ignore.case = T)) {

    } else if(grepl("group", trendline, ignore.case = T) & !is.null(by)){

    } else {
      dataModel<-lm(y~x,data=data.frame(x=x[[1]], y=x[[2]]))
      newX=seq(min(x[[1]]),max(x[[2]]),length.out = curvePoints)
      modelConf <- predict(dataModel, newdata=data.frame(newX), interval = "confidence", level=.95)
      polygon(c(newX,rev(newX)),c(modelConf[,2],rev(modelConf[,3])),border = NA,col=plotColors$fill[1])
      walk(list(conf[,2],conf[,3]),function(y) lines(newX,y), lty=finalOptions$whiskerLineType[1],col=plotColors$lines[1])
      abline(dataModel,lwd=lWidth*2,col=plotColors$lines[1])
      modelStats<-tidy(dataModel)
      if(verbose==TRUE){
        cat(dataModel)
      }
      lm.out <- lm(y ~ x)
    }
  }
  points(x[[1]],x[[2]],
    col=myColors,
    pch=myShapes,
    cex=mySize,
    type=type)

  #Note from niceDensity, needs review
  if(length(legendColors)<length(legendLabels) & legend!=FALSE){
    legend<-FALSE
    warning("Not enough point colors to uniquely color subGroups levels\nPlease update plotColors point options to use legend options with this subgroup.", call.=FALSE)
  }

  if(legend!=FALSE) {
    makeNiceLegend(labels=legendLabels, title=legendTitle, fontCol=plotColors$labels, border=theme$LegendBorder, lineCol=theme$LegendLineCol, bg=theme$LegendBG, col=legendColors, shape="rect",size=theme$LegendSize,spacing=theme$LegendSpacing)
  }
  par(col.sub=oSubCol, col.lab=oLabCol,col.main=oMainCol,family=oFont,cex.main=oCexMain,cex.lab=oCexlab,cex.sub=oCexSub)

  #formating the output list and setting class int npData
  dataOut<-list(summary=finalSummary,stats=modelStats,plotType="scatter",options=ActiveOptions)
  class(dataOut)<-c("npData","list")

  invisible(dataOut)
}

