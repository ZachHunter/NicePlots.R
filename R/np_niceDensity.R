#' @include np_options_processing.R niceThemes.R np_categorical_plot_setup.R np_plotting_functions.R np_utility.R np_data_processing.R
#' @title draw a kernal density plot
#' @description Draws a kernal density plot of one or two variables.
#' @details
#' \code{niceDensity} is draws gernal density plots. This can be a single kernal density plot or a series of density plots overplotted
#' on the graph based on a grouping factor from \code{by}. The \code{subgroup} option should be set to \code{\link{TRUE}} to anable this functionality.
#' If two or more variables are passed in \code{x} in a \code{\link{data.frame}}, \code{\link{matrix}} or \code{\link[tibble]{tibble}}, then the
#' first two columns will be used to generate a scatter plot with a 2-D scatter plot with a \code{\link[graphics]{contour}} plot overlay giving the 2D
#' kernal density estimates. Setting \code{subgroup} to \code{\link{TRUE}} is this case will color the points by the first factor in \code{by}.
#' Alternatively, for two column input,  \code{plotType} can be set to "suface" to draw a 3D prepresnetation of the kernal density over the x-y plane using \code{\link[graphics]{persp}}.
#' Plotting options such as \code{theta}/\code{phi} to control rotation for \code{\link[graphics]{persp}} or \code{nlevels} for \code{\link[graphics]{contour}}, can be entered as arguments to \code{niceDensity} and will be passed along via \code{\link{do.call}}.
#' Finally, if the \code{rgl} package is installed and \code{useRgl} is set to \code{\link{TRUE}}, the interactive \code{\link[rgl]{persp3d}} function will be used instead.
#' All kernal densities are calculated using the \code{KernSmooth} package. The values in \code{npData} object returned by the function are slightly different.
#' The "summary" value gives the min and max for \code{x} and optionally \code{y} along with the density estimate \code{fhat}.
#' The "stats" value contains a \code{\link{data.frame}} the actual \code{x}, \code{y}, and \code{fhat} paired values.
#' The theme value \code{curvePoints} is used to determine the number of line segments per curve and can be overriden in the command line like all other theme options.
#'
#'
#'
#' @inheritParams prepNiceWindow
#' @param groupNames character vector; overrides the factor levels of \code{by} to label the groups
#' @param drawPoints logical; draws a dot plot overlay for contour plots
#' @param subgroup logical; Will use the factor levels in \code{by} to plot a series of distributions from the data in \code{x}.
#' @param bandwidth numeric; Manually sets the bandwith for the kernal density estimation overiding default calculations. For 2D plots \code{bandwidth} should be a vector of length 2. Set to \code{\link{NULL}} or \code{\link{NA}} to enable default calculations.
#' @param drawRug logical; adds a \code{\link[graphics]{rug}} plot overlay to a kernel density plot. Default is FALSE.
#' @param useRgl logical; Should the library \code{\link[rgl]{rgl}} be used to make 3D surface plots.
#' @param plotType character; Can be set to \code{contour} or \code{surface} to control the type of 2D plot.
#' @param theme list object; Themes are are an optional way of storing graphical preset options that are compatible with all nicePlot graphing functions.
#' @param main character; title for the graph which is supplied to the \code{main} argument.
#' @param sub character; subtitle for the graph which is supplied to the \code{sub} argument.
#' @param ylab character; y-axis label.
#' @param xlab character; x-axis label.
#' @param xLim numeric vector; lower a upper limits for the x-axis.
#' @param na.rm logical; Should \code{NA}s be removed from the data set? Both data input and the factor input from \code{by} with be checked.
#' @param verbose logical; Prints summary and p-value calculations to the screen. All data is silently by the function returned either way.
#' @param sidePlot logical; Swaps the x and y axis for ploting. Does not apply to 2D and 3D plots.
#' @param add logical; Adds the plot to the existing window rather the generating a new plotting enviroment
#' @param lWidth numeric; Line width, equivelent to the cex \code{lwd} option.
#' @param logScale numeric; A length two numeric vector indicating the log scale the x and y axis, respectively. If only one value is given, it is applied to boxth axis. Set to \code{\link{FALSE}} to disable (Default).
#' @param trimCurves logical; Should the density curves be restricted to the min and max of the data.
#' @param ... additional options for S3 method variants
#'
#' @examples
#' data(iris)
#' #Basic kernal density plot
#' niceDensity(iris[,1], main="Distrinbution of Sepal Length")
#'
#' #Kernal density plot subgrouped by species
#' niceDensity(iris[,1],iris$Species, main="Sepal Length by Species",
#' subgroup=TRUE)
#'
#' #Same thing with trimCurves=FALSE
#' niceDensity(iris[,1],iris$Species, main="Sepal Length by Species",
#' subgroup=TRUE, trimCurves=FALSE)
#'
#' #2D Density of Sepal Width vs. Length
#' niceDensity(iris[,1:2],iris$Species, main="Sepal Width vs. Length",
#' subgroup=TRUE)
#'
#' #Representing the 2D contour plot in 3D
#' niceDensity(iris[,1:2],iris$Species, main="Sepal Width vs. Length",
#' plotType="surface", theta=-60)
#'
#' @seealso \code{\link[stats]{density}}, \code{\link[graphics]{contour}}, \code{\link[graphics]{persp}}, \code{\link[rgl]{persp3d}}, \code{\link[KernSmooth]{bkde}}
#' @importFrom magrittr %>%
#' @importFrom dplyr bind_cols mutate arrange
#' @importFrom KernSmooth bkde bkde2D dpih
#' @importFrom purrr walk2 map_dbl
#' @importFrom graphics polygon contour lines persp rug
#' @export
niceDensity<-function(x, by=NULL, drawPoints=TRUE, groupNames=NULL, subgroup=FALSE, bandwidth=NULL, drawRug=FALSE, useRgl=FALSE, plotType=c("contour","surface"),theme=basicTheme, main=NULL,sub=NULL, ylab=NULL, xlab=NULL, minorTick=FALSE, guides=NULL, plotColors=NULL, logScale=FALSE, axisText=c(NULL,NULL), rotateLabels=FALSE, add=FALSE, minorGuides=NULL, extendTicks=TRUE, expLabels=FALSE, lWidth=NULL, na.rm=TRUE, verbose=FALSE,logAdjustment=1,xLim=NULL,yLim=NULL, strictLimits=FALSE, legend=FALSE,trimCurves=TRUE, sidePlot=FALSE, ...) {UseMethod("niceDensity",x)}

#' @importFrom dplyr bind_cols mutate arrange
#' @importFrom magrittr %>%
#' @importFrom KernSmooth bkde bkde2D dpih
#' @importFrom purrr walk2 map_dbl map
#' @importFrom tidyr gather
#' @importFrom graphics polygon contour lines persp rug
#' @export
niceDensity.default<-function(x, by=NULL, drawPoints=TRUE, groupNames=NULL,subgroup=FALSE, bandwidth=NULL, drawRug=FALSE, useRgl=FALSE, plotType=c("contour","surface"),theme=basicTheme, main=NULL,sub=NULL, ylab=NULL, xlab=NULL, minorTick=FALSE, guides=NULL, plotColors=NULL, logScale=FALSE, axisText=list(x=c(NULL,NULL),y=c(NULL,NULL)), rotateLabels=TRUE, add=FALSE, minorGuides=NULL, extendTicks=TRUE, expLabels=FALSE, lWidth=NULL, na.rm=TRUE, verbose=FALSE,logAdjustment=1,xLim=NULL,yLim=NULL, strictLimits=FALSE, legend=FALSE,trimCurves=TRUE, sidePlot=FALSE, ...)  {
  if(any(is.na(x)) | any(is.na(by))){warning("Warning: NAs detected in dataset",call.=FALSE)}
  prepedData<-NULL
  plotData<-NULL

  #documenting all the data and plotting options to attach to the output so the graph can be replotted if desired.
  moreOptions<-list(...)
  ActiveOptions<-list(x=x, by=by, drawPoints=drawPoints, groupNames=groupNames,subgroup=subgroup, useRgl=useRgl, plotType=plotType,theme=theme, main=main,sub=sub, ylab=ylab, xlab=xlab, minorTick=minorTick, guides=guides, plotColors=plotColors, logScale=logScale, axisText=axisText, showCalc=FALSE, calcType="none", rotateLabels=rotateLabels, add=add, minorGuides=minorGuides, extendTicks=extendTicks, expLabels=expLabels, lWidth=lWidth, na.rm=na.rm, verbose=verbose,logAdjustment=logAdjustment,xLim=xLim,yLim=yLim, strictLimits=strictLimits, legend=legend,trimCurves=trimCurves)
  ActiveOptions<-append(ActiveOptions,moreOptions)

  #Flight check data and remove na.
  checked<-dataFlightCheck(x,by,na.rm=na.rm,flipFacts = FALSE)
  x<-checked$d
  by<-checked$b
  rm(checked)
  if(is.data.frame(by)){
    by<-factor(by[,1])
  }
  finalStats<-NULL
  finalSummary<-NULL

  #Here we check to see if the user specified any options so that they not overwritten by the designated theme
  finalOptions<-procNiceOptions(x=rep(1,length(by)),by=by,minorTick=minorTick,pointShape=NULL,whiskerLineType=NULL,lWidth=lWidth,capWidth=NULL,pointLaneWidth=FALSE,width=NULL,guides=guides,pointSize=NULL,subgroup=subgroup,stack=F,pointHighlights=FALSE,type="VP",theme=theme,plotColors=plotColors,logScale=logScale,pointMethod=NULL,drawPoints=drawPoints,groupNames=groupNames,swarmOverflow=NULL, errorCap = NULL, CLOptions=moreOptions)
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
    if(is.data.frame(x) & useRgl[1]==FALSE & plotType[1]=="surface") {curvePoints<-round(curvePoints/4)}
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
  if(!is.null(by) & (subgroup==TRUE | is.data.frame(x))){
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

  #Dealing with 2D data
  if(is.data.frame(x)){
    if(is.null(xlab)) {xlab<-colnames(x)[1]}
    if(is.null(ylab)) {ylab<-colnames(x)[2]}
    if(add[1]==FALSE) {
      #RStudio seems not to update the graphics devices properly
      if(Sys.getenv("RSTUDIO") == "1" & is.null(moreOptions[["RSOveride"]])) {graphics.off()}

      x<-prepNiceWindow(x, by, minorTick=minorTick, guides=guides, yLim=yLim, xLim=xLim, rotateLabels=rotateLabels, theme=theme, plotColors=plotColors, logScaleX=logScaleX, logScaleY=logScaleY, axisText=axisText, minorGuides=minorGuides, extendTicks=extendTicks, expLabels=expLabels, legend=legend, logAdjustment=logAdjustment)
      title(main=main,sub=sub,ylab=ylab,xlab=xlab, col.main=plotColors$title,col.sub=plotColors$subtext,col.lab=plotColors$axisLabels)
    }
    #Calculate 2D Kernal Density

    tryCatch({
        #Calculate bandwidth of x and y if not specified manually
        if(length(bandwidth)!=2) {
          bandwidth<-c(dpih(x[,1],gridsize=curvePoints),dpih(x[,2],gridsize=curvePoints))
        } else {
          if (is.null(bandwidth[1]) | is.na(bandwidth[1])) {bandwidth[1]<-dpih(x[,1],gridsize=curvePoints)}
          #Note the second element can not be set to NULL without reducing the vector to length one.
          if (is.na(bandwidth[2])) {bandwidth[2]<-dpih(x[,2],gridsize=curvePoints)}
        }
        den2D<-bkde2D(as.matrix(x[,1:2]),bandwidth=bandwidth,gridsize=c(curvePoints,curvePoints))
      },
      error=function(e) {
        warning("KernSmooth bkde2D unable to estimate kernal density.")
        return(-1)
      }
    )
    #Saving Stats for output
    finalSummary<-data.frame(Min=c(min(x[,1]),min(x[,2]),0),Max=c(max(x[,1]),max(x[,2]),max(den2D$fhat)),row.names = c("x","y","fhat"))
    finalStats<-data.frame(x=den2D$x1,fhat=den2D$fhat)
    colnames(finalStats)<-c("x",den2D$x2)
    finalStats<-finalStats %>%
      gather(key="y",value="fhat",-.data$x) %>%
      mutate(y=as.numeric(.data$y)) %>%
      arrange(.data$x,.data$y)

    if(plotType[1]=="contour") {
      if(!is.numeric(den2D)) {
        myCol<-plotColors$lines[1]
        if(!is.null(moreOptions[["col"]])){myCol<-moreOptions[["col"]]}
        do.call("contour", append(list(x=den2D$x1,y=den2D$x2,z=den2D$fhat,col=myCol,main=main,sub=sub,ylab=ylab, add=TRUE,lwd=lWidth),moreOptions[!names(moreOptions) %in% c("z","x","y","col", "curvePoints")]))
      }
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
        if(length(pointShape) < n_groups) {pointShape<-rep(pointShape,n_groups)}
        if(length(plotColors$points) < n_groups) {plotColors$points<-rep(plotColors$points,n_groups)}
        for(i in 1:n_groups){
          points(x[groups==levels(groups)[i],1],x[groups==levels(groups)[i],2],col=plotColors$points[i],cex=pointSize,pch=pointShape[i])
        }
        if(is.vector(ActiveOptions$x)) {
          xfilter<-!is.na(ActiveOptions$x)
          IDs<-seq(length(ActiveOptions$x))
        } else {
          xfilter<-rowSums(is.na(ActiveOptions$x))==0
          IDs<-seq(nrow(ActiveOptions$x))
        }
        if(is.null(ActiveOptions$by)) {
          byfilter<-rep(TRUE,length(xfilter))
        } else {
          if(is.vector(ActiveOptions$by) | is.factor(ActiveOptions$by)) {
            byfilter<-!is.na(ActiveOptions$by)
          } else {
            byfilter<-rowSums(is.na(ActiveOptions$by))==0
          }
        }
        if(na.rm==TRUE) {
          ActiveOptions$xypos<-data.frame(x=x[[1]],y=x[[2]],ID=IDs[xfilter & byfilter])
        } else {
          ActiveOptions$xypos<-data.frame(x=x[[1]],y=x[[2]],ID=IDs)
        }
      }
    } else if(plotType=="surface" & !is.numeric(den2D)) {
      if(useRgl==TRUE) {
        if (! requireNamespace("rgl", quietly = TRUE)) {
          useRgl<-FALSE
          warning("Unable to load package rgl, proceeding with base R plotting options.\nPlease install rgl: install.packages('rgl') or set useRgl to FALSE", call.=FALSE)
        } else {
          rgl::open3d()
          rgl::bg3d("white")
          rgl::material3d(col = "black")
          myCol<-plotColors$fill[1]
          if(!is.null(moreOptions[["col"]])){myCol<-moreOptions[["col"]]}
          rgl::persp3d(den2D$x1, den2D$x2, den2D$fhat, col=myCol,xlab = xlab,ylab=ylab,zlab = "Density")
        }
      }
      if(useRgl==FALSE){
        legend<-FALSE
        theta <- 0
        phi <- 15
        r <- sqrt(3)
        d <- 1
        if(!is.null(moreOptions[["theta"]])) {theta<-moreOptions[["theta"]]}
        if(!is.null(moreOptions[["phi"]])) {phi<-moreOptions[["phi"]]}
        if(!is.null(moreOptions[["r"]])) {r<-moreOptions[["r"]]}
        if(!is.null(moreOptions[["d"]])) {d<-moreOptions[["d"]]}
        if(!is.null(moreOptions[["col"]])) {col<-moreOptions[["col"]]}else{col<-plotColors$fill[1]}
        do.call("persp", append(list(x=den2D$x1, y=den2D$x2, z=den2D$fhat,col=col,main = main,xlab = xlab,ylab=ylab,zlab = "Density",theta=theta,phi=phi,r=r,d=d),moreOptions[!names(moreOptions) %in% c("theta","phi","d","r","col","curvePoints")]))
      }
    } else {
      stop("Error: plotType should be set to either \'contour\' or \'surface\' for 2D desntsity plots")
    }
  } else {
  #Dealing with typical 1D densties
    if(logScaleX+logScaleY!=0) {
       x<-log(x + logAdjustment, logScaleX)
    }
    if(is.null(ylab)) {ylab<-"Density"}
    if(!is.null(by) & subgroup==TRUE){
      if(length(plotColors$fill)==1) {plotColors$fill<-plotColors$points}
      n_groups<-length(levels(by))
      densities<-vector(mode="list",length=n_groups)
      if(trimCurves[1]==TRUE) {
        for(i in 1:n_groups){
          if(length(x[by==levels(by)[i]])>1) {
            tryCatch({
                myX<-x[by==levels(by)[i]]
                if(is.null(bandwidth)){
                  densities[[i]]<-bkde(myX,gridsize=curvePoints,range.x=c(min(myX),max(myX)))
                } else if (is.na(bandwidth[1]) | !is.numeric(bandwidth[1])) {
                  densities[[i]]<-bkde(myX,gridsize=curvePoints,range.x=c(min(myX),max(myX)))
                } else {
                  densities[[i]]<-bkde(myX,gridsize=curvePoints,bandwidth=bandwidth[1], range.x=c(min(myX),max(myX)))
                }
                densities[[i]]$x<-c(min(myX),densities[[i]]$x,max(myX))
                densities[[i]]$y<-c(0,densities[[i]]$y,0)
              },
              warning=function(w) {
                warning(paste0("KernSmooth bkde could not generate a kernal density estimate for ",levels(by)[i]),call.=FALSE)
                densities[[1]]<- NA
              },
              error=function(e) {
                warning(paste0("KernSmooth bkde could not generate a kernal density estimate for ",levels(by)[i]),call.=FALSE)
                densities[[1]]<- NA
              }
            )
          }
        }
      } else {
        for(i in 1:n_groups){
          if(length(x[by==levels(by)[i]])>1) {
            tryCatch({
                if(is.null(bandwidth)) {
                  densities[[i]]<-bkde(x[by==levels(by)[i]],gridsize=curvePoints)
                } else if (is.na(bandwidth[1]) | !is.numeric(bandwidth[1])) {
                  densities[[i]]<-bkde(x[by==levels(by)[i]],gridsize=curvePoints)
                } else {
                  densities[[i]]<-bkde(x[by==levels(by)[i]],bandwidth=bandwidth[1],gridsize=curvePoints)
                }
              },
              warning=function(w) {
                warning(paste0("KernSmooth bkde could not generate a kernal density estimate for ",levels(by)[i]),call.=FALSE)
                densities[[i]]<- NA
              },
              error=function(e) {
                warning(paste0("KernSmooth bkde could not generate a kernal density estimate for ",levels(by)[i]),call.=FALSE)
                densities[[i]]<- NA
              }
            )
          }
        }
      }
      #Saving stats for output
      finalSummary<-data.frame(xMin=map_dbl(densities, function(d) d$x[1]),xMax=map_dbl(densities, function(d) d$x[length(d$x)]), fhatMax=map_dbl(densities, function(d) max(d$y)),row.names=levels(by))
      finalStats<-densities

      maxx<-max(map_dbl(densities, function(z) if(!is.list(z)) {median(x,na.rm=T)} else {max(z$x,na.rm=T)}))
      minx<-min(map_dbl(densities, function(z) if(!is.list(z)) {median(x,na.rm=T)} else {min(z$x,na.rm=T)}))
      #Note that we are setting the kernal density estimate to zero if bkde failed.
      maxy<-max(map_dbl(densities, function(z) if(!is.list(z)){0}else{if(is.na(max(z$y)) | is.infinite(max(z$y))){0}else{max(z$y)}}))
      if(add[1]==FALSE) {
        #RStudio seems not to update the graphics devices properly
        if(Sys.getenv("RSTUDIO") == "1" & is.null(moreOptions[["RSOveride"]])) {graphics.off()}

        if(is.null(xLim)){xLim<-c(minx,maxx)}
        if(is.null(yLim)){yLim<-c(0,maxy)}
        if(sidePlot[1]==TRUE) {
          temp<-xLim
          xLim<-yLim
          yLim<-temp
          temp<-xlab
          xlab<-ylab
          ylab<-temp
        }
        test<-prepNiceWindow(data.frame(x=x,y=x), by, minorTick=minorTick, guides=guides, yLim=yLim, xLim=xLim, rotateLabels=rotateLabels, theme=theme, plotColors=plotColors, logScaleX=logScaleX, logScaleY=FALSE, axisText=axisText, minorGuides=minorGuides, extendTicks=extendTicks, expLabels=expLabels, legend=legend, logAdjustment=logAdjustment)
        title(main=main,sub=sub,ylab=ylab,xlab=xlab, col.main=plotColors$title,col.sub=plotColors$subtext,col.lab=plotColors$axisLabels)
      }
      #plot(-1,-1,type="n",xlim=c(minx,maxx),ylim=c(0,maxy),main=main,sub=sub,ylab=ylab)
      densities2<-densities
      if(sidePlot[1]==TRUE)
        densities2<-map(densities, function(d) data.frame(x=d$y, y=d$x))
      if(!is.na(plotColors$fill[1]) & !is.null(plotColors$fill[1])){
        walk2(.x=densities2,.y=plotColors$fill[1:n_groups],~polygon(.x,col=.y,border=0))
      }
      if(length(plotColors$lines)<n_groups){
        plotColors$lines<-rep(plotColors$lines,trunc(n_groups/length(plotColors$lines))+1)
      }
      walk2(.x=densities2,.y=plotColors$lines[1:n_groups],~lines(.x,col=.y,lwd=lWidth))
      if(drawRug[1]==TRUE){
        walk(1:n_groups,~rug(x[as.character(by)==levels(by)[.x]], col=plotColors$fill[.x], side=(1+sidePlot[1]),ticksize=0.05*pointSize[1],lwd=lWidth*.7))
      }
    } else {
      if(is.null(bandwidth)) {
        densities<-bkde(x,gridsize=curvePoints)
      } else if (is.na(bandwidth[1]) | !is.numeric(bandwidth[1])) {
        densities<-bkde(x,gridsize=curvePoints)
      } else {
        densities<-bkde(x,gridsize=curvePoints, bandwidth=bandwidth[1])
      }
      densities2<-densities
      if(sidePlot==TRUE) {
        densities2<-data.frame(x=densities$y,y=densities$x)
      }
      maxx<-max(densities$x)
      minx<-min(densities$x)
      maxy<-max(densities$y)

      finalSummary<-data.frame(Min=c(min(x),0),Max=c(max(x),max(densities$y)),row.names=c("x","fhat"))
      finalStats<-data.frame(x=densities$x,fhat=densities$y)

      if(add[1]==FALSE) {
        #RStudio seems not to update the graphics devices properly
        if(Sys.getenv("RSTUDIO") == "1" & is.null(moreOptions[["RSOveride"]])) {graphics.off()}

        if(is.null(xLim)){xLim<-c(minx,maxx)}
        if(is.null(yLim)){yLim<-c(0,maxy)}
        if(sidePlot[1]==TRUE) {
          temp<-xLim
          xLim<-yLim
          yLim<-temp
          temp<-xlab
          xlab<-ylab
          ylab<-temp
        }
        prepData<-prepNiceWindow(data.frame(x=x,y=x), by, minorTick=minorTick, guides=guides, yLim=yLim, xLim=xLim, rotateLabels=rotateLabels, theme=theme, plotColors=plotColors, logScaleX=logScaleX, logScaleY=logScaleY, axisText=axisText, minorGuides=minorGuides, extendTicks=extendTicks, expLabels=expLabels, legend=legend, logAdjustment=logAdjustment)
        title(main=main,sub=sub,ylab=ylab,xlab=xlab, col.main=plotColors$title,col.sub=plotColors$subtext,col.lab=plotColors$axisLabels)
      }
      #plot(-1,-1,type="n",xlim=c(minx,maxx),ylim=c(0,maxy),main=main,sub=sub,ylab=ylab)
      polygon(densities2,col=theme$plotColors$fill[1],border=0)
      lines(densities2,col=theme$plotColors$lines[1], lwd=lWidth)
      if(drawRug[1]==TRUE){
        rug(x,col=theme$plotColors$lines[1], side=1+sidePlot[1],ticksize=0.05*pointSize[1],lwd=lWidth*.7)
      }
    }
  }
  #Draw legend and set associated options if indicated
  if(length(legendColors)<length(legendLabels) & legend!=FALSE){
    legend<-FALSE
    warning("Not enough point colors to uniquely color subgroups levels\nPlease update plotColors point options to use legend options with this subgroup.", call.=FALSE)
  }

  if(legend!=FALSE) {
    makeNiceLegend(labels=legendLabels, title=legendTitle, fontCol=plotColors$labels, border=theme$legendBorder, lineCol=plotColors$legendLineCol, bg=plotColors$legendBG, col=legendColors, shape="rect",size=theme$legendSize,spacing=theme$legendSpacing)
  }
  par(col.sub=oSubCol, col.lab=oLabCol,col.main=oMainCol,family=oFont,cex.main=oCexMain,cex.lab=oCexlab,cex.sub=oCexSub)

  #formating the output list and setting class int npData
  dataOut<-list(summary=finalSummary,stats=finalStats,plotType="density",options=ActiveOptions)
  class(dataOut)<-c("npData","list")

  invisible(dataOut)
}

#' @export
niceDensity.npData <- function(x, ...) {
  clOptions<-list(...)
  for(opt in names(clOptions)) {
    if(is.null(x$options[opt])){
      append(x$options,list(opt=clOptions[[opt]]))
    }else{
      x$options[[opt]]<-clOptions[[opt]]
    }
  }
  dataOut<-do.call("niceDensity",x$options)
  invisible(dataOut)
}
