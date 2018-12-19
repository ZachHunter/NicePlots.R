#' @include np_utility.R
#' @title draw custom error bars
#' @description
#' Draws error bars with an optional cap at one end
#'
#' @details
#' The input data frame \code{x} should have columns labels 'at','start',and 'stop' with at determining the x-axis location and start and stop indicating the position of the segment on the y-axis. If \code{side=TRUE} then the x and y axises are swapped to support horizontal plotting. Each row of the data frame will produce one bar and an optional cap can be drawn at the 'stop' location.
#'
#' @param x named list or data frame; \code{x$start}, \code{x$stop} and \code{x$at} must all be defined as numeric vectors in a named list or data.frame object. In the case of a data frame, each row returns a single error bar.
#' @param capType character; can be set to 'none', 'bar', 'ball'. If set to 'bar' or ball, a round point or a line segment will be used to cap the end of the error bar.
#' @param capSize numeric; \code{capSize} is the distance that the cap extends away from the error bar. Set to \code{\link{NULL}} to suppress the cap regardless of the \code{capType} setting.
#' @param side logical; if set to true, the error bars will be drawn horizontally.
#' @param col color; a vector of line colors.
#' @param lType positive integer; corresponds to lty line type in base R.
#' @param width positive numeric; corresponds to lwd line width setting in base R.\#'
#' @examples
#' data(iris)
#' iData<-iris %>% group_by(Species) %>%
#'    summarize(Average=mean(Sepal.Length),SD=sd(Sepal.Length))
#' barplot(iData$Average,ylim=c(0,10),names=levels(iris$Species),ylab="sepal length")
#' loc<-c(.7,1.9,3.1)
#' top<-iData$SD*2+iData$Average
#' bottom<-iData$SD*-2+iData$Average
#' errorBars(data.frame(at=loc,start=iData$Average,stop=top),capType="ball",capSize=2)
#' errorBars(data.frame(at=loc,start=iData$Average,stop=bottom),capType="ball",capSize=2)
#' @export
errorBars<-function(x,capType=c("none","bar","ball"),capSize=NULL,side=FALSE,col="black",lType=1,width=1) {
  if(side) {
    segments(x$start,x$at,x$stop,x$at,col=col,lty=lType,lwd=width)
    if(capType[1]=="bar" & !is.null(capSize)) {
      segments(x$stop,x$at-capSize,x$stop,x$at+capSize,col=col,lwd=width)
    } else if (capType[1]=="ball" & !is.null(capSize)) {
      points(y=c(x$at,x$at),x=c(x$stop,x$stop),col=col,pch=19,cex=capSize)
    }
  } else {
    segments(x$at,x$start,x$at,x$stop,col=col,lty=lType,lwd=width)
    if(capType[1]=="bar" & !is.null(capSize)) {
      segments(x$at-capSize,x$stop,x$at+capSize,x$stop,col=col,lwd=width)
    } else if (capType[1]=="ball" & !is.null(capSize)) {
      points(x=c(x$at,x$at),y=c(x$stop,x$stop),col=col,pch=19,cex=capSize)
    }
  }
}


#' @title draw a custom box and whisker plot
#' @description
#' takes a date frame with columns labeled 'at', 'q1', 'q3', 'min', 'max', 'median' and 'width' to draw a series of boxplots.
#'
#' @details
#' The input data frame \code{x} should include columns labels named 'at','q1',and 'q3', 'median', 'min', 'max' and 'width' in any order.
#' Each row will draw a box and whisker plot. The columns 'q1' and 'q3' refer to the 25\% and 75\% cumulative distribution values that bound the interquartile range.
#' If \code{side=TRUE} then the x and y axises are swapped to support horizontal plotting. The box and whiskers can be suppressed leaving only the median line and the optional center marker if so desired.
#'
#' @param x named list or data frame; \code{x$at}, \code{x$q2}, \code{x$q4}, \code{x$median}, \code{x$min}, \code{x$max} and \code{x$width} must all be defined as numeric vectors in a named list or data.frame object.
#' @param col character; color vector that controls the line color.
#' @param fill character; color vector that determines the interior color of the box.
#' @param drawBox logical; draws the box and whiskers if set to \code{\link{TRUE}}. The median line will be drawn regardless.
#' @param drawDot logical; draws a circle at the center of the median bar if set to \code{\link{TRUE}}.
#' @param whiskerLty positive integer; sets the line type or \code{lty} option for plotting the wiskers.
#' @param side logical; if set to \code{\link{TRUE}}, the box plots will be drawn horizontally.
#' @param lWidth positive integer; corresponds to lwd line width setting in base R.
#' @param capWidth numeric; size of the error bar cap relative to the box width.
#'
#' @examples
#' data(iris)
#' iData<-iris %>% group_by(Species) %>%
#'    summarize(median=median(Sepal.Length),min=min(Sepal.Length),max=max(Sepal.Length),
#'    q1=quantile(Sepal.Length)[2],q3=quantile(Sepal.Length)[4]) %>%
#'    bind_cols(at=c(1:3),width=c(.2,.3,.4))
#' plot(1,1,type="n",xlim=c(0,4),ylim=c(0,9))
#' \donttest{drawBoxPlot(iData)}
#' @importFrom graphics segments rect points
#' @seealso \code{\link[graphics]{boxplot}}, \code{\link{niceBox}}
drawBoxPlot<-function(x,col="black",fill=NULL,drawBox=T,drawDot=F, whiskerLty =2,side=FALSE,lWidth=1,capWidth=.25){
  if(side) {
    if(drawBox){
      rect(x$q1,x$at-x$width,x$q3,x$at+x$width,col=fill,lwd=lWidth,border=col)
      errorBars(bind_cols(at=x$at,start=x$q1,stop=x$min),capType="bar",capSize=capWidth*x$width,col=col,lType= whiskerLty,width=lWidth,side=side)
      errorBars(bind_cols(at=x$at,start=x$q3,stop=x$max),capType="bar",capSize=capWidth*x$width,col=col,lType= whiskerLty,width=lWidth,side=side)
    }
    segments(x$median,x$at-x$width,x$median,x$at+x$width,col=col,lwd=lWidth*2)
    if(drawDot){
      points(y=x$at,x=x$median,pch="O",lwd=lWidth*2,col=col)
    }
  } else {
    if(drawBox){
      rect(x$at-x$width,x$q1,x$at+x$width,x$q3,col=fill,lwd=lWidth,border=col)
      errorBars(bind_cols(at=x$at,start=x$q1,stop=x$min),capType="bar",capSize=capWidth*x$width,col=col,lType= whiskerLty,width=lWidth)
      errorBars(bind_cols(at=x$at,start=x$q3,stop=x$max),capType="bar",capSize=capWidth*x$width,col=col,lType= whiskerLty,width=lWidth)
    }
    segments(x$at-x$width,x$median,x$at+x$width,x$median,col=col,lwd=lWidth*2)
    if(drawDot){
      points(x=x$at,y=x$median,pch="O",lwd=lWidth*2,col=col)
    }
  }
}


#' @title draw dots for a dot plot
#' @description
#' takes a data frame of locations, values and an optional subgrouping factor and adds the data points to the active plot
#'
#' @details
#' This function adds data points to a chart. These can be organized exactly as specified (linear), as a jitter cloud (jitter), as a waterfall plot (distribution) or as a swarm (beeswarm).
#' A factor labeled pfact can be included in \code{x} and used to highlight individual data points by setting \code{subGroup=\link{TRUE}}. All graphic customization options can given as vectors and will be iterated over during plotting.
#' Note that the size/cex option can not be used to highlight pfact levels in a beeswarm plot and only the first element of the vector will be used.
#'
#' @param x named list or data frame; \code{x$at}, \code{x$data} and \code{x$pfact} (optional) should all be defined. These vectors are used to place the the point on the chart and determine the point level grouping (highlighting)
#' @param type character; determines how the points are arranged. Options are 'jitter', 'linear', 'beeswarm' and 'distribution'.
#' @param col character; vector of color names for plotting points. If length is greater than one it will be used for subgroups or will iterate over the groups.
#' @param size numeric; vector of cex values for point size. If length is greater than one it will be used for subgroups or will iterate over the groups.
#' @param shape numeric; vector determining point shapes (pch). If length is greater than one it will be used for subgroups or will iterate over the groups.
#' @param highlight logical; Should the point highlighting option be turned on (assumes that pfact is defined).
#' @param width numeric; determines how far points can deviate from the center category label for \code{type} options other than 'linear'.
#' @param sidePlot logical; plots dots for a horizontal rather than vertical axis.
#' @param swarmOverflow character; How to handle beeswarms that would normally overflow the \code{width} argument. Valid options are "random", "gutter", "wrap", "omit", and "none".
#' @import dplyr
#' @import beeswarm
#' @importFrom graphics points stripchart
#'
#' @examples
#' data(iris)
#' boxplot(iris$Sepal.Length~iris$Species,ylab="Sepal Length")
#' iData<-data.frame(at=as.numeric(iris$Species),data=iris$Sepal.Length)
#' drawPoints(iData,type="jitter",col=c("red","blue","purple"))
#' @export
#' @seealso \code{\link[graphics]{points}}, \code{\link[graphics]{stripchart}}, \code{\link[beeswarm]{beeswarm}}
drawPoints<-function(x, type="jitter",col="black",size=1,shape=1,highlight=FALSE,width=.2, sidePlot=FALSE,swarmOverflow="random") {
  #Process color options
  gfact<-NULL
  if(any(names(x)=="subGroup")) {
    gfact<-factor(x$subGroup)
  } else {
    gfact<-factor(x$fact)
  }
  myLevels<-levels(gfact)
  if(length(col)>1){
    if(highlight) {
      myLevels<-levels(factor(x$pfact))
      newCol<-rep(col[1],length(x$data))
      if(length(myLevels)>1) {
        for (i in 2:length(myLevels)){
          #Unlike grouping colors, mistakes in the highlighting would be hard to catch
          #Better to throw and error if the color vector does not meet expectations in this case
          newCol[which(as.character(x$pfact)==myLevels[i])]<-col[i]
        }
      }
      col<-newCol
    } else {
      newCol<-rep(col[1],length(x$data))
      if(length(myLevels)>1) {
        for (i in 2:length(myLevels)){
          newCol[which(as.character(gfact)==myLevels[i])]<-col[(i-1) %% length(col)+1]
        }
      }
      col<-newCol
    }
  }

  #Process pch shape options
  if(length(shape)>1){
    if(highlight) {
      myLevels<-levels(factor(x$pfact))
      newShape<-rep(shape[1],length(x$data))
      if(length(myLevels)>1) {
        for (i in 2:length(myLevels)){
          newShape[which(as.character(x$pfact)==myLevels[i])]<-shape[i]
        }
      }
      shape<-newShape
    } else {
      newShape<-rep(shape[1],length(x$data))
      if(length(myLevels)>1) {
        for (i in 2:length(myLevels)){
          newShape[which(as.character(gfact)==myLevels[i])]<-shape[(i-1) %% length(shape)+1]
        }
      }
      shape<-newShape
    }
  }

  #Process size/cex options
  if(length(size)>1){
    if(highlight) {
      myLevels<-levels(factor(x$pfact))
      newSize<-rep(size[1],length(x$data))
      if(length(myLevels)>1) {
        for (i in 2:length(myLevels)){
          newSize[which(as.character(x$pfact)==myLevels[i])]<-size[i]
        }
      }
      size<-newSize
    } else {
      newSize<-rep(size[1],length(x$data))
      if(length(myLevels)>1) {
        for (i in 2:length(myLevels)){
          newSize[which(as.character(gfact)==myLevels[i])]<-size[(i-1) %% length(size)+1]
        }
      }
      size<-newSize
    }
  }

  #Jitter ploting
  if(type=="jitter" | type=="Jitter") {
    if(sidePlot) {
      points(y=jitter(x$at,amount=width),x=x$data,pch=shape,col=col,cex=size)
    } else {
      points(x=jitter(x$at,amount=width),y=x$data,pch=shape,col=col,cex=size)
    }
    #Linear plotting
  } else if (type=="linear" | type=="Linear") {
    if(sidePlot) {
      points(y=x$at,x=x$data,pch=shape,col=col,cex=size)
    } else {
      points(x=x$at,y=x$data,pch=shape,col=col,cex=size)
    }
    #Beeswarm plotting
  } else if (type=="beeswarm" | type=="Beeswarm" |  type=="BeeSwarm") {
    if(length(col)==1){col<-rep(col[1],length(x$data))}
    if(length(shape)==1){shape<-rep(shape[1],length(x$data))}
    if(length(size)==1){size<-rep(size[1],length(x$data))}
    filter<-list()
    #I had a hard time getting the beeswarm point wise coloring and grouping working properly
    #While there is surely a better way, the swarms are calculated seperately for each group with only the new x coordinate saved for plotting later.
    for(i in 1:length(levels(factor(x$fact)))) {
      if(any(names(x)=="subGroup")){
        for(n in 1:length(levels(factor(x$subGroup)))){
          cFilter<-(x$fact==levels(factor(x$fact))[i] & x$subGroup==levels(factor(x$subGroup))[n])
          #this is here to avoid running subset calculations for subgroups with no samples
          if(any(cFilter)){
            filter[[length(filter)+1]]<-data.frame(x=beeswarm(x$data[cFilter],pch=shape[cFilter][1],at=x[cFilter,"at"][1],do.plot=F,corralWidth=width*2,corral=swarmOverflow,cex=size[cFilter][1])$x,y=x$data[cFilter],color=col[cFilter],size=size[cFilter],shape=shape[cFilter])
            #print(filter[[length(filter)]]$color)
          }
        }
      } else {
        cFilter<-x$fact==levels(factor(x$fact))[i]
        if(any(cFilter)){
          filter[[i]]<-data.frame(x=beeswarm(x$data[cFilter],pch=shape[cFilter][1],do.plot=F,at=x[cFilter,"at"][1],corralWidth=width*2,corral=swarmOverflow,cex=size[cFilter][1])$x,y=x$data[cFilter],color=col[cFilter],size=size[cFilter],shape=shape[cFilter])
        }
      }
    }
    #print(str(filter))
    for(i in 1:length(filter)){
      if(!is.null(filter[[i]])) {
        #Note that the col option has been factorized and needs an as.character wrapper to function propperly
        if(sidePlot) {
          points(y=filter[[i]]$x,x=filter[[i]]$y,pch=filter[[i]]$shape,col=as.character(filter[[i]]$color),cex=filter[[i]]$size)
        } else {
          points(x=filter[[i]]$x,y=filter[[i]]$y,pch=filter[[i]]$shape,col=as.character(filter[[i]]$color),cex=filter[[i]]$size)
        }
      }
    }

    #Distribution plotting
  } else if (type=="distribution" | type=="Distribution") {
    distData<-x %>% mutate(groupAt=paste0("level_",.data$at),rowNum=seq(1,length(data))) %>%
      group_by(.data$groupAt) %>%
      arrange(-data,.by_group=TRUE) %>%
      mutate(spread=.data$at-width+row_number()*width*2/length(data)) %>%
      bind_rows()
    colSelector<-1
    shapeSelector<-1
    sizeSelector<-1
    if(length(col)>1){colSelector<-distData$rowNum}
    if(length(shape)>1){shapeSelector<-distData$rowNum}
    if(length(size)>1){sizeSelector<-distData$rowNum}
    if(sidePlot) {
      points(y=distData$spread,x=distData$data,pch=shape[shapeSelector],col=col[colSelector],cex=size[sizeSelector])
    } else {
      points(x=distData$spread,y=distData$data,pch=shape[shapeSelector],col=col[colSelector],cex=size[sizeSelector])
    }
  } else {
    warning(paste0("drawPoints argument type=",type," is not a recognized option.\nPlease set to either 'jitter', 'linear', 'beeswarm', or 'distribution'"))
  }
}

#' @title draw a violin plot
#' @description Produce a violin plot with optional box plot and strip plot overlays
#'
#' @details
#' This uses  \code{\link[KernSmooth]{bkde}} from the \code{KernSmooth} package to calculated kernal desnisty estimates and estimate the
#' optimal bandwidth \code{h} setting. This data is then used to draw a violin plot with an optional boxplot drawn as an overlay
#' to better charactarize the quartile distributon. Likewise, a strip chart of individual data points
#' can be added on top of these two plots to full charactarize the data distribution. This uses \code{\link{niceBox}} to handle
#' the box plot and strip chart overlays.
#'
#' @param x numeric; A vector of numeric values that will be subset and formated by the factor(s) in \code{by}.
#' @param groups factor; A factor used to subset \code{x} to draw the violins.
#' @param at numeric; a numeric vector of where each factor level should be plotted
#' @param h numeric; Bandwidth for the kernal desnisty estimates. Will cycle over values if multiple bandwidths are given
#' @param plotColors list; a named list of vectors of colors that set the color options for all NicePlot functions.
#' @param sidePlot logical; If \code{\link{TRUE}}, the x and y ploting axis are swapped
#' @param borderCol R color string; Color of the border of the violins
#' @param borderWidth numeric; Thickness of the violin borders (lwd)
#' @param fill R color string; Color of the interior of the violins
#' @param width numeric; Relative width of the violins. A value of 1 will cause the violins to cover thier entire lane and potentially just touch.
#' @param trimViolins logical; Should the violins be truncated at the edges of the data range.
#' @param samplePoints integer; The number of points used to draw each side of the violin. This is generally obtained from \code{theme$curvePoints}.
#'
#' @examples
#' todo<-1
#' @importFrom KernSmooth bkde
#' @importFrom purrr map
#' @importFrom graphics polygon
#' @seealso \code{\link{niceVio}}, \code{\link[KernSmooth]{bkde}}, \code{\link[KernSmooth]{dpik}}
drawViolinPlot <- function(x,groups,at=seq(1,length(levels(groups))),h=NULL, plotColors=basicTheme$plotColors, sidePlot=FALSE,
                           borderCol=plotColors$lines, borderWidth=1, fill=plotColors$fill, width=1, trimViolins=TRUE,samplePoints=NULL) {
  points<-500
  if(!is.null(samplePoints)){
    points<-samplePoints
  }

  myLevels<-levels(groups)

  #Make an list of kernal desinsity estimates. Each list element as x and y components for plotting
  kernals<-NULL
  if(is.null(h)){
    #kernals<-map(myLevels, function(y) bkde(x[groups==y],gridsize=points,bandwidth = dpik(x[groups==y],gridsize = points)))
    if(trimViolins) {
      kernals<-map(myLevels, function(y) bkde(x[groups==y],gridsize = points,range.x = c(min(x[groups==y]),max(x[groups==y]))))
    } else {
      kernals<-map(myLevels, function(y) bkde(x[groups==y],gridsize = points))
    }

  } else {
    for(i in 1:length(myLevels)){
      if(trimViolins) {
        kernals<-bkde(x[groups==myLevels[i]],gridsize=points,bandwidth = h[(i-1) %% length(h) + 1],range.x = c(min(x[groups==myLevels[i]]),max(x[x[groups==myLevels[i]]])))
      } else {
        kernals<-bkde(x[groups==myLevels[i]],gridsize=points,bandwidth = h[(i-1) %% length(h) + 1])
      }
    }
  }

  #Use polygon to plot symetrical kernal densities by category to draw violins
  vioWidth<-map_dbl(kernals, function(z) max(z$y)*2/width)
  for(i in 1:length(myLevels)){
    if(sidePlot){
      polygon(c(kernals[[i]]$x,rev(kernals[[i]]$x)),c(at[i]+kernals[[i]]$y/vioWidth[i],rev(at[i]-kernals[[i]]$y/vioWidth[i])),col=fill[(i-1) %% length(fill) + 1],border=borderCol[(i-1) %% length(borderCol) + 1],lwd=borderWidth)
    } else {
      polygon(c(at[i]+kernals[[i]]$y/vioWidth[i],rev(at[i]-kernals[[i]]$y/vioWidth[i])),c(kernals[[i]]$x,rev(kernals[[i]]$x)),col=fill[(i-1) %% length(fill) + 1],border=borderCol[(i-1) %% length(borderCol) + 1],lwd=borderWidth)
    }
  }
}

#' @title drawBar
#' @description Add a barplot with options error pars to the active ploting enviroment
#'
#' @details
#' This function draws a series of bars based on a dataframe. The expected columns include \code{yt} (locaion top of the bar),
#' \code{yb} or bottom of the bar, \code{at} indicating where the bar should be drawn, \code{Group} which is a unique ID per row, \code{fact} which contains an optional stacking factor
#' \code{UpperError} for the top of the error bar and \code{LowerError} for the location of the bottom of the error bar. The construction of the dataframe is handled automatically from input data
#' by \code{\link{niceBar}}.
#'
#' @param x dataframe; number of interquartile ranges (IQR) past the Q1 (25\%) and Q3 (75\%) cumulative distribution values. Outliers are often defined as \eqn{1.5 \times IQR}{1.5 * IQR} and extreme outliers are more than \eqn{3 \times IQR}{3 * IQR} away from the inner 50\% data range.
#' @param plotColors list; a named list of vectors of colors that set the color options for all NicePlot functions. Names left unspecified will be added and set to default values automatically.
#' @param errorBars Logical; Should error bars be drawn. Defaults to true but is ignored if \code{stack=\link{TRUE}}.
#' @param errorCap character; Determines the style for the ends of the error bars. Valid options are \code{ball}, \code{bar} or \code{none}.
#' @param errorLineType numeric; Sets \code{lty} line type for drawing the error bars.
#' @param width numeric; cex like scaling factor controlling the width of the bars.
#' @param sidePlot logical; Plots bar hight on the x axis if set to \code{\link{TRUE}}.
#' @param stacked logical; draws a stacked barplot if set to \code{\link{TRUE}}.
#' @param capSize numeric; cex like scaling value the controls the size of the caps on the error bars.
#' @param lineWidth numeric; Sets the \code{lwd} options for controling line plotting thickness for the bar plot.
#'
#' @examples
#' data(iris)
#' data<-iris  %>% group_by(Species) %>%
#'     summarize(yt=mean(Sepal.Length),yb=0,UpperError=sd(Sepal.Length),
#'     LowerError=sd(Sepal.Length)) %>%
#'     ungroup() %>% select(yt,yb,UpperError,LowerError,Group=Species) %>%
#'     bind_cols(at=1:3,fact=1:3)
#' plot(type="n",xlim=c(0,4),ylim=c(0,max(iris$Sepal.Length)),-1,xaxt="n")
#' \donttest{drawBar(data,plotColors=list())}
#'
#' @import dplyr
#' @importFrom graphics rect
#' @seealso \code{\link[graphics]{barplot}}, \code{\link{niceBar}}, \code{\link{errorBars}}
drawBar <- function(x, plotColors, errorBars=FALSE, errorCap="ball", errorLineType=1, width=.5, sidePlot=FALSE, stacked=FALSE,capSize=2,lineWidth=1) {
  colorOrder<-plotColors$fill
  #This section builds out the color list to match the number of factors for stacted barplots
  if(stacked){
    if(length(colorOrder)<length(levels(x$fact))) {
      warning("There are fewer colors in plotColors$fill than factor levels for stacking. Stacks will have repeated colors. Use plotColors=list(fill=c(...)) to make a custom color vector.")
      colorOrder<-as.character(rep(colorOrder,1+trunc(length(levels(x$fact))/length(colorOrder)))[1:length(levels(x$fact))])
    } else {
      colorOrder<-as.character(colorOrder[1:length(levels(x$fact))])
    }
    names(colorOrder)<-levels(x$fact)
  }
  if(sidePlot) {
    #IE plot with xy axis fliped
    if(stacked){
      #Build stacks by position using factor levels. Missing factor levels are skipped.
      for (at in unique(x$at)) {
        temp<-x[x$at==at,]
        hAdjust<-0
        for(i in unique(factor(temp$fact))) {
          rect(temp[temp$fact==i,"yb"]+hAdjust,at-width,temp[temp$fact==i,"yt"]+hAdjust,at+width,col=colorOrder[as.character(i)],border=plotColors$lines[1], lwd=lineWidth)
          hAdjust<-temp[temp$fact==i,"yt"]+hAdjust
        }
      }
    } else {
      rect(x$yb,x$at-width,x$yt,x$at+width,col=plotColors$fill,border=plotColors$lines, lwd=lineWidth)
      if(errorBars==TRUE){
        if(errorCap[1]=="bar") {
          x %>% mutate(stop=.data$yt + .data$UpperError,start=.data$yt) %>% errorBars(capType="bar",capSize=width*.25,side=TRUE,col=plotColors$lines,width=lineWidth,lType=errorLineType)
          x %>% mutate(stop=.data$yt - .data$LowerError,start=.data$yt) %>% errorBars(capType="bar",capSize=width*.25,side=TRUE,col=plotColors$lines,width=lineWidth,lType=errorLineType)
        } else if(errorCap[1]=="ball"){
          x %>% mutate(stop=.data$yt + .data$UpperError,start=.data$yt) %>% errorBars(capType="ball",capSize=capSize,side=TRUE,col=plotColors$lines,width=lineWidth,lType=errorLineType)
          x %>% mutate(stop=.data$yt - .data$LowerError,start=.data$yt) %>% errorBars(capType="ball",capSize=capSize,side=TRUE,col=plotColors$lines,width=lineWidth,lType=errorLineType)
        } else {
          x %>% mutate(stop=.data$yt + .data$UpperError,start=.data$yt) %>% errorBars(capType="none",capSize=width*.25,side=TRUE,col=plotColors$lines,width=lineWidth,lType=errorLineType)
          x %>% mutate(stop=.data$yt - .data$LowerError,start=.data$yt) %>% errorBars(capType="none",capSize=width*.25,side=TRUE,col=plotColors$lines,width=lineWidth,lType=errorLineType)
        }
      }
    }
  }else {
    #IE plot xy normally
    if(stacked){
      #Build stacks by position using factor levels. Missing factor levels are skipped.
      for (at in unique(x$at)) {
        temp<-x[x$at==at,]
        hAdjust<-0
        for(i in unique(factor(temp$fact))) {
          rect(at-width, temp[temp$fact==i,"yb"]+hAdjust,at+width,temp[temp$fact==i,"yt"]+hAdjust,col=colorOrder[as.character(i)],border=plotColors$lines[1], lwd=lineWidth)
          hAdjust<-temp[temp$fact==i,"yt"]+hAdjust
        }
      }
    } else {
      rect(x$at-width,x$yb,x$at+width,x$yt,col=plotColors$fill,border=plotColors$lines, lwd=lineWidth)
      if(errorBars==TRUE){
        if(errorCap[1]=="bar") {
          x %>% mutate(stop=.data$yt + .data$UpperError,start=.data$yt) %>%
            errorBars(capType="bar",capSize=width*.25,side=FALSE,col=plotColors$lines,width=lineWidth,lType=errorLineType)
          x %>% mutate(stop=.data$yt - .data$LowerError,start=.data$yt) %>% errorBars(capType="bar",capSize=width*.25,side=FALSE,col=plotColors$lines,width=lineWidth,lType=errorLineType)
        } else if(errorCap[1]=="ball"){
          x %>% mutate(stop=.data$yt + .data$UpperError,start=.data$yt) %>%
            select(.data$at,.data$stop,.data$start) %>%
            errorBars(capType="ball",capSize=capSize,side=FALSE,col=plotColors$lines,width=lineWidth,lType=errorLineType)
          x %>% mutate(stop=.data$yt - .data$LowerError,start=.data$yt) %>%
            select(.data$at,.data$stop,.data$start) %>%
            errorBars(capType="ball",capSize=capSize,side=FALSE,col=plotColors$lines,width=lineWidth,lType=errorLineType)
        } else {
          x %>% mutate(stop=.data$yt + .data$UpperError,start=.data$yt) %>% errorBars(capType="none",capSize=width*.25,side=FALSE,col=plotColors$lines,width=lineWidth,lType=errorLineType)
          x %>% mutate(stop=.data$yt - .data$LowerError,start=.data$yt) %>% errorBars(capType="none",capSize=width*.25,side=FALSE,col=plotColors$lines,width=lineWidth,lType=errorLineType)
        }
      }
    }
  }
}

#' @title Add a datapoint overlay to a box or violin plot
#' @description This function prepares data based on settings from \code{\link{niceBox}}, \code{\link{niceDots}}, or \code{\link{niceVio}}
#' and passes the data on to \code{\link{drawPoints}}.
#' @details This funciton takes in cleaned data from \code{\link{prepCategoryWindow}} and reorganizes to to create a dot plot overlay for a graph.
#' This code is used by both \code{\link{niceBox}} and \code{\link{niceVio}} and has been moved to an independant funciton to make the code more compact and easier to maintain.
#' This code is also used to draw the outlier dots in a boxplot by setting \code{drawPoints = \link{FALSE}}.
#'
#' @examples
#' #Add a beeswarm plot overlay to a boxplot in the iris dataset:
#' data(iris)
#' data<-list(data=iris$Sepal.Length)
#' boxplot(iris$Sepal.Length~iris$Species)
#' addNicePoints(data,by=iris$Species,pointMethod="beeswarm",plotAt=1:3)
#'
#' #Add an outlier point to a boxplot:
#' boxplot(iris$Sepal.Length~iris$Species, outline=FALSE)
#' addNicePoints(data,by=iris$Species,pointMethod="linear",plotAt=1:3,
#'     drawPoints=FALSE,outliers=1.5)
#'
#' @param prepedData list; a list object returned by \code{\link{prepCategoryWindow}}
#' @param by factor or dataframe of factors; One or more factors that control how the data is grouped. The first column is the primary grouping factor and the second and thrid columns are used for sub-grouping and highlighting as needed.
#' @param filter logical vector; Used to further filter the data if necissary.
#' @param sidePlot logical; switches the axis to plot horizontally instead of vertically.
#' @param subGroup logical; Should the data be faceted into subgroups within the primary factor levels. Ignored if \code{by} is a \code{\link[base]{factor}}.
#' @param plotAt numeric; A vector of where to draw each set of points
#' @param pointHighlights logical; will use additional factors in \code{by} to highlight points in the dot plot.
#' @param pointMethod character; method to be used for ploting dots. Can be set to "jitter", "linear", "beeswarm" or "distribution".
#' @param pointShape positive integer; sets pty for plotting data points. Can be a vector to support additional graphical customization.
#' @param pointSize positive integer; sets the cex multiplier for point size.
#' @param width numeric; A multiplier that controls how wide the ploting elements will be. Setting \code{width=1.1} would result in plot elements being 10\% wider.
#' @param pointLaneWidth numeric; This controls how far data point dots can move along the categorical axis when plotting. Used for \code{pointMethod} options 'jitter', 'beeswarm', and 'distribution'.
#' @param plotColors list; a named list of vectors of colors that set the color options for all NicePlot functions. Names left unspecified will be added and set to default values automatically.
#' @param drawPoints logical; draws a dot plot overlay of the data for each box. Setting this to false causes just the outlier points to be ploted. Used in \code{\link{niceBox}}.
#' @param outliers positive numeric; number of interquartile ranges (IQR) past the Q1 (25\%) and Q3 (75\%) cumulative distribution values. Outliers are often defined as \eqn{1.5 \times IQR}{1.5 * IQR} and extreme outliers are more than \eqn{3 \times IQR}{3 * IQR} away from the inner 50\% data range.
#' @param dataCols numeric; A number of representing the number of data columns to be plotted. These is a combination of the dimentions of \code{prepedData} and/or the number of primary and secondary grouping factors. Used to determine the maximum ploting width for the points.
#' @param swarmOverflow character; How to handle beeswarms that would normally overflow the \code{pointLaneWidth} argument. Valid options are "random", "gutter", "wrap", "omit", and "none".
#'
#' @importFrom stats sd start
#' @import dplyr
#' @import tidyr
#'
#' @export
#' @seealso \code{\link{drawPoints}}, \code{\link{niceBox}}, \code{\link{niceVio}}, \code{\link{niceDots}}, \code{\link[beeswarm]{beeswarm}}, \code{\link[base]{jitter}}, \code{\link{drawPoints}}
addNicePoints<-function(prepedData,by,filter=TRUE,sidePlot=F,subGroup=F,plotAt,pointHighlights=F,pointMethod="jitter",pointShape=16,pointSize=1,width=1,pointLaneWidth=.9,plotColors=formatPlotColors(list(1)),drawPoints=T,outliers=F,dataCols=1,swarmOverflow="random") {
  #CASE: by is a factor data is a numeric vector
  facetLoc<-plotAt
  if(is.numeric(prepedData[[1]])){
    if(is.factor(by)) {
      if(outliers==FALSE){
        bind_cols(data=prepedData[[1]],fact=by[filter]) %>%
          mutate(at=facetLoc[.data$fact]) %>%
          drawPoints(highlight=FALSE,sidePlot=sidePlot,type=pointMethod,shape=pointShape,size=pointSize,width=.8*.25*width*pointLaneWidth,col=plotColors$points,swarmOverflow=swarmOverflow)
      } else {
        if(drawPoints){
          bind_cols(data=prepedData[[1]],fact=by[filter]) %>%
            mutate(at=facetLoc[.data$fact]) %>%
            drawPoints(highlight=FALSE,sidePlot=sidePlot,type=pointMethod,shape=pointShape,size=pointSize,width=.8*.25*width*pointLaneWidth,col=plotColors$points,swarmOverflow=swarmOverflow)
        } else {
          #draws outliers if drawPoints is off
          bind_cols(data=prepedData[[1]],fact=by[filter]) %>%
            mutate(at=facetLoc[.data$fact]) %>%
            group_by(.data$fact) %>%
            mutate(tFilter=quantileTrim(data,threshold=outliers,returnFilter = TRUE)[[2]]==FALSE) %>%
            filter(.data$tFilter) %>% bind_rows() %>%
            drawPoints(highlight=FALSE,sidePlot=sidePlot,type=pointMethod,shape=pointShape,size=pointSize,width=.8*.25*width*pointLaneWidth,col=plotColors$points,swarmOverflow=swarmOverflow)
        }
      }
    } else {
      #CASE: by is not a factor data is a numeric vector and subGroup is TRUE
      if(subGroup) {
        if(outliers==FALSE) {
          if(drawPoints) {
            if(pointHighlights) {
              bind_cols(data=prepedData[[1]],fact=by[filter,1],subGroup=by[filter,2],pfact=by[filter,3]) %>%
                mutate(facetLevel=paste0(.data$fact,.data$subGroup,sep="."),at=facetLoc[.data$facetLevel]) %>%
                drawPoints(highlight=TRUE,sidePlot=sidePlot,type=pointMethod,shape=pointShape,size=pointSize,width=.8*.25*width*pointLaneWidth/length(levels(by[,2])),col=plotColors$points,swarmOverflow=swarmOverflow)
            } else {
              bind_cols(data=prepedData[[1]],fact=by[filter,1],subGroup=by[filter,2]) %>%
                mutate(facetLevel=paste0(.data$fact,.data$subGroup,sep="."),at=facetLoc[.data$facetLevel]) %>%
                drawPoints(highlight=FALSE,sidePlot=sidePlot,type=pointMethod,shape=pointShape,size=pointSize,width=.8*.25*width*pointLaneWidth/length(levels(by[,2])),col=plotColors$points,swarmOverflow=swarmOverflow)
            }
          }
        } else {
          if(drawPoints) {
            if(pointHighlights) {
              bind_cols(data=prepedData[[1]],fact=by[filter,1],subGroup=by[filter,2],pfact=by[filter,3]) %>%
                mutate(facetLevel=paste0(.data$fact,.data$subGroup,sep="."),at=facetLoc[.data$facetLevel]) %>%
                drawPoints(highlight=TRUE,sidePlot=sidePlot,type=pointMethod,shape=pointShape,size=pointSize,width=.8*.25*width*pointLaneWidth/length(levels(by[,2])),col=plotColors$points,swarmOverflow=swarmOverflow)
            } else {
              bind_cols(data=prepedData[[1]],fact=by[filter,1],subGroup=by[filter,2]) %>%
                mutate(facetLevel=paste0(.data$fact,.data$subGroup,sep="."),at=facetLoc[.data$facetLevel]) %>%
                drawPoints(highlight=FALSE,sidePlot=sidePlot,type=pointMethod,shape=pointShape,size=pointSize,width=.8*.25*width*pointLaneWidth/length(levels(by[,2])),col=plotColors$points,swarmOverflow=swarmOverflow)
            }
          } else {
            #draw the outlier points if drawPoints == FALSE and outliers != FALSE
            bind_cols(data=prepedData[[1]],fact=by[filter,1],subGroup=by[filter,2]) %>%
              mutate(facetLevel=paste0(.data$fact,.data$subGroup,sep="."),at=facetLoc[.data$facetLevel]) %>%
              group_by(.data$facetLevel) %>%
              mutate(tFilter=quantileTrim(data,threshold=outliers,returnFilter = TRUE)[[2]]==FALSE) %>%
              filter(.data$tFilter) %>% bind_rows() %>%
              drawPoints(highlight=FALSE,sidePlot=sidePlot,type=pointMethod,shape=pointShape,size=pointSize,width=.8*.25*width*pointLaneWidth/length(levels(by[,2])),col=plotColors$points,swarmOverflow=swarmOverflow)
          }
        }
      } else {
        #CASE: by is not a factor, data is a numeric vector and subGroup is FALSE
        if(outliers==FALSE) {
          if(drawPoints) {
            if(pointHighlights) {
              bind_cols(data=prepedData[[1]],fact=by[filter,1],pfact=by[filter,2]) %>%
                mutate(at=facetLoc[.data$fact]) %>%
                drawPoints(highlight=TRUE,sidePlot=sidePlot,type=pointMethod,shape=pointShape,size=pointSize,width=.8*.25*width*pointLaneWidth,col=plotColors$points,swarmOverflow=swarmOverflow)
            } else {
              bind_cols(data=prepedData[[1]],fact=by[filter,1]) %>%
                mutate(at=facetLoc[.data$fact]) %>%
                drawPoints(highlight=FALSE,sidePlot=sidePlot,type=pointMethod,shape=pointShape,size=pointSize,width=.8*.25*width*pointLaneWidth,col=plotColors$points,swarmOverflow=swarmOverflow)
            }
          }
        } else {
          if(drawPoints) {
            if(pointHighlights) {
              bind_cols(data=prepedData[[1]],fact=by[filter,1],pfact=by[filter,2]) %>%
                mutate(at=facetLoc[.data$fact]) %>%
                drawPoints(highlight=TRUE,sidePlot=sidePlot,type=pointMethod,shape=pointShape,size=pointSize,width=.8*.25*width*pointLaneWidth,col=plotColors$points,swarmOverflow=swarmOverflow)
            } else {
              bind_cols(data=prepedData[[1]],fact=by[filter,1]) %>%
                mutate(at=facetLoc[.data$fact]) %>%
                drawPoints(highlight=FALSE,sidePlot=sidePlot,type=pointMethod,shape=pointShape,size=pointSize,width=.8*.25*width*pointLaneWidth,col=plotColors$points,swarmOverflow=swarmOverflow)
            }
          } else {
            #draws outliers if drawPoints is off
            bind_cols(data=prepedData[[1]],fact=by[filter,1]) %>%
              mutate(at=facetLoc[.data$fact]) %>%
              group_by(.data$fact) %>%
              mutate(tFilter=quantileTrim(data,threshold=outliers,returnFilter = TRUE)[[2]]==FALSE) %>%
              filter(.data$tFilter) %>% bind_rows() %>%
              drawPoints(highlight=FALSE,sidePlot=sidePlot,type=pointMethod,shape=pointShape,size=pointSize,width=.8*.25*width*pointLaneWidth,col=plotColors$points,swarmOverflow=swarmOverflow)
          }
        }
      }
    }
  } else {
    #CASE: data is a dataframe, by is a factor, subGroup is ignored
    if(is.factor(by)) {
      if(outliers==FALSE) {
        if(drawPoints){
          bind_cols(prepedData[[1]],fact=by[filter]) %>%
            gather(key=subGroup,value=data,-.data$fact) %>%
            mutate(facetLevel=paste0(.data$fact,.data$subGroup,sep="."),at=facetLoc[.data$facetLevel]) %>%
            drawPoints(highlight=FALSE,sidePlot=sidePlot,type=pointMethod,shape=pointShape,size=pointSize,width=.8*.25*width*pointLaneWidth/dataCols,col=plotColors$points,swarmOverflow=swarmOverflow)
        }
      } else {
        if(drawPoints) {
          bind_cols(prepedData[[1]],fact=by[filter]) %>%
            gather(key=subGroup,value=data,-.data$fact) %>%
            mutate(facetLevel=paste0(.data$fact,.data$subGroup,sep="."),at=facetLoc[.data$facetLevel]) %>%
            drawPoints(highlight=FALSE,sidePlot=sidePlot,type=pointMethod,shape=pointShape,size=pointSize,width=.8*.25*width*pointLaneWidth/dataCols,col=plotColors$points,swarmOverflow=swarmOverflow)
        } else {
          #draws outliers if drawPoints is off
          bind_cols(data=prepedData[[1]],fact=by[filter]) %>%
            gather(key=subGroup,value=data,-.data$fact) %>%
            mutate(facetLevel=paste0(.data$fact,.data$subGroup,sep="."),at=facetLoc[.data$facetLevel]) %>%
            group_by(.data$facetLevel) %>%
            mutate(tFilter=quantileTrim(data,threshold=outliers,returnFilter = TRUE)[[2]]==FALSE) %>%
            filter(.data$tFilter) %>% bind_rows() %>%
            drawPoints(highlight=FALSE,sidePlot=sidePlot,type=pointMethod,shape=pointShape,size=pointSize,width=.8*.25*width*pointLaneWidth/dataCols,col=plotColors$points,swarmOverflow=swarmOverflow)
        }
      }
    } else {
      #CASE: data is a dataframe, by is a dataframe, subGroup is ignored
      if(outliers==FALSE) {
        if(drawPoints){
          if(pointHighlights) {
            bind_cols(prepedData[[1]],fact=by[filter,1],pfact=by[filter,2]) %>%
              gather(key=subGroup,value=data,-.data$fact,-.data$pfact) %>%
              mutate(facetLevel=paste0(.data$fact,.data$subGroup,sep="."),at=facetLoc[.data$facetLevel]) %>%
              drawPoints(highlight=TRUE,sidePlot=sidePlot,type=pointMethod,shape=pointShape,size=pointSize,width=.8*.25*width*pointLaneWidth/dataCols,col=plotColors$points,swarmOverflow=swarmOverflow)
          } else {
            bind_cols(prepedData[[1]],fact=by[filter,1]) %>%
              gather(key=subGroup,value=data,-.data$fact) %>%
              mutate(facetLevel=paste0(.data$fact,.data$subGroup,sep="."),at=facetLoc[.data$facetLevel]) %>%
              drawPoints(highlight=FALSE,sidePlot=sidePlot,type=pointMethod,shape=pointShape,size=pointSize,width=.8*.25*width*pointLaneWidth/dataCols,col=plotColors$points,swarmOverflow=swarmOverflow)
          }
        }
      } else {
        if(drawPoints) {
          if(pointHighlights) {
            bind_cols(prepedData[[1]],fact=by[filter,1],pfact=by[filter,2]) %>%
              gather(key=subGroup,value=data,-.data$fact,-.data$pfact) %>%
              mutate(facetLevel=paste0(.data$fact,.data$subGroup,sep="."),at=facetLoc[.data$facetLevel]) %>%
              drawPoints(highlight=TRUE,sidePlot=sidePlot,type=pointMethod,shape=pointShape,size=pointSize,width=.8*.25*width*pointLaneWidth/dataCols,col=plotColors$points,swarmOverflow=swarmOverflow)
          } else {
            bind_cols(prepedData[[1]],fact=by[filter,1]) %>%
              gather(key=subGroup,value=data,-.data$fact) %>%
              mutate(facetLevel=paste0(.data$fact,.data$subGroup,sep="."),at=facetLoc[.data$facetLevel]) %>%
              drawPoints(highlight=FALSE,sidePlot=sidePlot,type=pointMethod,shape=pointShape,size=pointSize,width=.8*.25*width*pointLaneWidth/dataCols,col=plotColors$points,swarmOverflow=swarmOverflow)
          }
        } else {
          #draws outliers if drawPoints is off
          bind_cols(data=prepedData[[1]],fact=by[filter,1]) %>%
            gather(key=subGroup,value=data,-.data$fact) %>%
            mutate(facetLevel=paste0(.data$fact,.data$subGroup,sep="."),at=facetLoc[.data$facetLevel]) %>%
            group_by(.data$facetLevel) %>%
            mutate(tFilter=quantileTrim(data,threshold=outliers,returnFilter = TRUE)[[2]]==FALSE) %>%
            filter(.data$tFilter) %>% bind_rows() %>%
            drawPoints(highlight=FALSE,sidePlot=sidePlot,type=pointMethod,shape=pointShape,size=pointSize,width=.8*.25*width*pointLaneWidth/dataCols,col=plotColors$points,swarmOverflow=swarmOverflow)
        }
      }
    }
  }
}
