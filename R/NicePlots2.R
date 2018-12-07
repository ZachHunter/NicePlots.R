

#' @title Filter data by interquartile range
#' @description
#' \code{quantileTrim} takes a numeric vector and removes data points that fall more than \code{threshold} * the interquartile range outside of the interquartile range. If \code{returnFilter} is set to TRUE then the function returns a named list with the trimmed data and a logical vector
#'
#'@details
#'The interquartile range (IQR) also known as the H-spread, represents the range encompassing the middle 50% of the data.
#'This is is used to as a measure of dispersion around the median and more frequently to detect outlier data points.
#' Here data points are filtered if \eqn{x <  Q_{1} - threshold\times IQR}{x < Q1 - threshold * IQR} and \eqn{x > Q_{3} + threshold\times IQR}{x > Q3 + threshold * IQR} where \eqn{Q_{1}}{Q1} and \eqn{Q_{3}}{Q3} represent the cumulative 25% and 75% quartiles, respectively. Typical values for the \code{threshold} argument are 1.5 for outliers and 3 for extreme outliers.
#'
#' @param x a numeric vector or a object compatible with the \code{\link[stats]{quantile}} function
#' @param threshold numeric; the number of interquartile ranges out side of the inner 50\% range of the data to use as a cutoff from trimming. Typical values include 1.5 for outliers and 3 for extreme outliers.
#' @param na.rm logical; if true will remove all \code{\link{NA}} values from \code{x} before analyzing the data.
#' @param returnFilter logical; will cause the function to return a list including with both the trimmed data and a logical vector that can be used to filter objects of the same length as \code{x}.
#'
#' @return The trimmed numeric vector or a \code{returnFilter} is \code{\link{TRUE}} then a named list labeled data and filter is returned with the trimmed data and the logical filtering vector, respectively.
#' @examples
#' x<-rnorm(1000)
#' paste0(mean(x)," (",range(x),")")
#' x<-quantileTrim(x,threshold=1.5)
#' paste0(mean(x)," (",range(x),")")
#'
#' #Example using the filter function:
#' myData<-c(NA,rnorm(100),NA,NA,rnorm(100),NA,NA,NA,rnorm(300),NA,10000)
#' myIndex<-1:508
#' newData<-quantileTrim(myData,na.rm=TRUE,returnFilter=TRUE)
#' identical(newData$data,myData[newData$filter])
#' @export
#' @importFrom stats quantile
#' @seealso \code{\link[stats]{quantile}}.
quantileTrim<-function(x,threshold=3,na.rm=FALSE,returnFilter=FALSE){
  naFilter<-1:length(x)
  finalFilter<-rep(FALSE,length(x))
  if(na.rm){
    NAloc<-(is.na(x)==FALSE)
    x<-x[NAloc]
    naFilter<-naFilter[NAloc]
  }
  if(length(x)==0){return(NULL)}
  #Note: if x has less than seven elements outlier detection is probabaly not a good idea
  #May need to be increased even more.
  else if(length(x)<7){
    if(returnFilter) {
      return(list(data=x,filter=rep(1,length(x))))
    } else {
      return(x)
    }
  } else {
    iqr<-quantile(x)[c(2,4)]
    thresholdUpper<-(iqr[2]-iqr[1])*threshold +iqr[2]
    thresholdLower<-iqr[1] - (iqr[2]-iqr[1])*threshold
    filter<-which(x>thresholdLower & x<thresholdUpper)
    #naFilter<-naFilter[filter]
    finalFilter[naFilter[filter]]<-TRUE
    if(returnFilter) {
      return(list(data=x[filter],filter=finalFilter))
    }
    return(x[filter])
  }
}

#' @title Create a matrix of increasingly transparent colors
#' @description
#' \code{makeColorMatrix} is a convenience function for plotting with transparent colors.
#'
#' @details
#' This function take no arguments, but generates rows corresponding to red, blue, green, gray, purple and gold with increasing transparency moving from left to right across the columns.
#'
#' @return A \code{6 x 5} matrix of colors.
#' @examples
#' plot(1,1,col="white",xlim=c(0,10),ylim=c(0,10))
#' for(n in 1:6){rect(0:4,rep(8-n,5),1:5,rep(9-n,5),col=as.matrix(makeColorMatrix())[n,])}
#'
#' #An example how it can be used in practice:
#' myData<-rnorm(600)
#' fact<-factor(c(rep("a",100),rep("b",100),rep("c",100),rep("d",100),rep("e",100),rep("f",100)))
#' plot(myData,col=makeColorMatrix()[fact,3])
#' @export
#' @importFrom grDevices col2rgb rgb rainbow
#' @import RColorBrewer
#' @seealso \code{\link[grDevices]{rainbow}}, \code{\link[grDevices]{col2rgb}}, \code{\link[grDevices]{rgb}}.
makeColorMatrix<-function(){
  myColors<-list(base=c("red","blue","green","gray","purple","gold"))
  for(i in 1:4) {
    r<-col2rgb("red",alpha=(1-.2*i))
    b<-col2rgb("blue",alpha=(1-.2*i))
    g<-col2rgb("green",alpha=(1-.2*i))
    gr<-col2rgb("black",alpha=(1-.2*i))
    p<-col2rgb("purple",alpha=(1-.2*i))
    gl<-col2rgb("gold",alpha=(1-.2*i))
    r<-rgb(r[1]/255,r[2]/255,r[3]/255,alpha=(1-.2*i))
    b<-rgb(b[1]/255,b[2]/255,b[3]/255,alpha=(1-.2*i))
    g<-rgb(g[1]/255,g[2]/255,g[3]/255,alpha=(1-.2*i))
    gr<-rgb(gr[1]/255, gr[2]/255, gr[3]/255,alpha=(1-.2*i))
    p<-rgb(p[1]/255, p[2]/255, p[3]/255,alpha=(1-.2*i))
    gl<-rgb(gl[1]/255, gl[2]/255, gl[3]/255,alpha=(1-.2*i))
    myColors[[i+1]]<-c(r,b,g,gr,p,gl)
  }
  names(myColors)[2:5]<-paste0("alpha",seq(.2,.8,by=.2))
  as.matrix(bind_cols(myColors))
}

#' @title add alpha transparency to a named color
#' @description
#' Takes a named color such as "red" or "darkgreen" and adds a level of transparancy based on the alpha setting.
#'
#' @details
#' \code{setAlpha} is a convenience function that uses the \code{\link[grDevices]{col2rgb}} and \code{\link[grDevices]{rgb}} to add transparancy to named colors.
#'
#' @param x character string; a text string corresponding to an R color
#' @param alpha numeric [0-1]; sets the level of transparency.
#' @return An rbg color with transparancy \code{alpha}.
#' @examples
#' plot(1,1,col="white",xlim=c(0,10),ylim=c(0,10))
#' rect(1,1,7,7,col=setAlpha("darkblue"))
#' rect(3,3,9,9, col=setAlpha("red"))
#'
#' @export
#' @importFrom grDevices col2rgb rgb rainbow
#' @seealso \code{\link{makeColorMatrix}}, \code{\link[grDevices]{rainbow}}, \code{\link[grDevices]{col2rgb}}, \code{\link[grDevices]{rgb}}.
setAlpha<-function(x,alpha=.2){
  myCol<-col2rgb(x,alpha=alpha)
  myCol<-rgb(myCol[1]/255, myCol[2]/255, myCol[3]/255,alpha=alpha)
  myCol
}

#' @title Generate plotting locations for subgrouping data
#' @description
#' \code{facetSpacing} generates a vector for the \code{at=} specification in functions for data sub-grouping
#'
#'@details
#' \code{facetSpacing} takes the number factor levels from the primary and secodary grouping factors to generate a vector of positions for plotting subgrouped data for the nicePlots package.
#'The spacing assumes that each primary factor levels is plot on positive integers 1, 2, 3 etc.
#' For a primary factor at position \code{i} with \code{f} subgroup levels, the subgrouping comes from generating equally spaced intervals starting at \eqn{i-\frac{1}{2}+\frac{1}{f+1}}{i-.5+1/(f+1)} and ending at \eqn{i+\frac{1}{2}-\frac{1}{f+1}}{i+.5-1/(f+1)}. Simply put: \deqn{Spacing = \frac{1}{NSubGroups-1}}
#'
#' @param subGroup positive integer; number of levels in the subgrouping factor
#' @param labels positive integer; number of levels in the primary factor
#'
#' @return a numeric vector of where to plot the subgrouped data. Can be supplied to that \code{at=} option in plotting functions
#' @examples
#' \donttest{boxplot(CNA$BM~ CNA$Status,border="white")}
#' \donttest{stripchart(CNA$BM~factor(paste0(CNA$Status,CNA$Sex)),add=T,at=facetSpacing(2,2))}
#' @seealso \code{\link{prepCategoryWindow}}
facetSpacing<-function(subGroup,labels) {
  subLabLoc<-NULL
  padding<-1/(subGroup+1)
  for (i in 1:labels){
    subLabLoc<-c(subLabLoc,seq(i-0.5+padding,i+0.5-padding,length.out=subGroup))
  }
  subLabLoc
}


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
#' @import tidyverse
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
    distData<-x %>% mutate(groupAt=paste0("level_",at),rowNum=seq(1,length(data))) %>%
      group_by(groupAt) %>%
      arrange(-data,.by_group=TRUE) %>%
      mutate(spread=at-width+row_number()*width*2/length(data)) %>%
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

#' @title format a NicePlots color list
#' @description
#' To simplify code and user options, any color option not set by the user is added to the list and set to the default value.
#'
#' @details
#' The \code{NicePlots} plotColors object is a list of named color values/vectors. The options NicePlots colors include \code{bg} (background color), \code{marginBg} (color of area surrounding the plot), \code{guides} (guide lines for major tick-marks), \code{minorGuides} (guide lines for minor tick-marks)
#' \code{lines} (lines for box/bar plots etc.), \code{points} (plotting data points), \code{fill} (fill for box/bar plots etc.), \code{axis} (axis colors), \code{majorTick} (major tick-mark color),
#' \code{minorTick} (minor tick-mark color), \code{labels} (label colors), \code{subGroupLabels} (subgroup label colors), \code{rectCol} (inner quartile range box used in \code{\link{niceVio}}), and \code{medianMarkerCol} (the median value marker used in \code{\link{niceVio}}).
#' Any option not set be the user will be added to the list and set to the default in order to insure compatibility with downstream NicePlot functions.
#' If a theme is given, any option not set by the user will be set by the theme.
#'
#' @param plotColors list; a named list of vectors of colors that set the color options for all NicePlot functions. Names left unspecified will be added and set to default values automatically.
#' @param theme list; A \code{NicePlots} plotColor list from a theme.
#'
#' @return a formated NicePlots color list.
#' @examples
#' myCols<-list(bg="lightgrey",fill=c("red","green","blue"),lines="darkgrey")
#' \donttest{myCols<-formatPlotColors(myCols)}
#' print(myCols)
formatPlotColors<-function(plotColors, theme=NA){
  moreOptions<-makeColorMatrix()
  if(is.null(plotColors$bg)){
    if(is.na(theme[1])) {plotColors$bg<-"open"}
    else if (is.null(theme$bg)) {plotColors$bg<-"open"}
    else {plotColors$bg<-theme$bg}
  }
  if(is.null(plotColors$marginBg)){
    if(is.na(theme[1])) {plotColors$marginBg<-"transparent"}
    else if (is.null(theme$marginBg)) {plotColors$marginBg<-"transparent"}
    else {plotColors$marginBg<-theme$marginBg}
  }
  if(is.null(plotColors$guides)){
    if(is.na(theme[1])) {plotColors$guides<-"lightgrey"}
    else if (is.null(theme$guides)) {plotColors$guides<-"lightgrey"}
    else {plotColors$guides<-theme$guides}
  }
  if(is.null(plotColors$minorGuides)){
    if(is.na(theme[1])) {plotColors$minorGuides<-"lightgrey"}
    else if (is.null(theme$minorGuides)) {plotColors$minorGuides<-"lightgrey"}
    else {plotColors$minorGuides<-theme$minorGuides}
  }
  if(is.null(plotColors$lines)){
    if(is.na(theme[1])) {plotColors$lines<-"darkred"}
    else if (is.null(theme$lines)) {plotColors$lines<-"darkred"}
    else {plotColors$lines<-theme$lines}
  }
  if(is.null(plotColors$points)){
    if(is.na(theme[1])) {plotColors$points<-moreOptions[,3]}
    else if (is.null(theme$points)) {plotColors$points<-moreOptions[,3]}
    else {plotColors$points<-theme$points}
  }
  if(is.null(plotColors$fill)){
    if(is.na(theme[1])) {plotColors$fill<-moreOptions[4,3]}
    else if (is.null(theme$fill)) {plotColors$fill<-moreOptions[4,3]}
    else {plotColors$fill<-theme$fill}
  }
  if(is.null(plotColors$axis)){
    if(is.na(theme[1])) {plotColors$axis<-"black"}
    else if (is.null(theme$axis)) {plotColors$axis<-"black"}
    else {plotColors$axis<-theme$axis}
  }
  if(is.null(plotColors$majorTick)){
    if(is.na(theme[1])) {plotColors$majorTick<-"black"}
    else if (is.null(theme$majorTick)) {plotColors$majorTick<-"black"}
    else {plotColors$majorTick<-theme$majorTick}
  }
  if(is.null(plotColors$minorTick)){
    if(is.na(theme[1])) {plotColors$minorTick<-"black"}
    else if (is.null(theme$minorTick)) {plotColors$minorTick<-"black"}
    else {plotColors$minorTick<-theme$minorTick}
  }
  if(is.null(plotColors$labels)){
    if(is.na(theme[1])) {plotColors$labels<-"black"}
    else if (is.null(theme$labels)) {plotColors$labels<-"black"}
    else {plotColors$labels<-theme$labels}
  }
  if(is.null(plotColors$subGroupLabels)){
    if(is.na(theme[1])) {plotColors$subGroupLabels<-"black"}
    else if (is.null(theme$subGroupLabels)) {plotColors$subGroupLabels<-"black"}
    else {plotColors$subGroupLabels<-theme$subGroupLabels}
  }
  if(is.null(plotColors$rectCol)){
    if(is.na(theme[1])) {plotColors$rectCol<-setAlpha("black",.8)}
    else if (is.null(theme$rectCol)) {plotColors$rectCol<-setAlpha("black",.8)}
    else {plotColors$rectCol<-theme$rectCol}
  }
  if(is.null(plotColors$medianMarkerCol)){
    if(is.na(theme[1])) {plotColors$medianMarkerCol<-setAlpha("white",.8)}
    else if (is.null(theme$medianMarkerCol)) {plotColors$medianMarkerCol<-setAlpha("white",.8)}
    else {plotColors$medianMarkerCol<-theme$medianMarkerCol}
  }
  plotColors
}

#' @title calculate preliminary statistical significance analysis
#' @description
#' \code{calcStats} takes a numeric vector and a factor and runs a preliminary statistical analysis. Output is printed to the screen and the p-value is returned as a character string.
#'
#' @details
#' This is designed to be used in conjunction with data visualization plots to help with data exploration and should not be used for a robust statistical analysis. Normal distribution, variance and other data characteristics are not evaluated and there is no guarantee that the underling test assumptions are met. For two level factors \code{\link{wilcox.test}} or \code{\link{t.test}} is recommended. If the factor has more than two levels then \code{\link{pairwise.wilcox.test}} and \code{\link{pairwise.t.test}} are automatically selected. In this case \code{\link{anova}} and the optional follow-up \code{\link{TukeyHSD}} can also be used. All output it printed to the console and for the two level tests and \code{\link{anova}} the p-value is returned as a text string.
#'
#' @param x numeric; numeric vector of data points to analyze.
#' @param by factor; factor describing the groups within \code{x} to test.
#' @param type character; determines which statistical test should be used. Accepted values are 'wilcox', 't.test', 'ttest', 'anova' and 'tukey'. Values not matching a valid input will produce a warning.
#' @param verbose logical; will print statistical output to the screen if set \code{\link{TRUE}}. Calculations returned by the function either way.
#'
#' @return a character string describing the test run and the p-value.
#' @importFrom stats t.test wilcox.test anova TukeyHSD pairwise.wilcox.test pairwise.t.test aov median
#' @examples
#' data(iris)
#' \donttest{pv<-calcStats(iris$Petal.Length,by=iris$Species,type="anova")}
#' \donttest{boxplot(iris$Petal.Length~iris$Species,main="Petal Length by Species",sub=pv)}
#'
#' @seealso \code{\link[stats]{wilcox.test}}, \code{\link[stats]{pairwise.wilcox.test}}, \code{\link[stats]{t.test}}, \code{\link[stats]{pairwise.t.test}}, \code{\link[stats]{anova}}, \code{\link[stats]{TukeyHSD}}
calcStats<-function(x,by,type=c("Wilcox","Tukey","T.Test","ANOVA"),verbose=FALSE){
  pvalue<-NULL
  p<-NULL
  if(length(levels(factor(by)))>2){
    if(type[1]=="wilcox"| type[1]=="Wilcox") {
      pairwise<-pairwise.wilcox.test(x,by,p.adjust.method="holm")
      if(verbose){print(pairwise)}
    } else if(type[1]=="t.test"| type[1]=="ttest" | type[1]=="T.test") {
      pairwise<-pairwise.t.test(x,by,p.adjust.method="holm")
      if(verbose){print(pairwise)}
    } else if(type[1]=="ANOVA"| type[1]=="anova") {
      m<-aov(x~by)
      if(verbose){print(anova(m))}
      pvalue <-"ANOVA p-value "
      p<-unlist(anova(m)[5])
    } else if (type[1]=="tukey"| type[1]=="Tukey") {
      m<-aov(x~by)
      if(verbose){print(anova(m))}
      pairwise<-TukeyHSD(m)
      if(verbose){print(pairwise)}
      pvalue <-"ANOVA p-value "
      p<-unlist(anova(m)[5])
    } else {
      warning(paste0("Statistic type ",type," not recognized.\nPlease check spelling and/or documentation for more information."))
      #return("P=NA")
    }
  } else if(length(levels(factor(by)))==2){
    if(type[1]=="ANOVA" | type[1]=="anova") {
      warning("Only two levels detected for analysis of variance (ANOVA).\nReccomend using wilcoxon rank sum instead.")
      m<-aov(x~by)
      if(verbose){print(anova(m))}
      pvalue <-"ANOVA p-value "
      p<-unlist(anova(m)[5])
    } else if (type[1]=="tukey"| type[1]=="Tukey") {
      warning("Only two levels detected for Tukey's honestly significant difference analysis.\nRecommend using wilcoxon rank sum instead.")
      m<-aov(x~by)
      if(verbose){print(anova(m))}
      pairwise<-TukeyHSD(m)
      if(verbose){print(pairwise)}
      pvalue <-"ANOVA p-value "
      p<-unlist(anova(m)[5])
    } else if(type[1]=="wilcox" | type[1]=="Wilcox") {
      p<-wilcox.test(x~by)
      if(verbose){print(p)}
      p<-p$p.value
      pvalue <-"Wilcoxon rank sum p-value "
    } else if(type[1]=="t.test" | type[1]=="ttest" | type[1]=="T.test") {
      p<-t.test(x~by)
      if(verbose){print(p)}
      p<-p$p.value
      pvalue <-"Welch Two Sample t-test p-value "
    } else {
      warning(paste0("Statistic type ",type," not recognized.\nPlease check spelling and/or documentation for more information."))
      #return("P=NA")
    }
  } else {
    warning("Only one level detected in the factor. Statistics can not be calculated.")
  }
  if(!(length(levels(factor(by)))>2 & (type[1]=="wilcox" | type[1]=="Wilcox" | type[1]=="t.test" | type[1]=="ttest" | type[1]=="T.test"))) {
    if(as.numeric(p[1])<0.00001){pvalue<-paste0(pvalue[1],"< 0.00001")}
    else {pvalue<-paste0(pvalue[1],"= ",round(p,5)) }
    return(pvalue)
  }
}


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
#'
#' @return a list with the following elements: major tick marks locations [[1]], major tick labels [[2]], minor tick mark locations [[3]].
#' @examples
#' plot(1:10,log(1:10,2),yaxt="n",ylab="")
#' \donttest{majorTicks<-makeLogTicks(c(0,10),minorCount= 4,logScale=2, axisText=c("","mg"), expLabels=TRUE)}
#' \donttest{axis(side=2,lab=majorTicks[[2]],at=majorTicks[[1]],las=2)}
#' \donttest{axis(side = 2, at = majorTicks[[3]], labels = FALSE, tcl = -0.2)}
#' @importFrom grDevices axisTicks
#' @seealso \code{\link[grDevices]{axisTicks}}, \code{\link[graphics]{axis}}, \code{\link{prepCategoryWindow}}
makeLogTicks<-function(dataRange,minorCount=10,logScale=2,axisText=c(NULL,NULL), expLabels=TRUE) {
  majorLoc<-axisTicks(log(dataRange+1, logScale),log=F,nint=5)
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


#' @title Check data formating for the NicePlots package
#' @description Formats and cleans data prior to setting up the plotting enviroment
#'
#' @details
#' This funciton makes sure the \code{data} input is a numeric vector or a data frame of numeric vectors.
#' It will also check to make sure \code{by} is a factor or a dataframe of factors. If requested, it will also remove missing data and rearrage numeric dataframe inputs.
#'
#' @param data vector or dataframe; data to be plotted
#' @param by factor or dataframe; factors to be used to format data
#' @param na.rm logical; Removes all data and factor rows were \code{NA} is presenst.
#' @param flipFacts logical; If a dataframe is used for plotting data input, this will covert the data to a vector with the dataframe colums trasfered a factor in the second column of the \code{by} input.
#'
#' @return A named list with \code{d=data} and \code{b=by}.
#' @examples
#'	todo<-1
#'
#' @import tidyverse
dataFlightCheck<-function(data,by,flipFacts,na.rm=FALSE) {
  if(is.vector(data)){
    if(is.list(data)){
      warning("List provided as data: unlisting and attempting to proceed but use with caution.\nUnlist data to silence this warning.")
      data<-as.numeric(unlist(data))
    } else {
      data<-as.numeric(data)
    }
  } else if(is.factor(data)){
    data<-as.numeric(as.character(data))
  } else if(is.tibble(data)){
    data<-as.data.frame(data)
    for(i in 1:dim(data)[2]){data[,i]<-as.numeric(as.character(data[,i]))}
  } else if(is.matrix(data)){
    data<-as.data.frame(data)
    #for(i in 1:dim(data)[2]){data[,i]<-as.numeric(as.character(data[,i]))}
  } else if(is.data.frame(data)){
    for(i in 1:dim(data)[2]){data[,i]<-as.numeric(as.character(data[,i]))}
  } else {
    warning(paste0("Data type not recognized.\nClasses observered: ",class(data)))
  }
  if(!is.factor(by)){
    if(is.vector(by)){
      if(is.list(by)) {
        warning("List provided as a factor for by: unlisting and attempting to proceed but use with caution.\nUnlist data to silence this warning.")
        by<-factor(unlist(by))
      } else {
        by<-factor(by)
      }
    } else if(is.data.frame(by)){
      if(dim(by)[2]>1) {
        for(i in 1:dim(by)[2]){by[,i]<-factor(by[,i])}
      } else {
        by<-factor(by[,1])
      }
    } else if(is.tibble(by)) {
      by<-as.data.frame(by)
      if(dim(by)[2]>1) {
        for(i in 1:dim(by)[2]){by[,i]<-factor(by[,i])}
      } else {
        by<-factor(by[,1])
      }
    } else if(is.matrix(by)) {
      by<-as.data.frame(by)
      if(dim(by)[2]>1) {
        for(i in 1:dim(by)[2]){by[,i]<-factor(by[,i])}
      } else {
        by<-factor(by[,1])
      }
    } else {
      warning(paste0("By factor input type not recognized.\nClasses observered: ",class(by)))
    }
  }
  if(na.rm==TRUE){
    naFilter<-NULL
    if(is.data.frame(data)){
      naFilter<-apply(data,1,function(x) anyNA(x))
    } else {
      naFilter<-is.na(data)
    }
    if(is.factor(by)) {
      naFilter<- naFilter | is.na(by)
    } else {
      naFilter<- naFilter | apply(by,1, function(x) anyNA(x))
    }
    if(is.data.frame(data)) {
      data<-data[!naFilter,]
    } else {
      data<-data[!naFilter]
    }
    if(is.factor(by)) {
      by<-factor(by[!naFilter])
    } else {
      by<-by[!naFilter,]
      for(i in 1:dim(by)[2]){by[,i]<-factor(by[,i])}
    }
    warning(paste("NAs detected in data input.",sum(naFilter),"observations removed."))
  }
  #The default handling for data is for the column names of an input dataframe to be used
  #as the default secondary factor. with by taking priority. This changes the order prior to plotting.
  if(!is.vector(data) & !is.data.frame(data)){data<-as.data.frame(data)}
  if(flipFacts & is.data.frame(data) | is.matrix(data)) {
    byDim<-dim(by)
    dataDim<-dim(data)
    if(is.null(byDim)){
      temp<-data %>% bind_cols(by=by) %>% gather(key=newFact,value=Value,-by)
      by<-data.frame(factOne=factor(temp[,2]),factTwo=factor(temp[,1]))
      data<-as.numeric(temp$Value)
    } else {
      temp<-data %>% bind_cols(by=by) %>% gather(key=newFact,value=Value,1:dataDim[2])
      by<-data.frame(factor(temp$newFact),temp[,1:byDim[2]])
      for(i in 1:dim(by)[2]){by[,i]<-factor(by[,i])}
      data<-as.numeric(temp$Value)
    }
  }
  #if(!is.vector(data)){data<-as.data.frame(data)}
  list(d=data,b=by)
}

#' @title prepare a plotting environment for categorical data such as bar plots or box plots
#' @description
#' takes untransformed data and draws the x and y axis with support of subgrouping data within factors, log transformation and outlier trimming.
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
#'
#' @return formats the plotting area and returns a named list with 'data' and 'labels' corresponding to the trimmed and/or transformed data and the labels for the primary factors, respectively.
#' @examples
#'	todo<-1
#'
#' @import graphics
#' @import grDevices
#' @importFrom utils data str
#'
#' @seealso \code{\link[grDevices]{axisTicks}}, \code{\link[graphics]{axis}}, \code{\link{makeLogTicks}}, \code{\link{facetSpacing}}
prepCategoryWindow<-function(x,by=NULL, groupNames=levels(by), minorTick=FALSE, guides=TRUE, yLim=NULL, rotateLabels=FALSE, rotateY=TRUE, theme=NA, plotColors=if(is.na(theme)){list(bg="open",guides="black",lines="gray22",points="darkgrey",fill="white")}else{theme$plotColors}, trim=FALSE, logScale=FALSE, axisText=c(NULL,NULL), minorGuides=FALSE, extendTicks=F,subGroup=FALSE, expLabels=TRUE,sidePlot=FALSE,subGroupLabels=NULL,strictLimits=F, legend=FALSE, pointHighlights=FALSE) {
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
  legendIndex<-NA
  legendTitle<-""
  legendSize<-.66
  legendLevels<-NULL
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
          warn(paste0("Warning: Unable to determine level which factor to use for legend.\nExpected ",legendIndex," columns for by but only found ",dim(by)[2],".\nProceeding  using the 2nd column of by."))
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
    maxLabelW<-map_dbl(legendLevels,strwidth,cex=legendSize,units="in") %>% max()
    titleW<-strwidth(legendTitle,font=2,cex=legendSize,units="in")
    if(titleW>maxLabelW){maxLabelW<-titleW}
    maxLabelH<-map_dbl(legendLevels, strheight,cex=legendSize,units="in") %>% max()
    titleH<-strheight(legendTitle,font=2,cex=legendSize,units="in")
    nMai<-oMai
    nMai[4]<-nMai[4]+maxLabelW
    par(mai=nMai)
  }

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
  if(logScale>0){
    if(dataRange[1]<0){
      stop(paste0("Error: you can not log scale numbers less than or equal to zero\nLowest number detected: ",dataRange[1]))
    }
    majorTicks<-makeLogTicks(dataRange,minorCount= minorTick,logScale=logScale, axisText=axisText, expLabels=expLabels)
    tData <-log(x +1,logScale)
    if(is.null(yLim)==FALSE) {
      dataRange<-log(yLim+1,logScale)
    } else {
      dataRange<-range(tData)
      if(strictLimits){
        dataRange[1]<-0
        dataRange[2]<-dataRange[2]*1.05
      }
    }

    if(trim>0) {
      if(is.numeric(x)){
        tData<-log(quantileTrim(x,threshold=trim,na.rm=T)+1,logScale)
      } else if(is.data.frame(x)){
        tData<-apply(x,2,function(y) {log(quantileTrim(y,threshold=trim,na.rm=T)+1,logScale)})
      } else {
        stop(paste0("Non-numeric input passed to function.\nData structure:\n",str(x)))
      }
      if(is.null(yLim)==FALSE) {dataRange<-log(yLim+1,logScale)}
      else {
        dataRange<-range(tData)
        if(strictLimits){
          dataRange[1]<-0
          dataRange[2]<-dataRange[2]*1.05
        }
        majorTicks<-makeLogTicks(c(logScale^dataRange[1] -1,logScale^dataRange[2] -1),minorCount= minorTick,logScale=logScale, axisText=axisText, expLabels=expLabels)
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
        axis(side=2,at=seq(1:levelCount),labels=groupNames,las=rotateLabels,lwd=0,line=.85,col=plotColors$axis,col.ticks=plotColors$majorTicks,cex.axis=groupCex)
        axis(side=2,at=subLabLoc,labels=rep(subGroupLabels,length(groupNames)),lwd=0,lwd.ticks=1,cex.axis=subGroupCex,col=plotColors$axis,col.ticks=plotColors$minorTicks)
      } else {
        axis(side=2,at=seq(1:levelCount),labels=groupNames,las=rotateLabels,lwd=0,lwd.ticks=1,col=plotColors$axis,col.ticks=plotColors$majorTicks,cex.axis=groupCex)
      }
    } else {
      if(legend==FALSE | pointHighlights==TRUE) {
        axis(side=1,at=seq(1:levelCount),labels=groupNames,las=rotateLabels,lwd=0,line=.85,col=plotColors$axis,col.ticks=plotColors$majorTicks,cex.axis=groupCex)
        axis(side=1,at=subLabLoc,labels=rep(subGroupLabels,length(groupNames)),lwd=0,lwd.ticks=1,cex.axis=0.66,col=plotColors$axis,col.ticks=plotColors$minorTicks,cex.axis=subGroupCex)
      } else {
        axis(side=1,at=seq(1:levelCount),labels=groupNames,las=rotateLabels,lwd=0,lwd.ticks=1,col=plotColors$axis,col.ticks=plotColors$majorTicks,cex.axis=groupCex)
      }
      whichSide<-2
    }
  } else if(subGroup==TRUE & is.data.frame(by)) {
    subLabLoc<-facetSpacing(length(levels(by[,2])),length(groupNames))
    if(is.null(subGroupLabels)){subGroupLabels<-levels(by[,2])}
    if(sidePlot) {
      if(legend==FALSE | (legend!=FALSE & pointHighlights==TRUE)) {
        axis(side=2,at=seq(1:levelCount),labels=groupNames,las=rotateLabels,lwd=0,line=.85,col=plotColors$axis,col.ticks=plotColors$majorTicks,cex.axis=groupCex)
        axis(side=2,at=subLabLoc,labels=rep(subGroupLabels,length(groupNames)),lwd=0,lwd.ticks=1,cex.axis=0.66,col=plotColors$axis,col.ticks=plotColors$minorTicks,cex.axis=subGroupCex)
      } else {
        axis(side=2,at=seq(1:levelCount),labels=groupNames,las=rotateLabels,lwd=0,lwd.ticks=1,col=plotColors$axis,col.ticks=plotColors$majorTicks,cex.axis=groupCex)
      }
    } else {
      if(legend==FALSE | (legend!=FALSE & pointHighlights==TRUE)) {
        axis(side=1,at=seq(1:levelCount),labels=groupNames,las=rotateLabels,lwd=0,line=.85,col=plotColors$axis,col.ticks=plotColors$majorTicks,cex.axis=groupCex)
        axis(side=1,at=subLabLoc,labels=rep(subGroupLabels,length(groupNames)),lwd=0,lwd.ticks=1,cex.axis=0.66,col=plotColors$axis,col.ticks=plotColors$minorTicks,cex.axis=subGroupCex)
      } else {
        axis(side=1,at=seq(1:levelCount),labels=groupNames,las=rotateLabels,lwd=0,lwd.ticks=1,col=plotColors$axis,col.ticks=plotColors$majorTicks,cex.axis=groupCex)
      }
      whichSide<-2
    }
  } else {
    if(sidePlot) {
      axis(side=2,at=seq(1:levelCount),labels=groupNames,las=rotateLabels,lwd=0,lwd.ticks=1,col=plotColors$axis,col.ticks=plotColors$majorTicks,cex.axis=groupCex)
    } else {
      axis(side=1,at=seq(1:levelCount),labels=groupNames,las=rotateLabels,lwd=0,lwd.ticks=1,col=plotColors$axis,col.ticks=plotColors$majorTicks,cex.axis=groupCex)
      whichSide<-2
    }
  }
  #Formating the numeric axis and making sure it fits withing the margins.
  #If it is too long, the axis cex is lowered untill .666 at which point it switches to scientific notation
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
  maxLabelLength<-map_dbl(myLabels,strwidth,units="in",cex=labelCex) %>% max()
  if(par("mai")[whichSide]*.6< maxLabelLength){
    if(par("mai")[whichSide]*.6/maxLabelLength<.666){
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
      axis(side = 1, at = minorLoc, labels = FALSE, tcl = -0.2,col=plotColors$axis,col.ticks=plotColors$minorTicks)
    } else {
      axis(side = 2, at = minorLoc, labels = FALSE, tcl = -0.2,col=plotColors$axis,col.ticks=plotColors$minorTicks)
    }
  }
  clCex<-par()$cex.axis
  par(cex.axis=labelCex)
  if (sidePlot) {
    axis(side=1,labels=myLabels,at=myMajorTicks,las=rotateY,col=plotColors$axis,col.ticks=plotColors$majorTicks)
  } else {
    axis(side=2,labels=myLabels,at= myMajorTicks,las=rotateY,col=plotColors$axis,col.ticks=plotColors$majorTicks)
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
#' @import tidyverse
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
          mutate(at=facetLoc[fact]) %>%
          drawPoints(highlight=FALSE,sidePlot=sidePlot,type=pointMethod,shape=pointShape,size=pointSize,width=.8*.25*width*pointLaneWidth,col=plotColors$points,swarmOverflow=swarmOverflow)
      } else {
        if(drawPoints){
          bind_cols(data=prepedData[[1]],fact=by[filter]) %>%
            mutate(at=facetLoc[fact]) %>%
            drawPoints(highlight=FALSE,sidePlot=sidePlot,type=pointMethod,shape=pointShape,size=pointSize,width=.8*.25*width*pointLaneWidth,col=plotColors$points,swarmOverflow=swarmOverflow)
        } else {
          #draws outliers if drawPoints is off
          bind_cols(data=prepedData[[1]],fact=by[filter]) %>%
            mutate(at=facetLoc[fact]) %>%
            group_by(fact) %>%
            mutate(tFilter=quantileTrim(data,threshold=outliers,returnFilter = TRUE)[[2]]==FALSE) %>%
            filter(tFilter) %>% bind_rows() %>%
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
                mutate(facetLevel=paste0(fact,subGroup,sep="."),at=facetLoc[facetLevel]) %>%
                drawPoints(highlight=TRUE,sidePlot=sidePlot,type=pointMethod,shape=pointShape,size=pointSize,width=.8*.25*width*pointLaneWidth/length(levels(by[,2])),col=plotColors$points,swarmOverflow=swarmOverflow)
            } else {
              bind_cols(data=prepedData[[1]],fact=by[filter,1],subGroup=by[filter,2]) %>%
                mutate(facetLevel=paste0(fact,subGroup,sep="."),at=facetLoc[facetLevel]) %>%
                drawPoints(highlight=FALSE,sidePlot=sidePlot,type=pointMethod,shape=pointShape,size=pointSize,width=.8*.25*width*pointLaneWidth/length(levels(by[,2])),col=plotColors$points,swarmOverflow=swarmOverflow)
            }
          }
        } else {
          if(drawPoints) {
            if(pointHighlights) {
              bind_cols(data=prepedData[[1]],fact=by[filter,1],subGroup=by[filter,2],pfact=by[filter,3]) %>%
                mutate(facetLevel=paste0(fact,subGroup,sep="."),at=facetLoc[facetLevel]) %>%
                drawPoints(highlight=TRUE,sidePlot=sidePlot,type=pointMethod,shape=pointShape,size=pointSize,width=.8*.25*width*pointLaneWidth/length(levels(by[,2])),col=plotColors$points,swarmOverflow=swarmOverflow)
            } else {
              bind_cols(data=prepedData[[1]],fact=by[filter,1],subGroup=by[filter,2]) %>%
                mutate(facetLevel=paste0(fact,subGroup,sep="."),at=facetLoc[facetLevel]) %>%
                drawPoints(highlight=FALSE,sidePlot=sidePlot,type=pointMethod,shape=pointShape,size=pointSize,width=.8*.25*width*pointLaneWidth/length(levels(by[,2])),col=plotColors$points,swarmOverflow=swarmOverflow)
            }
          } else {
            #draw the outlier points if drawPoints == FALSE and outliers != FALSE
            bind_cols(data=prepedData[[1]],fact=by[filter,1],subGroup=by[filter,2]) %>%
              mutate(facetLevel=paste0(fact,subGroup,sep="."),at=facetLoc[facetLevel]) %>%
              group_by(facetLevel) %>%
              mutate(tFilter=quantileTrim(data,threshold=outliers,returnFilter = TRUE)[[2]]==FALSE) %>%
              filter(tFilter) %>% bind_rows() %>%
              drawPoints(highlight=FALSE,sidePlot=sidePlot,type=pointMethod,shape=pointShape,size=pointSize,width=.8*.25*width*pointLaneWidth/length(levels(by[,2])),col=plotColors$points,swarmOverflow=swarmOverflow)
          }
        }
      } else {
        #CASE: by is not a factor, data is a numeric vector and subGroup is FALSE
        if(outliers==FALSE) {
          if(drawPoints) {
            if(pointHighlights) {
              bind_cols(data=prepedData[[1]],fact=by[filter,1],pfact=by[filter,2]) %>%
                mutate(at=facetLoc[fact]) %>%
                drawPoints(highlight=TRUE,sidePlot=sidePlot,type=pointMethod,shape=pointShape,size=pointSize,width=.8*.25*width*pointLaneWidth,col=plotColors$points,swarmOverflow=swarmOverflow)
            } else {
              bind_cols(data=prepedData[[1]],fact=by[filter,1]) %>%
                mutate(at=facetLoc[fact]) %>%
                drawPoints(highlight=FALSE,sidePlot=sidePlot,type=pointMethod,shape=pointShape,size=pointSize,width=.8*.25*width*pointLaneWidth,col=plotColors$points,swarmOverflow=swarmOverflow)
            }
          }
        } else {
          if(drawPoints) {
            if(pointHighlights) {
              bind_cols(data=prepedData[[1]],fact=by[filter,1],pfact=by[filter,2]) %>%
                mutate(at=facetLoc[fact]) %>%
                drawPoints(highlight=TRUE,sidePlot=sidePlot,type=pointMethod,shape=pointShape,size=pointSize,width=.8*.25*width*pointLaneWidth,col=plotColors$points,swarmOverflow=swarmOverflow)
            } else {
              bind_cols(data=prepedData[[1]],fact=by[filter,1]) %>%
                mutate(at=facetLoc[fact]) %>%
                drawPoints(highlight=FALSE,sidePlot=sidePlot,type=pointMethod,shape=pointShape,size=pointSize,width=.8*.25*width*pointLaneWidth,col=plotColors$points,swarmOverflow=swarmOverflow)
            }
          } else {
            #draws outliers if drawPoints is off
            bind_cols(data=prepedData[[1]],fact=by[filter,1]) %>%
              mutate(at=facetLoc[fact]) %>%
              group_by(fact) %>%
              mutate(tFilter=quantileTrim(data,threshold=outliers,returnFilter = TRUE)[[2]]==FALSE) %>%
              filter(tFilter) %>% bind_rows() %>%
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
            gather(key=subGroup,value=data,-fact) %>%
            mutate(facetLevel=paste0(fact,subGroup,sep="."),at=facetLoc[facetLevel]) %>%
            drawPoints(highlight=FALSE,sidePlot=sidePlot,type=pointMethod,shape=pointShape,size=pointSize,width=.8*.25*width*pointLaneWidth/dataCols,col=plotColors$points,swarmOverflow=swarmOverflow)
        }
      } else {
        if(drawPoints) {
          bind_cols(prepedData[[1]],fact=by[filter]) %>%
            gather(key=subGroup,value=data,-fact) %>%
            mutate(facetLevel=paste0(fact,subGroup,sep="."),at=facetLoc[facetLevel]) %>%
            drawPoints(highlight=FALSE,sidePlot=sidePlot,type=pointMethod,shape=pointShape,size=pointSize,width=.8*.25*width*pointLaneWidth/dataCols,col=plotColors$points,swarmOverflow=swarmOverflow)
        } else {
          #draws outliers if drawPoints is off
          bind_cols(data=prepedData[[1]],fact=by[filter]) %>%
            gather(key=subGroup,value=data,-fact) %>%
            mutate(facetLevel=paste0(fact,subGroup,sep="."),at=facetLoc[facetLevel]) %>%
            group_by(facetLevel) %>%
            mutate(tFilter=quantileTrim(data,threshold=outliers,returnFilter = TRUE)[[2]]==FALSE) %>%
            filter(tFilter) %>% bind_rows() %>%
            drawPoints(highlight=FALSE,sidePlot=sidePlot,type=pointMethod,shape=pointShape,size=pointSize,width=.8*.25*width*pointLaneWidth/dataCols,col=plotColors$points,swarmOverflow=swarmOverflow)
        }
      }
    } else {
      #CASE: data is a dataframe, by is a dataframe, subGroup is ignored
      if(outliers==FALSE) {
        if(drawPoints){
          if(pointHighlights) {
            bind_cols(prepedData[[1]],fact=by[filter,1],pfact=by[filter,2]) %>%
              gather(key=subGroup,value=data,-fact,-pfact) %>%
              mutate(facetLevel=paste0(fact,subGroup,sep="."),at=facetLoc[facetLevel]) %>%
              drawPoints(highlight=TRUE,sidePlot=sidePlot,type=pointMethod,shape=pointShape,size=pointSize,width=.8*.25*width*pointLaneWidth/dataCols,col=plotColors$points,swarmOverflow=swarmOverflow)
          } else {
            bind_cols(prepedData[[1]],fact=by[filter,1]) %>%
              gather(key=subGroup,value=data,-fact) %>%
              mutate(facetLevel=paste0(fact,subGroup,sep="."),at=facetLoc[facetLevel]) %>%
              drawPoints(highlight=FALSE,sidePlot=sidePlot,type=pointMethod,shape=pointShape,size=pointSize,width=.8*.25*width*pointLaneWidth/dataCols,col=plotColors$points,swarmOverflow=swarmOverflow)
          }
        }
      } else {
        if(drawPoints) {
          if(pointHighlights) {
            bind_cols(prepedData[[1]],fact=by[filter,1],pfact=by[filter,2]) %>%
              gather(key=subGroup,value=data,-fact,-pfact) %>%
              mutate(facetLevel=paste0(fact,subGroup,sep="."),at=facetLoc[facetLevel]) %>%
              drawPoints(highlight=TRUE,sidePlot=sidePlot,type=pointMethod,shape=pointShape,size=pointSize,width=.8*.25*width*pointLaneWidth/dataCols,col=plotColors$points,swarmOverflow=swarmOverflow)
          } else {
            bind_cols(prepedData[[1]],fact=by[filter,1]) %>%
              gather(key=subGroup,value=data,-fact) %>%
              mutate(facetLevel=paste0(fact,subGroup,sep="."),at=facetLoc[facetLevel]) %>%
              drawPoints(highlight=FALSE,sidePlot=sidePlot,type=pointMethod,shape=pointShape,size=pointSize,width=.8*.25*width*pointLaneWidth/dataCols,col=plotColors$points,swarmOverflow=swarmOverflow)
          }
        } else {
          #draws outliers if drawPoints is off
          bind_cols(data=prepedData[[1]],fact=by[filter,1]) %>%
            gather(key=subGroup,value=data,-fact) %>%
            mutate(facetLevel=paste0(fact,subGroup,sep="."),at=facetLoc[facetLevel]) %>%
            group_by(facetLevel) %>%
            mutate(tFilter=quantileTrim(data,threshold=outliers,returnFilter = TRUE)[[2]]==FALSE) %>%
            filter(tFilter) %>% bind_rows() %>%
            drawPoints(highlight=FALSE,sidePlot=sidePlot,type=pointMethod,shape=pointShape,size=pointSize,width=.8*.25*width*pointLaneWidth/dataCols,col=plotColors$points,swarmOverflow=swarmOverflow)
        }
      }
    }
  }
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
#' @import tidyverse
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
    rect(LegendCo-oMai[4]/9/ConvertW,startH-totalLegendH/ConvertH-oMai[4]/9/ConvertH*2,LegendCo+ maxLabelW/ConvertW+oMai[4]/9/ConvertW, startH+oMai[4]/9/ConvertH*2,col=bg,border=border)
  }
  text(LegendCo, startH, label=title,cex=size, font=2, offset=0, pos=4, col=fontCol)
  for(i in 1:length(labels)){
    cH<-startH-titleH/ConvertH*(1+ spacing/2)-.5*maxLabelH/ConvertH-(i-1)*maxLabelH/ConvertH*(1+ spacing)
    rect(LegendCo, cH-.3*maxLabelH/ConvertH,LegendCo+maxLabelH/ConvertW, cH +.7* maxLabelH/ConvertH, border=lineCol,col=col[i])
    text(LegendCo+maxLabelH/ConvertW, cH,labels=labels[i],cex=size,pos=4,offset=.2,col=fontCol)
  }

  par(xpd=F,mai=oMai)

}

#' @title Prepare and print basic statistics for niceBox and niceVio
#' @description Uses filtred data with subgroup and factor information to calculate quartile data for display and plotting.
#'
#' @details
#' To aid in data interpretation and exploration, quartile distribution statistics are calculated for each group and subgroup
#' if specified. For \code{\link{niceBox}} this data is also used to plot the data. The data is parsed by checking \code{outlier} and \code{subGroup} status
#' as weel as checking if either \code{prepedData} or \code{by} are a \code{\link[base]{data.frame}} or a \code{\link[base]{vector}}.
#'
#' @examples
#' data(iris)
#' filter<-rep(TRUE,length(iris$Species))
#' loc<-seq(1,length(levels(iris$Species)))
#' data<-list(data=iris[,1:4])
#' \donttest{myData<-prepNiceData(data,by=iris$Species,filter=filter,plotLoc=loc,
#'     groupNames=levels(iris$Species),outliers=FALSE)}
#' \donttest{print(myData)}
#'
#' @param prepedData list; a list object returned by \code{\link{prepCategoryWindow}}
#' @param by factor or dataframe of factors; One or more factors that control how the data is grouped. The first column is the primary grouping factor and the second and thrid columns are used for sub-grouping and highlighting as needed.
#' @param subGroup logical; Should the data be faceted into subgroups within the primary factor levels. Ignored if \code{by} is a \code{\link[base]{factor}}.
#' @param outliers positive numeric; number of interquartile ranges (IQR) past the Q1 (25\%) and Q3 (75\%) cumulative distribution values. Outliers are often defined as \eqn{1.5 \times IQR}{1.5 * IQR} and extreme outliers are more than \eqn{3 \times IQR}{3 * IQR} away from the inner 50\% data range.
#' @param filter logical vector; Used to further filter the data if necissary.
#' @param groupNames character; A character vector for the primary group names
#' @param plotLoc numeric vector; A vector indicating where each element should be plotted
#' @param width numeric; A multiplier that controls how wide the ploting elements will be. Setting \code{width=1.1} would result in plot elements being 10\% wider.
#' @param flipFacts logical; When a dataframe of values is given, column names are used as a secondary grouping factor by default. Setting \code{flipFacts=\link{TRUE}} makes the column names the primary factor and \code{by} the secondary factor.
#' @param verbose logical; Will print summary statistics to the screen if set to \code{\link{TRUE}}. The function will return the calculations either way.
#'
#' @import tidyverse
#' @seealso \code{\link{niceBox}}, \code{\link{niceVio}}, \code{\link{niceDots}}
prepNiceData<- function(prepedData,by, subGroup=FALSE,outliers=TRUE,filter,groupNames,plotLoc,width=1,flipFacts=FALSE,verbose=FALSE){
  #CASE: by is a factor data is a numeric vector
  if(is.numeric(prepedData[[1]])){
    if(is.factor(by)) {
      if(outliers==FALSE){
        plotData<-bind_cols(data=prepedData[[1]],fact=by[filter]) %>%
          group_by(fact) %>%
          summarize(median=median(data),min=min(data),max=max(data),n=n(),q1=quantile(data,.25),q3=quantile(data,.75)) %>%
          bind_cols(at=plotLoc,width=rep(.25*width,length(groupNames)))
        if(verbose){print(select(plotData,fact,n,median,q1,q3,min,max))}
        return(plotData)
      } else {
        plotData<-bind_cols(data=prepedData[[1]],fact=by[filter]) %>%
          group_by(fact) %>%
          summarize(median=median(data),tmin=min(data),min=min(quantileTrim(data,threshold=outliers)),tmax=max(data),max=max(quantileTrim(data,threshold=outliers)),n=n(),q1=quantile(data,.25),q3=quantile(data,.75)) %>%
          bind_cols(at=plotLoc,width=rep(.25*width,length(groupNames)))
        if(verbose){print(select(plotData,fact,n,median,q1,q3,tmin,tmax))}
        return(plotData)
      }
    } else {
      #CASE: by is not a factor data is a numeric vector and subGroup is TRUE
      if(subGroup) {
        if(outliers==FALSE) {
          plotData<-bind_cols(data=prepedData[[1]],fact=by[filter,1],subGroup=by[filter,2]) %>%
            group_by(fact,subGroup) %>%
            summarize(median=median(data),min=min(data),max=max(data),n=n(),q1=quantile(data,.25),q3=quantile(data,.75)) %>%
            mutate(facetLevel=paste0(fact,subGroup,sep="."))
          if(verbose){print(select(plotData,fact,subGroup,n,median,q1,q3,min,max))}
          return(plotData)
        } else {
          plotData<-bind_cols(data=prepedData[[1]],fact=by[filter,1],subGroup=by[filter,2]) %>%
            group_by(fact,subGroup) %>%
            summarize(median=median(data),tmin=min(data),min=min(quantileTrim(data,threshold=outliers)),tmax=max(data),max=max(quantileTrim(data,threshold=outliers)),n=n(),q1=quantile(data,.25),q3=quantile(data,.75)) %>%
            mutate(facetLevel=paste0(fact,subGroup,sep="."))
          if(verbose){print(select(plotData,fact,subGroup,n,median,q1,q3,tmin,tmax))}
          return(plotData)
        }
      } else {
        #CASE: by is not a factor, data is a numeric vector and subGroup is FALSE
        if(outliers==FALSE) {
          plotData<-bind_cols(data=prepedData[[1]],fact=by[filter,1]) %>%
            group_by(fact) %>%
            summarize(median=median(data),min=min(data),max=max(data),n=n(),q1=quantile(data,.25),q3=quantile(data,.75)) %>%
            bind_cols(at=plotLoc,width=rep(.25*width,length(groupNames)))
          if(verbose){print(select(plotData,fact,n,median,q1,q3,min,max))}
          return(plotData)
        } else {
          plotData<-bind_cols(data=prepedData[[1]],fact=by[filter,1]) %>%
            group_by(fact) %>%
            summarize(median=median(data),tmin=min(data),min=min(quantileTrim(data,threshold=outliers)),tmax=max(data),max=max(quantileTrim(data,threshold=outliers)),n=n(),q1=quantile(data,.25),q3=quantile(data,.75)) %>%
            bind_cols(at=plotLoc,width=rep(.25*width,length(groupNames)))
          if(verbose){print(select(plotData,fact,n,median,q1,q3,tmin,tmax))}
          return(plotData)
        }
      }
    }
  } else {
    #CASE: data is a dataframe, by is a factor, subGroup is ignored
    if(is.factor(by)) {
      if(outliers==FALSE) {
        plotData<-bind_cols(prepedData[[1]],fact=by[filter]) %>%
          gather(key=subGroup,value=data,-fact) %>%
          group_by(fact,subGroup) %>%
          summarize(median=median(data),min=min(data),max=max(data),n=n(),q1=quantile(data,.25),q3=quantile(data,.75)) %>%
          mutate(facetLevel=paste0(fact,subGroup,sep="."))
        if(verbose){print(select(plotData,fact,subGroup,n,median,q1,q3,min,max))}
        return(plotData)
      } else {
        plotData<-bind_cols(prepedData[[1]],fact=by[filter]) %>%
          gather(key=subGroup,value=data,-fact) %>%
          group_by(fact,subGroup) %>%
          summarize(median=median(data),tmin=min(data),min=min(quantileTrim(data,threshold=outliers)),tmax=max(data),max=max(quantileTrim(data,threshold=outliers)),n=n(),q1=quantile(data,.25),q3=quantile(data,.75)) %>%
          mutate(facetLevel=paste0(fact,subGroup,sep="."))
        if(verbose){print(select(plotData,fact,subGroup,n,median,q1,q3,tmin,tmax))}
        return(plotData)
      }
    } else {
      #CASE: data is a dataframe, by is a dataframe, subGroup is ignored
      if(outliers==FALSE) {
        plotData<-bind_cols(prepedData[[1]],fact=by[filter,1]) %>%
          gather(key=subGroup,value=data,-fact) %>%
          group_by(fact,subGroup) %>%
          summarize(median=median(data),min=min(data),max=max(data),n=n(),q1=quantile(data,.25),q3=quantile(data,.75)) %>%
          mutate(facetLevel=paste0(fact,subGroup,sep="."))
        if(verbose){print(select(plotData,fact,subGroup,n,median,q1,q3,min,max))}
        return(plotData)

      } else {
        plotData<-bind_cols(prepedData[[1]],fact=by[filter,1]) %>%
          gather(key=subGroup,value=data,-fact) %>%
          group_by(fact,subGroup) %>%
          summarize(median=median(data),tmin=min(data),min=min(quantileTrim(data,threshold=outliers)),tmax=max(data),max=max(quantileTrim(data,threshold=outliers)),n=n(),q1=quantile(data,.25),q3=quantile(data,.75)) %>%
          mutate(facetLevel=paste0(fact,subGroup,sep="."))
        if(verbose){print(select(plotData,fact,subGroup,n,median,q1,q3,tmin,tmax))}
        return(plotData)
      }
    }
  }
}


#' @title Process ploting options
#' @description Integrates theme and user arguments to finalize all options prior to plotting
#'
#' @details
#' This is a private utility function used by NicePlots to integrate user options with theme defaults.
#' Anything specified by the user is treated literally with no additional optimization.
#' Colors, point shapes, fills, etc. are optimized to best enhance the relevant factor visualisations selected.
#' The finalized parameters are returned as a named list.
#'
#' @param x Data to be plotted which has been preprocessed by \code{\link{dataFlightCheck}}
#' @param by factor or dataframe of factors; One or more factors that control how the data is grouped. The first column is the primary grouping factor and the second and thrid columns are used for sub-grouping and highlighting as needed.
#' @param minorTick numeric; Number of minor tickmarks to be drawn between the major marks
#' @param pointShape numeric; vector of numbers corresponding to pty options for ploting data overlays.
#' @param wiskerLineType numeric; number corresponding to lty option for drawing the whiskers and error bars for box plots and bar plots, respectively.
#' @param lWidth numeric; number creesponding to the lwd option for ploting lines on the graph
#' @param capWidth numeric; Width of the cap relative to the bar/box width for box plots and bar plots.
#' @param pointLaneWidth numeric; This controls how far data point dots can move along the categorical axis when plotting. Used for \code{pointMethod} options 'jitter', 'beeswarm', and 'distribution'.
#' @param width numeric; A multiplier that controls how wide the ploting elements will be. Setting \code{width=1.1} would result in plot elements being 10\% wider.
#' @param guides logical; Should guidelines be drawn at the major tick marks.
#' @param pointSize numeric; vector of numerics controling the size of points on the data overlay
#' @param subGroup logical; Should the data be faceted into subgroups within the primary factor levels. Ignored if \code{by} is a \code{\link[base]{factor}}.
#' @param stack logical; Triggers stacked bar analysis for bar plots
#' @param pointHighlights logical; will use additional factors in \code{by} to highlight points in the dot plot
#' @param type character; What kind of plot is this for? Case sensitive options are "BP", "DP", "VP", and "Bar" corresponding to box plots, dot plots, violin plots, and bar plots, respectively.
#' @param theme list object; Themes are are an optional way of storing graphical preset options that are compatible with all nicePlot graphing functions.
#' @param plotColors list; a named list of vectors of colors that set the color options for all NicePlot functions. Names left unspecified will be added and set to default values automatically.
#' @param pointMethod character; method to be used for ploting dots. Can be set to "jitter", "linear", "beeswarm" or "distribution".
#' @param logScale numeric; Should a log scale use used (\code{TRUE}/\code{FALSE})? Otherwise indicates the base for the log transformation.
#' @param drawPoints logical; draws a dot plot overlay of the data.
#' @param groupNames character; A character vector for the primary group names
#' @param swarmOverflow character; Valid options are: "none", "wrap", "gutter", "random", and "omit". Controls how to wantly point stacks that would overflow the pointLaneWidth option.
#' @param errorCap character; Determines the style for the ends of the error bars. Valid options are \code{ball}, \code{bar} or \code{none}.
#'
#'
#' @return Named listed of graphical options
#' @import tidyverse
#' @seealso \code{\link{formatPlotColors}}, \code{\link{niceBox}}, \code{\link{niceDots}}, \code{\link{niceVio}}, \code{\link{niceBar}}
procNiceOptions<-function(x,by,minorTick,pointShape,wiskerLineType,lWidth,capWidth,pointLaneWidth,width,guides,pointSize,subGroup=FALSE,stack=F,pointHighlights=F,type=c("BP","VP","DP","Bar"),theme,plotColors,pointMethod,logScale,drawPoints,groupNames,swarmOverflow,errorCap=NULL){
  #Here we check to see if the user specified any options so that they are left unaltered if present
  defaultPoints<-FALSE
  defaultLines<-FALSE
  defaultFill<-FALSE
  defaultShapes<-FALSE
  if(is.list(plotColors)){
    pcNames<-names(plotColors)
    if(!("points" %in% pcNames)){defaultPoints<-TRUE}
    if(!("lines" %in% pcNames)){defaultLines<-TRUE}
    if(!("fill" %in% pcNames)){defaultFill<-TRUE}
  }
  #Formating all options
  if(!is.list(theme)) {
    plotColors<-formatPlotColors(plotColors)
    if(is.null(minorTick)){minorTick<-FALSE}
    if(is.null(guides)){guides<-TRUE}
    if(is.null(pointSize)){pointSize<-1}
    if(is.null(width)){width<-1}
    if(is.null(pointShape)){
      pointShape<-1
      defaultShapes<-TRUE
    }
    if(is.null(pointLaneWidth)){pointLaneWidth<-1}
    if(is.null(lWidth)){lWidth<-1}
    if(is.null(capWidth)){capWidth<-.25}
    if(is.null(errorCap)){errorCap<-"ball"}
    if(is.null(wiskerLineType)){
      if(type=="BP"){
        wiskerLineType<-2
      } else {
        wiskerLineType<-1
      }
    }
    if(is.null(pointMethod)){
      if(drawPoints==FALSE){
        pointMethod<-"linear"
      }else {
        pointMethod<-"jitter"
      }
    }
    if(is.null(swarmOverflow)){
      swarmOverflow<-"random"
    }
  } else {
    if(is.null(plotColors)){plotColors<-theme$plotColors}
    else (plotColors<-formatPlotColors(plotColors,theme$plotColors))
    if(is.null(minorTick)){
      if(logScale==FALSE){
        minorTick<-theme$minorTick
      } else {
        minorTick<-theme$minorTickLS
      }
    }
    if(is.null(swarmOverflow)){swarmOverflow<-theme$swarmOverflow}
    if(is.null(guides)){guides<-theme$guides}
    if(is.null(pointSize)){pointSize<-theme[[paste0("pointSize",type)]]}
    if(is.null(width)){width<-theme[[paste0("width",type)]]}
    if(is.null(pointShape)){
      pointShape<-theme[[paste0("pointShape",type)]]
      defaultShapes<-TRUE
    }
    if(is.null(pointLaneWidth)){pointLaneWidth<-theme[[paste0("pointLaneWidth",type)]]}
    if(is.null(lWidth)){lWidth<-theme[[paste0("lWidth",type)]]}
    if(is.null(capWidth)){capWidth<-theme[[paste0("errorBarCapWidth",type)]]}
    if(is.null(wiskerLineType)){wiskerLineType<-theme[[paste0("errorBarLineType",type)]]}
    if(is.null(errorCap)){errorCap<-theme$errorCapType}
    if(is.null(pointMethod)){
      if(drawPoints==FALSE & type != "DP"){
        pointMethod<-"linear"
      }else {
        pointMethod<-theme[[paste0("pointMethod",type)]]
      }
    }
  }
  myLevels<-1
  #Calcuate the relevant factor levels formating the graph.
  #Note that most of this is for handling empty factor levels
  if(is.data.frame(x)){
    if(pointHighlights==TRUE) {
      if(type=="DP") {
        plotColors$lines<-plotColors$lines[1]
      }
      myLevels<-length(levels(by[,2]))
    } else {
      myLevels<-dim(x)[2]
    }
    cFilter<-NULL
    byLength<-1
    if(is.data.frame(by)){
      byLength<-length(levels(by[,1]))
      cFilter<-map(1:dim(x)[2], function(n) map_lgl(levels(by[,1]), function(y) length(x[by[,1]==y,n])>0)) %>% reduce(c)
    } else {
      byLength<-length(levels(by))
      cFilter<-map(1:dim(x)[2], function(n) map_lgl(levels(by), function(y) length(x[by==y,n])>0)) %>% reduce(c)
    }
    if(length(plotColors$fill)>1 & defaultFill==FALSE){
      if(length(plotColors$fill)<length(seq(1,dim(x)[2]))) {
        warning("Not enough fill colors specified to uniquely cover factor levels!")
        plotColors$fill<-rep(plotColors$fill,length(plotColors$fill) %% dim(x)[2] +1)
      }
      plotColors$fill<-rep(plotColors$fill[1:dim(x)[2]],byLength)[cFilter]
    }
    if(length(plotColors$lines)>1 & defaultLines==FALSE){
      if(length(plotColors$lines)<length(seq(1,dim(x)[2]))) {
        warning("Not enough line colors specified to uniquely cover factor levels!")
        plotColors$lines<-rep(plotColors$lines,length(plotColors$lines) %% dim(x)[2] +1)
      }
      plotColors$lines<-rep(plotColors$lines[1:dim(x)[2]],byLength)[cFilter]
    }
  } else if(subGroup==TRUE) {
    cFilter<-NULL
    byLevel<-1
    byFactor<-1
    if(is.data.frame(by)){
      if(pointHighlights==TRUE) {
        if(type=="DP") {
          plotColors$lines<-plotColors$lines[1]
        }
        myLevels<-length(levels(factor(by[,3])))
      } else {
        myLevels<-length(levels(factor(by[,2])))
      }
      byLevel<-length(levels(by[,2]))
      byFactor<-length(levels(by[,1]))
      cFilter<-map(levels(by[,1]), function(n) map_lgl(levels(by[,2]), function(y) length(x[by[,1]==n & by[,2]==y])>0)) %>% reduce(c)
    } else {
      byLevel<-length(levels(by))
      myLevels<-length(levels(by))
      byFactor<-1
      cFilter<-map_lgl(levels(by), function(y) length(x[by==y])>0)
    }
    if(length(plotColors$fill)>1 & defaultFill==FALSE){
      if(length(plotColors$fill)<byLevel) {
        warning("Not enough fill colors specified to uniquely cover factor levels!")
        plotColors$fill<-rep(plotColors$fill,length(plotColors$fill) %% byLevel +1)
      }
      plotColors$fill<-rep(plotColors$fill[1:byLevel],byFactor)[cFilter]
    }
    if(length(plotColors$lines)>1 & defaultLines==FALSE){
      if(length(plotColors$lines)<byLevel) {
        warning("Not enough line colors specified to uniquely cover factor levels!")
        plotColors$lines<-rep(plotColors$lines,length(plotColors$lines) %% byLevel +1)
      }
      plotColors$lines<-rep(plotColors$lines[1:byLevel],byFactor)[cFilter]
    }
  } else if(is.data.frame(by)){
    if(pointHighlights==TRUE) {
      if(type=="DP") {
        plotColors$lines<-plotColors$lines[1]
      }
      myLevels<-length(levels(by[,2]))
    } else {
      myLevels<-length(levels(by[,1]))
    }
    cFilter<-map_lgl(levels(by[,1]), function(n) length(x[by[,1]==n])>0)
    if(length(plotColors$fill)>1 & defaultFill==FALSE){
      if(length(plotColors$fill)<length(levels(by[,1]))) {
        warning("Not enough fill colors specified to uniquely cover factor levels!")
        plotColors$fill<-rep(plotColors$fill,length(plotColors$fill) %% length(levels(by[,1])) +1)
      }
      plotColors$fill<-plotColors$fill[1:length(levels(by[,1]))][cFilter]
    }
    if(length(plotColors$lines)>1 & defaultLines==FALSE){
      if(length(plotColors$lines)<length(levels(by[,1]))) {
        warning("Not enough line colors specified to uniquely cover factor levels!")
        plotColors$lines<-rep(plotColors$lines,length(plotColors$lines) %% length(levels(by[,1])) +1)
      }
      plotColors$lines<-plotColors$lines[1:length(levels(by[,1]))][cFilter]
    }
  } else {
    myLevels<-length(levels(by))
    cFilter<-map_lgl(levels(by), function(n) length(x[by==n])>0)
    if(length(plotColors$fill)>1 & defaultFill==FALSE){
      if(length(plotColors$fill)<myLevels) {
        warning("Not enough fill colors specified to uniquely cover factor levels!")
        plotColors$fill<-rep(plotColors$fill,length(plotColors$fill) %% myLevels +1)
      }
      plotColors$fill<-plotColors$fill[1:myLevels][cFilter]
    }
    if(length(plotColors$lines)>1 & defaultLines==FALSE){
      if(length(plotColors$lines)<myLevels) {
        warning("Not enough line colors specified to uniquely cover factor levels!")
        plotColors$lines<-rep(plotColors$lines,length(plotColors$lines) %% myLevels +1)
      }
      plotColors$lines<-plotColors$lines[1:myLevels][cFilter]
    }
  }
  #If left blank by the user, colors and shapes are adjust so that the repeat based on factor levels
  if(length(pointShape)>1 & defaultShapes==FALSE){pointShape<-pointShape[1:myLevels]}
  if(length(plotColors$points)>1 & defaultPoints==FALSE){plotColors$points<-plotColors$points[1:myLevels]}

  #Capturing default group names
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
  theme$plotColors<-plotColors
  list(groupNames=groupNames,minorTick=minorTick,pointShape=pointShape,wiskerLineType=wiskerLineType,lWidth=lWidth,capWidth=capWidth,pointLaneWidth=pointLaneWidth,width=width,guides=guides,pointSize=pointSize,subGroup=subGroup,stack=stack,pointHighlights=pointHighlights,theme=theme,plotColors=plotColors,pointMethod=pointMethod,swarmOverflow=swarmOverflow,errorCap=errorCap)
}


#' @title draw a box plot
#' @description draws a box plot with optional scatter plot overlays, subgrouping options and log scale support.
#'
#' @details
#' This box plot function offers extensive log scale support, outlier detection, data point overlay options, data subsetting with a secondary factor, and data point highlighting with a tertiary factor.
#' The complicated part of using this function is handling its many options. A wrapper function to set up and run it with preset options may be a good idea if you are using it along. The function \code{\link{niceDots}} is an example of this.
#' Briefly put, the \code{by} argument can be a data frame of factors and the function will  work through the columns in order as needed.
#' If \code{x} is a numeric vector, then \code{by} should be a factor to group it into categories. If \code{by} is a data frame of factors and \code{subGroup=\link{TRUE}}, then the first column for \code{by}
#' is used as the grouping factor and the second column is used as the sub-grouping factor. If \code{pointHighlights==\link{TRUE}}, and \code{subGroup=\link{TRUE}}, the the third column of \code{by}
#' is used to highlight points data point overlay (assuming \code{drawPoints=\link{TRUE}}). If \code{subGroup=\link{FALSE}} and \code{subGroup=\link{TRUE}}, then the second column of \code{by} is used to control
#' the point highlighting. If \code{x} itself is a data frame of numeric vectors, \code{subGroup} is automatically set to false and each column of \code{x} is plotted like a sub-group and grouped
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
#' niceBox(iris$Sepal.Length, by=factorFrame, minorTick=4,subGroup=TRUE,
#'     ylab=Lab,main=Title,plotColors=myCols)
#' @import tidyverse
#' @export
#' @seealso \code{\link{boxplot}}, \code{\link[beeswarm]{beeswarm}}, \code{\link{quantileTrim}}, \code{\link{prepCategoryWindow}}
niceBox <- function(x, by=NULL, groupNames=NULL, main=NULL,sub=NULL, ylab=NULL, theme=basicTheme, minorTick=FALSE, guides=TRUE, outliers=1.5, pointSize=1, width=1, pointShape=16, plotColors=list(bg="open"), logScale=FALSE, trim=FALSE, pointMethod="jitter", axisText=c(NULL,NULL), showCalc=FALSE, calcType="none", drawBox=TRUE, yLim=NULL, rotateLabels=FALSE, rotateY=FALSE, add=FALSE, minorGuides=NULL, extendTicks=TRUE, subGroup=FALSE, subGroupLabels=NULL, expLabels=TRUE, sidePlot=FALSE, drawPoints=TRUE, pointHighlights=FALSE, pointLaneWidth=.7, flipFacts=FALSE, na.rm=FALSE, verbose=FALSE, legend=FALSE, ...) {UseMethod("niceBox",x)}

#' @import tidyverse
#' @export
niceBox.default <- function(x, by=NULL, groupNames=NULL, main=NULL,sub=NULL, ylab=NULL, theme=basicTheme, minorTick=NULL, guides=NULL, outliers=1.5, pointSize=NULL, width=NULL, pointShape=NULL, plotColors=NULL, logScale=FALSE, trim=FALSE, pointMethod=NULL, axisText=c(NULL,NULL), showCalc=FALSE, calcType="none", drawBox=TRUE, yLim=NULL, rotateLabels=FALSE, rotateY=TRUE, add=FALSE, minorGuides=NULL, extendTicks=TRUE, subGroup=FALSE, subGroupLabels=NULL, expLabels=FALSE, sidePlot=FALSE, drawPoints=TRUE, pointHighlights=FALSE, pointLaneWidth=NULL, flipFacts=FALSE, na.rm=FALSE, verbose=FALSE, legend=FALSE, ...) {
  if(any(is.na(x))){warning("Warning: NAs detected in dataset")}
  prepedData<-NULL
  plotData<-NULL
  lWidth<-NULL
  wiskerLineType<-NULL
  capWidth<-NULL
  checked<-dataFlightCheck(x,by,na.rm=na.rm,flipFacts = flipFacts)
  x<-checked$d
  by<-checked$b
  rm(checked)
  swarmOverflow<-NULL

  #Here we check to see if the user specified any options so that they are left unaltered if present
  finalOptions<-procNiceOptions(x=x,by=by,minorTick=minorTick,pointShape=pointShape,wiskerLineType=wiskerLineType,lWidth=lWidth,capWidth=capWidth,pointLaneWidth=pointLaneWidth,width=width,guides=guides,pointSize=pointSize,subGroup=subGroup,stack=F,pointHighlights=pointHighlights,type="BP",theme=theme,plotColors=plotColors,logScale=logScale,pointMethod=pointMethod,drawPoints=drawPoints,groupNames=groupNames,swarmOverflow = swarmOverflow)
  minorTick<-finalOptions$minorTick
  pointShape<-finalOptions$pointShape
  wiskerLineType<-finalOptions$wiskerLineType
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

  #if(flipFacts & is.data.frame(x)){subGroup<-TRUE}
  #Handling adding plots to existing graph
  if(add==TRUE) {
    if(logScale>0) {
      prepedData<-list(data=log(x+1,logScale))
    } else {
      prepedData<-list(data=x)
    }
  #Make a new graph with a new plotting enviroment
  } else {
    if(is.null(minorGuides)){
      if(guides!=FALSE & logScale > 0) {
        minorGuides<-TRUE
      } else {
        minorGuides<-FALSE
      }
    }
    prepedData<-prepCategoryWindow(x,by=by, groupNames=groupNames, minorTick=minorTick, guides=guides, plotColors=plotColors, yLim=yLim, rotateLabels=rotateLabels, rotateY=rotateY, trim=trim, logScale=logScale, axisText=axisText, minorGuides=minorGuides, extendTicks=extendTicks, subGroup=subGroup, expLabels=expLabels,sidePlot=sidePlot,subGroupLabels=subGroupLabels, theme=theme, legend=legend, pointHighlights=pointHighlights)
  }
  pvalue<-NULL
  filter<-rep(TRUE,length(x))
  if(trim>0){filter<-quantileTrim(x,trim,na.rm=T,returnFilter=T)[[2]]}

  #Initialize legend variables so we can update based on options
  legendTitle<-"Legend"
  legendLabels<-NULL
  legendColors<-plotColors$points
  #Handles cases where users want the points overlay to be consistant and the fill to change.
  if(length(legendColors)<=1 & length(plotColors$fill)>1){
    legendColors<-plotColors$fill
  }

  #Data is set and ready to go. Plotting is handled based on cases handling if 'x' and 'by' are vectors or dataframes
  if(is.numeric(prepedData[[1]])){
    #CASE: by is a factor and data is a numeric vector
    if(is.factor(by)) {
      if(calcType[1]!="none"){pvalue<-calcStats(prepedData[[1]],by[filter],calcType[1],verbose=verbose)}
      plotLoc<-seq(1,length(groupNames),by=1)
      names(plotLoc)<-groupNames
      legend<-FALSE
      plotData<-prepNiceData(prepedData=prepedData,by=by, subGroup=subGroup, outliers=outliers, filter=filter, groupNames=groupNames, plotLoc=plotLoc, width=width,verbose=verbose)
      plotData %>% drawBoxPlot(side=sidePlot,col=plotColors$lines,fill=plotColors$fill,drawDot=F,drawBox=drawBox, lWidth=lWidth,whiskerLty=wiskerLineType,capWidth=capWidth)
      addNicePoints(prepedData=prepedData, by=by, filter=filter, sidePlot=sidePlot, subGroup=subGroup, plotAt=plotLoc,pointHighlights=pointHighlights, pointMethod=pointMethod, pointShape=pointShape, pointSize=pointSize, width=width, pointLaneWidth=pointLaneWidth, plotColors=plotColors, drawPoints=drawPoints, outliers=outliers,swarmOverflow = swarmOverflow)
    } else {
      if(calcType[1]!="none"){pvalue<-calcStats(prepedData[[1]],by[filter,1],calcType[1])}
    #CASE: by is not a factor, data is a numeric vector and subGroup is TRUE
      if(subGroup) {
        facetLoc<-facetSpacing(length(levels(by[,2])),length(groupNames))
        names(facetLoc)<-unlist(lapply(levels(by[,1]),FUN=function(x) paste0(x,levels(by[,2]),sep=".")))
        plotData<-prepNiceData(prepedData=prepedData,by=by, subGroup=subGroup, outliers=outliers, filter=filter, groupNames=groupNames, plotLoc=plotLoc, width=width,verbose=verbose)
        cLoc<-facetLoc[plotData$facetLevel]
        plotData %>% bind_cols(at=cLoc,width=rep(.25*width/length(levels(by[,2])),length(cLoc))) %>%
          drawBoxPlot(side=sidePlot,col=plotColors$lines,fill=plotColors$fill,drawDot=F,drawBox=drawBox,lWidth = lWidth,whiskerLty=wiskerLineType,capWidth=capWidth)
        addNicePoints(prepedData=prepedData, by=by, filter=filter, sidePlot=sidePlot, subGroup=subGroup, plotAt=facetLoc,pointHighlights=pointHighlights, pointMethod=pointMethod, pointShape=pointShape, pointSize=pointSize, width=width, pointLaneWidth=pointLaneWidth, plotColors=plotColors, drawPoints=drawPoints, outliers=outliers,swarmOverflow = swarmOverflow)
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
      #CASE: by is not a factor, data is a numeric vector and subGroup is FALSE
        plotLoc<-seq(1,length(groupNames),by=1)
        names(plotLoc)<-groupNames
        plotData<-prepNiceData(prepedData=prepedData,by=by, subGroup=subGroup, outliers=outliers, filter=filter, groupNames=groupNames, plotLoc=plotLoc, width=width,verbose=verbose)
        plotData %>% drawBoxPlot(side=sidePlot,col=plotColors$lines,fill=plotColors$fill,drawDot=F,drawBox=drawBox,lWidth=lWidth,whiskerLty=wiskerLineType,capWidth=capWidth)
        addNicePoints(prepedData=prepedData, by=by, filter=filter, sidePlot=sidePlot, subGroup=subGroup, plotAt=plotLoc,pointHighlights=pointHighlights, pointMethod=pointMethod, pointShape=pointShape, pointSize=pointSize, width=width, pointLaneWidth=pointLaneWidth, plotColors=plotColors, drawPoints=drawPoints, outliers=outliers,swarmOverflow = swarmOverflow)
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
    #CASE: data is a dataframe, by is a factor, subGroup is ignored
    if(is.factor(by)) {
      if(calcType[1]!="none"){pvalue<-calcStats(prepedData[[1]][,1],by,calcType[1],verbose=verbose)}
      facetLoc<-facetSpacing(length(prepedData[[1]]),length(groupNames))
      names(facetLoc)<-unlist(lapply(levels(by),FUN=function(y) paste0(y,names(x),sep=".")))
      plotData<-prepNiceData(prepedData=prepedData,by=by, subGroup=subGroup, outliers=outliers, filter=filter, groupNames=groupNames, plotLoc=plotLoc, width=width,flipFacts=flipFacts,verbose=verbose)
      cLoc<-facetLoc[plotData$facetLevel]
      plotData %>% bind_cols(at=cLoc,width=rep(.25*width/length(x),length(cLoc))) %>%
        drawBoxPlot(side=sidePlot,col=plotColors$lines,fill=plotColors$fill,drawDot=F,drawBox=drawBox, lWidth=lWidth,whiskerLty=wiskerLineType,capWidth=capWidth)
      addNicePoints(prepedData=prepedData, by=by, filter=filter, sidePlot=sidePlot, subGroup=subGroup, plotAt=facetLoc,pointHighlights=pointHighlights, pointMethod=pointMethod, pointShape=pointShape, pointSize=pointSize, width=width, pointLaneWidth=pointLaneWidth, plotColors=plotColors, drawPoints=drawPoints, outliers=outliers, dataCols=length(x),swarmOverflow = swarmOverflow)
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
    #CASE: data is a dataframe, by is a dataframe, subGroup is ignored
      facetLoc<-facetSpacing(length(prepedData[[1]]),length(groupNames))
      names(facetLoc)<-unlist(lapply(levels(by[,1]),FUN=function(y) paste0(y,names(x),sep=".")))
      if(calcType[1]!="none"){pvalue<-calcStats(prepedData[[1]][,1],by[,1],calcType[1])}
      plotData<-prepNiceData(prepedData=prepedData,by=by, subGroup=subGroup, outliers=outliers, filter=filter, groupNames=groupNames, plotLoc=plotLoc, width=width,flipFacts=flipFacts,verbose=verbose)
      cLoc<-facetLoc[plotData$facetLevel]
      plotData %>% bind_cols(at=cLoc,width=rep(.25*width/length(x),length(cLoc))) %>%
        drawBoxPlot(side=sidePlot,col=plotColors$lines,fill=plotColors$fill,drawDot=F,drawBox=drawBox,lWidth=lWidth,whiskerLty=wiskerLineType,capWidth=capWidth)
      addNicePoints(prepedData=prepedData, by=by, filter=filter, sidePlot=sidePlot, subGroup=subGroup, plotAt=facetLoc,pointHighlights=pointHighlights, pointMethod=pointMethod, pointShape=pointShape, pointSize=pointSize, width=width, pointLaneWidth=pointLaneWidth, plotColors=plotColors, drawPoints=drawPoints, outliers=outliers, dataCols=length(x),swarmOverflow = swarmOverflow)
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
  dataOut<-list(data=data.frame(prepedData$data,by),summary=plotData,stats=pvalue)
  invisible(dataOut)
}


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
niceDots <- function(x, by=NULL, groupNames=NULL, main=NULL,sub=NULL, ylab=NULL, minorTick=FALSE, theme=basicTheme, guides=TRUE, outliers=1.5, pointSize=1, width=1, pointShape=1, plotColors=list(bg="open"), logScale=FALSE, trim=FALSE, pointMethod=NULL, axisText=c(NULL,NULL), showCalc=FALSE, calcType="none", yLim=NULL, rotateLabels=FALSE, rotateY=FALSE, add=FALSE, minorGuides=NULL, extendTicks=TRUE, subGroup=FALSE, subGroupLabels=NULL, expLabels=TRUE, sidePlot=FALSE, pointHighlights=FALSE, pointLaneWidth=1, na.rm=FALSE, flipFacts=FALSE, verbose=FALSE, legend=FALSE, ...) {UseMethod("niceDots",x)}

#' @export
niceDots.default<-function(x, by=NULL, groupNames=NULL, main=NULL,sub=NULL, ylab=NULL, minorTick=FALSE, theme=basicTheme, guides=TRUE, outliers=1.5, pointSize=NULL, width=NULL, pointShape=NULL, plotColors=NULL, logScale=FALSE, trim=FALSE, pointMethod=NULL, axisText=c(NULL,NULL), showCalc=FALSE, calcType="none", yLim=NULL, rotateLabels=FALSE, rotateY=FALSE, add=FALSE, minorGuides=NULL, extendTicks=TRUE, subGroup=FALSE, subGroupLabels=NULL, expLabels=TRUE, sidePlot=FALSE, pointHighlights=FALSE, pointLaneWidth=NULL, na.rm=FALSE, flipFacts=FALSE, verbose=FALSE, legend=FALSE, ...) {
  #Here we check to see if the user specified any options so that they are left unaltered if present
  lWidth<-NULL
  checked<-dataFlightCheck(x,by,na.rm=na.rm,flipFacts = flipFacts)
  finalOptions<-procNiceOptions(x=checked$d,by=checked$b,minorTick=minorTick,pointShape=pointShape,wiskerLineType=1,lWidth=lWidth,capWidth=1,pointLaneWidth=pointLaneWidth,width=width,guides=guides,pointSize=pointSize,subGroup=subGroup,stack=F,pointHighlights=pointHighlights,type="DP",theme=theme,plotColors=plotColors,logScale=logScale,pointMethod=pointMethod,drawPoints=TRUE,groupNames=groupNames,swarmOverflow=NULL)
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

  niceBox(x=x,by=by,groupNames=groupNames,theme=theme,main=main,ylab=ylab,minorTick=minorTick,guides=guides,outliers=outliers,pointSize=pointSize,width=width,pointShape=pointShape,plotColors=plotColors,logScale=logScale,trim=trim,pointMethod=pointMethod, axisText=axisText, showCalc=showCalc, calcType=calcType, yLim=yLim, rotateLabels=rotateLabels, rotateY=rotateY, add=add, minorGuides=minorGuides, extendTicks=extendTicks,subGroup=subGroup,subGroupLabels=subGroupLabels,expLabels=expLabels,sidePlot=sidePlot, pointHighlights=pointHighlights, pointLaneWidth=pointLaneWidth, drawBox=FALSE, drawPoints=TRUE,na.rm=na.rm, flipFacts=flipFacts, verbose=verbose, legend=legend)
}

#' @title draw a violin plot
#' @description draws a violin plot with optional scatter plot overlays, subgrouping options and log scale support.
#'
#'
#' @details
#' This violin plot function offers extensive log scale support, outlier detection, data point overlay options, data subsetting with a secondary factor, and data point highlighting with a tertiary factor.
#' The complicated part of using this function is handling its many options. A wrapper function to set up and run it with preset options may be a good idea if you are using it along. The function \code{\link{niceDots}} is an example of this.
#' Briefly put, the \code{by} argument can be a data frame of factors and the function will  work through the columns in order as needed.
#' If \code{x} is a numeric vector, then \code{by} should be a factor to group it into categories. If \code{by} is a data frame of factors and \code{subGroup=\link{TRUE}}, then the first column for \code{by}
#' is used as the grouping factor and the second column is used as the sub-grouping factor. If \code{pointHighlights==\link{TRUE}}, and \code{subGroup=\link{TRUE}}, the the third column of \code{by}
#' is used to highlight points data point overlay (assuming \code{drawPoints=\link{TRUE}}). If \code{subGroup=\link{FALSE}} and \code{subGroup=\link{TRUE}}, then the second column of \code{by} is used to control
#' the point highlighting. If \code{x} itself is a data frame of numeric vectors, \code{subGroup} is automatically set to false and each column of \code{x} is plotted like a sub-group and grouped
#' by the first column of \code{by}. Data point highlighting with \code{pointHighlights=\link{TRUE}} can still be used when \code{x} is a data frame and the highlighting factor will be drawn from the second column of \code{by}.
#' Please note that the p-values can not always be calculated and are for general exploratory use only. More careful analysis is necessary to determine statistical significance.
#' This function is as S3 generic and can be extended to provide class specific functionality. To further facilitate data exploration, outputs from statistical testing and data set summaries
#' are printed to the console.
#'
#' @inheritParams prepCategoryWindow
#' @param outliers positive numeric; number of interquartile ranges (IQR) past the Q1 (25\%) and Q3 (75\%) cumulative distribution values. Outliers are often defined as \eqn{1.5 \times IQR}{1.5 * IQR} and extreme outliers are more than \eqn{3 \times IQR}{3 * IQR} away from the inner 50\% data range.
#' @param pointSize positive integer; sets the cex multiplier for point size.
#' @param pointMethod character; method to be used for ploting dots. Can be set to "jitter", "linear", "beeswarm" or "distribution".
#' @param width numeric; scaling factor controlling the width of the violins.
#' @param theme list object; Themes are are an optional way of storing graphical preset options that are compatible with all nicePlot graphing functions.
#' @param h numeric; Used to override the \code{h} hight of density estimator setting in \code{\link[vioplot]{vioplot}}. Default value is \code{\link{NULL}}.
#' @param pointShape positive integer; sets pty for plotting data points. Can be a vector to support additional graphical customization.
#' @param showCalc logical; if a p-value can be easily calculated for your data, it will be displayed using the \code{sub} annotation setting.
#' @param calcType character; should match one of 'none', 'wilcox', 'Tukey','t.test','anova' which will determine which, if any statistical test should be performed on the data.
#' @param drawBox logical; should the interquartile boxes be drawn.
#' @param add logical; causes plotting to be added to the existing plot rather the start a new one.
#' @param main character; title for the graph which is supplied to the \code{main} argument.
#' @param sub character; subtitle for the graph which is supplied to the \code{sub} argument. If \code{\link{NULL}} and \code{showCalc=\link{TRUE}} it will be used to display the output form \code{\link{calcStats}}.
#' @param ylab character; y-axis label.
#' @param drawPoints logical; draws a dot plot overlay of the data for each box
#' @param pointHighlights logical; will use additional factors in \code{by} to highlight points in the dot plot
#' @param pointLaneWidth numeric; This controls how far data point dots can move along the categorical axis when plotting. Used for \code{pointMethod} options 'jitter', 'beeswarm', and 'distribution'.
#' @param flipFacts logical; When a dataframe of values is given, column names are used as a secondary grouping factor by default. Setting \code{flipFacts=\link{TRUE}} makes the column names the primary factor and \code{by} the secondary factor.
#' @param na.rm logical; Should \code{NA}s be removed from the data set? Both data input and the factor input from \code{by} with be checked.
#' @param legend logical/character; if not equal to \code{\link{FALSE}} with cause a legend to be drawn in the margins. If set to a character string instead of a logical value, the string will be used as the legend title insteas of the factor column name from \code{by}.
#' @param verbose logical; Prints summary and p-value calculations to the screen. All data is silently by the function returned either way.
#' @param ... additional options for S3 method variants
#'
#' @examples
#' data(iris)
#' mCols<-makeColorMatrix()
#' myCols<-list(fill=c(mCols[1,3],mCols[2,3],mCols[3,3]),lines="darkblue")
#' Lab<-"Sepal Length"
#' niceVio(iris$Sepal.Length,by=iris$Species,minorTick=4,showCalc=TRUE,
#'     calcType="anova",ylab=Lab,main="Sepal Length by Species",plotColors=myCols)
#'
#'
#' plot(density(iris$Petal.Length))
#' lengthFact<-factor(iris$Petal.Length>2.82,labels=c("short","long"))
#'
#'
#' Title<-"Sepal Length by Species and Petal Length"
#' factorFrame<-data.frame(Species=iris$Species,PetalLength=lengthFact)
#' niceVio(iris$Sepal.Length, by=factorFrame, minorTick=4,subGroup=TRUE,
#'     ylab=Lab,main=Title,plotColors=myCols)
#' @import tidyverse
#' @export
#' @seealso \code{\link[vioplot]{vioplot}}, \code{\link{boxplot}}, \code{\link{niceBox}}, \code{\link[beeswarm]{beeswarm}}, \code{\link{prepCategoryWindow}}
niceVio <- function(x, by=NULL, h=NULL, groupNames=NULL, main=NULL,sub=NULL, ylab=NULL, minorTick=FALSE, guides=TRUE, theme=basicTheme, outliers=1.5, pointSize=NULL, width=NULL, pointShape=NULL, plotColors=NULL, logScale=FALSE, trim=FALSE, pointMethod=NULL, axisText=c(NULL,NULL), showCalc=FALSE, calcType="none", drawBox=TRUE, yLim=NULL, rotateLabels=FALSE, rotateY=FALSE, add=FALSE, minorGuides=NULL, extendTicks=TRUE, subGroup=FALSE, subGroupLabels=NULL, expLabels=TRUE, sidePlot=FALSE, drawPoints=TRUE, pointHighlights=FALSE, pointLaneWidth=.7,flipFacts=FALSE, na.rm=FALSE, verbose=FALSE, legend=FALSE, ...) {UseMethod("niceVio",x)}

#' @import tidyverse
#' @export
niceVio.default <- function(x, by=NULL, h=NULL, groupNames=NULL, main=NULL,sub=NULL, ylab=NULL, minorTick=FALSE, guides=TRUE, theme=basicTheme, outliers=FALSE, pointSize=NULL, width=NULL, pointShape=NULL, plotColors=NULL, logScale=FALSE, trim=FALSE, pointMethod=NULL, axisText=c(NULL,NULL), showCalc=FALSE, calcType="none", drawBox=TRUE, yLim=NULL, rotateLabels=FALSE, rotateY=TRUE, add=FALSE, minorGuides=NULL, extendTicks=TRUE, subGroup=FALSE, subGroupLabels=NULL, expLabels=FALSE, sidePlot=FALSE, drawPoints=TRUE, pointHighlights=FALSE, pointLaneWidth=NULL,flipFacts=FALSE,  na.rm=FALSE, verbose=FALSE,legend=FALSE, ...) {
  if(any(is.na(x))){warning("Warning: NAs detected in dataset")}
  prepedData<-NULL
  plotData<-NULL
  checked<-dataFlightCheck(x,by,na.rm=na.rm,flipFacts = flipFacts)
  x<-checked$d
  by<-checked$b
  rm(checked)
  swarmOverflow<-NULL
  lWidth<-NULL

  #Here we check to see if the user specified any options so that they are left unaltered if present
  finalOptions<-procNiceOptions(x=x,by=by,minorTick=minorTick,pointShape=pointShape,wiskerLineType=NULL,lWidth=lWidth,capWidth=NULL,pointLaneWidth=pointLaneWidth,width=width,guides=guides,pointSize=pointSize,subGroup=subGroup,stack=F,pointHighlights=pointHighlights,type="VP",theme=theme,plotColors=plotColors,logScale=logScale,pointMethod=pointMethod,drawPoints=drawPoints,groupNames=groupNames,swarmOverflow=swarmOverflow, errorCap = "bar")
  minorTick<-finalOptions$minorTick
  pointShape<-finalOptions$pointShape
  wiskerLineType<-finalOptions$wiskerLineType
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
  theme$plotColors<-plotColors
  medianMarkerShape<-theme$medianMarkerShape

  #Capturing default group names
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
  #if(flipFacts==TRUE & is.data.frame(x)){subGroup<-TRUE}
  #Handling adding plots to existing graph
  if(add) {
    if(logScale>0) {
      prepedData<-list(data=log(x+1,logScale))
    } else {
      prepedData<-list(data=x)
    }
    #Make a new graph with a new plotting enviroment
  } else {
    if(is.null(minorGuides)){
      if(guides!=FALSE & logScale > 0) {
        minorGuides<-TRUE
      } else {
        minorGuides<-FALSE
      }
    }
    prepedData<-prepCategoryWindow(x,by=by, groupNames=groupNames, minorTick=minorTick, guides=guides, plotColors=plotColors, yLim=yLim, rotateLabels=rotateLabels, rotateY=rotateY, trim=trim, logScale=logScale, axisText=axisText, minorGuides=minorGuides, extendTicks=extendTicks, subGroup=subGroup, expLabels=expLabels,sidePlot=sidePlot,subGroupLabels=subGroupLabels, theme=theme, legend=legend, pointHighlights=pointHighlights)
  }

  #Initialize legend variables so we can update based on options
  legendTitle<-"Legend"
  legendLabels<-NULL
  legendColors<-plotColors$points
  #Handles cases where users want the points overlay to be consistant and the fill to change.
  if(length(legendColors)<=1 & length(plotColors$fill)>1){
    legendColors<-plotColors$fill
  }

  pvalue<-NULL
  if(subGroup==TRUE){width<-width*2}
  filter<-rep(TRUE,length(x))
  if(trim>0){filter<-quantileTrim(x,trim,na.rm=T,returnFilter=T)[[2]]}
  if(is.numeric(prepedData[[1]])){
    #CASE: by is a factor data is a numeric vector
    if(is.factor(by)) {
      if(calcType[1]!="none"){pvalue<-calcStats(prepedData[[1]],by[filter],calcType[1],verbose=verbose)}
      plotLoc<-seq(1,length(groupNames),by=1)
      names(plotLoc)<-groupNames
      plotData<-prepNiceData(prepedData=prepedData,by=by, subGroup=subGroup, outliers=outliers, filter=filter, groupNames=groupNames, plotLoc=plotLoc, width=width,verbose=verbose)
      cLevels<-levels(factor(by[filter]))
      legend<-FALSE
      lapply(cLevels, function(x) if(sum(by[filter]==x)>0){vioplot(
        prepedData[[1]][which(by[filter]==x)],at=grep(x,cLevels),add=T,horizontal=sidePlot,
        col=plotColors$fill,border=plotColors$lines,wex=width,drawRect=drawBox,
        rectCol=plotColors$rectCol,colMed=plotColors$medianMarkerCol,pchMed=medianMarkerShape, h=h,lwd=lWidth)})
      if(drawPoints){
        addNicePoints(prepedData=prepedData, by=by, filter=filter, sidePlot=sidePlot, subGroup=subGroup, plotAt=plotLoc,pointHighlights=pointHighlights, pointMethod=pointMethod, pointShape=pointShape, pointSize=pointSize, width=width, pointLaneWidth=pointLaneWidth, plotColors=plotColors, drawPoints=drawPoints, outliers=outliers,swarmOverflow = swarmOverflow)
      }
    } else {
      if(calcType[1]!="none"){pvalue<-calcStats(prepedData[[1]],by[filter,1],calcType[1],verbose=verbose)}
      #CASE: by is not a factor data is a numeric vector and subGroup is TRUE
      if(subGroup) {
        facetLoc<-facetSpacing(length(levels(by[,2])),length(groupNames))
        names(facetLoc)<-unlist(lapply(levels(by[,1]),FUN=function(x) paste0(x,levels(by[,2]),sep=".")))
        plotData<-prepNiceData(prepedData=prepedData,by=by, subGroup=subGroup, outliers=outliers, filter=filter, groupNames=groupNames, plotLoc=plotLoc, width=width,verbose=verbose)
        cLevels<-names(facetLoc)
        gFactor<-paste0(by[filter,1],by[filter,2],sep=".")
        lapply(cLevels, function(x)
          if(sum(grepl(x,gFactor))>0){
            vioplot(prepedData[[1]][gFactor==x],
              at=facetLoc[grep(x,cLevels)],add=T,horizontal=sidePlot,col=plotColors$fill,
              border=plotColors$lines,wex=.25*width/length(levels(by[,2])),drawRect=drawBox,
              rectCol=plotColors$rectCol,colMed=plotColors$medianMarkerCol,pchMed=medianMarkerShape, h=h,lwd=lWidth)
          }
        )
        if(drawPoints) {
          addNicePoints(prepedData=prepedData, by=by, filter=filter, sidePlot=sidePlot, subGroup=subGroup, plotAt=facetLoc,pointHighlights=pointHighlights, pointMethod=pointMethod, pointShape=pointShape, pointSize=pointSize, width=width, pointLaneWidth=pointLaneWidth, plotColors=plotColors, drawPoints=drawPoints, outliers=outliers,swarmOverflow = swarmOverflow)
        }
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
       #CASE: by is not a factor, data is a numeric vector and subGroup is FALSE
        plotLoc<-seq(1,length(groupNames),by=1)
        names(plotLoc)<-groupNames
        plotData<-prepNiceData(prepedData=prepedData,by=by, subGroup=subGroup, outliers=outliers, filter=filter, groupNames=groupNames, plotLoc=plotLoc, width=width,verbose=verbose)
        cLevels<-groupNames
        gFactor<-by[filter,1]
        lapply(cLevels, function(x)
          if(sum(grepl(x,gFactor))>0){
            vioplot(prepedData[[1]][gFactor==x],
                    at=grep(x,cLevels),add=T,horizontal=sidePlot,col=plotColors$fill,
                    border=plotColors$lines,wex=.25*width/length(levels(by[,2])),drawRect=drawBox,
                    rectCol=plotColors$rectCol,colMed=plotColors$medianMarkerCol,pchMed=medianMarkerShape, h=h,lwd=lWidth)
          }
        )
        if(drawPoints) {
          addNicePoints(prepedData=prepedData, by=by, filter=filter, sidePlot=sidePlot, subGroup=subGroup, plotAt=plotLoc,pointHighlights=pointHighlights, pointMethod=pointMethod, pointShape=pointShape, pointSize=pointSize, width=width, pointLaneWidth=pointLaneWidth, plotColors=plotColors, drawPoints=drawPoints, outliers=outliers,swarmOverflow = swarmOverflow)
        }
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
    #CASE: data is a dataframe, by is a factor, subGroup is ignored
    if(is.factor(by)) {
      if(calcType[1]!="none"){pvalue<-calcStats(prepedData[[1]][,1],by,calcType[1],verbose=verbose)}
      facetLoc<-facetSpacing(length(prepedData[[1]]),length(groupNames))
      names(facetLoc)<-unlist(lapply(levels(by),FUN=function(y) paste0(y,names(x),sep=".")))
      plotData<-prepNiceData(prepedData=prepedData,by=by, subGroup=subGroup, outliers=outliers, filter=filter, groupNames=groupNames, plotLoc=plotLoc, width=width,verbose=verbose)
      #plotData<-prepNiceData(prepedData=prepedData,by=by, subGroup=subGroup, outliers=outliers, filter=filter, groupNames=groupNames, plotLoc=plotLoc, width=width,flipFacts=flipFacts,verbose=verbose)
      cLoc<-facetLoc[plotData$facetLevel]
      cLevels<-names(facetLoc)
      lapply(levels(by[filter]), function(z)
        if(sum(grepl(z,by[filter]))>0){
          lapply(names(x), function(y)
            vioplot(prepedData[[1]][by[filter]==z,y],
                  at=facetLoc[grep(paste0(z,y,sep="."),cLevels)],add=T,horizontal=sidePlot,col=plotColors$fill,
                  border=plotColors$lines,wex=.25*width/length(names(x)),drawRect=drawBox,
                  rectCol=plotColors$rectCol,colMed=plotColors$medianMarkerCol,pchMed=medianMarkerShape, h=h,lwd=lWidth)
          )
        }
      )
      if(drawPoints) {
        addNicePoints(prepedData=prepedData, by=by, filter=filter, sidePlot=sidePlot, subGroup=subGroup, plotAt=facetLoc,pointHighlights=pointHighlights, pointMethod=pointMethod, pointShape=pointShape, pointSize=pointSize, width=width, pointLaneWidth=pointLaneWidth, plotColors=plotColors, drawPoints=drawPoints, outliers=outliers, dataCols=length(x),swarmOverflow = swarmOverflow)
      }
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
      facetLoc<-facetSpacing(length(prepedData[[1]]),length(groupNames))
      names(facetLoc)<-unlist(lapply(levels(by[,1]),FUN=function(y) paste0(y,names(x),sep=".")))
      if(calcType[1]!="none"){pvalue<-calcStats(prepedData[[1]][,1],by[,1],calcType[1], verbose=verbose)}
      plotData<-prepNiceData(prepedData=prepedData,by=by, subGroup=subGroup, outliers=outliers, filter=filter, groupNames=groupNames, plotLoc=plotLoc, width=width,flipFacts=flipFacts,verbose=verbose)
      cLoc<-facetLoc[plotData$facetLevel]
      cLevels<-names(facetLoc)
      lapply(levels(by[filter,1]), function(z)
        if(sum(grepl(z,by[filter,1]))>0){
          lapply(names(x), function(y)
            vioplot(prepedData[[1]][by[filter,1]==z,y],
                    at=facetLoc[grep(paste0(z,y,sep="."),cLevels)],add=T,horizontal=sidePlot,col=plotColors$fill,
                    border=plotColors$lines,wex=.25*width/length(names(x)),drawRect=drawBox,
                    rectCol=plotColors$rectCol,colMed=plotColors$medianMarkerCol,pchMed=medianMarkerShape, h=h,lwd=lWidth)
          )
        }
      )
      if(drawPoints) {
        addNicePoints(prepedData=prepedData, by=by, filter=filter, sidePlot=sidePlot, subGroup=subGroup, plotAt=facetLoc,pointHighlights=pointHighlights, pointMethod=pointMethod, pointShape=pointShape, pointSize=pointSize, width=width, pointLaneWidth=pointLaneWidth, plotColors=plotColors, drawPoints=drawPoints, outliers=outliers, dataCols=length(x),swarmOverflow = swarmOverflow)
      }
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
  dataOut<-list(data=data.frame(prepedData$data,by),summary=plotData,stats=pvalue)
  invisible(dataOut)
}
