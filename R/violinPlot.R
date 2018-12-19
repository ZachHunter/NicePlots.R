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
