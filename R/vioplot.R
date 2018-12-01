#' @title violin plot
#' @description Produce violin plot(s) of the given (grouped) values.
#'
#' @details
#' This is a modified version of the \code{vioplot} package from CRAN.
#' Code and documentation by Daniel Adler and repurposed here with the author's permission.
#' Original code obtained from \code{https://github.com/cran/vioplot/}.
#'
#' A violin plot is a combination of a box plot and a kernel density plot.
#' Specifically, it starts with a box plot. It then adds a rotated kernel density plot to each side of the box plot.
#'
#' @author Daniel Adler \email{dadler@uni-goettingen.de}
#' @references Hintze, J. L. and R. D. Nelson (1998).  \emph{Violin plots: a box plot-density trace synergism.}  The American Statistician, 52(2):181-4.
#'
#' @param x data vector
#' @param ... additional data vectors
#' @param range a factor to calculate the upper/lower adjacent values
#' @param h the height for the density estimator, if omit as explained in sm.density, h will be set to an optimum
#' @param ylim y limits
#' @param names one label, or a vector of labels for the datas must match the number of datas given
#' @param col, color for the interior of the violin
#' @param rectCol, colMed, pchMed Graphical parameters to control the look of the box
#' @param drawRect logical. the box is drawn if \code{TRUE}.
#' @param at position of each violin. Default to \code{1:n}
#' @param add logical. if FALSE (default) a new plot is created
#' @param wex relative expansion of the violin.
#' @param horizontal logical. horizontal or vertical violins
#' @param border color border lines for the violin plot
#' @param lty numeric, R line type for the border lines in the violin plot
#' @param lwd numeric, line width for the border lines in the violin plot
#' @param colMed color of the median marker
#' @param pchMed shape of the median marker using the \code{pch} R graphical parameter
#'
#' @examples
#' # box- vs violin-plot
#' par(mfrow=c(2,1))
#' mu<-2
#' si<-0.6
#' bimodal<-c(rnorm(1000,-mu,si),rnorm(1000,mu,si))
#' uniform<-runif(2000,-4,4)
#' normal<-rnorm(2000,0,3)
#' vioplot(bimodal,uniform,normal)
#' boxplot(bimodal,uniform,normal)
#' # add to an existing plot
#' x <- rnorm(100)
#' y <- rnorm(100)
#' plot(x, y, xlim=c(-5,5), ylim=c(-5,5))
#' vioplot(x, col="tomato", horizontal=TRUE, at=-4, add=TRUE,lty=2, rectCol="gray")
#' vioplot(y, col="cyan", horizontal=FALSE, at=-4, add=TRUE,lty=2)
#' @import sm
#' @export
#' @seealso \code{\link{boxplot}} \code{\link[sm]{sm}}
vioplot <- function(x,...,range=1.5,h=NULL,ylim=NULL,names=NULL, horizontal=FALSE,
                    col="magenta", border="black", lty=1, lwd=1, rectCol="black", colMed="white", pchMed=19, at, add=FALSE, wex=1,
                    drawRect=TRUE)
{
  # process multiple datas
  datas <- list(x,...)
  n <- length(datas)

  if(missing(at)) at <- 1:n

  # pass 1
  #
  # - calculate base range
  # - estimate density
  #

  # setup parameters for density estimation
  upper  <- vector(mode="numeric",length=n)
  lower  <- vector(mode="numeric",length=n)
  q1     <- vector(mode="numeric",length=n)
  q3     <- vector(mode="numeric",length=n)
  med    <- vector(mode="numeric",length=n)
  base   <- vector(mode="list",length=n)
  height <- vector(mode="list",length=n)
  baserange <- c(Inf,-Inf)

  # global args for sm.density function-call
  args <- list(display="none")

  if (!(is.null(h)))
    args <- c(args, h=h)

  for(i in 1:n) {
    data<-datas[[i]]

    # calculate plot parameters
    #   1- and 3-quantile, median, IQR, upper- and lower-adjacent
    data.min <- min(data)
    data.max <- max(data)
    q1[i]<-quantile(data,0.25)
    q3[i]<-quantile(data,0.75)
    med[i]<-median(data)
    iqd <- q3[i]-q1[i]
    upper[i] <- min( q3[i] + range*iqd, data.max )
    lower[i] <- max( q1[i] - range*iqd, data.min )

    #   strategy:
    #       xmin = min(lower, data.min))
    #       ymax = max(upper, data.max))
    #
    est.xlim <- c( min(lower[i], data.min), max(upper[i], data.max) )

    # estimate density curve
    smout <- do.call("sm.density", c( list(data, xlim=est.xlim), args ) )

    # calculate stretch factor
    #
    #  the plots density heights is defined in range 0.0 ... 0.5
    #  we scale maximum estimated point to 0.4 per data
    #
    hscale <- 0.4/max(smout$estimate) * wex

    # add density curve x,y pair to lists
    base[[i]]   <- smout$eval.points
    height[[i]] <- smout$estimate * hscale

    # calculate min,max base ranges
    t <- range(base[[i]])
    baserange[1] <- min(baserange[1],t[1])
    baserange[2] <- max(baserange[2],t[2])

  }

  # pass 2
  #
  # - plot graphics

  # setup parameters for plot
  if(!add){
    xlim <- if(n==1)
      at + c(-.5, .5)
    else
      range(at) + min(diff(at))/2 * c(-1,1)
    if (is.null(ylim)) {
      ylim <- baserange
    }
  }
  if (is.null(names)) {
    label <- 1:n
  } else {
    label <- names
  }

  boxwidth <- 0.05 * wex

  # setup plot
  if(!add)
    plot.new()
  if(!horizontal) {
    if(!add){
      plot.window(xlim = xlim, ylim = ylim)
      axis(2)
      axis(1,at = at, labels=label )
    }

    #box()
    for(i in 1:n) {
      # plot left/right density curve
      polygon( c(at[i]-height[[i]], rev(at[i]+height[[i]])),
               c(base[[i]], rev(base[[i]])),
               col = col, border=border, lty=lty, lwd=lwd)

      if(drawRect){
        # plot IQR
        lines( at[c( i, i)], c(lower[i], upper[i]) ,lwd=lwd, lty=lty)

        # plot 50% KI box

        rect( at[i]-boxwidth/2, q1[i], at[i]+boxwidth/2, q3[i], col=rectCol)

        # plot median point

        points( at[i], med[i], pch=pchMed, col=colMed )
      }
    }
  }
  else {
    if(!add){
      plot.window(xlim = ylim, ylim = xlim)
      axis(1)
      axis(2,at = at, labels=label )
    }

    #box()
    for(i in 1:n) {

      # plot left/right density curve
      polygon( c(base[[i]], rev(base[[i]])),
               c(at[i]-height[[i]], rev(at[i]+height[[i]])),
               col = col, border=border, lty=lty, lwd=lwd)

      if(drawRect){
        # plot IQR
        lines( c(lower[i], upper[i]), at[c(i,i)] ,lwd=lwd, lty=lty)

        # plot 50% KI box

        rect( q1[i], at[i]-boxwidth/2, q3[i], at[i]+boxwidth/2,  col=rectCol)

        # plot median point
        points( med[i], at[i], pch=pchMed, col=colMed )
      }
    }
  }
  invisible (list( upper=upper, lower=lower, median=med, q1=q1, q3=q3))
}
