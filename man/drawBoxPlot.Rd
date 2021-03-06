% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/np_plotting_functions.R
\name{drawBoxPlot}
\alias{drawBoxPlot}
\title{draw a custom box and whisker plot}
\usage{
drawBoxPlot(
  x,
  col = "black",
  fill = NULL,
  drawBox = T,
  drawDot = F,
  whiskerLty = 2,
  side = FALSE,
  lWidth = 1,
  capWidth = 0.25,
  capType = "bar"
)
}
\arguments{
\item{x}{named list or data frame; \code{x$at}, \code{x$q2}, \code{x$q4}, \code{x$median}, \code{x$min}, \code{x$max} and \code{x$width} must all be defined as numeric vectors in a named list or data.frame object.}

\item{col}{character; color vector that controls the line color.}

\item{fill}{character; color vector that determines the interior color of the box.}

\item{drawBox}{logical; draws the box and whiskers if set to \code{\link{TRUE}}. The median line will be drawn regardless.}

\item{drawDot}{logical; draws a circle at the center of the median bar if set to \code{\link{TRUE}}.}

\item{whiskerLty}{positive integer; sets the line type or \code{lty} option for plotting the whiskers.}

\item{side}{logical; if set to \code{\link{TRUE}}, the box plots will be drawn horizontally.}

\item{lWidth}{positive integer; corresponds to lwd line width setting in base R.}

\item{capWidth}{numeric; size of the error bar cap relative to the box width.}

\item{capType}{character; should be equal to \code{none}, \code{bar}, or \code{ball}. Controls how the caps are drawn for the box plot whiskers.}
}
\description{
takes a date frame with columns labeled 'at', 'q1', 'q3', 'min', 'max', 'median' and 'width' to draw a series of boxplots.
}
\details{
The input data frame \code{x} should include columns labels named 'at','q1',and 'q3', 'median', 'min', 'max' and 'width' in any order.
Each row will draw a box and whisker plot. The columns 'q1' and 'q3' refer to the 25\% and 75\% cumulative distribution values that bound the interquartile range.
If \code{side=TRUE} then the x and y axises are swapped to support horizontal plotting. The box and whiskers can be suppressed leaving only the median line and the optional center marker if so desired.
}
\examples{
data(iris)
library(dplyr)
iData<-iris \%>\% group_by(Species) \%>\%
   summarize(median=median(Sepal.Length),min=min(Sepal.Length),max=max(Sepal.Length),
   q1=quantile(Sepal.Length)[2],q3=quantile(Sepal.Length)[4]) \%>\%
   bind_cols(at=c(1:3),width=c(.2,.3,.4))
plot(1,1,type="n",xlim=c(0,4),ylim=c(0,9))
#\donttest{drawBoxPlot(iData)}
}
\seealso{
\code{\link[graphics]{boxplot}}, \code{\link{niceBox}}
}
