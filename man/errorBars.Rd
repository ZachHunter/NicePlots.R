% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/np_plotting_functions.R
\name{errorBars}
\alias{errorBars}
\title{draw custom error bars}
\usage{
errorBars(
  x,
  capType = c("none", "bar", "ball"),
  capSize = NULL,
  side = FALSE,
  col = "black",
  lType = 1,
  width = 1
)
}
\arguments{
\item{x}{named list or data frame; \code{x$start}, \code{x$stop} and \code{x$at} must all be defined as numeric vectors in a named list or data.frame object. In the case of a data frame, each row returns a single error bar.}

\item{capType}{character; can be set to 'none', 'bar', 'ball'. If set to 'bar' or ball, a round point or a line segment will be used to cap the end of the error bar.}

\item{capSize}{numeric; \code{capSize} is the distance that the cap extends away from the error bar. Set to \code{\link{NULL}} to suppress the cap regardless of the \code{capType} setting.}

\item{side}{logical; if set to true, the error bars will be drawn horizontally.}

\item{col}{color; a vector of line colors.}

\item{lType}{positive integer; corresponds to lty line type in base R.}

\item{width}{positive numeric; corresponds to lwd line width setting in base R.}
}
\description{
Draws error bars with an optional cap at one end
}
\details{
The input data frame \code{x} should have columns labels 'at','start',and 'stop' with at determining the x-axis location and start and stop indicating the position of the segment on the y-axis. If \code{side=TRUE} then the x and y axises are swapped to support horizontal plotting. Each row of the data frame will produce one bar and an optional cap can be drawn at the 'stop' location.
}
\examples{
data(iris)
library(dplyr)
iData<-iris \%>\% group_by(Species) \%>\%
   summarize(Average=mean(Sepal.Length),SD=sd(Sepal.Length))
barplot(iData$Average,ylim=c(0,10),names=levels(iris$Species),ylab="sepal length")
loc<-c(.7,1.9,3.1)
top<-iData$SD*2+iData$Average
bottom<-iData$SD*-2+iData$Average
errorBars(data.frame(at=loc,start=iData$Average,stop=top),capType="ball",capSize=2)
errorBars(data.frame(at=loc,start=iData$Average,stop=bottom),capType="ball",capSize=2)
}
