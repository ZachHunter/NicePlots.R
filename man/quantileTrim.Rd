% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/np_utility.R
\name{quantileTrim}
\alias{quantileTrim}
\title{Filter data by interquartile range}
\usage{
quantileTrim(x, threshold = 3, na.rm = FALSE, returnFilter = FALSE)
}
\arguments{
\item{x}{a numeric vector or a object compatible with the \code{\link[stats]{quantile}} function}

\item{threshold}{numeric; the number of interquartile ranges out side of the inner 50\% range of the data to use as a cutoff from trimming. Typical values include 1.5 for outliers and 3 for extreme outliers.}

\item{na.rm}{logical; if true will remove all \code{\link{NA}} values from \code{x} before analyzing the data.}

\item{returnFilter}{logical; will cause the function to return a list including with both the trimmed data and a logical vector that can be used to filter objects of the same length as \code{x}.}
}
\value{
The trimmed numeric vector or a \code{returnFilter} is \code{\link{TRUE}} then a named list labeled data and filter is returned with the trimmed data and the logical filtering vector, respectively.
}
\description{
\code{quantileTrim} takes a numeric vector and removes data points that fall more than \code{threshold} * the interquartile range outside of the interquartile range. If \code{returnFilter} is set to TRUE then the function returns a named list with the trimmed data and a logical vector
}
\details{
The interquartile range (IQR) also known as the H-spread, represents the range encompassing the middle 50% of the data.
This is is used to as a measure of dispersion around the median and more frequently to detect outlier data points.
Here data points are filtered if \eqn{x <  Q_{1} - threshold\times IQR}{x < Q1 - threshold * IQR} and \eqn{x > Q_{3} + threshold\times IQR}{x > Q3 + threshold * IQR} where \eqn{Q_{1}}{Q1} and \eqn{Q_{3}}{Q3} represent the cumulative 25% and 75% quartiles, respectively. Typical values for the \code{threshold} argument are 1.5 for outliers and 3 for extreme outliers.
}
\examples{
x<-rnorm(1000)
paste0(mean(x)," (",range(x),")")
x<-quantileTrim(x,threshold=1.5)
paste0(mean(x)," (",range(x),")")

#Example using the filter function:
myData<-c(NA,rnorm(100),NA,NA,rnorm(100),NA,NA,NA,rnorm(300),NA,10000)
myIndex<-1:508
newData<-quantileTrim(myData,na.rm=TRUE,returnFilter=TRUE)
identical(newData$data,myData[newData$filter])
}
\seealso{
\code{\link[stats]{quantile}}.
}
