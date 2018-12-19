#' @include np_utility.R
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
#' @import dplyr
#' @import tibble
#' @import tidyr
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
      temp<-data %>% bind_cols(by=by) %>% gather(key="newFact",value="Value",-by)
      by<-data.frame(factOne=factor(temp[,2]),factTwo=factor(temp[,1]))
      data<-as.numeric(temp$Value)
    } else {
      temp<-data %>% bind_cols(by=by) %>% gather(key="newFact",value="Value",1:dataDim[2])
      by<-data.frame(factor(temp$newFact),temp[,1:byDim[2]])
      for(i in 1:dim(by)[2]){by[,i]<-factor(by[,i])}
      data<-as.numeric(temp$Value)
    }
  }
  #if(!is.vector(data)){data<-as.data.frame(data)}
  list(d=data,b=by)
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
#' @import dplyr
#' @import tidyr
#' @seealso \code{\link{niceBox}}, \code{\link{niceVio}}, \code{\link{niceDots}}
prepNiceData<- function(prepedData,by, subGroup=FALSE,outliers=TRUE,filter,groupNames,plotLoc,width=1,flipFacts=FALSE,verbose=FALSE){
  #CASE: by is a factor data is a numeric vector
  if(is.numeric(prepedData[[1]])){
    if(is.factor(by)) {
      if(outliers==FALSE){
        plotData<-bind_cols(data=prepedData[[1]],fact=by[filter]) %>%
          group_by(.data$fact) %>%
          summarize(median=median(data),min=min(data),max=max(data),n=n(),q1=quantile(data,.25),q3=quantile(data,.75)) %>%
          bind_cols(at=plotLoc,width=rep(.25*width,length(groupNames)))
        if(verbose){print(select(plotData,.data$fact,.data$n,.data$median,.data$q1,.data$q3,.data$min,.data$max))}
        return(plotData)
      } else {
        plotData<-bind_cols(data=prepedData[[1]],fact=by[filter]) %>%
          group_by(.data$fact) %>%
          summarize(median=median(data),tmin=min(data),min=min(quantileTrim(data,threshold=outliers)),tmax=max(data),max=max(quantileTrim(data,threshold=outliers)),n=n(),q1=quantile(data,.25),q3=quantile(data,.75)) %>%
          bind_cols(at=plotLoc,width=rep(.25*width,length(groupNames)))
        if(verbose){print(select(plotData,.data$fact,.data$n,.data$median,.data$q1,.data$q3,.data$tmin,.data$tmax))}
        return(plotData)
      }
    } else {
      #CASE: by is not a factor data is a numeric vector and subGroup is TRUE
      if(subGroup) {
        if(outliers==FALSE) {
          plotData<-bind_cols(data=prepedData[[1]],fact=by[filter,1],subGroup=by[filter,2]) %>%
            group_by(.data$fact,.data$subGroup) %>%
            summarize(median=median(data),min=min(data),max=max(data),n=n(),q1=quantile(data,.25),q3=quantile(data,.75)) %>%
            mutate(facetLevel=paste0(.data$fact,.data$subGroup,sep="."))
          if(verbose){print(select(plotData,.data$fact,.data$subGroup,.data$n,.data$median,.data$q1,.data$q3,.data$min,.data$max))}
          return(plotData)
        } else {
          plotData<-bind_cols(data=prepedData[[1]],fact=by[filter,1],subGroup=by[filter,2]) %>%
            group_by(.data$fact,.data$subGroup) %>%
            summarize(median=median(data),tmin=min(data),min=min(quantileTrim(data,threshold=outliers)),tmax=max(data),max=max(quantileTrim(data,threshold=outliers)),n=n(),q1=quantile(data,.25),q3=quantile(data,.75)) %>%
            mutate(facetLevel=paste0(.data$fact,.data$subGroup,sep="."))
          if(verbose){print(select(plotData,.data$fact,.data$subGroup,.data$n,.data$median,.data$q1,.data$q3,.data$tmin,.data$tmax))}
          return(plotData)
        }
      } else {
        #CASE: by is not a factor, data is a numeric vector and subGroup is FALSE
        if(outliers==FALSE) {
          plotData<-bind_cols(data=prepedData[[1]],fact=by[filter,1]) %>%
            group_by(.data$fact) %>%
            summarize(median=median(data),min=min(data),max=max(data),n=n(),q1=quantile(data,.25),q3=quantile(data,.75)) %>%
            bind_cols(at=plotLoc,width=rep(.25*width,length(groupNames)))
          if(verbose){print(select(plotData,.data$fact,.data$n,.data$median,.data$q1,.data$q3,.data$min,.data$max))}
          return(plotData)
        } else {
          plotData<-bind_cols(data=prepedData[[1]],fact=by[filter,1]) %>%
            group_by(.data$fact) %>%
            summarize(median=median(data),tmin=min(data),min=min(quantileTrim(data,threshold=outliers)),tmax=max(data),max=max(quantileTrim(data,threshold=outliers)),n=n(),q1=quantile(data,.25),q3=quantile(data,.75)) %>%
            bind_cols(at=plotLoc,width=rep(.25*width,length(groupNames)))
          if(verbose){print(select(plotData,.data$fact,.data$n,.data$median,.data$q1,.data$q3,.data$tmin,.data$tmax))}
          return(plotData)
        }
      }
    }
  } else {
    #CASE: data is a dataframe, by is a factor, subGroup is ignored
    if(is.factor(by)) {
      if(outliers==FALSE) {
        plotData<-bind_cols(prepedData[[1]],fact=by[filter]) %>%
          gather(key=subGroup,value=data,-.data$fact) %>%
          group_by(.data$fact,.data$subGroup) %>%
          summarize(median=median(data),min=min(data),max=max(data),n=n(),q1=quantile(data,.25),q3=quantile(data,.75)) %>%
          mutate(facetLevel=paste0(.data$fact,.data$subGroup,sep="."))
        if(verbose){print(select(plotData,.data$fact,.data$subGroup,.data$n,.data$median,.data$q1,.data$q3,.data$min,.data$max))}
        return(plotData)
      } else {
        plotData<-bind_cols(prepedData[[1]],fact=by[filter]) %>%
          gather(key=subGroup,value=data,-.data$fact) %>%
          group_by(.data$fact,.data$subGroup) %>%
          summarize(median=median(data),tmin=min(data),min=min(quantileTrim(data,threshold=outliers)),tmax=max(data),max=max(quantileTrim(data,threshold=outliers)),n=n(),q1=quantile(data,.25),q3=quantile(data,.75)) %>%
          mutate(facetLevel=paste0(.data$fact,.data$subGroup,sep="."))
        if(verbose){print(select(plotData,.data$fact,.data$subGroup,.data$n,.data$median,.data$q1,.data$q3,.data$tmin,.data$tmax))}
        return(plotData)
      }
    } else {
      #CASE: data is a dataframe, by is a dataframe, subGroup is ignored
      if(outliers==FALSE) {
        plotData<-bind_cols(prepedData[[1]],fact=by[filter,1]) %>%
          gather(key=subGroup,value=data,-.data$fact) %>%
          group_by(.data$fact,.data$subGroup) %>%
          summarize(median=median(data),min=min(data),max=max(data),n=n(),q1=quantile(data,.25),q3=quantile(data,.75)) %>%
          mutate(facetLevel=paste0(.data$fact,.data$subGroup,sep="."))
        if(verbose){print(select(plotData,.data$fact,.data$subGroup,.data$n,.data$median,.data$q1,.data$q3,.data$min,.data$max))}
        return(plotData)

      } else {
        plotData<-bind_cols(prepedData[[1]],fact=by[filter,1]) %>%
          gather(key=subGroup,value=data,-.data$fact) %>%
          group_by(.data$fact,.data$subGroup) %>%
          summarize(median=median(data),tmin=min(data),min=min(quantileTrim(data,threshold=outliers)),tmax=max(data),max=max(quantileTrim(data,threshold=outliers)),n=n(),q1=quantile(data,.25),q3=quantile(data,.75)) %>%
          mutate(facetLevel=paste0(.data$fact,.data$subGroup,sep="."))
        if(verbose){print(select(plotData,.data$fact,.data$subGroup,.data$n,.data$median,.data$q1,.data$q3,.data$tmin,.data$tmax))}
        return(plotData)
      }
    }
  }
}
