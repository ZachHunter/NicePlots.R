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
#' @importFrom tibble is.tibble
#' @importFrom tidyr gather
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
  } else if(tibble::is.tibble(data)){
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
    } else if(tibble::is.tibble(by)) {
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
    } else if (is.null(by)) {
      if(is.data.frame(data)) {
        by<-factor(rep("Group",length(data[,1])))
      } else {
        by<-factor(rep("Group",length(data)))
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
      temp<-data %>% bind_cols(by=by) %>% tidyr::gather(key="newFact",value="Value",-by)
      by<-data.frame(factOne=factor(temp[,2]),factTwo=factor(temp[,1]))
      data<-as.numeric(temp$Value)
    } else {
      temp<-data %>% bind_cols(by=by) %>% tidyr::gather(key="newFact",value="Value",1:dataDim[2])
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
#' @importFrom grDevices boxplot.stats
#' @seealso \code{\link{niceBox}}, \code{\link{niceVio}}, \code{\link{niceDots}}, \code{\link[grDevices]{boxplot.stats}}
prepNiceData<- function(prepedData,by, subGroup=FALSE,outliers=TRUE,filter,groupNames,plotLoc,width=1,flipFacts=FALSE,verbose=FALSE){
  #CASE: by is a factor; data is a numeric vector
  if(is.numeric(prepedData[[1]])){
    if(is.factor(by)) {
      plotData<-bind_cols(data=prepedData[[1]],fact=by[filter]) %>%
        group_by(.data$fact) %>%
        do(data.frame(t(boxplot.stats(.data$data,coef=outliers)$stats),n=length(.data$data))) %>%
        ungroup() %>%
        select(fact=.data$fact,n=n,min=.data$X1,q1=.data$X2,median=.data$X3,q3=.data$X4, max=.data$X5) %>%
        mutate(at=plotLoc,width=.25*width)

      if(verbose){print(select(plotData,.data$fact,.data$n,.data$median,.data$q1,.data$q3,.data$tmin,.data$tmax))}
      return(plotData)
    } else {
      #CASE: by is not a factor; data is a numeric vector; subGroup is TRUE
      if(subGroup) {
        plotData<-bind_cols(data=prepedData[[1]],fact=by[filter,1],subGroup=by[filter,2]) %>%
          group_by(.data$fact,.data$subGroup) %>%
          do(data.frame(t(boxplot.stats(.data$data,coef=outliers)$stats),n=length(.data$data))) %>%
          ungroup() %>%
          select(fact=.data$fact,subGroup=.data$subGroup,n=n,min=.data$X1,q1=.data$X2,median=.data$X3,q3=.data$X4, max=.data$X5) %>%
          mutate(facetLevel=paste0(.data$fact,.data$subGroup,sep="."))

        if(verbose){print(select(plotData,.data$fact,.data$subGroup,.data$n,.data$median,.data$q1,.data$q3,.data$min,.data$max))}
        return(plotData)
      } else {
        #CASE: by is not a factor; data is a numeric vector; subGroup is FALSE
        plotData<-bind_cols(data=prepedData[[1]],fact=by[filter,1]) %>%
          group_by(.data$fact) %>%
          do(data.frame(t(boxplot.stats(.data$data,coef=outliers)$stats),n=length(.data$data))) %>%
          ungroup() %>%
          select(fact=.data$fact,n=n,min=.data$X1,q1=.data$X2,median=.data$X3,q3=.data$X4, max=.data$X5) %>%
          mutate(at=plotLoc,width=.25*width)

        if(verbose){print(select(plotData,.data$fact,.data$n,.data$median,.data$q1,.data$q3,.data$min,.data$max))}
        return(plotData)
      }
    }
  } else {
    #CASE: data is a dataframe; by is a factor; subGroup is ignored
    if(is.factor(by)) {
      plotData<-bind_cols(prepedData[[1]],fact=by[filter]) %>%
        tidyr::gather(key=subGroup,value=data,-.data$fact) %>%
        group_by(.data$fact,.data$subGroup) %>%
        do(data.frame(t(boxplot.stats(.data$data,coef=outliers)$stats),n=length(.data$data))) %>%
        ungroup() %>%
        select(fact=.data$fact,subGroup=.data$subGroup,n=n,min=.data$X1,q1=.data$X2,median=.data$X3,q3=.data$X4, max=.data$X5) %>%
        mutate(facetLevel=paste0(.data$fact,.data$subGroup,sep="."))

      if(verbose){print(select(plotData,.data$fact,.data$subGroup,.data$n,.data$median,.data$q1,.data$q3,.data$min,.data$max))}
      return(plotData)
    } else {
      #CASE: data is a dataframe; by is a dataframe; subGroup is ignored
      plotData<-bind_cols(prepedData[[1]],fact=by[filter,1]) %>%
        tidyr::gather(key=subGroup,value=data,-.data$fact) %>%
        group_by(.data$fact,.data$subGroup) %>%
        do(data.frame(t(boxplot.stats(.data$data,coef=outliers)$stats),n=length(.data$data))) %>%
        ungroup() %>%
        select(fact=.data$fact,subGroup=.data$subGroup,n=n,min=.data$X1,q1=.data$X2,median=.data$X3,q3=.data$X4, max=.data$X5) %>%
        mutate(facetLevel=paste0(.data$fact,.data$subGroup,sep="."))

      if(verbose){print(select(plotData,.data$fact,.data$subGroup,.data$n,.data$median,.data$q1,.data$q3,.data$min,.data$max))}
      return(plotData)
    }
  }
}



#' @title Prepare and print basic statistics for niceBar
#' @description Processess the input data, factors, and options to produce summary data for drawing bar plots
#'
#' @details
#' todo<-1
#'
#' @examples
#' todo<-1
#'
#' @param x list; a list object returned by \code{\link{prepCategoryWindow}}
#' @param by factor or dataframe of factors; One or more factors that control how the data is grouped. The first column is the primary grouping factor and the second and thrid columns are used for sub-grouping and/or stacking as needed.
#' @param subGroup logical; If \code{\link{TRUE}} the data will be faceted into subgroups within the primary factor levels. Ignored if \code{by} is a \code{\link[base]{factor}} or \code{x} is a \code{\link[base]{data.frame}}.
#' @param errorMultiple numeric; How many standard errors/deviations should be represented by the error bars.
#' @param upperErrorFun character string; Determines how the error barse are calculated. Options are \code{\link[stats]{sd}} (standard deviation), \code{\link{se}} (standard error), \code{\link[base]{min}}, \code{\link[base]{max}} and \code{\link{boot95ci}} (bootstrap 95\% confidence interval).
#' @param lowerErrorFun character; A character vector for the primary group names
#' @param aggFunction character string; An string naming the function to be used to aggregate the grouped data. Typically should be either \code{\link[stats]{median}} or \code{\link[base]{mean}}.
#' @param stack logical; Should one of the factors in \code{by} be used make a stacked bar plot. Note that this sort of analysis is nonsensical for many data sets.
#'
#' @import dplyr
#' @import tidyr
#' @importFrom purrr invoke
#' @seealso \code{\link{niceBar}}, \code{\link{boot95ci}}, \code{\link{drawBar}}
prepBarData<-function(x,by,errorMultiple=1,upperErrorFun="sd",lowerErrorFun=upperErrorFun,aggFunction="mean",stack=FALSE,subGroup=FALSE){
  optsU<-NULL
  optsL<-NULL
  if(upperErrorFun=="boot95ci") {
    optsU<-list(agg=aggFunction,upper=TRUE)
    optsL<-list(agg=aggFunction,upper=FALSE)
  }

  plotData<-NULL
  #vars will be used to select out just the columns needed from plotData
  vars<-c("fact","N",aggFunction,upperErrorFun,lowerErrorFun)

  #x is a vector and by is a factor
  if(is.numeric(x) & is.factor(by)) {
    plotData<-bind_cols(data=x,fact=by) %>%
      group_by(.data$fact) %>%
      summarize(AData=invoke(aggFunction, list(x=.data$data)),upperError=invoke(upperErrorFun,append(list(x=.data$data),optsU))*errorMultiple,lowerError=invoke(lowerErrorFun,append(list(x=.data$data),optsL))*errorMultiple,N=n()) %>%
      bind_cols(at=seq(1,length(levels(by))))

  #x is a vector and by is a dataframe
  } else if (is.numeric(x) & is.data.frame(by)) {
    if(subGroup){
      facetLoc<-facetSpacing(length(levels(by[,2])),length(levels(by[,1])))
      names(facetLoc)<-unlist(lapply(levels(by[,1]),FUN=function(y) paste(y,levels(by[,2]),sep=".")))
      #Stack is TRUE and subGroup is TRUE
      if(stack==T & ncol(by)>2) {
        plotData<-bind_cols(data=x,fact=by[,1],subGroup=by[,2],Stack=by[,3]) %>%
          group_by(.data$fact,.data$subGroup,.data$Stack) %>%
          summarize(AData=invoke(aggFunction, list(x=.data$data)),upperError=invoke(upperErrorFun,append(list(x=.data$data),optsU))*errorMultiple,lowerError=invoke(lowerErrorFun,append(list(x=.data$data),optsL))*errorMultiple,N=n()) %>%
          mutate(facetLevel=paste(.data$fact,.data$subGroup,sep="."),at=facetLoc[.data$facetLevel]) %>%
          ungroup()
        vars<-c("fact","subGroup","Stack","N",aggFunction,upperErrorFun,lowerErrorFun)
      #Stack is FALSE and subGroup is TRUE
      } else {
        plotData<-bind_cols(data=x,fact=by[,1],subGroup=by[,2]) %>%
          group_by(.data$fact,.data$subGroup) %>%
          summarize(AData=invoke(aggFunction, list(x=.data$data)),upperError=invoke(upperErrorFun,append(list(x=.data$data),optsU))*errorMultiple,lowerError=invoke(lowerErrorFun,append(list(x=.data$data),optsL))*errorMultiple,N=n()) %>%
          mutate(facetLevel=paste(.data$fact,.data$subGroup,sep="."),at=facetLoc[.data$facetLevel]) %>%
          ungroup()
        vars<-c("fact","subGroup","N",aggFunction,upperErrorFun,lowerErrorFun)
      }
    } else {
      facetLoc<-seq(1,length(levels(by[,1])))
      names(facetLoc)<-levels(by[,1])
      #Stack is TRUE and subGroup is FALSE
      if(stack==T & ncol(by)>1) {
        plotData<-bind_cols(data=x,fact=by[,1],Stack=by[,2]) %>%
          group_by(.data$fact,.data$Stack) %>%
          summarize(AData=invoke(aggFunction, list(x=.data$data)),upperError=invoke(upperErrorFun,append(list(x=.data$data),optsU))*errorMultiple,lowerError=invoke(lowerErrorFun,append(list(x=.data$data),optsL))*errorMultiple,N=n()) %>%
          mutate(facetLevel=.data$fact,at=facetLoc[.data$facetLevel]) %>%
          ungroup()
        vars<-c("fact","Stack","N",aggFunction,upperErrorFun,lowerErrorFun)
      #Stack is FALSE and subGroup is FALSE
      } else {
        plotData<-bind_cols(data=x,fact=by[,1]) %>%
          group_by(.data$fact) %>%
          summarize(AData=invoke(aggFunction, list(x=.data$data)),upperError=invoke(upperErrorFun,append(list(x=.data$data),optsU))*errorMultiple,lowerError=invoke(lowerErrorFun,append(list(x=.data$data),optsL))*errorMultiple,N=n()) %>%
          mutate(facetLevel=.data$fact,at=facetLoc[.data$facetLevel]) %>%
          ungroup()
      }
    }

  #x is a dataframe and by is a factor (subGroup and stack are ignored)
  } else if(is.data.frame(x) & is.factor(by)){
    facetLoc<-facetSpacing(length(x),length(levels(by)))
    names(facetLoc)<-unlist(lapply(levels(by),FUN=function(y) paste(y,colnames(x),sep=".")))
    plotData<-bind_cols(data=x,fact=by) %>%
      tidyr::gather(key=subGroup,value=data,-.data$fact) %>%
      group_by(.data$fact,.data$subGroup) %>%
      summarize(AData=invoke(aggFunction, list(x=.data$data)),upperError=invoke(upperErrorFun,append(list(x=.data$data),optsU))*errorMultiple,lowerError=invoke(lowerErrorFun,append(list(x=.data$data),optsL))*errorMultiple,N=n()) %>%
      mutate(facetLevel=paste(.data$fact,.data$subGroup,sep="."),at=facetLoc[.data$facetLevel]) %>%
      ungroup()
    vars<-c("fact","subGroup","N",aggFunction,upperErrorFun,lowerErrorFun)

  #x is a dataframe and by is a dataframe (subGroup is ignored)
  } else if(is.data.frame(x) & is.data.frame(by)) {
    facetLoc<-facetSpacing(length(x),length(levels(by[,1])))
    names(facetLoc)<-unlist(lapply(levels(by[,1]),FUN=function(y) paste(y,colnames(x),sep=".")))
    #Stack is TRUE
    if(stack==T & ncol(by)>1) {
      plotData<-bind_cols(data=x,fact=by[,1],Stack=by[,2]) %>%
        tidyr::gather(key=subGroup,value=data,-.data$fact,-.data$Stack) %>%
        group_by(.data$fact,.data$subGroup,.data$Stack) %>%
        summarize(AData=invoke(aggFunction, list(x=.data$data)),upperError=invoke(upperErrorFun,append(list(x=.data$data),optsU))*errorMultiple,lowerError=invoke(lowerErrorFun,append(list(x=.data$data),optsL))*errorMultiple,N=n()) %>%
        mutate(facetLevel=paste(.data$fact,.data$subGroup,sep="."),at=facetLoc[.data$facetLevel]) %>%
        ungroup()
      vars<-c("fact","subGroup","Stack","N",aggFunction,upperErrorFun,lowerErrorFun)
    #Stack is FALSE
    } else {
      plotData<-bind_cols(data=x,fact=by[,1]) %>%
        tidyr::gather(key=subGroup,value=data,-.data$fact) %>%
        group_by(.data$fact,.data$subGroup) %>%
        summarize(AData=invoke(aggFunction, list(x=.data$data)),upperError=invoke(upperErrorFun,append(list(x=.data$data),optsU))*errorMultiple,lowerError=invoke(lowerErrorFun,append(list(x=.data$data),optsL))*errorMultiple,N=n()) %>%
        mutate(facetLevel=paste(.data$fact,.data$subGroup,sep="."),at=facetLoc[.data$facetLevel]) %>%
        ungroup()
      vars<-c("fact","subGroup","N",aggFunction,upperErrorFun,lowerErrorFun)
    }
  } else {
    stop("Error in prepBarData: x and by are misformated... this appears to be a bug.\nThey should be factors and/or dataframes.\n")
  }

  #Data for displaying a summary table is separated from plotData and formated accordingly
  printData<-plotData
  if(upperErrorFun!=lowerErrorFun) {
    plotData$upperError<- (plotData$upperError/errorMultiple - plotData$AData)*errorMultiple
    plotData$lowerError<- (plotData$AData - plotData$lowerError/errorMultiple)*errorMultiple
  }
  #based on options, the column position of AData can vary
  ADataLoc<-grep("AData",colnames(printData))
  if(upperErrorFun==lowerErrorFun & upperErrorFun != "boot95ci") {
    vars<-vars[-length(vars)]
    colnames(printData)[c(ADataLoc,ADataLoc+1)]<-c(aggFunction,upperErrorFun)
  } else {
    if(upperErrorFun == "boot95ci") {
      vars[c(length(vars)-1,length(vars))]<-c("95ci_Upper","95ci_Lower")
      colnames(printData)[ADataLoc:(ADataLoc+2)]<-c(aggFunction,vars[c(length(vars)-1,length(vars))])
    } else {
      colnames(printData)[ADataLoc:(ADataLoc+2)]<-c(aggFunction,upperErrorFun,lowerErrorFun)
    }
  }

  #select out just the columns we want to print
  printData <- printData %>% select(!!vars)
  #based on options, the column position of aggFunction values can vary
  ADataLoc<-grep(aggFunction,colnames(printData))
  if(errorMultiple!=1){
    colnames(printData)[ADataLoc+1]<-c(paste0(colnames(printData)[ADataLoc+1],"_",errorMultiple,"x"))
    if(upperErrorFun!=lowerErrorFun) {
      colnames(printData)[ADataLoc+2]<-paste0(colnames(printData)[ADataLoc+1],"_",errorMultiple,"x")
    }
  }
  #plotData<-plotData %>% mutate(yt=AData,at=at,yb=bVal,UpperError=upperError,LowerError=lowerError)
  list(plot=plotData,print=printData)
}


