#' @include np_utility.R
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
#' #\donttest{myCols<-formatPlotColors(myCols)}
#' print(myCols)
#' @importFrom dplyr bind_cols
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
  if(is.null(plotColors$vioBoxFill)){
    if(is.na(theme[1])) {plotColors$vioBoxFill<-setAlpha("black",.8)}
    else if (is.null(theme$vioBoxFill)) {plotColors$vioBoxFill<-setAlpha("black",.8)}
    else {plotColors$vioBoxFill<-theme$vioBoxFill}
  }
  if(is.null(plotColors$vioBoxLineCol)){
    if(is.na(theme[1])) {plotColors$vioBoxLineCol<-"black"}
    else if (is.null(theme$vioBoxLineCol)) {plotColors$vioBoxLineCol<-"black"}
    else {plotColors$vioBoxLineCol<-theme$vioBoxLineCol}
  }
  if(is.null(plotColors$title)){
    if(is.na(theme[1])) {plotColors$title<-"black"}
    else if (is.null(theme$title)) {plotColors$title<-"black"}
    else {plotColors$title<-theme$title}
  }
  if(is.null(plotColors$numbers)){
    if(is.na(theme[1])) {plotColors$numbers<-"black"}
    else if (is.null(theme$numbers)) {plotColors$numbers<-"black"}
    else {plotColors$numbers<-theme$numbers}
  }
  if(is.null(plotColors$subtext)){
    if(is.na(theme[1])) {plotColors$subtext<-"black"}
    else if (is.null(theme$subtext)) {plotColors$subtext<-"black"}
    else {plotColors$subtext<-theme$subtext}
  }
  plotColors
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
#' @param whiskerLineType numeric; number corresponding to lty option for drawing the whiskers and error bars for box plots and bar plots, respectively.
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
#' @param CLOptions list; A list of command line options captured by \code{...} being passed along to allow for theme values to be set directly from the function call even if it is not an explicit option.
#'
#' @return Named listed of graphical options
#' @importFrom magrittr %>%
#' @importFrom purrr reduce map map_lgl map_dbl
#' @seealso \code{\link{formatPlotColors}}, \code{\link{niceBox}}, \code{\link{niceDots}}, \code{\link{niceVio}}, \code{\link{niceBar}}
procNiceOptions<-function(x,by,minorTick,pointShape,whiskerLineType,lWidth,capWidth,pointLaneWidth,width,guides,pointSize,subGroup=FALSE,stack=F,pointHighlights=F,type=c("BP","VP","DP","Bar"),theme,plotColors,pointMethod,logScale,drawPoints,groupNames,swarmOverflow,errorCap=NULL,CLOptions=NULL){
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
  if(!is.list(theme) | !("npTheme" %in% class(theme))) {
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
    if(is.null(whiskerLineType)){
      if(type=="BP"){
        whiskerLineType<-2
      } else {
        whiskerLineType<-1
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
    if(length(CLOptions)>=1){
      #We are checking options derived from the ... capture to see if there are any theme options
      #Note we are only implmenting this for when a valid theme has been selected.
      #Themes let you make different settings for different plot types but since this is a one off
      #We check to see if the shorter version of the settting was used and update all the settings for all plot types
      themeNames<-names(theme)
      for(i in 1:length(CLOptions)){
        if(names(CLOptions)[i] %in% themeNames){
          theme[[names(CLOptions)[i]]]<-CLOptions[[i]]
        } else if(names(CLOptions)[i]=="pointSize"){
          loc<-grep("pointSize[BV|VP|DP|2D]",themeNames)
          theme[loc]<-rep(list(CLOptions[[i]]),length(loc))
        } else if(names(CLOptions)[i]=="width"){
          loc<-grep("width[BV|VP|DP|Bar|2D]",themeNames)
          theme[loc]<-rep(list(CLOptions[[i]]),length(loc))
        } else if(names(CLOptions)[i]=="pointShape"){
          loc<-grep("pointShape[BV|VP|DP|2D]",themeNames)
          theme[loc]<-rep(list(CLOptions[[i]]),length(loc))
        } else if(names(CLOptions)[i]=="pointLaneWidth"){
          loc<-grep("pointLaneWidth[BV|VP|DP|2D]",themeNames)
          theme[loc]<-rep(list(CLOptions[[i]]),length(loc))
        } else if(names(CLOptions)[i]=="pointMethod"){
          loc<-grep("pointMethod[BV|VP|DP|2D]",themeNames)
          theme[loc]<-rep(list(CLOptions[[i]]),length(loc))
        } else if(names(CLOptions)[i]=="lWidth"){
          loc<-grep("lWidth[BV|VP|DP|Bar|2D]",themeNames)
          theme[loc]<-rep(list(CLOptions[[i]]),length(loc))
        } else if(names(CLOptions)[i]=="errorBarLineType"){
          loc<-grep("errorBarLineType[BV|VP|DP|Bar|2D]",themeNames)
          theme[loc]<-rep(list(CLOptions[[i]]),length(loc))
        } else if(names(CLOptions)[i]=="errorBarCapWidth"){
          loc<-grep("errorBarCapWidth[BV|VP|DP|Bar|2D]",themeNames)
          theme[loc]<-rep(list(CLOptions[[i]]),length(loc))
        }
      }
    }
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
    if(is.null(whiskerLineType)){whiskerLineType<-theme[[paste0("errorBarLineType",type)]]}
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
    if(pointHighlights==TRUE | stack==TRUE) {
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
      if(stack==TRUE & subGroup==TRUE) {
        cFilter<-purrr::map(1:dim(x)[2], function(n) purrr::map_lgl(levels(by[,2]), function(y) length(x[by[,2]==y,n])>0)) %>% purrr::reduce(c)
      } else {
        cFilter<-purrr::map(1:dim(x)[2], function(n) purrr::map_lgl(levels(by[,1]), function(y) length(x[by[,1]==y,n])>0)) %>% purrr::reduce(c)
      }
    } else {
      byLength<-length(levels(by))
      cFilter<-purrr::map(1:dim(x)[2], function(n) purrr::map_lgl(levels(by), function(y) length(x[by==y,n])>0)) %>% purrr::reduce(c)
    }
    if(length(plotColors$fill)>1 & defaultFill==FALSE){
      if(stack==TRUE) {
        if(length(plotColors$fill)<myLevels) {
          warning("Not enough fill colors specified to uniquely cover factor levels!")
          plotColors$fill<-rep(plotColors$fill,length(plotColors$fill) %% myLevels +1)
        }
        plotColors$fill<-rep(plotColors$fill[1:myLevels],byLength)[cFilter]
      } else {
        if(length(plotColors$fill)<length(seq(1,dim(x)[2]))) {
          warning("Not enough fill colors specified to uniquely cover factor levels!")
          plotColors$fill<-rep(plotColors$fill,length(plotColors$fill) %% dim(x)[2] +1)
        }
        plotColors$fill<-rep(plotColors$fill[1:dim(x)[2]],byLength)[cFilter]
      }
    }
    if(length(plotColors$lines)>1 & defaultLines==FALSE){
      if(length(plotColors$lines)<length(seq(1,dim(x)[2]))) {
        warning("Not enough line colors specified to uniquely cover factor levels!")
        plotColors$lines<-rep(plotColors$lines,length(plotColors$lines) %% dim(x)[2] +1)
      }
      plotColors$lines<-rep(plotColors$lines[1:dim(x)[2]],byLength)[cFilter]
    }
    if(length(plotColors$vioBoxFill)>1 & defaultFill==FALSE){
      if(length(plotColors$vioBoxFill)<length(seq(1,dim(x)[2]))) {
        warning("Not enough vioBox fill colors specified to uniquely cover factor levels!")
        plotColors$vioBoxFill<-rep(plotColors$vioBoxFill,length(plotColors$vioBoxFill) %% dim(x)[2] +1)
      }
      plotColors$vioBoxFill<-rep(plotColors$vioBoxFill[1:dim(x)[2]],byLength)[cFilter]
    }
    if(length(plotColors$vioBoxLineCol)>1 & defaultLines==FALSE){
      if(length(plotColors$vioBoxLineCol)<length(seq(1,dim(x)[2]))) {
        warning("Not enough vioBox line colors specified to uniquely cover factor levels!")
        plotColors$vioBoxLineCol<-rep(plotColors$vioBoxLineCol,length(plotColors$vioBoxLineCol) %% dim(x)[2] +1)
      }
      plotColors$vioBoxLineCol<-rep(plotColors$vioBoxLineCol[1:dim(x)[2]],byLength)[cFilter]
    }
  } else if(subGroup==TRUE) {
    cFilter<-NULL
    byLevel<-1
    byFactor<-1
    if(is.data.frame(by)){
      if(pointHighlights==TRUE) {
        #if(type=="DP") {
        #  plotColors$lines<-plotColors$lines[1]
        #}
        myLevels<-length(levels(factor(by[,3])))
      } else {
        myLevels<-length(levels(factor(by[,2])))
      }
      byLevel<-length(levels(by[,2]))
      byFactor<-length(levels(by[,1]))
      cFilter<-purrr::map(levels(by[,1]), function(n) purrr::map_lgl(levels(by[,2]), function(y) length(x[by[,1]==n & by[,2]==y])>0)) %>% purrr::reduce(c)
    } else {
      byLevel<-length(levels(by))
      myLevels<-length(levels(by))
      byFactor<-1
      cFilter<-purrr::map_lgl(levels(by), function(y) length(x[by==y])>0)
    }
    if(length(plotColors$fill)>1 & defaultFill==FALSE){
      if(stack==TRUE){
        cFilterStack<-purrr::map(levels(by[,1]), function(n) purrr::map_lgl(levels(by[,3]), function(y) length(x[by[,1]==n & by[,3]==y])>0)) %>% purrr::reduce(c)
        if(length(plotColors$fill)<length(levels(by[,3]))) {
          warning("Not enough fill colors specified to uniquely cover factor levels!")
          plotColors$fill<-rep(plotColors$fill,length(plotColors$fill) %% length(levels(by[,3])) +1)
        }
        plotColors$fill<-rep(plotColors$fill[1:length(levels(by[,3]))],byFactor)[cFilterStack]
      } else {
        if(length(plotColors$fill)<byLevel) {
          warning("Not enough fill colors specified to uniquely cover factor levels!")
          plotColors$fill<-rep(plotColors$fill,length(plotColors$fill) %% byLevel +1)
        }
        plotColors$fill<-rep(plotColors$fill[1:byLevel],byFactor)[cFilter]
      }
    }
    if(length(plotColors$lines)>1 & defaultLines==FALSE){
      if(length(plotColors$lines)<byLevel) {
        warning("Not enough line colors specified to uniquely cover factor levels!")
        plotColors$lines<-rep(plotColors$lines,length(plotColors$lines) %% byLevel +1)
      }
      plotColors$lines<-rep(plotColors$lines[1:byLevel],byFactor)[cFilter]
    }
    if(length(plotColors$vioBoxFill)>1 & defaultFill==FALSE){
      if(length(plotColors$vioBoxFill)<byLevel) {
        warning("Not enough vioBox fill colors specified to uniquely cover factor levels!")
        plotColors$vioBoxFill<-rep(plotColors$vioBoxFill,length(plotColors$vioBoxFill) %% byLevel +1)
      }
      plotColors$vioBoxFill<-rep(plotColors$vioBoxFill[1:byLevel],byFactor)[cFilter]
    }
    if(length(plotColors$vioBoxLineCol)>1 & defaultLines==FALSE){
      if(length(plotColors$vioBoxLineCol)<byLevel) {
        warning("Not enough vioBox line colors specified to uniquely cover factor levels!")
        plotColors$vioBoxLineCol<-rep(plotColors$vioBoxLineCol,length(plotColors$vioBoxLineCol) %% byLevel +1)
      }
      plotColors$vioBoxLineCol<-rep(plotColors$vioBoxLineCol[1:byLevel],byFactor)[cFilter]
    }
  } else if(is.data.frame(by)){
    myCol<-1
    if(pointHighlights==TRUE | stack==TRUE) {
      #Not sure this is necessary
      #if(type=="DP") {
      #  plotColors$lines<-plotColors$lines[1]
      #}
      myLevels<-length(levels(by[,2]))
      if(stack==TRUE & type=="Bar") {
        if (subGroup==TRUE) {
          myCol<-3
        } else {
          myCol<-2
        }
      }
    } else {
      myLevels<-length(levels(by[,1]))
    }
    cFilter<-purrr::map_lgl(levels(by[,myCol]), function(n) length(x[by[,myCol]==n])>0)
    if(length(plotColors$fill)>1 & defaultFill==FALSE){
      if(length(plotColors$fill)<length(levels(by[,myCol]))) {
        warning("Not enough fill colors specified to uniquely cover factor levels!")
        plotColors$fill<-rep(plotColors$fill,length(plotColors$fill) %% length(levels(by[,myCol])) +1)
      }
      plotColors$fill<-plotColors$fill[1:length(levels(by[,myCol]))][cFilter]
    }
    if(length(plotColors$lines)>1 & defaultLines==FALSE){
      if(length(plotColors$lines)<length(levels(by[,1]))) {
        warning("Not enough line colors specified to uniquely cover factor levels!")
        plotColors$lines<-rep(plotColors$lines,length(plotColors$lines) %% length(levels(by[,1])) +1)
      }
      plotColors$lines<-plotColors$lines[1:length(levels(by[,1]))][cFilter]
    }
    if(length(plotColors$vioBoxFill)>1 & defaultFill==FALSE){
      if(length(plotColors$vioBoxFill)<length(levels(by[,1]))) {
        warning("Not enough vioBox fill colors specified to uniquely cover factor levels!")
        plotColors$vioBoxFill<-rep(plotColors$vioBoxFill,length(plotColors$vioBoxFill) %% length(levels(by[,1])) +1)
      }
      plotColors$vioBoxFill<-plotColors$vioBoxFill[1:length(levels(by[,1]))][cFilter]
    }
    if(length(plotColors$vioBoxLineCol)>1 & defaultLines==FALSE){
      if(length(plotColors$vioBoxLineCol)<length(levels(by[,1]))) {
        warning("Not enough vioBox line colors specified to uniquely cover factor levels!")
        plotColors$vioBoxLineCol<-rep(plotColors$vioBoxLineCol,length(plotColors$vioBoxLineCol) %% length(levels(by[,1])) +1)
      }
      plotColors$vioBoxLineCol<-plotColors$vioBoxLineCol[1:length(levels(by[,1]))][cFilter]
    }
  } else {
    myLevels<-length(levels(by))
    cFilter<-purrr::map_lgl(levels(by), function(n) length(x[by==n])>0)
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
    if(length(plotColors$vioBoxFill)>1 & defaultFill==FALSE){
      if(length(plotColors$vioBoxFill)<myLevels) {
        warning("Not enough line colors specified to uniquely cover factor levels!")
        plotColors$vioBoxFill<-rep(plotColors$vioBoxFill,length(plotColors$vioBoxFill) %% myLevels +1)
      }
      plotColors$vioBoxFill<-plotColors$vioBoxFill[1:myLevels][cFilter]
    }
    if(length(plotColors$vioBoxLineCol)>1 & defaultLines==FALSE){
      if(length(plotColors$vioBoxLineCol)<myLevels) {
        warning("Not enough line colors specified to uniquely cover factor levels!")
        plotColors$vioBoxLineCol<-rep(plotColors$vioBoxLineCol,length(plotColors$vioBoxLineCol) %% myLevels +1)
      }
      plotColors$vioBoxLineCol<-plotColors$vioBoxLineCol[1:myLevels][cFilter]
    }
  }
  #If left blank by the user, colors and shapes are adjusted so that the repeat based on factor levels
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
  list(groupNames=groupNames,minorTick=minorTick,pointShape=pointShape,whiskerLineType=whiskerLineType,lWidth=lWidth,capWidth=capWidth,pointLaneWidth=pointLaneWidth,width=width,guides=guides,pointSize=pointSize,subGroup=subGroup,stack=stack,pointHighlights=pointHighlights,theme=theme,plotColors=plotColors,pointMethod=pointMethod,swarmOverflow=swarmOverflow,errorCap=errorCap)
}

