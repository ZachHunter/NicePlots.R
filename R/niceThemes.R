#Themes
#' @include np_options_processing.R np_utility.R
#' @title NicePlots Theme: Basic
#' @description This is the default theme for nicePlots
#' @details This default theme has uses transparent solid circles for point overlays with up to 8 colors.
#' Fill and line options as constant.
#' @export
basicTheme<- list(
  #General Plot Settings
  fontFamily="sans", #Possible values: 'sans', 'mono', or 'serif'
  groupLabSize=1, #Label cex for the primary group labels
  subGroupLabSize=.68, #Label cex for subgroup labels
  groupLabelSpacing=.96, #distance of group labels from axis in lines
  subgroupLabelSpacing=.26, #distance of subgroup labels from axis in lines
  yAxisLabSize=.9, #Label cex for y-axis tick labels
  axisLabelSize=1, #overall axis label cex (ei, not the tick mark labels)
  titleSize=1.2, #Plot title cex
  subSize=1, #Sub label cex
  guides=TRUE, #Logical; draws guide lines at major ticks
  minorTick=FALSE, #Numeric; number of minor tick marks to draw between major marks. Set to FALSE to disable
  minorTickLS=4, #Numeric; number of minor tick marks to draw between major marks if logScale is active. Set to FALSE to disable
  swarmOverflow="wrap", #Valid options are: "none", "wrap", "gutter", "random", and "omit". Controls how to wantly point stacks that would overflow the pointLaneWidth option.
  curvePoints=200, #Number of points to sample for drawing density curves

  #Legend Settings
  LegendBorder=NULL, #Color of the border box around legend. Set to NULL to turn off
  LegendLineCol=NA, #Border color for the legend color boxes
  LegendBG=NA, #Legend background color
  LegendSize=.66, #overall legend cex
  LegendSpacing=.2, #Relative spacing padding legend entries

  #Plot Specific Defaults
  pointSizeBP=.7, #cex-like Numeric; size of points in overlay for boxplots
  pointSizeVP=.6, #cex-like Numeric; size of points in overlay for violin plots
  pointSizeDP=.5, #cex-like Numeric; size of points in overlay for dot plots
  widthBP=.85, #Relative box width of each category for box plots
  widthVP=.85, #Relative violin width of each category for violin plots
  widthDP=.45, #Relative category width of each category for dot plots
  widthBar=1, #Relative bar width of each category for bar plots
  pointShapeBP=16, #Numeric vector; point shapes for box plots
  pointShapeVP=16, #Numeric vector; point shapes for vioin plots
  pointShapeDP=1, #Numeric vector; point shapes for dot plots
  pointLaneWidthBP=.4, #Restricts the point overlay to a fraction of the box width
  pointLaneWidthDP=4, #Restricts the point overlay to a fraction of the category width
  pointLaneWidthVP=1, #Restricts the point overlay to a fraction of the violin width
  pointMethodBP="jitter", #Point drawing method for box plots
  pointMethodVP="beeswarm", #Point drawing method for violin plots
  pointMethodDP="distribution", #Point drawing method for dot plots
  lWidthBP=1, #Line width (lwd) for box plots
  lWidthDP=1, #Line width (lwd) for dot plots
  lWidthVP=1, #Line width (lwd) for violin plots
  lWidthBar=1, #Line width (lwd) for violin plots
  errorBarLineTypeBP=2, #Line type (lty) for boxplot wiskers
  errorBarLineTypeBar=1, #Line type (lty) for bar plot error bars
  errorBarLineTypeDP=1, #Line type (lty) for dot plot error bars
  errorBarLineTypeVP=1, #Whisker line type (lty) for box plot overlay in violin plots
  errorBarCapWidthBP=.25, #relative width of cap on box plot wiskers
  errorBarCapWidthBar=.33, #relative width of cap on bar plot error bars
  errorBarCapWidthDP=.25, #relative width of cap on dot plot error bars
  errorBarCapWidthVP=0, #Whisker cap width for box plot overlay in violin plots
  errorCapType="ball", #Error bar cap type for bar plots.
  vioBoxWidth=.25, #Factor by which the box plot width should shrick relative to the violins (note: this should be inverted to be more intuitive)

  #PlotColor Options
  plotColors=formatPlotColors(list(
    bg="open", #Plot area background
    marginBg="transparent", #plot margin background
    guides="lightgrey", #Color of guide lines
    minorGuides="lightgrey", #Color of minor guide lines
    lines="darkred", #Line color(s)
    points=purrr::map_chr(RColorBrewer::brewer.pal(9,"Set1"),function(x) setAlpha(x,.5)), #Point color(s)
    fill=setAlpha("grey",.4), #Object fill color(s) (eg box/violin/bar interiors)
    axis="black", #Axis color
    majorTick="black", #Major tick mark color
    minorTick="black", #Minor tick mark color
    title="black", #Title color
    numbers="black", #Color of y-axis numbers
    subtext="black", #Color of sub text
    labels="black", #label color
    subGroupLabels="black", #color of subgroups
    vioBoxFill=setAlpha("black",.8), #Color of interquartile box for violin plots
    vioBoxLineCol="black" #Line color for boxplot overlay in violin plots
  ))
)
class(basicTheme)<-c("npTheme","list")

#' @title NicePlots Theme: Colors
#' @description A more colorful and sparse option compared to the default
#' @details This theme uses fill, line, and point colors. Not a good option for \code{pointHighlights}
#' @export
npColorTheme<- list(
  #General Plot Settings
  fontFamily="serif", #Possible values: 'sans', 'mono', or 'serif'
  groupLabSize=1, #Label cex for the primary group labels
  subGroupLabSize=.75, #Label cex for subgroup labels
  groupLabelSpacing=.96, #distance of group labels from axis in lines
  subgroupLabelSpacing=.26, #distance of subgroup labels from axis in lines
  yAxisLabSize=.95, #Label cex for y-axis tick labels
  axisLabelSize=1, #overall axis label cex (ei, not the tick mark labels)
  titleSize=1.2, #Plot title cex
  subSize=1, #Sub label cex
  guides=FALSE, #Logical; draws guide lines at major ticks
  minorTick=FALSE, #Numeric; number of minor tick marks to draw between major marks. Set to FALSE to disable
  minorTickLS=FALSE, #Numeric; number of minor tick marks to draw between major marks if logScale is active. Set to FALSE to disable
  swarmOverflow="random", #Valid options are: "none", "wrap", "gutter", "random", and "omit". Controls how to wantly point stacks that would overflow the pointLaneWidth option.
  curvePoints=200, #Number of points to sample for drawing density curves

  #Legend Settings
  LegendBorder=NULL, #Color of the border box around legend. Set to NULL to turn off
  LegendLineCol=NA, #Border color for the legend color boxes
  LegendBG=NA, #Legend background color
  LegendSize=.85, #overall legend cex
  LegendSpacing=.3, #Relative spacing padding legend entries

  #Plot Specific Defaults
  pointSizeBP=.7, #cex-like Numeric; size of points in overlay for boxplots
  pointSizeVP=.6, #cex-like Numeric; size of points in overlay for violin plots
  pointSizeDP=.5, #cex-like Numeric; size of points in overlay for dot plots
  widthBP=.8, #Relative box width of each category for box plots
  widthVP=.8, #Relative violin width of each category for violin plots
  widthDP=.75, #Relative category width of each category for dot plots
  widthBar=.9, #Relative bar width of each category for bar plots
  pointShapeBP=1:10, #Numeric vector; point shapes for box plots
  pointShapeVP=1:10, #Numeric vector; point shapes for vioin plots
  pointShapeDP=1:10, #Numeric vector; point shapes for dot plots
  pointLaneWidthBP=.65, #Restricts the point overlay to a fraction of the box width
  pointLaneWidthDP=4, #Restricts the point overlay to a fraction of the category width
  pointLaneWidthVP=2.9, #Restricts the point overlay to a fraction of the violin width
  pointMethodBP="jitter", #Point drawing method for box plots
  pointMethodVP="beeswarm", #Point drawing method for violin plots
  pointMethodDP="distribution", #Point drawing method for dot plots
  lWidthBP=1, #Line width (lwd) for box plots
  lWidthDP=3, #Line width (lwd) for dot plots
  lWidthVP=1, #Line width (lwd) for violin plots
  lWidthBar=1.5, #Line width (lwd) for violin plots
  errorBarLineTypeBP=2, #Line type (lty) for boxplot wiskers
  errorBarLineTypeBar=1, #Line type (lty) for bar plot error bars
  errorBarLineTypeVP=2, #Whisker line type (lty) for box plot overlay in violin plots
  errorBarLineTypeVP=2,  #Line type (lty) for dot plot error bars
  errorBarCapWidthBP=.25, #relative width of cap on box plot wiskers
  errorBarCapWidthDP=.25, #relative width of cap on dot plots
  errorBarCapWidthBar=1, #relative width of cap on bar plot error bars
  errorBarCapWidthVP=.25, #Whisker cap width for box plot overlay in violin plots
  errorCapType="bar", #Error bar cap type for bar plots.
  vioBoxWidth=.5, #Factor by which the box plot width should shrick relative to the violins (note: this should be inverted to be more intuitive)

  #PlotColor Options
  plotColors=formatPlotColors(list(
    bg="open", #Plot area background
    marginBg="transparent", #plot margin background
    guides="lightgrey", #Color of guide lines
    minorGuides="lightgrey", #Color of minor guide lines
    lines=RColorBrewer::brewer.pal(9,"Set1"), #Line color(s)
    points=RColorBrewer::brewer.pal(9,"Set1"), #Point color(s)
    fill=purrr::map_chr(RColorBrewer::brewer.pal(9,"Set1"),function(x) setAlpha(x,.3)), #Object fill color(s) (eg box/violin/bar interiors)
    axis="black", #Axis color
    majorTick="black", #Major tick mark color
    minorTick="black", #Minor tick mark color
    title="black", #Title color
    numbers="black", #Color of y-axis numbers
    subtext="black", #Color of sub text
    labels="black", #label color
    subGroupLabels="black", #color of subgroups
    vioBoxFill=purrr::map_chr(RColorBrewer::brewer.pal(9,"Set1"),function(x) setAlpha(x,.3)), #Color of interquartile box for violin plots
    vioBoxLineCol=RColorBrewer::brewer.pal(9,"Set1") #Line color for boxplot overlay in violin plots
  ))
)
class(npColorTheme)<-c("npTheme","list")

#' @title NicePlots Theme: Stata Like
#' @description Looks a bit like stata plots
#' @details Todo
#' @export
npStataTheme<- list(
  #General Plot Settings
  fontFamily="sans", #Possible values: 'sans', 'mono', or 'serif'
  groupLabSize=1, #Label cex for the primary group labels
  subGroupLabSize=.66, #Label cex for subgroup labels
  groupLabelSpacing=.96, #distance of group labels from axis in lines
  subgroupLabelSpacing=.3, #distance of subgroup labels from axis in lines
  yAxisLabSize=.9, #Label cex for y-axis tick labels
  axisLabelSize=1, #overall axis label cex (ei, not the tick mark labels)
  titleSize=1.2, #Plot title cex
  subSize=1, #Sub label cex
  guides=FALSE, #Logical; draws guide lines at major ticks
  minorTick=FALSE, #Numeric; number of minor tick marks to draw between major marks. Set to FALSE to disable
  minorTickLS=FALSE, #Numeric; number of minor tick marks to draw between major marks if logScale is active. Set to FALSE to disable
  swarmOverflow="gutter", #Valid options are: "none", "wrap", "gutter", "random", and "omit". Controls how to wantly point stacks that would overflow the pointLaneWidth option.
  curvePoints=200, #Number of points to sample for drawing density curves

  #Legend Settings
  LegendBorder="black", #Color of the border box around legend. Set to NULL to turn off
  LegendLineCol=NA, #Border color for the legend color boxes
  LegendBG="white", #Legend background color
  LegendSize=.75, #overall legend cex
  LegendSpacing=.3, #Relative spacing padding legend entries

  #Plot Specific Defaults
  pointSizeBP=.6, #cex-like Numeric; size of points in overlay for boxplots
  pointSizeVP=.6, #cex-like Numeric; size of points in overlay for violin plots
  pointSizeDP=.5, #cex-like Numeric; size of points in overlay for dot plots
  widthBP=.05, #Relative box width of each category for box plots
  widthVP=.85, #Relative violin width of each category for violin plots
  widthDP=.5, #Relative category width of each category for dot plots
  widthBar=1, #Relative bar width of each category for bar plots
  pointShapeBP=1:10, #Numeric vector; point shapes for box plots
  pointShapeVP=1:10, #Numeric vector; point shapes for vioin plots
  pointShapeDP=1:10, #Numeric vector; point shapes for dot plots
  pointLaneWidthBP=1, #Restricts the point overlay to a fraction of the box width
  pointLaneWidthDP=.95, #Restricts the point overlay to a fraction of the category width
  pointLaneWidthVP=5, #Restricts the point overlay to a fraction of the violin width
  pointMethodBP="beeswarm", #Point drawing method for box plots
  pointMethodVP="beeswarm", #Point drawing method for violin plots
  pointMethodDP="distribution", #Point drawing method for dot plots
  lWidthBP=2.5, #Line width (lwd) for box plots
  lWidthDP=3, #Line width (lwd) for dot plots
  lWidthVP=1, #Line width (lwd) for violin plots
  lWidthBar=1.5, #Line width (lwd) for violin plots
  errorBarLineTypeBP=1, #Line type (lty) for boxplot wiskers
  errorBarLineTypeBar=1, #Line type (lty) for bar plot error bars
  errorBarLineTypeDP=1, #Line type (lty) for dot plot error bars
  errorBarLineTypeVP=1, #Whisker line type (lty) for box plot overlay in violin plots
  errorBarCapWidthBP=10, #relative width of cap on box plot wiskers
  errorBarCapWidthDP=10,  #relative width of cap on dot plot
  errorBarCapWidthBar=1, #relative width of cap on bar plot error bars
  errorBarCapWidthVP=0, #Whisker cap width for box plot overlay in violin plots
  errorCapType="none", #Error bar cap type for bar plots.
  vioBoxWidth=.25, #Factor by which the box plot width should shrick relative to the violins (note: this should be inverted to be more intuitive)

  #PlotColor Options
  plotColors=formatPlotColors(list(
    bg="white", #Plot area background
    marginBg="lightGrey", #plot margin background
    guides="lightgrey", #Color of guide lines
    minorGuides="lightgrey", #Color of minor guide lines
    lines="black", #Line color(s)
    points=purrr::map_chr(RColorBrewer::brewer.pal(8,"Dark2"),function(x) setAlpha(x,.5)), #Point color(s)
    fill="white", #Object fill color(s) (eg box/violin/bar interiors)
    axis="black", #Axis color
    majorTick="black", #Major tick mark color
    minorTick="black", #Minor tick mark color
    title="black", #Title color
    numbers="black", #Color of y-axis numbers
    subtext="black", #Color of sub text
    labels="black", #label color
    subGroupLabels="black", #color of subgroups
    vioBoxFill=setAlpha("black",.8), #Color of interquartile box for violin plots
    vioBoxLineCol="black" #Line color for boxplot overlay in violin plots
  ))
)
class(npStataTheme)<-c("npTheme","list")

#' @title NicePlots Theme: ggPlot Like
#' @description Looks a bit like stata plots
#' @details Todo
#' @export
npGGTheme<- list(
  #General Plot Settings
  fontFamily="serif", #Possible values: 'sans', 'mono', or 'serif'
  groupLabSize=1, #Label cex for the primary group labels
  subGroupLabSize=.68, #Label cex for subgroup labels
  groupLabelSpacing=.96, #distance of group labels from axis in lines
  subgroupLabelSpacing=.25, #distance of subgroup labels from axis in lines
  yAxisLabSize=.95, #Label cex for y-axis tick labels
  axisLabelSize=1, #overall axis label cex (ei, not the tick mark labels)
  titleSize=1.2, #Plot title cex
  subSize=1, #Sub label cex
  guides=TRUE, #Logical; draws guide lines at major ticks
  minorTick=FALSE, #Numeric; number of minor tick marks to draw between major marks. Set to FALSE to disable
  minorTickLS=4, #Numeric; number of minor tick marks to draw between major marks if logScale is active. Set to FALSE to disable
  swarmOverflow="random", #Valid options are: "none", "wrap", "gutter", "random", and "omit". Controls how to wantly point stacks that would overflow the pointLaneWidth option.
  curvePoints=200, #Number of points to sample for drawing density curves

  #Legend Settings
  LegendBorder=NULL, #Color of the border box around legend. Set to NULL to turn off
  LegendLineCol="lightgrey", #Border color for the legend color boxes
  LegendBG=NULL, #Legend background color
  LegendSize=.75, #overall legend cex
  LegendSpacing=.3, #Relative spacing padding legend entries

  #Plot Specific Defaults
  pointSizeBP=.6, #cex-like Numeric; size of points in overlay for boxplots
  pointSizeVP=.6, #cex-like Numeric; size of points in overlay for violin plots
  pointSizeDP=.5, #cex-like Numeric; size of points in overlay for dot plots
  widthBP=.85, #Relative box width of each category for box plots
  widthVP=.85, #Relative violin width of each category for violin plots
  widthDP=.9, #Relative category width of each category for dot plots
  widthBar=1, #Relative bar width of each category for bar plots
  pointShapeBP=15:25, #Numeric vector; point shapes for box plots
  pointShapeVP=15:25, #Numeric vector; point shapes for vioin plots
  pointShapeDP=15:25, #Numeric vector; point shapes for dot plots
  pointLaneWidthBP=.7, #Restricts the point overlay to a fraction of the box width
  pointLaneWidthDP=5, #Restricts the point overlay to a fraction of the category width
  pointLaneWidthVP=3, #Restricts the point overlay to a fraction of the violin width
  pointMethodBP="beeswarm", #Point drawing method for box plots
  pointMethodVP="beeswarm", #Point drawing method for violin plots
  pointMethodDP="distribution", #Point drawing method for dot plots
  lWidthBP=1, #Line width (lwd) for box plots
  lWidthDP=3, #Line width (lwd) for dot plots
  lWidthVP=1, #Line width (lwd) for violin plots
  lWidthBar=1.5, #Line width (lwd) for violin plots
  errorBarLineTypeBP=1, #Line type (lty) for boxplot wiskers
  errorBarLineTypeBar=1, #Line type (lty) for bar plot error bars
  errorBarLineTypeDP=1, #Line type (lty) for dot plot error bars
  errorBarLineTypeVP=1, #Whisker line type (lty) for box plot overlay in violin plots
  errorBarCapWidthDP=0, #relative width of error cap on dot plots
  errorBarCapWidthBP=0, #relative width of cap on box plot wiskers
  errorBarCapWidthBar=0, #relative width of cap on bar plot error bars
  errorBarCapWidthVP=0, #Whisker cap width for box plot overlay in violin plots
  errorCapType="none", #Error bar cap type for bar plots.
  vioBoxWidth=.20, #Factor by which the box plot width should shrick relative to the violins (note: this should be inverted to be more intuitive)

  #PlotColor Options
  plotColors=formatPlotColors(list(
    bg="lightgrey", #Plot area background
    marginBg="transparent", #plot margin background
    guides="white", #Color of guide lines
    minorGuides="white", #Color of minor guide lines
    lines=purrr::map_chr(RColorBrewer::brewer.pal(8,"Dark2"),function(x) setAlpha(x,.7)), #Line color(s)
    points=purrr::map_chr(RColorBrewer::brewer.pal(8,"Dark2"),function(x) setAlpha(x,.5)), #Point color(s)
    fill=purrr::map_chr(RColorBrewer::brewer.pal(8,"Dark2"),function(x) setAlpha(x,.2)), #Object fill color(s) (eg box/violin/bar interiors)
    axis="darkgrey", #Axis color
    majorTick="darkgrey", #Major tick mark color
    minorTick="darkgrey", #Minor tick mark color
    title="black", #Title color
    numbers="black", #Color of y-axis numbers
    subtext="black", #Color of sub text
    labels="black", #label color
    subGroupLabels="black", #color of subgroups
    vioBoxFill=setAlpha("black",.8), #Color of interquartile box for violin plots
    vioBoxLineCol="black" #Line color for boxplot overlay in violin plots
  ))
)
class(npGGTheme)<-c("npTheme","list")


#' @title Find Themes
#' @description Find NicePlot Themes Availble in the Namespace
#'
#' @details
#' This function takes the output of \code{\link{search}()} to check for variables that are members of \code{npTheme} \link{class}.
#' These variable names are then returned by the function.
#'
#' @examples
#' npThemes()
#' newBasic<-basicTheme
#' newBasic$plotColors$lines=rainbow(10)
#' npThemes()
#' @import dplyr
#' @importFrom purrr map map_lgl reduce
#' @export
#' @seealso \code{\link{search}}, \code{\link{class}}
npThemes<-function() {
  loadedNames<-map(search(),function(e) ls(name=e)) %>% reduce(.f=c)
  loadedNames[map_lgl(loadedNames,function(n) "npTheme" %in% do.call("class",list(x=as.symbol(n))))]
}


#' @title Creat a NicePlots Theme
#' @description
#' Makes a new \code{NicePlots} theme off of an existing template
#'
#' @details
#' Useing an exisiting template, this function updates all of the options theme options
#' supplied in \code{...}. To see the theme options, print an existing theme to the console.
#' You can see the available themes using the \code{\link{npThemes}} function.
#'
#' @param theme list; A \code{NicePlots} plotColor list from a theme or a list with values corresponding to the plotColor options you wish to update.
#' @param plotColors list; a named list of vectors of colors that set the color options for all NicePlot functions. Names left unspecified will be added and set to default values automatically.
#' @param ... All theme options can be set but as if they were comma separted funcitonal arguments. See example for details.
#'
#' @return A \code{NicePlots} theme of class "npTheme".
#' @examples
#' data(iris)
#' npThemes()
#' myTheme<-newNPTheme(theme=npColorTheme,
#'    plotColors=list(lines=c("darkgrey")),
#'    pointMethodBP="beeswarm")
#' npThemes()
#' niceBox(iris[,1:2],iris$Species,subGroup=TRUE,legend=TRUE,theme=myTheme)
#' @seealso \code{\link{npThemes}}
#' @export
newNPTheme<-function(theme=basicTheme, plotColors=NULL,...){
  if("npTheme" %in% class(theme)){
    newPC<-formatPlotColors(plotColors,theme=theme$plotColors)
    themeUpdates<-list(...)
    if(length(themeUpdates)>=1){
      for(i in 1:length(themeUpdates)){
        if(!is.null(theme[[names(themeUpdates)[i]]]) & class(theme[[names(themeUpdates)[i]]])[1] %in% class(themeUpdates[[i]])) {
          theme[[names(themeUpdates)[i]]]<-themeUpdates[[i]]
        } else if(names(themeUpdates)[i]=="pointSize"){
          theme[grep("pointSize[BV|VP|DP]",names(theme))]<-themeUpdates[[i]]
        } else if(names(themeUpdates)[i]=="width"){
          theme[grep("width[BV|VP|DP|Bar]",names(theme))]<-themeUpdates[[i]]
        } else if(names(themeUpdates)[i]=="pointShape"){
          theme[grep("pointShape[BV|VP|DP]",names(theme))]<-themeUpdates[[i]]
        } else if(names(themeUpdates)[i]=="pointLaneWidth"){
          theme[grep("pointLaneWidth[BV|VP|DP]",names(theme))]<-themeUpdates[[i]]
        } else if(names(themeUpdates)[i]=="pointMethod"){
          theme[grep("pointMethod[BV|VP|DP]",names(theme))]<-themeUpdates[[i]]
        } else if(names(themeUpdates)[i]=="lWidth"){
          theme[grep("lWidth[BV|VP|DP|Bar]",names(theme))]<-themeUpdates[[i]]
        } else if(names(themeUpdates)[i]=="errorBarLineType"){
          theme[grep("errorBarLineType[BV|VP|DP|Bar]",names(theme))]<-themeUpdates[[i]]
        } else if(names(themeUpdates)[i]=="errorBarCapWidth"){
          theme[grep("errorBarCapWidth[BV|VP|DP|Bar]",names(theme))]<-themeUpdates[[i]]
        }
      }
    }
    theme$plotColors<-newPC
    class(theme)<-c("npTheme","list")
  } else {
    stop("The theme argument must be a NicePlots theme of class 'npTheme'", call. = FALSE)
  }
  theme
}
