#Themes

#' @title Nice Plots Theme: Basic
#' @description This is the default theme for nicePlots
#' @details This default theme has uses transparent solid circles for point overlays with up to 8 colors.
#' Fill and line options as constant.
#' @export
basicTheme<- list(
  #General Plot Settings
  fontFamily="sans", #Possible values: 'sans', 'mono', or 'serif'
  groupLabSize=1, #Label cex for the primary group labels
  subGroupLabSize=.6, #Label cex for subgroup labels
  yAxisLabSize=.9, #Label cex for y-axis tick labels
  axisLabelSize=1, #overall axis label cex (ei, not the tick mark labels)
  titleSize=1.2, #Plot title cex
  subSize=1, #Sub label cex
  guides=TRUE, #Logical; draws guide lines at major ticks
  minorTick=FALSE, #Numeric; number of minor tick marks to draw between major marks. Set to FALSE to disable
  minorTickLS=4, #Numeric; number of minor tick marks to draw between major marks if logScale is active. Set to FALSE to disable
  swarmOverflow="wrap", #Valid options are: "none", "wrap", "gutter", "random", and "omit". Controls how to wantly point stacks that would overflow the pointLaneWidth option.

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
  widthBP=1, #Relative box width of each category for box plots
  widthVP=1, #Relative violin width of each category for violin plots
  widthDP=1, #Relative category width of each category for dot plots
  widthBar=1, #Relative bar width of each category for bar plots
  pointShapeBP=16, #Numeric vector; point shapes for box plots
  pointShapeVP=16, #Numeric vector; point shapes for vioin plots
  pointShapeDP=1, #Numeric vector; point shapes for dot plots
  pointLaneWidthBP=.85, #Restricts the point overlay to a fraction of the box width
  pointLaneWidthDP=.85, #Restricts the point overlay to a fraction of the category width
  pointLaneWidthVP=.75, #Restricts the point overlay to a fraction of the violin width
  pointMethodBP="jitter", #Point drawing method for box plots
  pointMethodVP="beeswarm", #Point drawing method for violin plots
  pointMethodDP="distribution", #Point drawing method for dot plots
  lWidthBP=1, #Line width (lwd) for box plots
  lWidthDP=1, #Line width (lwd) for dot plots
  lWidthVP=1, #Line width (lwd) for violin plots
  lWidthBar=1, #Line width (lwd) for violin plots
  errorBarLineTypeBP=2, #Line type (lty) for boxplot wiskers
  errorBarLineTypeBar=1, #Line type (lty) for bar plot error bars
  errorBarCapWidthBP=.25, #relative width of cap on box plot wiskers
  errorBarCapWidthBar=1, #relative width of cap on bar plot error bars
  errorCapType="ball", #Error bar cap type for bar plots.
  medianMarkerShape=16, #Numeric; pch shape for median marker for violin plots

  #PlotColor Options
  plotColors=formatPlotColors(list(
    bg="open", #Plot area background
    marginBg="transparent", #plot margin background
    guides="lightgrey", #Color of guide lines
    minorGuides="lightgrey", #Color of minor guide lines
    lines="darkred", #Line color(s)
    points=map_chr(brewer.pal(9,"Set1"),function(x) setAlpha(x,.5)), #Point color(s)
    fill=setAlpha("grey",.4), #Object fill color(s) (eg box/violin/bar interiors)
    axis="black", #Axis color
    majorTick="black", #Major tick mark color
    minorTick="black", #Minor tick mark color
    labels="black", #label color
    subGroupLabels="black", #color of subgroups
    rectCol=setAlpha("black",.8), #Color of interquartile box for violin plots
    medianMarkerCol=setAlpha("white",.8) #Median marker color for violin plots
  ))
)
class(basicTheme)<-c("npTheme","list")

#' @title Nice Plots Theme: Colors
#' @description A more colorful and sparse option compared to the default
#' @details This theme uses fill, line, and point colors. Not a good option for \code{pointHighlights}
#' @export
npColorTheme<- list(
  #General Plot Settings
  fontFamily="serif", #Possible values: 'sans', 'mono', or 'serif'
  groupLabSize=1, #Label cex for the primary group labels
  subGroupLabSize=.6, #Label cex for subgroup labels
  yAxisLabSize=.9, #Label cex for y-axis tick labels
  axisLabelSize=1, #overall axis label cex (ei, not the tick mark labels)
  titleSize=1.2, #Plot title cex
  subSize=1, #Sub label cex
  guides=FALSE, #Logical; draws guide lines at major ticks
  minorTick=FALSE, #Numeric; number of minor tick marks to draw between major marks. Set to FALSE to disable
  minorTickLS=FALSE, #Numeric; number of minor tick marks to draw between major marks if logScale is active. Set to FALSE to disable
  swarmOverflow="random", #Valid options are: "none", "wrap", "gutter", "random", and "omit". Controls how to wantly point stacks that would overflow the pointLaneWidth option.

  #Legend Settings
  LegendBorder=NULL, #Color of the border box around legend. Set to NULL to turn off
  LegendLineCol=NA, #Border color for the legend color boxes
  LegendBG=NA, #Legend background color
  LegendSize=.75, #overall legend cex
  LegendSpacing=.3, #Relative spacing padding legend entries

  #Plot Specific Defaults
  pointSizeBP=.7, #cex-like Numeric; size of points in overlay for boxplots
  pointSizeVP=.6, #cex-like Numeric; size of points in overlay for violin plots
  pointSizeDP=.5, #cex-like Numeric; size of points in overlay for dot plots
  widthBP=.8, #Relative box width of each category for box plots
  widthVP=1, #Relative violin width of each category for violin plots
  widthDP=1, #Relative category width of each category for dot plots
  widthBar=.9, #Relative bar width of each category for bar plots
  pointShapeBP=1:10, #Numeric vector; point shapes for box plots
  pointShapeVP=1:10, #Numeric vector; point shapes for vioin plots
  pointShapeDP=1:10, #Numeric vector; point shapes for dot plots
  pointLaneWidthBP=.85, #Restricts the point overlay to a fraction of the box width
  pointLaneWidthDP=.85, #Restricts the point overlay to a fraction of the category width
  pointLaneWidthVP=.75, #Restricts the point overlay to a fraction of the violin width
  pointMethodBP="jitter", #Point drawing method for box plots
  pointMethodVP="beeswarm", #Point drawing method for violin plots
  pointMethodDP="distribution", #Point drawing method for dot plots
  lWidthBP=1, #Line width (lwd) for box plots
  lWidthDP=3, #Line width (lwd) for dot plots
  lWidthVP=1, #Line width (lwd) for violin plots
  lWidthBar=1.5, #Line width (lwd) for violin plots
  errorBarLineTypeBP=2, #Line type (lty) for boxplot wiskers
  errorBarLineTypeBar=1, #Line type (lty) for bar plot error bars
  errorBarCapWidthBP=.25, #relative width of cap on box plot wiskers
  errorBarCapWidthBar=1, #relative width of cap on bar plot error bars
  errorCapType="bar", #Error bar cap type for bar plots.
  medianMarkerShape=16, #Numeric; pch shape for median marker for violin plots

  #PlotColor Options
  plotColors=formatPlotColors(list(
    bg="open", #Plot area background
    marginBg="transparent", #plot margin background
    guides="lightgrey", #Color of guide lines
    minorGuides="lightgrey", #Color of minor guide lines
    lines=brewer.pal(9,"Set1"), #Line color(s)
    points=brewer.pal(9,"Set1"), #Point color(s)
    fill=map_chr(brewer.pal(9,"Set1"),function(x) setAlpha(x,.3)), #Object fill color(s) (eg box/violin/bar interiors)
    axis="black", #Axis color
    majorTick="black", #Major tick mark color
    minorTick="black", #Minor tick mark color
    labels="black", #label color
    subGroupLabels="black", #color of subgroups
    rectCol=setAlpha("black",.8), #Color of interquartile box for violin plots
    medianMarkerCol=setAlpha("white",.8) #Median marker color for violin plots
  ))
)
class(npColorTheme)<-c("npTheme","list")

#' @title Nice Plots Theme: Stata Like
#' @description Looks a bit like stata plots
#' @details Todo
#' @export
npStataTheme<- list(
  #General Plot Settings
  fontFamily="sans", #Possible values: 'sans', 'mono', or 'serif'
  groupLabSize=1, #Label cex for the primary group labels
  subGroupLabSize=.6, #Label cex for subgroup labels
  yAxisLabSize=.9, #Label cex for y-axis tick labels
  axisLabelSize=1, #overall axis label cex (ei, not the tick mark labels)
  titleSize=1.2, #Plot title cex
  subSize=1, #Sub label cex
  guides=FALSE, #Logical; draws guide lines at major ticks
  minorTick=FALSE, #Numeric; number of minor tick marks to draw between major marks. Set to FALSE to disable
  minorTickLS=FALSE, #Numeric; number of minor tick marks to draw between major marks if logScale is active. Set to FALSE to disable
  swarmOverflow="gutter", #Valid options are: "none", "wrap", "gutter", "random", and "omit". Controls how to wantly point stacks that would overflow the pointLaneWidth option.

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
  widthVP=1, #Relative violin width of each category for violin plots
  widthDP=1, #Relative category width of each category for dot plots
  widthBar=1, #Relative bar width of each category for bar plots
  pointShapeBP=1:10, #Numeric vector; point shapes for box plots
  pointShapeVP=1:10, #Numeric vector; point shapes for vioin plots
  pointShapeDP=1:10, #Numeric vector; point shapes for dot plots
  pointLaneWidthBP=20, #Restricts the point overlay to a fraction of the box width
  pointLaneWidthDP=.95, #Restricts the point overlay to a fraction of the category width
  pointLaneWidthVP=.75, #Restricts the point overlay to a fraction of the violin width
  pointMethodBP="beeswarm", #Point drawing method for box plots
  pointMethodVP="beeswarm", #Point drawing method for violin plots
  pointMethodDP="distribution", #Point drawing method for dot plots
  lWidthBP=2.5, #Line width (lwd) for box plots
  lWidthDP=3, #Line width (lwd) for dot plots
  lWidthVP=1, #Line width (lwd) for violin plots
  lWidthBar=1.5, #Line width (lwd) for violin plots
  errorBarLineTypeBP=1, #Line type (lty) for boxplot wiskers
  errorBarLineTypeBar=1, #Line type (lty) for bar plot error bars
  errorBarCapWidthBP=10, #relative width of cap on box plot wiskers
  errorBarCapWidthBar=1, #relative width of cap on bar plot error bars
  errorCapType="none", #Error bar cap type for bar plots.
  medianMarkerShape=16, #Numeric; pch shape for median marker for violin plots

  #PlotColor Options
  plotColors=formatPlotColors(list(
    bg="white", #Plot area background
    marginBg="lightGrey", #plot margin background
    guides="lightgrey", #Color of guide lines
    minorGuides="lightgrey", #Color of minor guide lines
    lines="black", #Line color(s)
    points=map_chr(brewer.pal(8,"Dark2"),function(x) setAlpha(x,.5)), #Point color(s)
    fill="white", #Object fill color(s) (eg box/violin/bar interiors)
    axis="black", #Axis color
    majorTick="black", #Major tick mark color
    minorTick="black", #Minor tick mark color
    labels="black", #label color
    subGroupLabels="black", #color of subgroups
    rectCol=setAlpha("black",.8), #Color of interquartile box for violin plots
    medianMarkerCol=setAlpha("white",.8) #Median marker color for violin plots
  ))
)
class(npStataTheme)<-c("npTheme","list")

#' @title Nice Plots Theme: ggPlot Like
#' @description Looks a bit like stata plots
#' @details Todo
#' @export
npGGTheme<- list(
  #General Plot Settings
  fontFamily="serif", #Possible values: 'sans', 'mono', or 'serif'
  groupLabSize=1, #Label cex for the primary group labels
  subGroupLabSize=.6, #Label cex for subgroup labels
  yAxisLabSize=.9, #Label cex for y-axis tick labels
  axisLabelSize=1, #overall axis label cex (ei, not the tick mark labels)
  titleSize=1.2, #Plot title cex
  subSize=1, #Sub label cex
  guides=TRUE, #Logical; draws guide lines at major ticks
  minorTick=FALSE, #Numeric; number of minor tick marks to draw between major marks. Set to FALSE to disable
  minorTickLS=4, #Numeric; number of minor tick marks to draw between major marks if logScale is active. Set to FALSE to disable
  swarmOverflow="random", #Valid options are: "none", "wrap", "gutter", "random", and "omit". Controls how to wantly point stacks that would overflow the pointLaneWidth option.

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
  widthVP=1, #Relative violin width of each category for violin plots
  widthDP=1, #Relative category width of each category for dot plots
  widthBar=1, #Relative bar width of each category for bar plots
  pointShapeBP=15:25, #Numeric vector; point shapes for box plots
  pointShapeVP=15:25, #Numeric vector; point shapes for vioin plots
  pointShapeDP=15:25, #Numeric vector; point shapes for dot plots
  pointLaneWidthBP=.9, #Restricts the point overlay to a fraction of the box width
  pointLaneWidthDP=.95, #Restricts the point overlay to a fraction of the category width
  pointLaneWidthVP=.75, #Restricts the point overlay to a fraction of the violin width
  pointMethodBP="beeswarm", #Point drawing method for box plots
  pointMethodVP="beeswarm", #Point drawing method for violin plots
  pointMethodDP="distribution", #Point drawing method for dot plots
  lWidthBP=1, #Line width (lwd) for box plots
  lWidthDP=3, #Line width (lwd) for dot plots
  lWidthVP=1, #Line width (lwd) for violin plots
  lWidthBar=1.5, #Line width (lwd) for violin plots
  errorBarLineTypeBP=1, #Line type (lty) for boxplot wiskers
  errorBarLineTypeBar=1, #Line type (lty) for bar plot error bars
  errorBarCapWidthBP=0, #relative width of cap on box plot wiskers
  errorBarCapWidthBar=0, #relative width of cap on bar plot error bars
  errorCapType="none", #Error bar cap type for bar plots.
  medianMarkerShape=16, #Numeric; pch shape for median marker for violin plots

  #PlotColor Options
  plotColors=formatPlotColors(list(
    bg="lightgrey", #Plot area background
    marginBg="transparent", #plot margin background
    guides="white", #Color of guide lines
    minorGuides="white", #Color of minor guide lines
    lines=map_chr(brewer.pal(8,"Dark2"),function(x) setAlpha(x,.7)), #Line color(s)
    points=map_chr(brewer.pal(8,"Dark2"),function(x) setAlpha(x,.5)), #Point color(s)
    fill=map_chr(brewer.pal(8,"Dark2"),function(x) setAlpha(x,.2)), #Object fill color(s) (eg box/violin/bar interiors)
    axis="darkgrey", #Axis color
    majorTick="darkgrey", #Major tick mark color
    minorTick="darkgrey", #Minor tick mark color
    labels="black", #label color
    subGroupLabels="black", #color of subgroups
    rectCol=setAlpha("black",.8), #Color of interquartile box for violin plots
    medianMarkerCol=setAlpha("white",.8) #Median marker color for violin plots
  ))
)
class(npStataTheme)<-c("npGGTheme","list")
