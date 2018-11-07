#Themes
basicTheme<- list(
  guides=TRUE, #Logical; draws guide lines at major ticks
  minorTick=FALSE, #Numeric; number of minor tick marks to draw between major marks. Set to FALSE to disable
  minorTickLS=4, #Numeric; number of minor tick marks to draw between major marks if logScale is active. Set to FALSE to disable
  pointSizeBP=.7, #cex-like Numeric; size of points in overlay for boxplots
  pointSizeVP=.6, #cex-like Numeric; size of points in overlay for violin plots
  pointSizeDP=.5, #cex-like Numeric; size of points in overlay for dot plots
  widthBP=1, #Relative box width of each category for box plots
  widthVP=1, #Relative violin width of each category for box plots
  widthDP=1, #Relative category width of each category for box plots
  widthBar=1, #Relative bar width of each category for box plots
  pointShapeBP=16, #Numeric vector; point shapes for box plots
  pointShapeVP=16, #Numeric vector; point shapes for vioin plots
  pointShapeDP=16, #Numeric vector; point shapes for dot plots
  pointLaneWidthBP=.7, #Restricts the point overlay to a fraction of the box width
  pointLaneWidthDP=.7, #Restricts the point overlay to a fraction of the category width
  pointLaneWidthVP=.7, #Restricts the point overlay to a fraction of the violin width
  pointMethodBP="jitter", #Point drawing method for box plots
  pointMethodVP="beeswarm", #Point drawing method for violin plots
  pointMethodDP="distribution", #Point drawing method for dot plots
  lWidthBP=1, #Line width (lwd) for box plots
  lWidthDP=1, #Line width (lwd) for dot plots
  lWidthVP=1, #Line width (lwd) for violin plots
  lWidthBar=1, #Line width (lwd) for violin plots
  medianMarkerShape=16, #Numeric; pch shape for median marker for violin plots
  plotColors=formatPlotColors(list(
    bg="open", #Plot area background
    marginBg="transparent", #plot margin background
    guides="lightgrey", #Color of guide lines
    minorGuides="lightgrey", #Color of minor guide lines
    lines="darkred", #Line color(s)
    points=sapply(brewer.pal(9,"Set1"),function(x) setAlpha(x,.5)), #Point color(s)
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

#strwidth("hello")

#legend("topright", inset=c(-0.2,0), legend=c("A","B"), pch=c(1,3), title="Group")

#legend(x=1,y=1.7,legend=LETTERS[1:5],col=unique(cols),pch=16,bty="n",xpd=NA)

#w <- grconvertX(l$rect$w, to='ndc') - grconvertX(0, to='ndc')
#par(omd=c(0, 1-w, 0, 1))
#plot(1:3, rnorm(3), pch=1, lty=1, type="o", ylim=c(-2, 2))
#lines(1:3, rnorm(3), pch=2, lty=2, type="o")
#legend(par('usr')[2], par('usr')[4], bty='n', xpd=NA,
#       c("group A", "group B"), pch=c(1, 2), lty=c(1, 2))
