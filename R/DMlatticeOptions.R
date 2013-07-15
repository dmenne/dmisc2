#' @title Dieter's default lattice options
#' 
#' @description Set default lattice options, e.g. for use by Sweave.
#' 
#' 
#' @param superpose.polygon.col a palette for polygon colors, e.g.
#' \code{brewer.pal(5,"Set1")}
#' @param superpose.line.col a palette for line colors, e.g.
#' \code{brewer.pal(8,"Spectral")}
#' @param par.strip.text.cex text size of strips
#' @param transparent.strip If true, make panel strips transparent
#' @export DMlatticeOptions
#' @author Dieter Menne, \email{dieter.menne@@menne-biomed.de}

"DMlatticeOptions" <- function(superpose.polygon.col=NULL,
                              superpose.line.col=NULL,
                              par.strip.text.cex=NULL,
                              transparent.strip=FALSE) {
  ltheme = canonical.theme(color=TRUE)
  
  if (!is.null(superpose.line.col))
    ltheme$superpose.line$col =  superpose.line.col else
    ltheme$superpose.line$col = # omit yellow
      c(brewer.pal(7,"Set1")[-6],"gray50", "gray80")
#  ltheme$superpose.line$col =  superpose.line.col
  ltheme$superpose.fill$col = ltheme$superpose.line$col
  if (!is.null(superpose.polygon.col)) {
    ltheme$superpose.polygon$col =  superpose.polygon.col
    ltheme$superpose.fill$col =  superpose.polygon.col
    ltheme$superpose.symbol$fill = superpose.polygon.col # used in panel.polygon
  }
  ltheme$strip.shingle$col = ltheme$superpose.polygon$col

  ltheme$superpose.symbol$pch = c(16,17,18,15,1,2,3,4)
  if (!is.null(superpose.line.col))
    ltheme$superpose.symbol$col = superpose.line.col else
    ltheme$superpose.symbol$col = ltheme$superpose.line$col
  ltheme$superpose.symbol$cex = 0.4
  if (transparent.strip)
    ltheme$strip.background = list(col = c("transparent", "transparent"))
  else
    ltheme$strip.background$col = c("gray90", "gray80")
  ltheme$background$col = "transparent"
  ltheme$par.main.text$cex = 0.9 # default is 1.2
  ltheme$par.ylab.text$cex =0.8
  ltheme$par.xlab.text$cex =0.8
  ltheme$add.text$cex = 1
  ltheme$axis.text$cex = 0.6
  ltheme$box.rectangle$col = "black"
  ltheme$box.umbrella$col = "black"
  ltheme$dot.symbol$col = "black"

  ltheme$plot.symbol$col = "black"
  ltheme$plot.line$col = "black"
  ltheme$plot.symbol$cex = 0.3
  ltheme$plot.symbol$pch = c(16,17,18,15,1,2,3,4)

  ltheme$plot.polygon$col = "#A6D96A"
  if (!is.null(par.strip.text.cex))
    ltheme$par.strip.text$cex = par.strip.text.cex
  ltheme$par.sub.text$cex=0.7
  ltheme$par.sub.text$font=1
  ltheme$layout.heights$top.padding = 0
  ltheme$layout.heights$bottom.padding = 0
  ltheme$layout.width$left.padding = 0
  ltheme$layout.width$right.padding = 0

  lattice.options(default.theme=ltheme)
}  

#library(lattice)
#library(latticeExtra)
#display.brewer.all(8)
#DMlatticeOptions(brewer.pal(4,"Set1"),brewer.pal(6,"Set1"))
#DMlatticeOptions(NULL,brewer.pal(6,"Set1"))
#DMlatticeOptions()
#DMlatticeOptions(transparent.strip=TRUE)
#show.settings()
