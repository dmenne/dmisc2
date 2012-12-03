#' @title lattice panel layout for multipage consistency
#' 
#' @description Gabor Grothendiek's solution in 
#' \url{http://stackoverflow.com/questions/9654244/multipage-lattice-panel-arrangement}
#' 
#' @param layout the layout argument in high level functions, suitably standardized.
#' @param condlevels a list of levels of conditioning variables, after relevant permutations and/or reordering of levels
#' @param page location of panel on page
#' @param row  location of panel in row
#' @param column the location of the panel in the coordinate system columns
#' @param skip the skip argument in high level functions
#' @author Dieter Menne, \email{dieter.menne@@menne-biomed.de}
#' @seealso \code{\link[lattice]{packet.panel.default}}
#' @examples
#' library(lattice)
#' library(latticeExtra)
#' d <- expand.grid(f1 = as.factor(letters[1:8]),
#'                  f2 = as.factor(LETTERS[1:3]),
#'                  x  = 0:10)
#' d$y <- rnorm(nrow(d))
#' p <- xyplot(y~x|f1+f2, data=d, cex=0.5, pch=16)
#' p <- useOuterStrips(p)
#' p <- update(p, layout=c(5,3)) # plotting from here on gives a warning
#' suppressWarnings(plot(p, packet.panel=packet.panel.bycolumn))

#' @export
packet.panel.bycolumn <- 
  function (layout, condlevels, page, row, column, skip) { 
    x <- c(layout[1] * (page - 1) + column, row); 
    if (x[1] <= length(condlevels[[1]])) 
      x
}

packet.panel.bycolumnAlternate <- 
  function (layout, condlevels, page, row, column, skip) {
  dims <- sapply(condlevels, length)
  if(layout[2] != dims[2]) {
    stop("rows in layout must be equal to rows of second conditioning variable")
  }
  panels.per.row <- layout[1]
  panels.per.column <- layout[2]
  total.columns <- dims[1]
  panels.needed <- total.columns * panels.per.column
  panels.per.page <- layout[1] * layout[2]
  pages.needed <- ceiling(panels.needed / panels.per.page)
  empty.columns <- (panels.per.row - total.columns) %% panels.per.row
  panel.matrix <- rbind(matrix(1:panels.needed,ncol=panels.per.column),
                        matrix(NA, nrow=empty.columns, ncol=panels.per.column))
  panel.order <- as.vector(aperm(array(panel.matrix,
                                       dim=c(panels.per.row, pages.needed, panels.per.column)),
                                 c(1,3,2)))
  packet.order <- do.call(expand.grid, condlevels)[panel.order,]
  panel.number <- 1 + (page - 1) * panels.per.page + (row - 1) * panels.per.row + (column - 1)
  out <- as.numeric(packet.order[panel.number, ])
  if (any(is.na(out))) out <- NULL
  out
}
