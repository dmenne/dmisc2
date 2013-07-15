#' @title Pairs plot for mixed data sets
#' 
#' @description A variant of the pairs plot to create a 
#' scatterplot matrix, allowing for categorical variables.
#' @param df dataframe with numericals and factor data
#' @param abbr allow abbreviated labels
#' @param abbr.len lenght of abbreviated labels
#' @export pairsdf
#' @author Biostattmat, \email{http://biostatmatt.com/archives/2398}
#' @examples
#' require(MASS)
#' pairsdf(coop)
#' pairsdf(farms)

pairsdf <- function(df, abbr = TRUE, abbr.len = 4) {
  par(mfrow = rep(length(df), 2))
  for (row in 1:length(df)) {
    xr <- df[[row]]
    if (is.character(xr) || is.logical(xr)) 
      xr <- as.factor(xr)
    if (is.factor(xr) && abbr) 
      levels(xr) <- abbreviate(levels(xr), 4)
    for (col in 1:length(df)) {
      xc <- df[[col]]
      if (is.character(xc) || is.logical(xc)) 
        xc <- as.factor(xc)
      if (inherits(xc, "factor") && abbr) 
        levels(xc) <- abbreviate(levels(xc), 4)
      cnm <- names(df)[col]
      rnm <- names(df)[row]
      if (col == row) {
        plot(c(0, 1), c(0, 1), type = "n", xaxt = "n", 
             yaxt = "n", bty = "n", xlab = "", ylab = "", 
             main = "")
        text(x = 0.5, y = 0.5, labels = cnm, adj = c(0.5, 
                                                     0.5), cex = 2)
      }
      else {
        iscf <- is.factor(xc)
        iscn <- is.numeric(xc)
        isrf <- is.factor(xr)
        isrn <- is.numeric(xr)
        if (isrf && iscf) {
          mosaicplot(table(xc, xr), xlab = cnm, ylab = rnm, 
                     main = "", las = 2, color = TRUE, cex = 1.1)
        }
        else if (isrn && iscn) {
          plot(xc, xr, xlab = cnm, ylab = rnm, main = "", 
               las = 2, cex = 1.1)
        }
        else if (isrn && iscf) {
          boxplot(xr ~ xc, xlab = cnm, ylab = rnm, main = "", 
                  las = 2, cex = 1.1)
        }
        else if (isrf && iscn) {
          boxplot(xc ~ factor(xr, levels = rev(levels(xr))), 
                  xlab = cnm, ylab = rnm, main = "", las = 2, 
                  cex = 1.1, horizontal = TRUE)
        }
        else stop("urecognized variable type")
      }
    }
  }
}
