#' @title Remove variables with wildcard
#' 
#' @description Same as \code{rm}, but takes the first characters of the variables to be
#' removed as parameter.
#' 
#' 
#' @param \dots items, quoted or unquoted; will remove variables with names
#' starting with elements in this list.
#' @export rmlike
#' @author Dieter Menne
#' @seealso \code{\link[base]{rm}}
#' @keywords misc
#' @examples
#' 
#' we = 1:10
#' ab = 1:3
#' ad = 1:5
#' rmlike(we,ab)
#' ls()
#' rmlike(a)
#' ls()
#' 
"rmlike" <- function(...) {
  names <- sapply(
    match.call(expand.dots = FALSE)$..., as.character)
  names = paste(names,collapse="|")
  Vars <- ls(1)
  r <- Vars[grep(paste("^(",names,").*",sep=""),Vars)]
  rm(list=r,pos=1)
}
