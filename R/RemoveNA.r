#' @title Removes NA columns or rows in a data frame
#' @description
#' RemoveNARows is different from na.omit, because it removes only rows that
#' are completely NA
#' 
#' @aliases RemoveNAColumns RemoveNARows
#' @param x a data frame
#' @return A data frame with removed columns or rows that are all-NA removed
#' @export RemoveNAColumns
#' @export RemoveNARows
#' @author Dieter Menne, \email{dieter.menne@@menne-biomed.de}
#' @keywords misc

RemoveNAColumns = function(x) {
  x[,!unlist(lapply(x,function(y) all(is.na(y)|str_trim(y)=="" )))]
} 

RemoveNARows = function(x) {
  x[!unlist(apply(x,1,function(y) all(is.na(y)|str_trim(y)=="" ))),]
} 
