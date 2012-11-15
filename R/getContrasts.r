#' @title Read and format contrast tables from Excel
#' 
#' @description Reads contrast from an Excel file for use with \code{estimable} of package
#' gmodels.
#' 
#' @aliases getContrasts readContrasts
#' @param cname named region in Excel file with contrast table
#' @param excelfile path of Excel file with contrast table
#' @param rows optionally only select some rows of the table
#' @return \code{readContrasts} read a contrast table; \code{getContrasts}
#' additionally includes column and row name information to label contrast
#' tables with estimable (gmodels)
#' @author Dieter Menne, \email{dieter.menne@@menne-biomed.de}
#' @keywords models
#' @seealso See \code{constrast.xlsx} in the extdata directory for some
#' rules of thumb on constructing a contrast matrix.
#' @examples
#' library(nlme)
#' library(gmodels)
#' library(RODBC)
#' options(digits=3)
#' set.seed(4711)
#' excelfile = system.file("extdata", "contrasts.xlsx", package = "Dmisc2")
#' d = expand.grid(subject=LETTERS[1:8],
#'                 peri= c("Wine","Tea"),
#'                 post = c("Water","Kirsch"),
#'                 interval = c("Pre","Post"))
#' d$vol = round(rnorm(nrow(d),10,2),1)
#' d.lme = lme(vol~interval+peri+post+peri:interval+interval:post,
#'             data=d,random=~1|subject)
#' summary(d.lme)
#' ct = getContrasts("peripostinterval",excelfile)
#' estimable(d.lme,ct,conf.int=0.95)

#' @export
#' @rdname getContrasts
"getContrasts" = function(cname,excelfile,rows=NULL) {
  cn=readContrasts(cname,excelfile) 
  if (!is.null(rows)) cn = cn[rows,]
  colnames= cn[,1]
  rownames = gsub('#','.',colnames(cn))#[-1]
  # Use _ as placeholder for an empty field
  vars = do.call("rbind", strsplit(rownames,'\\.'))
  vars[vars=='_'] = ''
  # upper left corner must contain the names of the variables
  varnames = vars[1,]
  vars = vars[-1,,drop=FALSE]
  rownames(vars)=rownames[-1]
  colnames(vars)=varnames
  cn = t(as.matrix(cn[,-1]))
  rownames(cn) = rownames[-1]
  colnames(cn) = colnames
  attr(cn,"vars") = data.frame(vars)
  attr(cn,"varnames") = varnames
  cn
}
#' @export
#' @rdname getContrasts
"readContrasts" = 
  function(cname,excelfile) {
    if (!file.exists(excelfile))
      stop(str_c("Contrast file <<",excelfile,">> not found"))
    channel=odbcConnectExcel2007(excelfile)
    cn=sqlQuery(channel,paste("select * from",cname),as.is=TRUE)
    odbcClose(channel)
    if (class(cn) != "data.frame") 
      stop(str_c("Range ",cname," not found in file ",excelfile))
    cn[,1] = sub('\\s+$', '', cn[,1], perl = TRUE)
    cn
  }

