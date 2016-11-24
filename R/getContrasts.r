#' @title Read and format contrast tables from Excel
#'
#' @description Reads contrast from an Excel file for use with \code{estimable} 
#' of package gmodels.
#'
#' @aliases getContrasts readContrasts
#' @param cname Sheet name in Excel file
#' @param excelfile path of Excel file with contrast table
#' @param rows optionally only select some rows of the table
#' @return \code{readContrasts} read a contrast table. Only columns with at 
#' least on period  in the header are kept. 
#' \code{getContrasts} additionally includes column and row name 
#' information to label contrast tables with estimable (gmodels)
#' @author Dieter Menne, \email{dieter.menne@@menne-biomed.de}
#' @keywords models
#' @seealso See \code{constrast.xlsx} in the extdata directory for some
#' rules of thumb on constructing a contrast matrix.
#' @examples
#' library(nlme)
#' library(gmodels)
#' options(digits=3)
#' set.seed(4711)
#' excelfile = system.file("extdata", "contrasts.xlsx", package = "Dmisc2")
#' d = expand.grid(subject = LETTERS[1:8],
#'                 peri= c("Wine", "Tea"),
#'                 post = c("Water", "Kirsch"),
#'                 interval = c("Pre", "Post"))
#' d$vol = round(rnorm(nrow(d),10,2),1)
#' d.lme = lme(vol~interval+peri+post+peri:interval+interval:post,
#'             data=d,random=~1|subject)
#' summary(d.lme)
#' ct = getContrasts("peripostinterval",excelfile)
#' estimable(d.lme,ct,conf.int=0.95)
#' @import readxl
#' @export
#' @rdname getContrasts
"getContrasts" = function(cname,excelfile,rows = NULL) {
  cn = readContrasts(cname, excelfile)

  # Use _ as placeholder for an empty field
  vars = do.call("rbind", strsplit(colnames(cn),'\\.'))
  vars[vars == '_'] = ''
  varnames = vars[1,]
  colnames(vars) =  varnames

  cn_1 = as.data.frame(t(as.matrix(cn[,-1])))
  colnames(cn_1) = cn[,1]

  attr(cn_1,"vars") = data.frame(vars[-1,])
  attr(cn_1,"varnames") = varnames
  cn_1
}

#' @export
#' @rdname getContrasts
"readContrasts" =
  function(cname,excelfile) {
    if (!file.exists(excelfile))
      stop(str_c("Contrast file <<",excelfile,">> not found"))
    cn = try(read_excel(excelfile, cname), silent = TRUE)
    if (!inherits(cn, "data.frame"))
      stop(str_c("Sheet ",cname," not found in file ",excelfile))
    # Remove lines without . in header
    colnames = colnames(cn)
    na.omit(cn[,str_detect(colnames(cn),"\\.")])
  }
