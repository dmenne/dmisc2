#' @title Convert survfit summary to a table.
#' 
#' @description The output of \code{print.survfit} normally is only printed and cannot be
#' stored in a variable. This function captures the output, as suggested by
#' Thomas Lumley in
#' \url{http://finzi.psych.upenn.edu/R/Rhelp02a/archive/42785.html}.
#' 
#' 
#' @param sv Kaplan-Meier output from \code{survfit}.
#' @author Dieter Menne
#' @seealso \code{\link[survival]{survfit}}
#' @export survtable
#' @examples
#' 
#' library(survival)
#' sv =  survfit( Surv(futime,fustat)~resid.ds+rx+ecog.ps,data=ovarian)
#' survtable(sv)
#' 
survtable = function(sv) {
  # Dirty way to get the the results of survfit
  #http://finzi.psych.upenn.edu/R/Rhelp02a/archive/42785.html
  svcap = capture.output(print(sv))
  zz = textConnection(svcap)
  svtable = read.table(zz, skip=grep("events",svcap),header=FALSE)
  close(zz)
  pars = strsplit(as.character(sv$call[2])," ")[[1]]
  pars = pars[seq(grep("~",pars)+1,length(pars),2)]
  # reorder such that first column changes slowly
  names(svtable) = c(pars,"n","events","median",  "Lower95","Upper95") 
  svtable = svtable[,c(length(pars):1,3:7)]
  svtable
}



