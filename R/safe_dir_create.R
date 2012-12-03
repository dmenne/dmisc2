#' @title Creates a directory if it does not exist
#' 
#' @param path path to the directory to be created
#' @export safe.dir.create

safe.dir.create <- function(path)
{
  dirTest <- function(x) !is.na(isdir <- file.info(x)$isdir) & isdir
  if(!dirTest(path) && !dir.create(path))
    stop(gettextf("cannot create directory '%s'", path), domain = NA)
}
