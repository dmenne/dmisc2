#' @title Creates a directory if it does not exist
#' @description Raises if creation was not successful
#' @param path path to the directory to be created
#' @return TRUE if the directory was created, FALSE if it already existed,
#' exception if the directory could not be created
#' @export safe.dir.create

safe.dir.create <- function(path)
{
  dirTest <- function(x) !is.na(isdir <- file.info(x)$isdir) &
    isdir
  dt = !dirTest(path)
  if (dt && !dir.create(path))
    stop(gettextf("cannot create directory '%s'", path),
         domain = NA)
  dt
}
