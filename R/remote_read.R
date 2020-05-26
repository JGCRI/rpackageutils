#` `remote_read.R`
#` @description Utilities that read in remote sources of data directly into the R environment.

#' Read in a file in table formate from a remote source.
#'
#' Currently this function only works with .csv and .txt files.
#'
#' @param url character.  URL to the file to import into R, it must be write-enabled for the user.
#' @param ... additional arguments that may be passed  \code{read.table}
#' @author Kalyn R. Dorheim (kalyn.dorheim@pnnl.gov)
#' @export
remote_read <- function(url, ...){

  # Check the url input
  assertthat::assert_that(grepl('.txt|.csv', url), msg = 'this function only supports data table import for files saved as a .txt or .csv')

  # Read the data
  if(grepl('.csv', url)){
    dat <- read.csv(url(url), ...)
  } else if (grepl('.txt', url)){
    dat <- read.table(url(url), ...)}

  # Return the data frame
  dat

}


