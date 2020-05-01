#` `common.R`
#` @description Utilities that are common to all package interactions


#' Get version as a character dot separated.
#'
#' Get the version number as a character dot separated from associated parent package
#'
#' @importFrom magrittr %>%
#' @return character. For example, c(0, 1, 9) to "0.1.9"
#' @author Chris R. Vernon (chris.vernon@pnnl.gov)
#' @export
version_as_character <- function() {

  # get package name from where this function is called from
  nm <- packageName(env = parent.frame())

  return(packageVersion(nm) %>%
           c() %>%
           paste(collapse = '.')
  )
}
