#` `download_unpack.R`
#` @description Utilities that conduct remote downloading and|or unpacking


#' Download and unzip zipped file (.zip) to a user-specified location
#'
#' Download and unpack example zipped file with .zip extension to a user-specified location
#'
#' @param data_directory character. Full path to the directory you wish to install the example data to.
#' Must be write-enabled for the user.
#' @param url character.  URL to download the target file
#' @author Chris R. Vernon (chris.vernon@pnnl.gov)
#' @importFrom logger log_info
#' @export
download_unpack_zip <- function(data_directory, url) {

  # hold zipped file in memory
  temp_file <- tempfile()

  # download zipped file from target URL
  download.file(url, temp_file)

  # unzip file to specified directory
  unzip(zipfile = temp_file, exdir = data_directory)

  log_info(paste0("Data extracted to ", data_directory))

  # dump zipped file from memory
  unlink(temp_file)
}


#' Get file name from URL path
#'
#' Get file name from URL path using the file extension to find the file in the string
#'
#' @param url character.  URL to download the target file
#' @param file_extension character. File extension without the dot (e.g., csv)
#' @author Chris R. Vernon (chris.vernon@pnnl.gov)
#' @importFrom logger log_info
#' @export
filename_from_url <- function(url, file_extension) {

  # seek and construct filename from extension
  base_filename <- basename(url) %>%
    tolower()

  fname <- strsplit(base_filename, split = tolower(file_extension))[[1]][1] %>%
    paste0(file_extension)

  log_info(paste0("Autodetected file name as '", fname, "'"))

  return(fname)
}


#' Download file from URL path
#'
#' Download file and name and save it to a user-specified location
#'
#' @param url character.  URL to download the target file
#' @param write_directory character. Full path to the directory you wish to install the example data to.
#' Must be write-enabled for the user.
#' @param fname character. The file name with extension to save the file as
#' @author Chris R. Vernon (chris.vernon@pnnl.gov)
#' @importFrom logger log_info
#' @export
download_file <- function(url, write_directory, fname) {

  # create full path reference to file to write
  write_path <- file.path(write_directory, fname)

  log_info(paste0("Downloading file to:  ", write_path))

  download.file(url, write_path, method = 'auto')

  log_info(paste0("File download complete for '", fname, "'"))
}


#' Install compressed remote data supplement locally
#'
#' Download and unpack example data supplement from Zenodo that matches the current installed
#' distribution.
#'
#' @param data_directory character. Full path to the directory you wish to install the example data to.
#' Must be write-enabled for the user.
#' @param url_list list.  A keyed list containing links to download remote data for each package version in the structure:
#' list(version_1 = c(data_url_1, data_url_n), #' version_n = c(data_url_1, data_url_n)); default from internal `constants.R`
#' @author Chris R. Vernon (chris.vernon@pnnl.gov)
#' @importFrom logger log_info
#' @importFrom magrittr %>%
#' @export
fetch_unpack_data <- function(data_directory, url_list=DATA_VERSION_URL_LIST) {

  # get the current version that is installed
  version <- version_as_character()

  log_info(paste0("Preparing data download for v", version))

  # get data associated with the target version
  tryCatch({
    # extract contents of list
    file_link <- url_list[[version]][[1]]

    file_extension <- url_list[[version]][[2]] %>%
      tolower()

    write_filename <- url_list[[version]][[3]]

  }, error = function(condition) {
    msg <- paste0("Link to data missing for current version: ", version, ".  Please contact admin.")
    log_info(msg)
    stop(msg)
  })

  log_info(paste0("Downloading from ", file_link))

  # auto detect file name if expected output name is NULL
  if (is.null(write_filename)) {

    # seek and construct filename from extension
    write_filename <- filename_from_url(file_link, file_extension)
  }

  # if file zipped download and unpack; else, just download
  if (file_extension == 'zip') {
    download_unpack_zip(data_directory, file_link)

  } else {
    download_file(file_link, data_directory, write_filename)
  }
}
