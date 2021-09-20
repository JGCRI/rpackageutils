#' smooth
#'
#' Function to smooth data across different time windows length and position (historical or surrounding)
#'
#' @param data Default = NULL. Input data as R dataframe or CSV
#' @param window_length Default = 5. Length of smoothing window to use in units of data time period.
#' @param window_type Default = "historical". Can be "historical" or "surround" where historical uses
#' time periods up to the value being smoothed and "surround" uses time period before and after each value being smoothed.
#' @param save Default = TRUE. Whether data should be printed as a csv or not.
#' @param diagnostics Default = FALSE. Whether diagnostic figures should be produced.
#' @return Returns a dataframe (and csv file if save = TRUE)
#' @keywords smoothing
#' @export
#' @examples
#' library(rpackageutils)


smooth <- function(data=NULL,
                   window_length=5,
                   window_type="historical",
                   save = TRUE,
                   diagnostics = FALSE) {

  #...............
  # Initialize
  #...............

  NULL -> data_i_smoothed

  data_smoothed <- list()


  #...............
  # Check input data
  #...............

  # Convert to list if not already a list
  if(!class(data) %in% "list"){
    if(class(data) != "list"){
      data <- list(data)
    }
  }


  # For each element in list
  for(i in length(data)){

    data_i = data[[i]]

    #...............
    # Check input data
    #...............

    if(class(data_i) %in% c("tbl_df","tbl","data.frame")){
     data_i_raw = data_i
    }


    if(class(data_i) %in% c(".csv")){
        if(file.exists(data_i)){
          data_i_raw <- data.table::fread(paste(data_i),encoding="Latin-1")%>%tibble::as_tibble()
        } else {stop(paste0(data_i," does not exist"))}
      }

    #...............
    # Read in data
    #...............

    #...............
    # Smooth Data
    #...............

    data_i_smoothed = data_i_raw

    #...............
    # Save outputs
    #...............

    data_smoothed[[i]] = data_i_smoothed

    #...............
    # Produce Diagnostics
    #...............


  } # Close if if(length(data)>1){

  #...............
  # Return Results
  #.............

  if(length(data_smoothed)==1){
    data_smoothed = data_smoothed[[1]]
  }

  return(data_smoothed)

  } # Close smooth function
