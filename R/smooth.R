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
#' @param diagnostics_n Default = 20. Number of sub-plots in each diagnostic figure.
#' @param diagnostics_col Default = NULL. Which column to plot diagnostics for.
#' @param filename Default = NULL
#' @param folder Default = NULL
#' @param output_type Default = "wide". Output dimensions long or wide.
#' @importFrom magrittr %>%
#' @return Returns a dataframe (and csv file if save = TRUE)
#' @keywords smoothing
#' @export
#' @examples
#' library(rpackageutils)


smooth <- function(data=NULL,
                   window_length=5,
                   window_type="historical",
                   save = TRUE,
                   diagnostics = FALSE,
                   diagnostics_n = 20,
                   diagnostics_col = NULL,
                   filename = NULL,
                   folder = NULL,
                   output_type = "wide") {

  #For testing:
  # data  = model_smooth_total

  # window_length=5
  # window_type="historical"
  # save = FALSE
  # diagnostics = FALSE
  # diagnostics_n = 20
  # diagnostics_col = NULL
  # filename = NULL
  # folder = NULL

  #...............
  # Initialize
  #...............

  NULL -> data_i_smoothed -> YEAR ->  Year -> mean_val_complete -> mean_value ->
    value -> x -> year

  data_smoothed <- list()

  if (is.null(data)){stop("Data is NULL. Please provide valid data.")}

  # Set Folder
  if(!is.null(folder)){
    if(!dir.exists(folder)){
      dir.create(folder)
      dirx <- paste0(getwd(),"/",folder)
    } else {
      dirx <- folder
    }
  } else {
    dirx <- getwd()
  }

  #...............
  # Check input data
  #...............

  # Convert to list if not already a list
  if(any(!class(data) %in% "list")){
    if(any(class(data) == "character") & length(data) > 0){
        data <- as.list(data)
    } else {
      data <- list(data=data)
    }
  }

  # For each element in list

  # For testing set i = 1
  for(i in 1:length(data)){

    data_i = data[[i]]

    #...............
    # Check input data
    #...............

    if(any(class(data_i) %in% c("tbl_df","tbl","data.frame"))){
      data_i_raw = data_i
    }
    # check if dataframe

    if(any(class(data_i) %in% c("character"))){
      # check if class data_i is a character
      if(file.exists(data_i)){
        data_i_raw <- data.table::fread(paste(data_i),encoding="Latin-1")%>%
          tibble::as_tibble()
      } else {stop(paste0(data_i," does not exist"))}
    }
    # check if character file

    #...............
    # Check Data format
    #...............

    # Check if data is in wide format adn ocnvert to long
    if(!any(grepl(c("x|year"),names(data_i_raw), ignore.case = T))){
      # Check data is in long format with a x column
      if(any(!is.na(as.numeric(names(data_i_raw))))){

        non_numeric_cols <- names(data_i_raw)[is.na(as.numeric(names(data_i_raw)))]

        # Gather into long format
        data_i_raw <- data_i_raw %>%
          tidyr::gather(key="x",value="value",-non_numeric_cols)

      } else {
        stop("None of the columns in the data are years")
      }
    } else {
      if(any(grepl("year",names(data_i_raw),ignore.case = T))){
      data_i_raw <- data_i_raw %>%
        dplyr::rename_all(tolower) %>%
        dplyr::rename("x"="year")
      }

      non_numeric_cols <- names(data_i_raw)[!names(data_i_raw) %in% c("x","value")]

    }


    #...............
    # Smooth Data
    #...............

    data_i_smoothed_raw <- data_i_raw

    # print(paste0("window_type = ",window_type))

    # if historical:
    if (window_type == "historical"){
      data_i_smoothed_raw <- data_i_raw %>%
        dplyr::group_by_at(non_numeric_cols)%>%
        dplyr::mutate(mean_value = zoo::rollmean(x = value, k = window_length, fill = NA)) %>%
        dplyr::ungroup()
    }
    # adjust

    # if surrounding
    if(window_type == "surround"){
      data_i_smoothed_raw <- data_i_raw %>%
        dplyr::group_by_at(non_numeric_cols)%>%
        dplyr::mutate(mean_value=zoo::rollmean(x = value,k=window_length,fill=NA)) %>%
        dplyr::ungroup()
    }

    # Replace NA values with initial and final data
    data_i_smoothed_all <- data_i_smoothed_raw %>%
      dplyr::group_by_at(non_numeric_cols) %>%
      dplyr::mutate(mean_val_complete = zoo::na.approx(mean_value,na.rm=F,rule=2)) %>%
      dplyr::ungroup(); data_i_smoothed_all

    # Remove intermediate columns
    data_i_smoothed <- data_i_smoothed_all %>%
      dplyr::select(-value,-mean_value)%>%
      dplyr::rename(value=mean_val_complete); data_i_smoothed

    #...............
    # Turn long format back into wide format
    #...............

    if(grepl("wide",output_type,ignore.case = T)){
    data_i_smoothed = data_i_smoothed %>%
      tidyr::spread(key="x", value ="value")}

    #...............
    # Save outputs
    #...............

    if (save == TRUE){

      # if data_i is a character, then we can save it this way:
      if (class(data_i) == "character"){
        if(is.null(filename)){
        fname_raw_i = paste0(dirx,"/",gsub(".csv","",basename(data_i)))
        } else {
          fname_raw_i = paste0(dirx,"/",basename(filename))
        }
      }
      else {
        fname_raw_i = paste0(dirx,"/smoothed_data")
      }

      fname_i = paste0(gsub(".csv","",fname_raw_i),"_window",window_length,window_type,".csv")
      # file name for new .csv file
      data.table::fwrite(x=data_i_smoothed,file=fname_i)
      print(paste0("File saved as ",fname_i))
    }

    data_smoothed[[i]] = data_i_smoothed
  }

  #...............
  # Produce Diagnostics
  #...............

  if (diagnostics == TRUE) {

    print("Starting diagnostics...")

    if(!dir.exists("diagnostics")){dir.create("diagnostics")}

    data_diagnostic <- data_i_raw %>%
      dplyr::mutate(data="raw") %>%
      dplyr::bind_rows(data_i_smoothed %>%
                         dplyr::mutate(data="smoothed")); data_diagnostic

    # Diagnostics in Groups of 50
    if(is.null(diagnostics_col)){
      col_name <- non_numeric_cols[1]
      } else {
        if(any(diagnostics_col %in% non_numeric_cols)){
          col_name <- diagnostics_col
        } else {
          col_name <- non_numeric_cols[1]
        }
      }; col_name

    diagnostics_n = min(diagnostics_n, length(unique(data_diagnostic[[col_name]]))); diagnostics_n
    groups_n = ceiling(length(unique(data_diagnostic[[col_name]]))/diagnostics_n); groups_n

    for(i in 1:groups_n){

      fname_diagnostics_i = paste0(dirname(fname_raw_i),
                                   "/diagnostics/",
                                   basename(fname_raw_i),"_window",
                                   window_length,window_type,"_",i,".png");fname_diagnostics_i

      lower_n = ((i-1)*diagnostics_n)+1; lower_n
      upper_n = (i*diagnostics_n); upper_n

      p1 <- ggplot2::ggplot(data = data_diagnostic %>%
                              dplyr::filter(!!as.symbol(col_name) %in% unique(data_diagnostic[[col_name]])[lower_n:upper_n]),
                            ggplot2::aes(x = x, y = value, group = data)) +
        ggplot2::geom_line(ggplot2::aes(color=data)) +
        ggplot2::facet_wrap(as.formula(paste0(". ~ ",col_name)),scales="free_y") +
        ggplot2::ggtitle(paste0(col_name," window length = ", window_length, " ", window_type)) +
        ggplot2::theme_bw() +
        ggplot2::theme(axis.text.x=ggplot2::element_text(angle=90,vjust=0.5)) +
        ggplot2::scale_x_discrete(limits=unique(data_diagnostic$x),
                                  breaks=pretty(unique(data_diagnostic$x),n=10)); p1

      ggplot2::ggsave(filename =  fname_diagnostics_i,
                      plot = p1,
                      width = 13*min(3,max(1,diagnostics_n/20)),
                      height = 10*min(3,max(1,diagnostics_n/20))) # save plot

      print(paste0("Diagnostic figure saved as ",fname_diagnostics_i))

    }

    print("Diagnostics complete.")

  } # Close diagnostics

  #...............
  # Return Results
  #.............

  if(length(data_smoothed)==1){
    data_smoothed = data_smoothed[[1]]
  }

  print("Smoothing complete.")

  return(data_smoothed)

}  # Close smooth function
