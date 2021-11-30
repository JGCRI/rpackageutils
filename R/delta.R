#' delta
#'
#' Function to calculate deltas from a GCAM base year value
#'
#' @param data Default = NULL. Input data as R dataframe or CSV.
#' @param baseline Default = NULL. Baseline data to apply deltas to.
#' @param start_year Default = 2015.
#' @param save Default = TRUE. Whether data should be printed as a csv or not.
#' @param diagnostics Default = FALSE. Whether diagnostic figures should be produced.
#' @param diagnostics_n Default = 20. Number of sub-plots in each diagnostic figure.
#' @param diagnostics_col Default = NULL. Which column to plot diagnostics for.
#' @param filename Default = NULL
#' @param folder Default = NULL
#' @param output_type Default = "wide". Output dimensions long or wide.
#' @importFrom magrittr %>%
#' @keywords delta
#' @export


delta <- function(data=NULL,
                  baseline = NULL,
                  start_year = 2015,
                  save = TRUE,
                  diagnostics = TRUE,
                  diagnostics_n = 20,
                  diagnostics_col = "name",
                  filename = NULL,
                  folder = NULL,
                  output_type = "wide") {

  #For testing:
#  data=list[1]
#  baseline = "C:/Users/wolf184/OneDrive - PNNL/Documents/Projects/GCAM-USA-IM3/Runoff files/xanthos_basin_runoff.csv"
#  start_year = 2015
#  save = TRUE
#  diagnostics = TRUE
#  diagnostics_n = 20
#  diagnostics_col = "name"
#  filename = NULL
#  folder = NULL
#  output_type = "wide"

  #...............
  # Initialize
  #...............

  NULL -> x -> value -> value_2015

  data_delta <- list()
  # data_delta <- list[1]

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

   for(i in 1:length(data)){
  # i = 1
  # For testing set i = 1
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

    # Check if data is in wide format and convert to long
    if(!any(grepl(c("x|year"),names(data_i_raw), ignore.case = T))){
      # Check data is in long format with a x column
      if(any(!is.na(as.numeric(names(data_i_raw))))){

        non_numeric_cols <- names(data_i_raw)[is.na(as.numeric(names(data_i_raw)))]

        # Gather into long format
        data_i_raw <- data_i_raw %>%
          tidyr::gather(key="x",value="value",-tidyselect::all_of(non_numeric_cols))

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
    # Calculate deltas
    #...............

    data_i_raw <- data_i_raw %>%
      # Raw data from /pic/projects/GCAM/gcam_hydrology/runs/xanthos/pm_abcd_mrtm have incomplete basn names.
      # Code below adjusts those
      dplyr::mutate(name  = gsub("^Adriatic Sea - Greece - Black Se$","Adriatic Sea - Greece - Black Sea Coast",name),
                    name  = gsub("^Africa Red Sea - Gulf of Aden Co$","Africa Red Sea - Gulf of Aden Coast",name),
                    name  = gsub("^Northeast South America South At$","Northeast South America South Atlantic Coast",name),
                    name  = gsub("^North Brazil South Atlantic Coas$","North Brazil South Atlantic Coast",name),
                    name  = gsub("^Uruguay - Brazil South Atlantic$","Uruguay - Brazil South Atlantic Coast",name),
                    name  = gsub("^North Argentina South Atlantic C$","North Argentina South Atlantic Coast",name),
                    name  = gsub("^South Argentina South Atlantic C$","South Argentina South Atlantic Coast",name))

    data_i_filtered <- data_i_raw %>%
      dplyr::filter(x >= start_year)


    data_i_deltas <- data_i_filtered %>%
      dplyr::group_by_at(dplyr::vars(tidyselect::all_of(non_numeric_cols))) %>%
      dplyr::mutate(value_2015 = value[x=="2015"],
                    delta = value/value_2015,
                    delta = dplyr::if_else(is.na(delta),1,delta)) %>%
      dplyr::select(-value,-value_2015); data_i_deltas
       # _deltas: deltas as factors normalized to 2015 values for 2015-2100 runoff values by GCM for each basin

    # data_i_deltas %>% filter(id==1) %>% as.data.frame() %>% tail()
     # test: check values for only one basin


    #...............
    # Apply deltas to baseline
    #...............

    if(!is.null(baseline)){

      if(file.exists(baseline)){
        baseline_df <- utils::read.csv(baseline, comment.char = "#") %>%
          tibble::as_tibble()
        names(baseline_df) <- gsub("X","", names(baseline_df), ignore.case = TRUE)

        # Check for any missing/different names between baseline and raw data and notify user
        missing_names <- (data_i_filtered$name%>%unique())[!data_i_filtered$name%>%unique() %in% baseline_df$name%>%unique()]; missing_names
        if(length(missing_names)>0){
          print(paste0("WARNING: Not all the names in raw data file: ", data_i))
          print(paste0("are present in the baseline data file", baseline))
          print(paste0("Missing basins are: ", paste(missing_names,collapse=", ")))
          }


        # Check if data is in wide format and convert to long
        if(!any(grepl(c("x|year"),names(baseline_df), ignore.case = T))){
          # Check data is in long format with a x column
          if(any(!is.na(as.numeric(names(baseline_df))))){

            non_numeric_cols_baseline <- names(baseline_df)[is.na(as.numeric(names(baseline_df)))]

            # Gather into long format
            baseline_df <- baseline_df %>%
              tidyr::gather(key="x",value="value",-tidyselect::all_of(non_numeric_cols_baseline))

          } else {
            stop("None of the columns in the data are years")
          }
        } else {
          if(any(grepl("year",names(baseline_df),ignore.case = T))){
            baseline_df <- baseline_df %>%
              dplyr::rename_all(tolower) %>%
              dplyr::rename("x"="year")
          }

          non_numeric_cols_baseline <- names(baseline_df)[!names(baseline_df) %in% c("x","value")]
        }
        }


    baseline_df_filtered <- baseline_df %>%
      dplyr::filter(x==start_year) %>%
      dplyr::select(-x); baseline_df_filtered

    baseline_df_delta_i <- baseline_df_filtered %>%
      dplyr::left_join(data_i_deltas %>%
                         dplyr::filter(x >= start_year), by = c("id","name")) %>%
      dplyr::mutate(value = delta*value) %>%
      dplyr::select(-delta)
    }


    #...............
    # Turn long format back into wide format
    #...............


    if(!is.null(baseline)){
      baseline_df_delta_i_long <- baseline_df_delta_i
    if(grepl("wide",output_type,ignore.case = T)){
      baseline_df_delta_i = baseline_df_delta_i %>%
        tidyr::spread(key="x", value ="value")}
    }

    data_i_deltas_long <- data_i_deltas

    if(grepl("wide",output_type,ignore.case = T)){
      data_i_deltas = data_i_deltas %>%
        tidyr::spread(key="x", value ="delta")}


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
        fname_raw_i = paste0(dirx,"/delta_data")
      }

      fname_i = paste0(gsub(".csv","",fname_raw_i),"_delta",start_year,".csv")
      # file name for new .csv file
      data.table::fwrite(x=data_i_deltas,file=fname_i)
      print(paste0("File saved as ",fname_i))

      # baseline_df_delta_i <- baseline_df_delta_i[1:(length(baseline_df_delta_i)-1)]
      # drop last column (<NA> column automatically introduced by R)

      if(!is.null(baseline)){
        fname_i = paste0(gsub(".csv","",fname_raw_i),"_delta_applied",start_year,".csv")
        # file name for new .csv file
        data.table::fwrite(x=baseline_df_delta_i,file=fname_i)
        print(paste0("File saved as ",fname_i))
      }
    }

    data_delta[[i]] = data_i_deltas
  }

  #...............
  # Produce Diagnostics
  #...............

  if (diagnostics == TRUE & !is.null(baseline)) {

    print("Starting diagnostics...")

    if(!dir.exists("diagnostics_delta")){dir.create("diagnostics_delta")}

    data_diagnostic <- baseline_df %>%
      dplyr::mutate(data="baseline") %>%
      dplyr::bind_rows(baseline_df_delta_i_long %>%
                         dplyr::mutate(data="delta")) %>%
      dplyr::bind_rows(data_i_raw %>%
                         dplyr::mutate(data="original")); data_diagnostic

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
                                   "/diagnostics_delta/",
                                   basename(fname_raw_i),"_delta",
                                   start_year,"_",i,".png");fname_diagnostics_i

      lower_n = ((i-1)*diagnostics_n)+1; lower_n
      upper_n = (i*diagnostics_n); upper_n

      p1 <- ggplot2::ggplot(data = data_diagnostic %>%
                              dplyr::filter(!!as.symbol(col_name) %in% unique(data_diagnostic[[col_name]])[lower_n:upper_n]),
                            ggplot2::aes(x = x, y = value, group = data)) +
        ggplot2::geom_line(ggplot2::aes(color=data)) +
        ggplot2::facet_wrap(stats::as.formula(paste0(". ~ ",col_name)),scales="free_y") +
        ggplot2::ggtitle(paste0(col_name," delta ", start_year)) +
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

  if(length(data_delta)==1){
    data_delta = data_delta[[1]]
  }

  print("Calculating deltas complete.")

  return(data_delta)

 }  # Close delta function
