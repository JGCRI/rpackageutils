#' append_npy
#'
#' Function to append each file in two lists or folders of csv or npy files by row or col and saving as npy
#' The function is particular used for wide format data used in models such as Xanthos, Tethys and Demeter.
#'
#' @param files1 Default = NULL.
#' @param files2 Default = NULL.
#' @param out_filenames Default = NULL. List of filenames of equal length as input data files.
#' @param by Default = "col". Append by "col" or "row".
#' @param out_dir Default = NULL. Whether diagnostic figures should be produced.
#' @param complete_years Default = F. Only used with csv files when col YYY_MM cols exist
#' @importFrom magrittr %>%
#' @export

append_npy <- function(files1  = NULL,
                       files2  = NULL,
                       out_filenames  = NULL,
                       by  = "col",
                       out_dir  = NULL,
                       complete_years = T) {

  ## For Testing
  # out_filenames  = NULL
  # by  = "col"
  # out_dir  = NULL
  # complete_years = T


  #...............
  # Initialize
  #...............

  print("Starting append_npy...")

  np <- reticulate::import("numpy",convert=FALSE)


  #...............
  # Check inputs
  #...............

  # If length == 1 then check if it is a folder.
  # Check if file inputs are folders. Convert to list of files
  if(!is.list(files1)){
  if(length(files1)==1){
    if(dir.exists(files1)){ # If directory
      files1 <- list.files(files1, full.names = T)
    } else if(file.exists(files1)){
      files1 <- list(files1)
    } else {
      stop(paste0("Input files1 is neither a folder nor a file. Please check input."))
    }
  }
    }

  if(!is.list(files2)){
  if(length(files2)==1){
    if(dir.exists(files2)){ # If directory
      files2 <- list.files(files2, full.names = T)
    } else if(file.exists(files2)){
      files2 <- list(files2)
    } else {
      stop(paste0("Input files2 is neither a folder nor a file. Please check input."))
    }
  }
  }

  # Check length of files is the same
  if(length(files1) != length(files2)){
    stop("files1 and files2 must have the same number of files.")
  }

  if(!is.null(out_filenames)){
    if(length(files1) != length(out_filenames)){
      stop("out_filenames must be the same length as files provided.")
    }
    }

  print("Merging the following files from files from files1: ")
  print(unlist(files1))
  print("to the following files from files from files2: ")
  print(unlist(files2))

  # Make sure all files in each list exist
  for(i in 1:length(files1)){

    if(!file.exists(files1[[i]])){stop(paste0("File: ", files1[[i]], " from files1 does not exist."))}
    if(!file.exists(files2[[i]])){stop(paste0("File: ", files2[[i]], " from files2 does not exist."))}

  #...............
  # Merge files
  #...............

    print(paste0("Merging file: ", files1[[i]]))
    print("with")
    print(paste0(files2[[i]]))

    # If .npy files
    if(grepl(".npy$",files1[[i]]) & grepl(".npy$",files2[[i]])){
    # Read in .npy files using reticulate
    files1x <- np$load(files1[[i]])
    files1r <- reticulate::py_to_r(files1x)
    files1_df <- tibble::as_tibble(files1r)

    # Read in .npy files using reticulate
    files2x <- np$load(files2[[i]])
    files2r <- reticulate::py_to_r(files2x)
    files2_df <- tibble::as_tibble(files2r)

    if(by=="col"){
      files_appended <- files1_df %>%
        dplyr::bind_cols(files2_df)
    }

    if(by=="row"){
      files_appended <- files1_df %>%
        dplyr::bind_rows(files2_df)}

    }

    # If .csv files
    if(grepl(".csv$",files1[[i]]) & grepl(".csv$",files2[[i]])){
      # Read in .csv files
      files1_df <- data.table::fread(files1[[i]], header = T) %>%
        tibble::as_tibble()

      files2_df <-  data.table::fread(files2[[i]], header = T) %>%
        tibble::as_tibble()

      # Check years columns in format YYYY_MM
      colnames1 <- names(files1_df)
      colnames1 <- colnames1[grepl("[0-9]{4}_[0-9]{2}",colnames1)]
      colnames2 <- names(files2_df)
      colnames2 <- colnames2[grepl("[0-9]{4}_[0-9]{2}",colnames2)]

      if(complete_years){
        max_years1 <- max(as.numeric(gsub("_.*$","",colnames1)), na.rm=T)
        # If incomplete final year remove it
        if(!any(grepl(paste0(max_years1,"_12"), colnames1))){
          colnames1 <- colnames1[!grepl(max_years1,colnames1)]
          files1_df <- files1_df %>%
            dplyr::select(-contains(as.character(max_years1)))
          print(paste0("Removing incomplete final year: ", max_years1, " from ",files1[[i]]))
        }

        min_years1 <- min(as.numeric(gsub("_.*$","",colnames1)), na.rm=T)
        # If incomplete final year remove it
        if(!any(grepl(paste0(min_years1,"_12"), colnames1))){
          colnames1 <- colnames1[!grepl(min_years1,colnames1)]
          files1_df <- files1_df %>%
            dplyr::select(-contains(as.character(min_years1)))
          print(paste0("Removing incomplete starting year: ", min_years1, " from ",files1[[i]]))
        }

      max_years2 <- max(as.numeric(gsub("_.*$","",colnames2)), na.rm=T)
      # If incomplete final year remove it
      if(!any(grepl(paste0(max_years2,"_12"), colnames2))){
        colnames2 <- colnames2[!grepl(max_years2,colnames2)]
        files2_df <- files2_df %>%
          dplyr::select(-contains(as.character(max_years2)))
        print(paste0("Removing incomplete final year: ", max_years2, " from ",files2[[i]]))
      }

      min_years2 <- min(as.numeric(gsub("_.*$","",colnames2)), na.rm=T)
      # If incomplete final year remove it
      if(!any(grepl(paste0(min_years2,"_12"), colnames2))){
        colnames2 <- colnames2[!grepl(min_years2,colnames2)]
        files2_df <- files2_df %>%
          dplyr::select(-contains(as.character(min_years2)))
        print(paste0("Removing incomplete starting year: ", min_years2, " from ",files2[[i]]))
      }
    }

    # Remove duplicated years from file2
    colnames_duplicated_numeric <- colnames2[colnames2 %in% colnames1]; colnames_duplicated_numeric
    colnames_duplicated_nonnumeric <- names(files2_df)[names(files2_df) %in% names(files1_df)]; colnames_duplicated_nonnumeric
    colnames_duplicated_nonnumeric <- colnames_duplicated_nonnumeric[!colnames_duplicated_nonnumeric %in% colnames_duplicated_numeric];
    colnames_duplicated <- c(colnames_duplicated_nonnumeric, colnames_duplicated_numeric); colnames_duplicated

    if(length(colnames_duplicated) == length(colnames1)){
      print("The two files being appended contain all the same columns.")
      print(paste0(files1[[i]], " and ", files2[[i]]))
      print("No columns will be appended and only the first file will be used to convert to .npy.")
      files_appended <- files1_df

      print(paste0("Appended file (file1) columns used: ", paste(colnames(files_appended),collapse=", ")))

    } else {

      if(by=="col"){

        files2_df <- files2_df %>%
          dplyr::select(-dplyr::all_of(colnames_duplicated))

        files_appended <- files1_df %>%
          dplyr::bind_cols(files2_df)

        # Keep only the YYYY_MM cols
        cols_keep <- colnames(files_appended)
        cols_keep <- cols_keep[grepl("[0-9]{4}_[0-9]{2}",cols_keep)]; cols_keep

        files_appended <- files_appended %>%
          dplyr::select(dplyr::all_of(cols_keep))

      }

      if(by=="row"){
        if(colnames1 == colnames2){
        files_appended <- files1_df %>%
          dplyr::bind_rows(files2_df)
        } else {
          print("The two files being appended do not have the same columns so cannot append by rows.")
          print(paste0(files1[[i]], " and ", files2[[i]]))
        }
      }
    }

      # Summarize colnames
      print(paste0("File 1 columns used: ", paste(colnames(files1_df),collapse=", ")))
      print(paste0("File 2 columns used: ", paste(colnames(files2_df),collapse=", ")))
      print(paste0("Appended file columns: ", paste(colnames(files_appended),collapse=", ")))

    }

    print("Converting to .npy...")
    out_matrix <- as.matrix(files_appended);
    colnames(out_matrix) <- NULL; out_matrix

    colnames_appended <- colnames(files_appended)[grepl("[0-9]{4}_[0-9]{2}",colnames(files_appended))]; colnames_appended
    min_year_month <-  colnames_appended[min(1:length(colnames_appended))]; min_year_month
    max_year_month <-  colnames_appended[max(1:length(colnames_appended))]; max_year_month

    if(is.null(out_filenames)){
    fname_csv <- paste0(getwd(),"_csv/appended_file_",i,
                        "_",min_year_month,"_to_",max_year_month,".csv")
    fname <- paste0(getwd(),"/appended_file_",i,
                    "_",min_year_month,"_to_",max_year_month,".npy")
    } else {
      if(dirname(out_filenames[i])=="."){fname_csv <- paste0("./csv/",gsub(".csv","",basename(out_filenames[i])),
                                                             "_",min_year_month,"_to_",max_year_month,".csv")}
      if(dirname(out_filenames[i])!="."){fname_csv <- paste0(dirname(out_filenames[i]),"_csv/",gsub(".csv","",basename(out_filenames[i])),
                                                             "_",min_year_month,"_to_",max_year_month,".csv")}
      fname <- out_filenames[i]
    }

    if(!is.null(out_dir)){
      if(!dir.exists(out_dir)){
        dir.create(out_dir)
      }
      fname <- paste0(out_dir,"/",basename(fname))

      out_dir_csv <- paste0(out_dir,"_csv")
      if(!dir.exists(out_dir_csv)){
        dir.create(out_dir_csv)
      }
      fname_csv <- paste0(out_dir_csv,"/",basename(fname_csv))
    }

    print("Saving...")
    np$save(fname, out_matrix)
    data.table::fwrite(file = fname_csv, x= files_appended)
    print(paste0("Appended csv file saved: ",fname_csv))
    print(paste0("Appended file saved: ",fname))
  }

  #...............
  # Close out
  #...............

  print("append_npy complete.")

}
