library(rpackageutils)
library(dplyr)

data <- list("C:/Z/models/00tests/xanthosGlobalRuns/Basin_runoff_km3peryear_pm_abcd_mrtm_noresm1-m_rcp8p5_1950_2099.csv",
             "C:/Z/models/00tests/xanthosGlobalRuns/Basin_runoff_km3peryear_pm_abcd_mrtm_noresm1-m_rcp8p5_1950_2099a.csv")

data <- list.files("C:/Z/models/00tests/xanthosGlobalRuns",pattern="*.csv", full.names = T); data
data <- data[!grepl("_window*",data)]; data

data_smoothed <- rpackageutils::smooth(data=data,
                                       window_length = 5,
                                       window_type = "surround",
                                       diagnostics = TRUE,
                                       diagnostics_n = 20,
                                       diagnostics_col = "name",
                                       filename = NULL,
                                       folder = NULL)
data_smoothed


df <- data.table::fread(data,header=T) %>% tibble::as_tibble(); df
df_smoothed <- rpackageutils::smooth(data=df,
                                     window_length = 5,
                                     window_type = "surround",
                                     diagnostics = TRUE,
                                     diagnostics_n = 20,
                                     diagnostics_col = "name",
                                     filename = NULL,
                                     folder = NULL)
df_smoothed


#------------ LDC

library(rpackageutils)
library(dplyr)

fname <- "C:/Z/projects/current/00_IM3/gcam_usa_im3/hddcdd_ldc_smoothing/resampled_wrf_to_xanthos_monthly_tempDegC_degC_1979_01_to_2021_01.csv"
data_raw <- data.table::fread(fname, header=T) %>%
  tibble::as.tibble() ; data_raw

data_raw_long <- data_raw %>%
  tidyr::gather(key="x", value="value", -lon,-lat,-gridid, -param, -unit) %>%
  dplyr::mutate(year = gsub("\\_.*$","",x),
                segment = gsub(".*_","",x)); data_raw_long


segment_i = "01"
data_raw_long_seg_i <- data_raw_long %>%
  dplyr::filter(segment == segment_i) %>%
  dplyr::select(-lon,-lat,-param,-unit,-x, -gridid); data_raw_long_seg_i

data = data_raw_long_seg_i

rpackageutils::smooth(data, window_type = "surround", save=F) -> data_smoothed; data_smoothed

data_raw_long_continuous <- data_raw_long %>%
  dplyr::mutate(yearOld = year,
                year = 1:n())%>%
  dplyr::select(-x); data_raw_long_continuous

data = data_raw_long_continuous

rpackageutils::smooth(data, window_type = "surround", save=F, output_type="long") -> data_smoothed; data_smoothed



# append_npy.R
files1 = c("C:/Z/models/00tests/xanthos_im3_test/example/input/climate/pr_gpcc_watch_monthly_mmpermth_1971_2001.npy",
                     "C:/Z/models/00tests/xanthos_im3_test/example/input/pet/penman_monteith/rhs_watch_monthly_percent_1971_2001.npy")
files2 = c("C:/Z/models/00tests/xanthos_im3_test/output_wrf_to_xanthos_process_historical_python/resampled_wrf_to_xanthos_monthly_RAIN_mm_1979_01_to_2021_01.npy",
                       "C:/Z/models/00tests/xanthos_im3_test/output_wrf_to_xanthos_process_historical_python/resampled_wrf_to_xanthos_monthly_rh_percent_1979_01_to_2021_01.npy")

append_npy(files1  = files1,
           files2  = files2,
           out_filenames  = NULL,
           by  = "col",
           out_dir  = NULL,
           complete_years = T)

# Read in .npy files using reticulate
file_i <-"C:/Z/models/rpackageutils/appended_file_1.npy"
library(reticulate)
np <- reticulate::import("numpy",convert=FALSE)
file_ax <- np$load(file_i)
file_ar <- reticulate::py_to_r(file_ax)
file_a_df <- tibble::as_tibble(file_ar); file_a_df

file_1x <- np$load(files1[[1]])
file_1r <- reticulate::py_to_r(file_1x)
file_1_df <- tibble::as_tibble(file_1r); file_1_df

file_2x <- np$load(files2[[1]])
file_2r <- reticulate::py_to_r(file_2x)
file_2_df <- tibble::as_tibble(file_2r); file_2_df

ncol(file_1_df);
ncol(file_2_df);
ncol(file_1_df) + ncol(file_2_df);
ncol(file_a_df)

nrow(file_1_df);
nrow(file_2_df);
nrow(file_1_df) + nrow(file_2_df);
nrow(file_a_df)


# Test Data
library(rpackageutils); library(dplyr)

files1 = "C:/Z/models/00tests/xanthos_im3_test/output_wrf_to_xanthos_process_historical_R"
files2 = "C:/Z/models/00tests/xanthos_im3_test/output_wrf_to_xanthos_process_ssp585_hot_near_R"

out_filenames <- c(
"resampled_wrf_to_xanthos_monthly_GLW_W m-2",
"resampled_wrf_to_xanthos_monthly_RAIN_mm",
"resampled_wrf_to_xanthos_monthly_rh_percent",
"resampled_wrf_to_xanthos_monthly_SWDOWN_W m-2",
"resampled_wrf_to_xanthos_monthly_tempDegC_degC",
"resampled_wrf_to_xanthos_monthly_tempDegCmin_degC",
"resampled_wrf_to_xanthos_monthly_v_m s-1")
#out_filenames <- NULL

# Append historical and near
append_npy(files1  = files1,
           files2  = files2,
           out_filenames  = out_filenames,
           by  = "col",
           out_dir  = "C:/Z/models/00tests/xanthos_im3_test/output_wrf_to_xanthos_process_historical_near",
           complete_years = T)

files1 = "C:/Z/models/00tests/xanthos_im3_test/output_wrf_to_xanthos_process_historical_near_csv"
files2 = "C:/Z/models/00tests/xanthos_im3_test/output_wrf_to_xanthos_process_ssp585_hot_far_R"

# Append historical_near with far
append_npy(files1  = files1,
           files2  = files2,
           out_filenames  = out_filenames,
           by  = "col",
           out_dir  = "C:/Z/models/00tests/xanthos_im3_test/output_wrf_to_xanthos_process_historical_near_far",
           complete_years = T)


# Xanthos to gcam xml
xanthos_runoff_csv = "C:/Z/models/00tests/xanthosGlobalRuns/Basin_runoff_km3peryear_pm_abcd_mrtm_noresm1-m_rcp4p5_1950_2100.csv"
gcamdata_folder = "C:/Z/models/GCAMVersions/gcam-usa-im3/input/gcamdata"
out_dir = NULL

xanthos_to_gcam_xml(xanthos_runoff_csv=xanthos_runoff_csv,
                    gcamdata_folder=gcamdata_folder)
