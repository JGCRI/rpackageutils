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
