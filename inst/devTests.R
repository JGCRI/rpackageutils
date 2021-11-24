library(rpackageutils)
library(dplyr)

setwd("C:/Z/projects/current/00_IM3/tests/xanthos_us_comb_global")

print("Start smoothing ... ")


# Get the list of .csv files
list <- list.files(pattern = "*_comb");
list <- list[grepl(".csv",list)]; list
list <- list[[1]]

model_smoothed <- rpackageutils::smooth(data=list,
                                        window_length = 5,
                                        window_type = "trail",
                                        diagnostics = FALSE,
                                        save = TRUE,
                                        diagnostics_n = 20,
                                        diagnostics_col = "name",
                                        filename = NULL,
                                        folder = NULL)

model_smoothed <- rpackageutils::smooth(data=list,
                                        window_length = 5,
                                        window_type = "surround",
                                        diagnostics = FALSE,
                                        save = TRUE,
                                        diagnostics_n = 20,
                                        diagnostics_col = "name",
                                        filename = NULL,
                                        folder = NULL)

data=list
window_length = 5
window_type = "trail"
diagnostics = TRUE
save = TRUE
diagnostics_n = 20
diagnostics_col = "name"
filename = NULL
folder = NULL


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


# .................
# Combine historical, near and far
# ................
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



# .................
# Combine us and global
# ................

#.......................
# libraries
#.......................
library(im3components)
library(reticulate)
library(dplyr)


#.......................
# Initialize
#.......................

base_npy_list = list("C:/Z/models/00tests/xanthos_im3_test/example/input/climate/pr_gpcc_watch_monthly_mmpermth_1971_2001.npy",
                     "C:/Z/models/00tests/xanthos_im3_test/example/input/pet/penman_monteith/rhs_watch_monthly_percent_1971_2001.npy",
                     "C:/Z/models/00tests/xanthos_im3_test/example/input/pet/penman_monteith/rlds_watch_monthly_wperm2_1971_2001.npy",
                     "C:/Z/models/00tests/xanthos_im3_test/example/input/pet/penman_monteith/rsds_watch_monthly_wperm2_1971_2001.npy",
                     "C:/Z/models/00tests/xanthos_im3_test/example/input/pet/penman_monteith/tas_watch_monthly_degc_1971_2001.npy",
                     "C:/Z/models/00tests/xanthos_im3_test/example/input/pet/penman_monteith/tasmin_watch_monthly_degc_1971_2001.npy",
                     "C:/Z/models/00tests/xanthos_im3_test/example/input/pet/penman_monteith/wind_watch_monthly_mpers_1971_2001.npy")
target_npy_list = list("C:/Z/models/00tests/xanthos_im3_test/output_wrf_to_xanthos_process_historical_near_far/resampled_wrf_to_xanthos_monthly_RAIN_mm_1979_01_to_2099_12.npy",
                       "C:/Z/models/00tests/xanthos_im3_test/output_wrf_to_xanthos_process_historical_near_far/resampled_wrf_to_xanthos_monthly_rh_percent_1979_01_to_2099_12.npy",
                       "C:/Z/models/00tests/xanthos_im3_test/output_wrf_to_xanthos_process_historical_near_far/resampled_wrf_to_xanthos_monthly_GLW_W m-2_1979_01_to_2099_12.npy",
                       "C:/Z/models/00tests/xanthos_im3_test/output_wrf_to_xanthos_process_historical_near_far/resampled_wrf_to_xanthos_monthly_SWDOWN_W m-2_1979_01_to_2099_12.npy",
                       "C:/Z/models/00tests/xanthos_im3_test/output_wrf_to_xanthos_process_historical_near_far/resampled_wrf_to_xanthos_monthly_tempDegC_degC_1979_01_to_2099_12.npy",
                       "C:/Z/models/00tests/xanthos_im3_test/output_wrf_to_xanthos_process_historical_near_far/resampled_wrf_to_xanthos_monthly_tempDegCmin_degC_1979_01_to_2099_12.npy",
                       "C:/Z/models/00tests/xanthos_im3_test/output_wrf_to_xanthos_process_historical_near_far/resampled_wrf_to_xanthos_monthly_v_m s-1_1979_01_to_2099_12.npy")
base_year_month_start_i = "1971_01"
target_year_month_start_i = "1979_01"
out_dir_i = "C:/Z/models/00tests/xanthos_im3_test/output_wrf_to_xanthos_process_historical_near_far_us_expand_global"

# Combine US Figures
comb_df_us <- tibble::tibble()
comb_df_global <- tibble::tibble()

# Expanded xanthos python data
for(i in 1:length(base_npy_list)){

  base_npy_i = base_npy_list[[i]]; base_npy_i
  target_npy_i = target_npy_list[[i]]; target_npy_i

  im3components::xanthos_npy_expand(
    base_npy =base_npy_i,
    base_year_month_start = base_year_month_start_i,
    target_npy = target_npy_i,
    target_year_month_start = target_year_month_start_i,
    out_dir = out_dir_i
  ) -> expanded_df

  # base_npy =base_npy_i
  # base_year_month_start = base_year_month_start_i
  # target_npy = target_npy_i
  # target_year_month_start = target_year_month_start_i
  # out_dir = out_dir_i

  # comb_df_us
  if(T){
    # Compare processed data
    library(dplyr); library(ggplot2)
    nersc_grid_cells <- unique((expanded_df$target_df %>%
                                  dplyr::bind_cols(im3components::data_coordinates_xanthos_reference) %>%
                                  dplyr::filter(complete.cases(.)))$gridid); nersc_grid_cells

    # Units
    units_i = gsub(".{23}$","",gsub(".*_monthly_","",target_npy_i)); units_i

    # Compare US aggregated data
    expanded_df$base_df %>%
      dplyr::bind_cols(im3components::data_coordinates_xanthos_reference) %>%
      dplyr::filter(gridid %in% nersc_grid_cells) %>%
      dplyr::select(-lon,-lat,-gridid,-lonid,-latid)%>%
      tidyr::gather(key="year_month",value="value") %>%
      dplyr::mutate(param="xanthos_base",
                    units=units_i) %>%
      dplyr::group_by(year_month,param, units)%>%
      dplyr::summarize(value=mean(value,na.rm=T)) %>%
      dplyr::ungroup()-> base_df_us; base_df_us

    expanded_df$target_df %>%
      dplyr::bind_cols(im3components::data_coordinates_xanthos_reference) %>%
      dplyr::filter(gridid %in% nersc_grid_cells) %>%
      dplyr::select(-lon,-lat,-gridid,-lonid,-latid)%>%
      tidyr::gather(key="year_month",value="value") %>%
      dplyr::mutate(param="wrf",
                    units=units_i) %>%
      dplyr::group_by(year_month,param, units)%>%
      dplyr::summarize(value=mean(value,na.rm=T)) %>%
      dplyr::ungroup() -> target_df_us; target_df_us

    expanded_df$out_df %>%
      dplyr::bind_cols(im3components::data_coordinates_xanthos_reference) %>%
      dplyr::filter(gridid %in% nersc_grid_cells) %>%
      dplyr::select(-lon,-lat,-gridid,-lonid,-latid)%>%
      tidyr::gather(key="year_month",value="value") %>%
      dplyr::mutate(param="combined",
                    units=units_i) %>%
      dplyr::group_by(year_month,param, units)%>%
      dplyr::summarize(value=mean(value,na.rm=T)) %>%
      dplyr::ungroup() -> out_df_us; out_df_us

    # Combine
    comb_df_us_i <- base_df_us %>%
      dplyr::bind_rows(target_df_us) %>%
      dplyr::bind_rows(out_df_us); comb_df_us_i

    comb_df_us <- comb_df_us %>%
      dplyr::bind_rows(comb_df_us_i)
  }

  # comb_df_global
  if(T){
    # Compare processed data
    library(dplyr); library(ggplot2)

    # Units
    units_i = gsub(".{23}$","",gsub(".*_monthly_","",target_npy_i)); units_i

    # Compare US aggregated data
    expanded_df$base_df %>%
      tidyr::gather(key="year_month",value="value") %>%
      dplyr::mutate(param="xanthos_base",
                    units=units_i) %>%
      dplyr::group_by(year_month,param, units)%>%
      dplyr::summarize(value=mean(value,na.rm=T)) %>%
      dplyr::ungroup()-> base_df_global; base_df_global

    expanded_df$target_df %>%
      tidyr::gather(key="year_month",value="value") %>%
      dplyr::mutate(param="wrf",
                    units=units_i) %>%
      dplyr::group_by(year_month,param, units)%>%
      dplyr::summarize(value=mean(value,na.rm=T)) %>%
      dplyr::ungroup() -> target_df_global; target_df_global

    expanded_df$out_df %>%
      tidyr::gather(key="year_month",value="value") %>%
      dplyr::mutate(param="combined",
                    units=units_i) %>%
      dplyr::group_by(year_month,param, units)%>%
      dplyr::summarize(value=mean(value,na.rm=T)) %>%
      dplyr::ungroup() -> out_df_global; out_df_global

    # Combine
    comb_df_global_i <- base_df_global %>%
      dplyr::bind_rows(target_df_global) %>%
      dplyr::bind_rows(out_df_global); comb_df_global_i

    comb_df_global <- comb_df_global %>%
      dplyr::bind_rows(comb_df_global_i)
  }
}

# Combined Data for US
comb_df_us
comb_df_global

# Plot
p <- ggplot() +
  geom_line(data=comb_df_us,aes(x=year_month, y=value, group=param, color=param), lwd=1, alpha=0.5) +
  scale_color_manual(values=c("red","green","blue")) +
  facet_wrap(units~., scales="free_y") +
  theme_bw() + xlab(NULL)+ylab(NULL)+
  ggtitle(paste0("US WRF Xanthos parameters: ",unique(comb_df_us$year_month)[1]," to ",unique(comb_df_us$year_month)[length(unique(comb_df_us$year_month))]))+
  ggplot2::theme_bw() +
  ggplot2::geom_vline(xintercept="2020_01", linetype="dashed",color = "red", size=0.5) +
  ggplot2::geom_vline(xintercept="2060_01", linetype="dashed",color = "red", size=0.5) +
  ggplot2::theme(axis.text.x=ggplot2::element_text(angle=90,vjust=0.5)) +
  ggplot2::scale_x_discrete(limits=unique(comb_df_global$year_month),
                            breaks=unique(comb_df_global$year_month)[grepl("0_01|5_01",unique(comb_df_global$year_month))]); p
ggsave(p,filename=paste0(out_dir_i,"/wrf_xanthos_compare_us.png"),width=15, height=7)

# Plot
p <- ggplot() +
  geom_line(data=comb_df_global,aes(x=year_month, y=value, group=param, color=param), lwd=1, alpha = 0.5) +
  scale_color_manual(values=c("red","green","blue")) +
  facet_wrap(units~., scales="free_y") +
  theme_bw() + xlab(NULL)+ylab(NULL)+
  ggtitle(paste0("Global WRF Xanthos parameters: ",unique(comb_df_us$year_month)[1]," to ",unique(comb_df_us$year_month)[length(unique(comb_df_us$year_month))])) +
  ggplot2::theme_bw() +
  ggplot2::geom_vline(xintercept="2020_01", linetype="dashed",color = "red", size=0.5) +
  ggplot2::geom_vline(xintercept="2060_01", linetype="dashed",color = "red", size=0.5) +
  ggplot2::theme(axis.text.x=ggplot2::element_text(angle=90,vjust=0.5)) +
  ggplot2::scale_x_discrete(limits=unique(comb_df_global$year_month),
                            breaks=unique(comb_df_global$year_month)[grepl("0_01|5_01",unique(comb_df_global$year_month))]); p
ggsave(p,filename=paste0(out_dir_i,"/wrf_xanthos_compare_global.png"),width=15, height=7)


# .................
# Run Xanthos
# ................


# .................
# Xanthos to gcam xml
# ................

xanthos_runoff_csv = "C:/Z/models/00tests/xanthosGlobalRuns/Basin_runoff_km3peryear_pm_abcd_mrtm_noresm1-m_rcp4p5_1950_2100.csv"
gcamdata_folder = "C:/Z/models/GCAMVersions/gcam-usa-im3/input/gcamdata"
out_dir = NULL

xanthos_to_gcam_xml(xanthos_runoff_csv=xanthos_runoff_csv,
                    gcamdata_folder=gcamdata_folder)
