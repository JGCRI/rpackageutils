library(rpackageutils)
library(dplyr)

data <- "C:/Z/models/00tests/xanthosGlobalRuns/Basin_runoff_km3peryear_pm_abcd_mrtm_noresm1-m_rcp8p5_1950_2099.csv"

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
