library(dplyr)
library(lme4)

sessionInfo()

path_pns_input_data <- "C:/Users/jamieyap/Desktop/input_data"
path_pns_output_data <- "C:/Users/jamieyap/Desktop/output_data"

dat_quit_dates <- read.csv(file.path(path_pns_input_data, "quit_dates_final.csv"), header = TRUE, na.strings = "")
dat_big_merged <- read.csv(file.path(path_pns_input_data, "merged.csv"), header = TRUE, na.strings = "")
ema_item_names <- read.csv(file.path(path_pns_input_data, "ema_item_names.csv"), header = TRUE, na.strings = "")
