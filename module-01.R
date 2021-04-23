# Package for data manipulation tasks
library(dplyr)
# Package for estimating Generalized Linear Mixed Models (GLMMs)
library(lme4)

# Specify location of input data files
path_pns_input_data <- Sys.getenv("path_pns_input_data")
# Specify location of output data files
path_pns_output_data <- Sys.getenv("path_pns_output_data")

# merged.csv is a file containing information only from records attributed to Event C
# this particular csv file merges several curated data files (named below) into one data file
# - post_quit_already_slipped_ema.csv : curated data file containing variables only from the Post-Quit Already Slipped EMA questionnaire
# - post_quit_random_ema.csv : curated data file containing variables only from the Post-Quit Random EMA questionnaire
# - post_quit_urge_ema.csv : curated data file containing variables only from the Post-Quit Urge EMA questionnaire
# - post_quit_about_to_slip_part_one_ema.csv : curated data file containing variables only from the Post-Quit About to Slip Part One EMA questionnaire
# - post_quit_about_to_slip_part_two_ema.csv : curated data file containing variables only from the Post-Quit About to Slip Part Two EMA questionnaire
# - pre_quit_random_ema.csv : curated data file containing variables only from the Pre-Quit Random EMA questionnaire
# - pre_quit_urge_ema.csv : curated data file containing variables only from the Pre-Quit Urge EMA questionnaire
# - pre_quit_smoking_part_one_ema.csv : curated data file containing variables only from the Pre-Quit Smoking Part One EMA questionnaire
# - pre_quit_smoking_part_two_ema.csv : curated data file containing variables only from the Pre-Quit Smoking Part Two EMA questionnaire
# - smoking.csv : curated data file containing variables that you would use to construct the smoking outcome
dat_big_merged <- read.csv(file.path(path_pns_input_data, "merged.csv"), header = TRUE, na.strings = "")

# quit_dates_final.csv is a file containing masterlist of all participant IDs
# Note that data from individuals who should be excluded from all data analysis (i.e., exclude = 1)
# will NOT be present in ALL curated data files
dat_quit_dates <- read.csv(file.path(path_pns_input_data, "quit_dates_final.csv"), header = TRUE, na.strings = "")

###############################################################################
# MODULE 1
#
# Work with merged file (merged.csv): identify columns and rows which 
# correspond to responses to EMA questionnaires and extract them into their
# own data file; identify columns and rows which will be used as the basis of 
# the curated smoking outcome variables
###############################################################################

# Here, we will show you how to select the correct rows and columns within
# the merged.csv file to end up with the following individual data files:

#- post_quit_random_ema.csv
#- pre_quit_random_ema.csv

# -----------------------------------------------------------------------------
# First, extract rows which come from the Post-Quit Random EMA questionnaire
# -----------------------------------------------------------------------------

# How do we get from merged.csv to post_quit_random_ema.csv?
dat_postquit_random_ema <- dat_big_merged %>%
  # Select columns corresponding to participant ID and time variables; 
  # select columns corresponding to items in Post-Quit Random EMA questionnaire  
  select(id:time_unixts, postquit_random_item_1:postquit_random_item_67) %>%
  # Select those rows corresponding to when Post-Quit Random EMA was launched
  filter(assessment_type == "Post-Quit Random") %>%
  # Remember: order according to increasing participant ID
  # and within each participant ID, according to increasing time 
  arrange(id, time_unixts)

# -----------------------------------------------------------------------------
# Second, extract rows which come from the Pre-Quit Random EMA questionnaire
# -----------------------------------------------------------------------------

# How do we get from merged.csv to pre_quit_random_ema.csv?
dat_prequit_random_ema <- dat_big_merged %>%
  # Select columns corresponding to participant ID and time variables; 
  # select columns corresponding to items in Pre-Quit Random EMA questionnaire  
  select(id:time_unixts, prequit_random_item_1:prequit_random_item_67) %>%
  # Select those rows corresponding to when Pre-Quit Random EMA was launched
  filter(assessment_type == "Pre-Quit Random") %>%
  # Remember: order according to increasing participant ID
  # and within each participant ID, according to increasing time 
  arrange(id, time_unixts)

# -----------------------------------------------------------------------------
# Third, extract rows which would be the basis of smoking outcome variables
# -----------------------------------------------------------------------------

# How to we get from merged.csv to smoking.csv?
dat_smoking <- dat_big_merged %>% 
  select(id:smoking_delta_minutes) %>%
  # We consider the "last assessment" to refer to either of the two situations below:
  # 1. participant-initiated EMAs (any type) having with_any_response=0 or with_any_response=1
  # 2. Random EMA having with_any_response=1
  # In other words, the only situation not included in the "last assessment"
  # are those Random EMAs which the participant did not provide any response
  # All rows in merged.csv having
  filter(!is.na(ema_order)) %>%
  # Remember: order according to increasing participant ID
  # and within each participant ID, according to increasing time 
  arrange(id, time_unixts)

# NOTE: By now, we have extracted smaller data files from merged.csv using 
# the procedure above. The correspondence between the names of the data files 
# we created above and the stand-alone csv files are enumerated below.
#
# - post_quit_random_ema.csv : dat_postquit_random_ema
# - pre_quit_random_ema.csv : dat_prequit_random_ema
# - smoking.csv : dat_smoking
#
# So, for example, if you executed the following line of code
#
#     write.csv(dat_random_ema, "post_quit_random_ema.csv", row.names = FALSE, na = "")
#
# An identical copy of post_quit_random_ema.csv will be generated
#

