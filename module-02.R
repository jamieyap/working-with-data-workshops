# Package for data manipulation tasks
library(dplyr)
# Package for estimating Generalized Linear Mixed Models (GLMMs)
library(lme4)

# Specify location of input data files
path_pns_input_data <- Sys.getenv("path_pns_input_data")
# Specify location of output data files
path_pns_output_data <- Sys.getenv("path_pns_output_data")

# quit_dates_final.csv is a file containing masterlist of all participant IDs
# Note that data from individuals who should be excluded from all data analysis (i.e., exclude = 1)
# will NOT be present in ALL curated data files
dat_quit_dates <- read.csv(file.path(path_pns_input_data, "quit_dates_final.csv"), header = TRUE, na.strings = "")
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

###############################################################################
# MODULE 1
#
# Work with merged file (merged.csv): identify columns and rows which 
# correspond to responses to EMA questionnaires and extract them into their
# own data file; identify columns and rows which will be used as the basis of 
# the curated smoking outcome variables
###############################################################################

# Here, we will show you how to select the correct rows and columns within
# the merged.csv file to end up with the 10 individual data files enumerated above

# -----------------------------------------------------------------------------
# First, extract rows which come from the following types of questionnaires:
# - Post-Quit Already Slipped EMA questionnaire
# - Post-Quit Random EMA questionnaire
# - Post-Quit Urge EMA questionnaire
# - Post-Quit About to Slip Part One EMA questionnaire
# - Post-Quit About to Slip Part Two EMA questionnaire
# -----------------------------------------------------------------------------

# How do we get from merged.csv to post_quit_already_slipped_ema.csv?
dat_postquit_alreadyslipped_ema <- dat_big_merged %>%
  # Select columns corresponding to participant ID and time variables; 
  # select columns corresponding to items in Post-Quit Already Slipped EMA questionnaire
  select(id:time_unixts, postquit_alreadyslipped_item_1:postquit_alreadyslipped_item_70) %>%
  # Select those rows corresponding to when Post-Quit Already Slipped EMA was launched
  filter(assessment_type == "Post-Quit Already Slipped") %>%
  # Remember: order according to increasing participant ID
  # and within each participant ID, according to increasing time 
  arrange(id, time_unixts)

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

# How do we get from merged.csv to post_quit_urge_ema.csv?
dat_postquit_urge_ema <- dat_big_merged %>%
  # Select columns corresponding to participant ID and time variables; 
  # select columns corresponding to items in Post-Quit Urge EMA questionnaire
  select(id:time_unixts, postquit_urge_item_1:postquit_urge_item_67) %>%
  # Select those rows corresponding to when Post-Quit Urge EMA was launched
  filter(assessment_type == "Post-Quit Urge") %>%
  # Remember: order according to increasing participant ID
  # and within each participant ID, according to increasing time 
  arrange(id, time_unixts)

# How do we get from merged.csv to post_quit_about_to_slip_part_one_ema.csv?
dat_postquit_partone_ema <- dat_big_merged %>%
  # Select columns corresponding to participant ID and time variables; 
  # select columns corresponding to items in Post-Quit About to Slip Part One EMA questionnaire  
  select(id:time_unixts, postquit_partone_item_1:postquit_partone_item_64) %>%
  # Select those rows corresponding to when Post-Quit About to Slip Part One EMA was launched
  filter(assessment_type == "Post-Quit About to Slip Part One") %>%
  # Remember: order according to increasing participant ID
  # and within each participant ID, according to increasing time 
  arrange(id, time_unixts)

# How do we get from merged.csv to post_quit_about_to_slip_part_two_ema.csv?
dat_postquit_parttwo_ema <- dat_big_merged %>%
  # Select columns corresponding to participant ID and time variables; 
  # select columns corresponding to items in Post-Quit About to Slip Part Two EMA questionnaire
  select(id:time_unixts, postquit_parttwo_item_1:postquit_parttwo_item_74) %>%
  # Select those rows corresponding to when Post-Quit About to Slip Part Two EMA was launched
  filter(assessment_type == "Post-Quit About to Slip Part Two") %>%
  # Remember: order according to increasing participant ID
  # and within each participant ID, according to increasing time 
  arrange(id, time_unixts)

# -----------------------------------------------------------------------------
# Second, extract rows which come from the following types of questionnaires:
# - Pre-Quit Random EMA questionnaire
# - Pre-Quit Urge EMA questionnaire
# - Pre-Quit Smoking Part One EMA questionnaire
# - Pre-Quit Smoking Part Two EMA questionnaire
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

# How do we get from merged.csv to pre_quit_urge_ema.csv?
dat_prequit_urge_ema <- dat_big_merged %>%
  # Select columns corresponding to participant ID and time variables; 
  # select columns corresponding to items in Pre-Quit Urge EMA questionnaire
  select(id:time_unixts, prequit_urge_item_1:prequit_urge_item_67) %>%
  # Select those rows corresponding to when Pre-Quit Urge EMA was launched
  filter(assessment_type == "Pre-Quit Urge") %>%
  # Remember: order according to increasing participant ID
  # and within each participant ID, according to increasing time 
  arrange(id, time_unixts)

# How do we get from merged.csv to pre_quit_smoking_part_one_ema.csv?
dat_prequit_partone_ema <- dat_big_merged %>%
  # Select columns corresponding to participant ID and time variables; 
  # select columns corresponding to items in Pre-Quit Smoking Part One EMA questionnaire
  select(id:time_unixts, prequit_partone_item_1:prequit_partone_item_64) %>%
  # Select those rows corresponding to when Pre-Quit Smoking Part One EMA was launched
  filter(assessment_type == "Pre-Quit Smoking Part One") %>%
  # Remember: order according to increasing participant ID
  # and within each participant ID, according to increasing time 
  arrange(id, time_unixts)

# How do we get from merged.csv to pre_quit_smoking_part_two_ema.csv?
dat_prequit_parttwo_ema <- dat_big_merged %>%
  # Select columns corresponding to participant ID and time variables; 
  # select columns corresponding to items in Pre-Quit Smoking Part Two EMA questionnaire
  select(id:time_unixts, prequit_parttwo_item_1:prequit_parttwo_item_69) %>%
  # Select those rows corresponding to when Pre-Quit Smoking Part Two EMA was launched
  filter(assessment_type == "Pre-Quit Smoking Part Two") %>%
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

# NOTE: By now, we have separated merged.csv into 10 individual data files.
# The correspondence between the names of the data files we created above and 
# the stand-alone csv files are enumerated below.
# So, for example, if you executed the following line of code
#
#     write.csv(dat_random_ema, "post_quit_random_ema.csv", row.names = FALSE, na = "")
#
# An identical copy of post_quit_random_ema.csv will be generated

# - post_quit_already_slipped_ema.csv : dat_postquit_alreadyslipped_ema
# - post_quit_random_ema.csv : dat_postquit_random_ema
# - post_quit_urge_ema.csv : dat_postquit_urge_ema
# - post_quit_about_to_slip_part_one_ema.csv : dat_postquit_partone_ema
# - post_quit_about_to_slip_part_two_ema.csv : dat_postquit_parttwo_ema
# - pre_quit_random_ema.csv : dat_prequit_random_ema
# - pre_quit_urge_ema.csv : dat_prequit_urge_ema
# - pre_quit_smoking_part_one_ema.csv : dat_prequit_partone_ema
# - pre_quit_smoking_part_two_ema.csv : dat_prequit_parttwo_ema
# - smoking.csv : dat_smoking


###############################################################################
# MODULE 2
#
# Work with Random EMA data files (dat_prequit_random_ema and 
# dat_postquit_random_ema)
#
###############################################################################

# -----------------------------------------------------------------------------
# If we were to conduct an investigation of smoking behavior during the 
# post-quit period, can we identify which rows to include/exclude in our
# analysis?
# -----------------------------------------------------------------------------

subset_dat_postquit_random_ema <- dat_postquit_random_ema %>%
  # Exclude rows corresponding to EMAs having no response to any item
  filter(with_any_response == 1) %>%
  # Exclude rows corresponding to EMAs launched before Quit Date
  filter(use_as_postquit == 1) %>%
  # Select only the columns you will need;
  # this process will result in a smaller data file
  select(id:time_unixts, postquit_random_item_8) %>%
  rename(selfeff = postquit_random_item_8)

subset_dat_prequit_random_ema <- dat_prequit_random_ema %>%
  # Exclude rows corresponding to EMAs having no response to any item
  filter(with_any_response == 1) %>%
  # Exclude rows corresponding to EMAs launched before Quit Date
  filter(use_as_postquit == 1) %>%
  # Select only the columns you will need;
  # this process will result in a smaller data file
  select(id:time_unixts, prequit_random_item_8) %>%
  rename(selfeff = prequit_random_item_8)

# -----------------------------------------------------------------------------
# Merge the two resulting smaller data files into a single data file
# named dat_analysis. We will work with dat_analysis from now on; 
# from here onward, we will perform more data manipulation tasks to 
# get dat_analysis into a structure that lme4 can recognize
# -----------------------------------------------------------------------------

# rbind simply stacks the two data files on top of each other
# rbind will work only if both data files have identical column names
# Hence, there was a need to call the rename() function above prior
# to calling rbind()
dat_analysis <- rbind(subset_dat_postquit_random_ema, subset_dat_prequit_random_ema)

# Remember: order according to increasing participant ID
# and within each participant ID, according to increasing time 
dat_analysis <- dat_analysis %>% arrange(id, time_unixts)

# -----------------------------------------------------------------------------
# Using dat_analysis:
# - Calculate number of hours elapsed between the current EMA and the next EMA
# - Calculate number of days elapsed since the beginning of post-quit period
# -----------------------------------------------------------------------------

# Remember: The calculations below will be incorrect if dat_analysis has not 
# yet been ordered according to increasing participant ID
# and within each participant ID, according to increasing time 

dat_analysis <- dat_analysis %>%
  # The group_by() function is needed to ensure that we do not accidentally 
  # use data from another participant to calculate lagged time variables 
  # for a particular participant
  group_by(id) %>%
  # When did the participant begin responding to the next EMA?
  # If there is no EMA that follows the current EMA, we will set time_unixts_plusone 
  # we will initially set time_unixts_plusone to a missing value
  mutate(time_unixts_plusone = c(tail(time_unixts, n=-1), NA)) %>%
  # If there is no EMA that follows the current EMA, we will set time_unixts_plusone 
  # to be equal to end_study_unixts
  mutate(time_unixts_plusone = if_else(is.na(time_unixts_plusone), end_study_unixts, time_unixts_plusone)) %>%
  # How many seconds elapsed between the current and next EMA?
  mutate(num_secs_elapsed_since_previous_ema = time_unixts_plusone - time_unixts) %>%
  # Now, make a conversion from seconds to hours
  mutate(num_hrs_elapsed_since_previous_ema = num_secs_elapsed_since_previous_ema/(60*60))

dat_analysis <- dat_analysis %>%
  # How many seconds elapsed between the current EMA and Quit Date?
  # Note that we consider 4AM on Quit Date to be the time when participants
  # quit smoking in the PNS study
  mutate(num_secs_elapsed_since_quit = time_unixts - quit_unixts) %>%
  # Now, make a conversion from seconds to hours
  mutate(num_hrs_elapsed_since_quit = num_secs_elapsed_since_quit/(60*60)) %>%
  # Now, make a conversion from hours to days
  mutate(num_days_elapsed_since_quit = num_hrs_elapsed_since_quit/24)

# Create a new time variable that captures the number of days elapsed since 12AM of start of study
dat_analysis <- dat_analysis %>%
  mutate(num_secs_elapsed_since_start_study = time_unixts - start_study_unixts) %>%
  mutate(num_hrs_elapsed_since_start_study = num_secs_elapsed_since_start_study/(60*60)) %>%
  mutate(num_days_elapsed_since_start_study = num_hrs_elapsed_since_start_study/24)
