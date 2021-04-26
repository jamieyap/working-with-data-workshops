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

# Remember: order according to increased participant ID
# and within each participant ID, according to increased time 
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
  # If there is no EMA that follows the current EMA,  
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

###############################################################################
# MODULE 3
#
# Thus far, we have not worked with dat_smoking. In this module,
# we will create a visual snapshot of the participant using dat_smoking.
###############################################################################

# -----------------------------------------------------------------------------
# Parameters that may be adjusted
# -----------------------------------------------------------------------------

choose_idx <- 38 # e.g., if this is 2, then we are selecting the 2nd participant ID in the list
xlim_min <- 0 # Minimum number of days since 12AM of start of study date
xlim_max <- 28 # Maximum number of days since 12AM of start of study date

# -----------------------------------------------------------------------------
# Do not adjust plotting parameters below this line
# -----------------------------------------------------------------------------

if(xlim_min > xlim_max){
  print("Error: xlim_min must be less than or equal to xlim_max")
}

# What are the unique participant IDs which are present in dat_smoking?
participant_ids <- unique(dat_smoking$id)

# Let's visualize the data for one particular participant
current_participant <- participant_ids[choose_idx]

# Take rows corresponding to this particular participant
plotdat_participant <- dat_smoking %>% filter(id == current_participant)

# Create a new time variable (just like we did before)
# that captures the number of days elapsed since 12AM of start of study
plotdat_participant <- plotdat_participant %>%
  mutate(num_secs_elapsed_since_start_study = time_unixts - start_study_unixts) %>%
  mutate(num_hrs_elapsed_since_start_study = num_secs_elapsed_since_start_study/(60*60)) %>%
  mutate(num_days_elapsed_since_start_study = num_hrs_elapsed_since_start_study/24)  %>%
  mutate(roundeddown_num_days_elapsed_since_start_study = floor(num_days_elapsed_since_start_study)) 

# Layer on each component of the plot
plot(-1, xaxt = "n", yaxt = "n", 
     xlab = "Day Since Start of Study ('0' represents midnight on the date when study began)", ylab = "", 
     xlim = c(xlim_min, xlim_max), ylim = c(0,0.3), 
     cex.lab = 2, 
     frame.plot = FALSE)

if(xlim_max - xlim_min <=7){
  # Use half-day increments
  axis(1, at = seq(xlim_min, xlim_max + 1 , 0.5), cex.axis = 2, lwd.ticks = 2, gap.axis = 1.2)
}else{
  # Use increments of 7 days
  axis(1, at = seq(xlim_min, xlim_max + 1 , 7), cex.axis = 2, lwd.ticks = 2, gap.axis = 1.2)
}

# Identify which rows correspond to each kind of EMA
plotdat_random <- plotdat_participant %>% filter(assessment_type == "Post-Quit Random")
plotdat_urge <- plotdat_participant %>% filter(assessment_type == "Post-Quit Urge")
plotdat_already_slipped <- plotdat_participant  %>% filter(assessment_type == "Post-Quit Already Slipped")
plotdat_part_one <- plotdat_participant %>% filter(assessment_type == "Post-Quit About to Slip Part One")
plotdat_part_two <- plotdat_participant %>% filter(assessment_type == "Post-Quit About to Slip Part Two")

abline(v = plotdat_random$num_days_elapsed_since_start_study, lty = 2, lwd = 2, col = "red")

# Note that if the number of rows in the plot data is equal to zero,
# then no new points will be added to the existing plot; no error message will be displayed
points(plotdat_random$num_days_elapsed_since_start_study, rep(0.1, nrow(plotdat_random)), pch = 17, cex = 2, col = "black")
points(plotdat_urge$num_days_elapsed_since_start_study, rep(0.1, nrow(plotdat_urge)), pch = 19, cex = 2, col = "orange")
points(plotdat_already_slipped$num_days_elapsed_since_start_study, rep(0.1, nrow(plotdat_already_slipped)), pch = 19, cex = 2, col = "seagreen")
points(plotdat_part_one$num_days_elapsed_since_start_study, rep(0.1, nrow(plotdat_part_one)), pch = 19, cex = 2, col = "lightblue")
points(plotdat_part_two$num_days_elapsed_since_start_study, rep(0.1, nrow(plotdat_part_two)), pch = 19, cex = 2, col = "blue")

# Identify which rows correspond to each kind of EMA
plotdat_random <- plotdat_participant %>% filter(assessment_type == "Pre-Quit Random")
plotdat_urge <- plotdat_participant %>% filter(assessment_type == "Pre-Quit Urge")
plotdat_part_one <- plotdat_participant %>% filter(assessment_type == "Pre-Quit About to Slip Part One")
plotdat_part_two <- plotdat_participant %>% filter(assessment_type == "Pre-Quit About to Slip Part Two")

abline(v = plotdat_random$num_days_elapsed_since_start_study, lty = 2, lwd = 2, col = "red")

# Note that if the number of rows in the plot data is equal to zero,
# then no new points will be added to the existing plot; no error message will be displayed
points(plotdat_random$num_days_elapsed_since_start_study, rep(0.1, nrow(plotdat_random)), pch = 17, cex = 2, col = "black")
points(plotdat_urge$num_days_elapsed_since_start_study, rep(0.1, nrow(plotdat_urge)), pch = 19, cex = 2, col = "orange")
points(plotdat_already_slipped$num_days_elapsed_since_start_study, rep(0.1, nrow(plotdat_already_slipped)), pch = 19, cex = 2, col = "seagreen")
points(plotdat_part_one$num_days_elapsed_since_start_study, rep(0.1, nrow(plotdat_part_one)), pch = 19, cex = 2, col = "lightblue")
points(plotdat_part_two$num_days_elapsed_since_start_study, rep(0.1, nrow(plotdat_part_two)), pch = 19, cex = 2, col = "blue")

legend("topright", c("Random", "Urge", "Already Slipped", "Part One", "Part Two"),
       col = c("black", "orange", "seagreen", "lightblue", "blue"),
       pch = c(17, 19, 19, 19, 19), pt.cex = rep(2,5), cex = 1.2)

