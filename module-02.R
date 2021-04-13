library(dplyr)
library(lme4)

path_pns_input_data <- Sys.getenv("path_pns_input_data")
path_pns_output_data <- Sys.getenv("path_pns_output_data")

dat_quit_dates <- read.csv(file.path(path_pns_input_data, "quit_dates_final.csv"), header = TRUE, na.strings = "")
ema_item_names <- read.csv(file.path(path_pns_input_data, "ema_item_names.csv"), header = TRUE, na.strings = "")
dat_big_merged <- read.csv(file.path(path_pns_input_data, "merged.csv"), header = TRUE, na.strings = "")

# Now, begin working with the data
# Arrange rows according to participant ID and time
dat_big_merged <- dat_big_merged %>% arrange(id, time_unixts)

# Select only those rows belonging to the post-quit period
dat_big_merged_postquit <- dat_big_merged %>% filter(use_as_postquit == 1)

# How many days after Quit Day?
# days_since_quit is equal to zero if an EMA occurred on Quit Day
# days_since_quit is equal to 1, 2, 3, ... if an EMA occurred one, two, three days after Quit Day 
dat_big_merged_postquit <- dat_big_merged_postquit %>%
  mutate(days_since_quit = (time_unixts - quit_unixts)/(60*60*24)) %>%
  mutate(days_since_quit = floor(days_since_quit))

# Identify which item corresponds to responses to the question 'I feel enthusiastic'
# Note that we will still include all rows corresponding to Post-Quit Already Slipped EMA
# even if the question was posed in past tense, i.e., 'I felt enthusiastic'.
# Including responses from Post-Quit Already Slipped EMA would need to be
# reconsidered depending on the kind of analysis being performed
dat_big_merged_postquit <- dat_big_merged_postquit %>%
  mutate(enthusiastic  = case_when(
    assessment_type == "Post-Quit Random" ~ postquit_random_item_9,
    assessment_type == "Post-Quit Urge" ~ postquit_urge_item_9,
    assessment_type == "Post-Quit About to Slip Part One" ~ postquit_partone_item_8,
    assessment_type == "Post-Quit About to Slip Part Two" ~ postquit_parttwo_item_9,
    assessment_type == "Post-Quit Random" ~ postquit_random_item_9,
    assessment_type == "Post-Quit Already Slipped EMA" ~ postquit_alreadyslipped_item_9,
    # Otherwise, set the value of enthusiastic to a missing value
    # Aside from NA_integer_, other options are NA_character, NA_real_, NA_complex_ etc
    # We selected NA_integer_ out of all these options because responses to the above
    # question are in the form of whole numbers without decimal places
    TRUE ~ NA_integer_  
  ))

# Get all participant IDs from our masterlist (aka, the file dat_quit_dates)
all_participant_ids <- dat_quit_dates$id
# Count total number of participants in that list
total_participants <- length(all_participant_ids)

# Step 1: Create a skeleton dataset in long format
# We will be creating a couple of new datasets which we will merge with dat1
dat1 <- data.frame(id = rep(all_participant_ids, each = 21), days_since_quit = rep(0:20, times = total_participants))

# Step 2: Grab columns from our masterlist
# The columns exclude and sensitivity will help us determine
# which participants to exclude from all data analysis
# and to include in main analysis/sensitivity analysis
dat2 <- dat_quit_dates %>% select(id, exclude, sensitivity)

# We merge dat1 and dat2
# Notice the use of left_join -- we wish to retain all rows in dat1
# and then slot in information from dat2 into each row within dat1
dat_new <- left_join(x = dat1, y = dat2, by = c("id"))

# Drop rows corresponding to those participants which will be excluded from all data analysis
dat_new <- dat_new %>% filter(exclude == 0)

# Step 3: We illustrate a subtle point.
#
# Several new variables were created using dat_big_merged_postquit:
# tot_ema_launched: count the total number of EMAs launched
# n_missing_enthusiastic: Out of those EMAs in tot_launched, how many had
# a missing value in enthusiastic
# n_complete_enthusiastic: Out of those EMAs in tot_launched, how many had
# a non-missing value in enthusiastic
#
# It is possible that the software is not able to launch an EMA
# due to the fact that the smartphone may be switched off, or due to 
# an unanticipated tech issue. Hence, after dat3 is merged with dat_new, 
# those days will be represented by a missing value in
# tot_ema_launched, n_missing_enthusiastic, n_complete_enthusiastic
# We then must replace the missing value in tot_ema_launched with a zero ('0') 
# to indicate that there were zero successfully launched EMA for those days.
# When there are no EMAs launched on a particular day, we will still
# retain a missing value for the 
# n_missing_enthusiastic and n_complete_enthusiastic variables

dat3 <- dat_big_merged_postquit %>%
  group_by(id, days_since_quit) %>%
  summarise(tot_ema_launched = n(),
            n_missing_enthusiastic = sum(is.na(enthusiastic)),
            n_complete_enthusiastic = sum(!is.na(enthusiastic)))

# We merge dat3 and dat_new
# Notice the use of left_join -- we wish to retain all rows indat_new
# and then slot in information from dat3 into each row within dat_new
dat_new <- left_join(x = dat_new, y = dat3, by = c("id", "days_since_quit"))
dat_new <- dat_new %>% mutate(tot_ema_launched = replace(tot_ema_launched, is.na(tot_ema_launched), 0))

###############################################################################
# This is just prior to where we left off in Module 01 (before creating the
# datasets dat_main_analysis and dat_sensitivity_analysis).
# Module 02 begins here
###############################################################################

# Define some functions which will be useful in the analyses
# We prefer to not use R's built-in mean and max functions
# to avoid undesirable behavior when taking the mean or max
# of an array in cases when all elements of the array are missing.
# In the PNS study, this can happen if we attempt to take
# a mean or max of a particular variable across all 
# EMAs launched within a day, but all such EMAs were ignored
# by the participant.

MyMean <- function(x){
  # Input: x is an array of numbers, e.g., (1, 4, 5, 2, 2)
  # About: MyMean calculates the mean of x only if 
  # x has at least one non-missing value and returns 'NA'
  # if all elements of x are missing
  
  # How many elements of x are not missing?
  count_not_missing <- sum(!is.na(x))
  
  if(count_not_missing==0){
    # In the first case, all elements of x are missing
    output <- NA
  }else{
    # In the second case, x may have missing elements,
    # but x has at least one non-missing value
    output <- mean(x, na.rm = TRUE)
  }
  
  return(output)
}

MyMax <- function(x){
  # Input: x is an array of numbers, e.g., (1, 4, 5, 2, 2)
  # About: MyMax calculates the maximum of x only if 
  # x has at least one non-missing value and returns 'NA'
  # if all elements of x are missing
  
  # How many elements of x are not missing?
  count_not_missing <- sum(!is.na(x))
  
  if(count_not_missing==0){
    # In the first case, all elements of x are missing
    output <- NA
  }else{
    # In the second case, x may have missing elements,
    # but x has at least one non-missing value
    output <- max(x, na.rm = TRUE)
  }
  
  return(output)
}

# Step 4:
#
# Several new variables were created using dat_big_merged_postquit. We note that
# these calculations only utilize available data to calculate means and maximums
# If there is no data (occurs when no rating for the question was provided)
# then the value of the variables created will be missing.
#
# any_smoking: use the variable smoking_indicator to check whether there was 
# a reported occurrence of smoking
# mean_enthusiastic: use the variable enthusiastic to calculate the average
# rating to the question, 'I feel enthusiastic'

dat4 <- dat_big_merged_postquit %>%
  group_by(id, days_since_quit) %>%
  summarise(any_smoking = MyMax(smoking_indicator),
            mean_enthusiastic = MyMean(enthusiastic))

# We merge dat4 and dat_new
# Notice the use of left_join -- we wish to retain all rows indat_new
# and then slot in information from dat4 into each row within dat_new
dat_new <- left_join(x = dat_new, y = dat4, by = c("id", "days_since_quit"))

# Create a copy of dat_new with the name dat_main_analysis, 
# which is the dataset we will use for Main Analysis
dat_main_analysis <- dat_new

# Now, using dat_new, take those rows which will be included in Sensitivity Analysis
dat_sensitivity_analysis <- dat_new %>% filter(sensitivity == 1)

###############################################################################
# Main Analysis
###############################################################################

# Estimate coefficients of the model using glmer
fit_main <- glmer(any_smoking ~ 1 + days_since_quit + mean_enthusiastic + days_since_quit:mean_enthusiastic + (1 | id), data = na.omit(dat_main_analysis), family = "binomial")
summary(fit_main)

###############################################################################
# Sensitivity Analysis
###############################################################################

# Estimate coefficients of the model using glmer
fit_sensitivity <- glmer(any_smoking ~ 1 + days_since_quit + mean_enthusiastic + days_since_quit:mean_enthusiastic + (1 | id), data = na.omit(dat_sensitivity_analysis), family = "binomial")
summary(fit_sensitivity)

# Let's save these two datasets to the location path_pns_output_data
write.csv(dat_main_analysis, file.path(path_pns_output_data, "dat_main_analysis_module02.csv"), row.names = FALSE, na = "")
write.csv(dat_sensitivity_analysis, file.path(path_pns_output_data, "dat_sensitivity_analysis_module02.csv"), row.names = FALSE, na = "")





