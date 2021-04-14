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
    assessment_type == "Post-Quit Already Slipped EMA" ~ postquit_alreadyslipped_item_9,
    assessment_type == "Post-Quit Random" ~ postquit_random_item_9,
    assessment_type == "Post-Quit Urge" ~ postquit_urge_item_9,
    assessment_type == "Post-Quit About to Slip Part One" ~ postquit_partone_item_8,
    assessment_type == "Post-Quit About to Slip Part Two" ~ postquit_parttwo_item_9,
    # One may wonder why there is a need to include Pre-Quit-type EMAs
    # The reason has to do with the fact that it is possible for 
    # some Pre-Quit Type EMA Questionnaires to be launched after the working quit date
    # Rows corresponding to such questionnaires will have use_as_postquit = 1
    assessment_type == "Pre-Quit Random" ~ prequit_random_item_9,
    assessment_type == "Pre-Quit Urge" ~ prequit_urge_item_9,
    assessment_type == "Pre-Quit Smoking Part One" ~ prequit_partone_item_8,
    assessment_type == "Pre-Quit Smoking Part Two" ~ prequit_parttwo_item_9,
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

# Create a copy of dat_new with the name dat_main_analysis, 
# which is the dataset we will use for Main Analysis
dat_main_analysis <- dat_new

# Now, using dat_new, take those rows which will be included in Sensitivity Analysis
dat_sensitivity_analysis <- dat_new %>% filter(sensitivity == 1)

# Let's save these two datasets to the location path_pns_output_data
write.csv(dat_main_analysis, file.path(path_pns_output_data, "dat_main_analysis_module01.csv"), row.names = FALSE, na = "")
write.csv(dat_sensitivity_analysis, file.path(path_pns_output_data, "dat_sensitivity_analysis_module01.csv"), row.names = FALSE, na = "")

###############################################################################
# Let's visualize how many and when EMAs occur during each participant-day
###############################################################################

current_participant <- all_participant_ids[1]
current_day <- 0

dat_participant_day <- dat_big_merged_postquit %>% 
  filter(id == current_participant) %>%
  filter(days_since_quit == current_day)

# ADD HERE LATER


