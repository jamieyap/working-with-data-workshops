library(dplyr)
library(lme4)

path_pns_input_data <- "C:/Users/jamieyap/Desktop/input_data"
path_pns_output_data <- "C:/Users/jamieyap/Desktop/output_data"
# We will use this location to store plots
path_pns_staged_data <- "C:/Users/jamieyap/Desktop/staged_data"

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

ids_main_analysis <- unique(dat_main_analysis$id)

for(idx_participant in 1:length(ids_main_analysis)){
  for(idx_day in 0:20){
    
    # Pick a participant-day
    current_participant <- ids_main_analysis[idx_participant]
    current_day <- idx_day
    
    # Create a visual snapshot of the entire participant day
    # Tells R where to save the plot and the plot's file name
    jpeg(filename=file.path(path_pns_staged_data, paste("Participant_", current_participant, "_Day_", current_day, ".jpg",sep="")), width=1200, height=350, units="px")
    
    # Take rows corresponding to this particular participant-day
    dat_participant_day <- dat_big_merged_postquit %>% 
      filter(id == current_participant) %>%
      filter(days_since_quit == current_day)
    
    # We note that the time_unixts variable would simultaneously capture
    # these two conditions. Hence, we simply refer to the time_unixts
    # variable in the steps below
    
    # Create more time variables for plotting
    # Let's create a new variable, start of day to be at 12am
    # of the date in time_hrts. Selecting 12am
    # is a convenient way to calculate the position of the
    # markers in our plot
    dat_participant_day <- dat_participant_day %>%
      mutate(start_of_day_hrts = strptime(time_hrts, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")) %>% 
      mutate(start_of_day_hrts = strftime(time_hrts, format = "%Y-%m-%d", tz = "UTC")) %>%
      mutate(start_of_day_unixts = 	as.numeric(as.POSIXct(start_of_day_hrts, tz = "UTC", origin="1970-01-01"))) 
    
    # For each EMA, calculate hour of day when it was
    # when participant began responding to the EMA (if with_any_response=1)
    # or the time of day when it was launched (if with_any_response=0)
    dat_participant_day <- dat_participant_day %>% 
      mutate(hours_elapsed = (time_unixts-start_of_day_unixts)/(60*60)) %>%
      # Simple rearrangement of columns so that the new variables we created would appear
      # in the first few columns
      select(start_of_day_hrts, start_of_day_unixts, hours_elapsed, everything())
    
    # Identify which rows correspond to each kind of EMA
    plotdat_random <- dat_participant_day %>% filter(assessment_type == "Post-Quit Random")
    plotdat_urge <- dat_participant_day %>% filter(assessment_type == "Post-Quit Urge")
    plotdat_already_slipped <- dat_participant_day %>% filter(assessment_type == "Post-Quit Already Slipped")
    plotdat_part_one <- dat_participant_day %>% filter(assessment_type == "Post-Quit About to Slip Part One")
    plotdat_part_two <- dat_participant_day %>% filter(assessment_type == "Post-Quit About to Slip Part Two")
    
    # Layer on each component of the plot
    plot(-1, xaxt = "n", yaxt = "n", 
         xlab = "Hour of the Day ('0' represents midnight)", ylab = "", 
         xlim = c(0,24), ylim = c(0,0.3), 
         cex.lab = 2, 
         frame.plot = FALSE)
    axis(1, at = seq(0,24,3), cex.axis = 2, lwd.ticks = 2, gap.axis = 1.2)
    
    # Note that if the number of rows in the plot data is equal to zero,
    # then no new points will be added to the existing plot; no error message will be displayed
    points(plotdat_random$hours_elapsed, rep(0.1, nrow(plotdat_random)), pch = 19, cex = 2, col = "black")
    points(plotdat_urge$hours_elapsed, rep(0.1, nrow(plotdat_urge)), pch = 19, cex = 2, col = "orange")
    points(plotdat_already_slipped$hours_elapsed, rep(0.1, nrow(plotdat_already_slipped)), pch = 19, cex = 2, col = "seagreen")
    points(plotdat_part_one$hours_elapsed, rep(0.1, nrow(plotdat_part_one)), pch = 19, cex = 2, col = "lightblue")
    points(plotdat_part_two$hours_elapsed, rep(0.1, nrow(plotdat_part_two)), pch = 19, cex = 2, col = "blue")
    
    legend("topleft", c("Random", "Urge", "Already Slipped", "Part One", "Part Two"),
           col = c("black", "orange", "seagreen", "lightblue", "blue"),
           pch = rep(19,5), pt.cex = rep(2,5), cex = 1.2)
    
    # Tells R that plot is complete
    dev.off()
  }
}



