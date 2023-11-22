# Create Day of Induction Data Set
# Gabriel Odom
# 2021-07-29

# We know that randomized people should receive their first dose of study drug
#   within a certain time of randomization. We need to then engineer a data set
#   which marks day of induction
# This is a cleaner variant of "scripts/scratch_mark_induction_20210702.R"

library(public.ctn0094data)
library(public.ctn0094extra)
library(tidyverse)



######  Data Setup  ###########################################################
data_ls <- loadRawData(c("randomization", "treatment"))
data_ls$randomization <-
  data_ls$randomization %>%
  select(who, when, treatment, randomized = which) %>%
  filter(randomized != 2)



######  Wrangle Data List  ####################################################

# The definition for Induction Failure should use this data:
treatTimeLong_df <-
  # Collapse Data List
  data_ls %>%
  reduce(.f = full_join, by = c("who", "when")) %>%
  as_tibble() %>%
  arrange(who, when) %>%
  # First pass: find any day with a dose of treatment drug
  rowwise() %>%
  mutate(
    treated = if_else(
      condition = !is.na(amount) & amount > 0,
      true = TRUE,
      false = FALSE,
      missing = FALSE
    )
  )



######  Create Induction Delay Dataset  #####################################
inductDelay_df <-
  treatTimeLong_df %>%
  # Find the day of the first treatment
  group_by(who) %>%
  arrange(when) %>%
  filter(treated) %>%
  slice(1) %>%
  mutate(treatStart = when) %>%
  select(who, when, treatStart) %>%
  # Add the first day back to the original data
  left_join(treatTimeLong_df, ., by = c("who", "when")) %>%
  group_by(who) %>%
  fill(treatStart, .direction = "updown") %>%
  # Calculate the delay
  filter(randomized == 1) %>%
  # This sets time to be missing if the induction was not observed
  mutate(inductDelay = treatStart - when) %>%
  select(who, treatment, inductDelay) %>%
  # to remove 1 duplicate record for subject 2486
  distinct() %>%
  ungroup()

# In version 0.0.0.9016, subject 2486 has 2 records
treatTimeLong_df %>% filter(who == 2486)
# This subject has two different induction doses on day 8


###  Save the Data Set  ###
# If you are re-running these scripts, change the name of the old version in
#   data/ (include the date of the script used to make it), move it to
#   inst/extdata/, then run this code to write the new data version into data/.
#   DO NOT overwrite the older versions (so we have a record of what changed).
derived_inductDelay <- inductDelay_df
usethis::use_data(derived_inductDelay)


