# Create Imputed Phase I Missing Visits
# Gabriel Odom
# 2021-09-09

# This work is based on "scratch_missing_visits_20210906.R" and
#   "scratch_missing_visits_20210909.R



######  Setup and Data Backbone  ##############################################
library(public.ctn0094data)
library(public.ctn0094extra)
library(tidyverse)


###  CTN-0027 and CTN-0051  ###
start_int <- c(`27` = -30L, `51` = -30L)
end_int   <- c(`27` = 168L, `51` = 168L)
backbone2751_df <- CreateProtocolHistory(
	start_vec = start_int, end_vec = end_int
)


###  CTN-0030  ###
backbone30_df <-
	randomization %>%
	full_join(everybody, by = "who") %>%
	filter(project == "30") %>%
	CreateCTN30ProtocolHistory() %>%
	mutate(project = "30") %>%
	select(who, project, when)


###  All Days  ###
backbone_df <-
  bind_rows(backbone2751_df, backbone30_df) %>%
	arrange(who)

rm(backbone2751_df, backbone30_df, start_int, end_int)



######  Impute Missing Visits  ################################################

###  Setting up the Timeline Data  ###
data_ls <- loadRawData(c("randomization", "visit"))

timelineRand1_df <-
	data_ls$randomization %>%
  # which is now a factor??? Normally the recommended soln is levels(x)[x],
  #   then as.numeric, but I don't need the safety net here (I trust Ray)
  mutate(which = as.integer(as.character(which))) %>% 
	# Extract Phase I randomization data and tidy
	filter(which == 1) %>%
	rename(randomized = which) %>%
	mutate(randomized = as.logical(randomized)) %>%
	# Join to backbone and arrange within subject by day
	full_join(backbone_df, by = c("who", "when")) %>%
	group_by(who) %>%
	arrange(when, .by_group = TRUE) %>%
	select(who, project, when, randomized)

timelineVisit1_df <-
	data_ls$visit %>%
	select(who, when, visit, status = what) %>%
	filter(status %in% c("visit", "final")) %>%
	select(-visit) %>%
	mutate(visit = TRUE) %>%
	select(who, when, visit) %>%
  # Drop from 41002 to 40752 rows with distinct()
	distinct() %>%
	left_join(timelineRand1_df, ., by = c("who", "when"))


system.time(
	timelineMissing1_df <-
		timelineVisit1_df %>%
		MarkMissing()
)
# 23.466 minutes for 691250 rows in 3560 participants
# (with the old version it only took 9 minutes; what happened?)
# UPDATE 2023-09-12: back to 7.9 minutes???



######  Collapse and Save  ####################################################
derived_visitImputed <-
	timelineMissing1_df %>%
  mutate(visit = as.character(visit)) %>%
	replace_na(list(visit = "", visitYM = "")) %>%
	mutate(visitImputed = paste0(visit, visitYM)) %>%
	mutate(
		visitImputed = str_replace(
			visitImputed, pattern = "TRUETRUE", replacement = "Present"
		)
	) %>%
	select(who, when, visitImputed) %>%
	filter(visitImputed != "") %>%
	ungroup()


###  Save the Data Set  ###
# If you are re-running these scripts, change the name of the old version in
#   data/ (include the date of the script used to make it), move it to
#   inst/extdata/, then run this code to write the new data version into data/.
#   DO NOT overwrite the older versions (so we have a record of what changed).
usethis::use_data(derived_visitImputed)

