# Create Weekly Use Pattern
# Gabriel Odom
# 2022-03-08



######  Setup and Data Backbone  ##############################################
library(public.ctn0094data)
# MAKE SURE THIS PACKAGE IS BUILT FROM ctn0094data_noGit/ or its copy in
#   Box/CTN0094/Harmonization/Ray/. The old versions are in Box/ctn0094data/.
# UPDATE 2021-11-23: use Box/ctn0094data_dev2/
# UPDATE 2021-12-09: use ctn0094data version 0.0.0.9014
# UPDATE 2022-03-08: use Box/ctn0094data_dev2/ v. 0.0.0.9016
# UPDATE 2022-08-15: use Box/ctn0094DataBurner/ v. 0.0.0.9023
# UPDATE 2023-03-24: Ray emailed me the zipped source for v. 0.0.0.9028
# UPDATE 2023-06-19: Ray added public.ctn0094data_0.9.0.tar.gz to Box/
# UPDATE 2023-09-12: public.ctn0094data is finally on GitHub
library(public.ctn0094extra)
# I rebuilt ctn0094DataExtra (v. 0.0.0.9007) using v. 0.0.0.9016 of ctn0094data
# I rebuilt ctn0094DataExtra (v. 0.0.0.9008) using v. 0.0.0.9023 of ctn0094data
# I rebuilt ctn0094DataExtra (v. 0.0.0.9010) using v. 0.0.0.9028 of ctn0094data
library(tidyverse)


###  CTN-0027 and CTN-0051  ###
start_int <- c(`27` = -30L, `51` = -30L)
end_int   <- c(`27` = 168L, `51` = 168L) # 24 weeks
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
backbone_df <- bind_rows(
	backbone2751_df, backbone30_df
) %>%
	arrange(who) %>%
  # In ctn0094data version 0.0.0.9014, project is now a factor
  mutate(project = factor(project, levels = c("27", "30", "51")))

rm(backbone2751_df, backbone30_df, start_int, end_int)



######  Mark Daily Opioid Use  ################################################
# UPDATE 2021-11-01: For CTN-0030, we also need the second randomization date,
#   because of course we do :/
randomized_df <-
  randomization %>%
  # UPDATE 2023-03-24: `which` is now a factor
  mutate(which = as.integer(as.character(which))) %>%
  rename(randomized = which) %>%
  select(who, when, randomized) %>%
  full_join(everybody, by = "who") %>%
  filter( !(randomized == 2 & project %in% c("27", "51")) ) %>%
  select(-project)

udsUse2_df <-
	backbone_df %>%
	left_join(randomized_df, by = c("who", "when")) %>%
	left_join(derived_visitImputed, by = c("who", "when")) %>%
	left_join(uds, by = c("who", "when")) %>%
	# So we can use MarkUse() with UDS data (instead of all_drugs)
	mutate(source = "UDS")


###  Mark Use Days  ###
nonStudyOpioids_char <- c("Opioid", "Methadone", "Buprenorphine")
# COMMENT 2022-03-08: Ray and Sean still disagree on how to handle methadone and
#   buprenorphine that shows up in the UDS/TLFB for a person prescribed those.
#   drugs. Are they self-medicating?

# UPDATE 2023-03-24: I will find the treatment arm for each person, and count
#   a "non-study" opioid as "Opioid" or either (or both) of the treatment drugs
#   that the subject was not assigned to.

# First question: did anyone in CTN-0030 switch away from Buprenorphine?
public.ctn0094data::randomization %>%
  left_join(everybody, by = "who") %>%
  select(who, treatment, project) %>%
  group_by(project, treatment) %>%
  summarise(nInArm = n())
# No, they only switched from SMM to EMM, so we can drop the second rand date

public.ctn0094data::randomization %>%
  filter(which == 1) %>%
  left_join(everybody, by = "who") %>%
  select(who, treatment, project) %>%
  group_by(project, treatment) %>%
  summarise(nInArm = n())
# Better. Now we want to create sets by treatment drug:
treatGroups_ls <-
  public.ctn0094data::randomization %>%
  filter(which == 1) %>%
  left_join(everybody, by = "who") %>%
  select(who, treatment) %>%
  mutate(
    treat_drug = case_when(
      str_detect(treatment, "BUP") ~ "Buprenorphine",
      treatment == "Methadone" ~ "Methadone",
      treatment == "Inpatient NR-NTX" ~ "Naltrexone"
    )
  ) %>%
  select(-treatment) %>%
  split(f = .$treat_drug) %>%
  map(.f = "who")

# # OLD code
# opioidUse_df <-
# 	udsUse2_df %>%
# 	# REQUIRES "source" COLUMN
# 	MarkUse(
# 		targetDrugs_char = nonStudyOpioids_char,
# 		drugs_df = .
# 	) %>%
#   mutate(udsOpioid = TRUE) %>%
# 	select(who, when, udsOpioid)

###  NEW code  ###
nonStudyOpioids_ls <- list(
  "Buprenorphine" = c("Opioid", "Methadone"),
  "Methadone"     = c("Opioid", "Buprenorphine"),
  "Naltrexone"    = nonStudyOpioids_char,
  "Not treated"   = nonStudyOpioids_char
)

# Which non-study drugs show up in which arms?
udsUse2_df %>%
  mutate(
    treat_group = case_when(
      who %in% treatGroups_ls$Buprenorphine ~ "Buprenorphine",
      who %in% treatGroups_ls$Methadone     ~ "Methadone",
      who %in% treatGroups_ls$Naltrexone    ~ "Naltrexone",
      TRUE                                  ~ "Not treated"
    )
  ) %>%
  select(what, treat_group) %>%
  distinct() %>%
  table()
# Methadone shows up in all the groups, no matter what the treatment; BUP does
#   not show up in the Methadone or untreated groups

opioidUse_df <-
  udsUse2_df %>%
  mutate(
    treat_group = case_when(
      who %in% treatGroups_ls$Buprenorphine ~ "Buprenorphine",
      who %in% treatGroups_ls$Methadone     ~ "Methadone",
      who %in% treatGroups_ls$Naltrexone    ~ "Naltrexone",
      TRUE                                  ~ "Not treated"
    )
  ) %>%
  split(f = .$treat_group) %>%
  # List of data in alphabetical order, so the non-study drugs ls should match
  map2(
    .y = nonStudyOpioids_ls,
    .f = ~{
      # REQUIRES "source" COLUMN
      MarkUse(
        targetDrugs_char = .y,
        drugs_df = .x
      ) %>%
        mutate(udsOpioid = TRUE)
    }
  ) %>%
  bind_rows() %>%
  select(who, when, udsOpioid)

# We see the expected warning that BUP was a target but didn't show up (for the
#   Methadone and untreated groups).
# After inspecting this data, I'm interested in the Naltrexone patients who
#   were positive for BUP on their day 0 but never came back.



###  Create a Patient Timeline  ###
# All of the drugs marked above would still be individual rows, so we want to
#   get back to "one row per person per day"
timelineUDS_df <-
	udsUse2_df %>%
	left_join(opioidUse_df, by = c("who", "when")) %>%
	select(-what, -source) %>%
  # 707,039 rows to 691,250
	distinct()

rm(opioidUse_df, udsUse2_df, randomized_df)



######  Counting Days Since Randomization  ####################################
# Do we state that by definition any person who wasn't randomized is an early
#   treatment failure? In the sense of evaluating treatment efficacy, yes; in
#   evaluating the subject, no. Regardless, no matter the treatment outcome
#   definition, these 1068 people will be listed as treatment failures under the
#   "intent to treat" paradigm
wasRandomized_int <-
	timelineUDS_df %>%
	group_by(who) %>%
	summarise(randomized = any(randomized %in% 1:2)) %>%
	filter(randomized) %>%
	pull(who)
notRandomized_int <-
	timelineUDS_df %>%
	filter( !(who %in% wasRandomized_int) ) %>%
	pull(who) %>%
	unique()



###  Study Day Ticker  ###
# This is for randomised subjects only
timelineUDS2_df <-
	timelineUDS_df %>%
	filter(who %in% wasRandomized_int) %>%
	group_by(who) %>%
  filter(!is.na(randomized)) %>%
  # randomized is a factor now, so make sure this `==`() call works
	mutate(
		whenRandomized1 = case_when(randomized == 1 ~ when),
		whenRandomized2 = case_when(randomized == 2 ~ when)
	) %>%
	select(who, when, whenRandomized1, whenRandomized2) %>%
	left_join(timelineUDS_df, ., by = c("who", "when")) %>%
	filter(who %in% wasRandomized_int) %>%
	# Add back in the groupings BEFORE the fill()
	group_by(who) %>%
	# fill(whenRandomized, .direction = "updown") %>%
	fill(whenRandomized1, .direction = "updown") %>%
	fill(whenRandomized2, .direction = "updown") %>%
	# mutate(daysSinceRand = when - whenRandomized) %>%
	mutate(daysSinceRand1 = when - whenRandomized1) %>%
	mutate(daysSinceRand2 = when - whenRandomized2) %>%
	select(-whenRandomized1, -whenRandomized2)



######  Use Pattern Post Randomization  #######################################


###  Participant UDS by Week  ###
weeklyUse_df <-
  timelineUDS2_df %>%
  # This puts the first day of study week 1 on the day of randomization, not on
  #   the day of consent. Consent could also be on the same day as randomization
  # UPDATE 2022-08-17: Sean and Mei-Chen count the randomization day as the last
  #   day of study week 0; this means that the first day of study week 1 is the
  #   day *after* randomization (daysSinceRand1 - 1). The "+1" at the end is so
  #   that we start on study week 1, not study week 0 (because `%/%` [the
  #   integer division operator] will treat 0:6 days since randomization as
  #   "week 0" instead of week 1: (0:6) %/% 7 = 0, 0, 0, 0, 0, 0, 0)
  # UPDATE 2023-03-27: I was incorrectly counting the randomization day as part
  #   of Treatment Week 1, when the UDS collected on day 0 would necessarily be
  #   indicative of the pre-treatment status. After speaking with Sean, we will
  #   move "week 1" to start the day AFTER randomization by `daysSinceRand1 - 1`
  # UPDATE 2023-03-29: apparently I had already fixed this problem back in
  #   AUGUST OF LAST YEAR, but I stupidly forgot to commit and push my changes.
  #   To quote Hermione, "what ... an ... idiot".
  mutate(studyWeek1 = (daysSinceRand1 - 1) %/% 7 + 1) %>%
  mutate(studyWeek2 = (daysSinceRand2 - 1) %/% 7 + 1) %>%
  group_by(who, studyWeek1) %>%
  summarise(
    nPosUDS  = sum(udsOpioid == 1, na.rm = TRUE),
    nNegUDS  = sum(visitImputed == "Present" & is.na(udsOpioid), na.rm = TRUE),
    nMissing = sum(visitImputed == "Missing", na.rm = TRUE),
    randWk1  = sum(randomized == 1, na.rm = TRUE) > 0,
    randWk2  = sum(randomized == 2 & project == "30", na.rm = TRUE) > 0
  ) %>%
  ungroup()


###  Single Symbol to Represent Weekly UDS Results  ###
# TRUMPING RULES: positive and negative trump missing. I think I have to mark
#   weeks with both positive and negative as "M" for "mixture"? Maybe "B" for
#   both? Ray said to use "*" because the symbol will be the same size
useByWeekRandomized_df <-
  weeklyUse_df %>%
	mutate(
		udsStatus = case_when(
			# If we see a positive UDS and no negative UDS, it's positive
			nPosUDS > 0  & nNegUDS == 0 ~ "+",
			# If we see a negative UDS and no positive UDS, it's negative
			nPosUDS == 0 & nNegUDS > 0  ~ "-",
			# If we see both positive and negative UDS in a single week, it's both
			#   (note that we can recode all "B"s to be "+" as necessary)
			nPosUDS > 0  & nNegUDS > 0  ~ "*",
			# If we don't have any UDS in a week after randomization, it's missing
			# UPDATE 2022-03-08: I had this as a 0 originally, and I was using this
			#   in context of consent, not randomization. This was wrong.
			nPosUDS == 0 & nNegUDS == 0 & studyWeek1 >= 1 ~ "o",
			# If none of the above are true, but we still have a missing value as
			#   marked by the MarkMissing() function, then it's missing
			nMissing > 0 ~ "o",
			# If none of the above conditions are true (probably because it's a week
			#   before randomization but not during a baseline visit for consent),
			#   then leave it blank (pre-study)
			TRUE ~ "_"
		)
	) %>%
  group_by(who) %>%
  # UPDATE 2022-08-17: as written, this again assumes that the randomization day
  #   is the *start of treatment*, when we consider it to be the *end of
  #   pre-treatment*. What to do for Phase II? For CTN-0030, Phase II could have
  #   started on any day of the week, even in the middle of a treatment week. If
  #   we try to start counting Phase II weeks the day after treatment arms are
  #   switched, we can end up with the last "week" of Phase I not having 7 days.
  #   this would be the only scenario where a "week" is not 7 days, and it would
  #   be completely arbitrary. I'm going to leave the first week of Phase II as
  #   whatever week the switch happened in.
  mutate(
    # rand1Active = cumsum(randWk1),
    rand1Active = studyWeek1 > 0,
    rand2Active = cumsum(randWk2),
    trialPhase  = rand1Active + rand2Active
  ) %>%
	select(
	  who, studyWeek = studyWeek1, randWk1, randWk2, udsStatus, trialPhase
	)


###  Create 3 Quinary Words  ###
weeklyOpioidPatterns_df <-
  useByWeekRandomized_df %>%
  mutate(
    phase = case_when(
      trialPhase == 0L ~ "Baseline",
      trialPhase == 1L ~ "Phase_1",
      trialPhase == 2L ~ "Phase_2"
    )
  ) %>%
  group_by(who, phase) %>%
  summarise(
    usePattern = paste0(udsStatus, collapse = "")
  ) %>%
  pivot_wider(names_from = "phase", values_from = "usePattern")


###  Weekly Opioid Patterns by Phase  ###
derived_weeklyOpioidPatternRand <-
  useByWeekRandomized_df %>%
  mutate(
    randWeek1  = randWk1 * studyWeek,
    randWeek2  = randWk2 * studyWeek
  ) %>%
  summarise(
    startWeek  = min(studyWeek),
    randWeek1  = max(randWeek1),
    randWeek2  = if_else( all(randWeek2 == 0), NA_real_, max(randWeek2) ),
    endWeek    = max(studyWeek)
  ) %>%
  left_join(weeklyOpioidPatterns_df, by = "who")



######  Use Pattern for Non-Randomized Subjects  ##############################
# For completeness, we can include the non-randomized subjects as well.
nonRandUDS_df <-
	backbone_df %>%
	filter(who %in% notRandomized_int) %>%
	left_join(uds) %>%
	# MarkUse() requires a "source" column
	mutate(source = "UDS") %>%
	MarkUse(
		targetDrugs_char = nonStudyOpioids_char,
		drugs_df = .
	) %>%
	mutate(udsOpioid = TRUE) %>%
	select(who, when, udsOpioid) %>%
	distinct()

# There are positive UDS events among the non-randomized
timelineNonRandUDS_df <-
	backbone_df %>%
	filter(who %in% notRandomized_int) %>%
	left_join(nonRandUDS_df, by = c("who", "when")) %>%
  # UPDATE 2022-08-16: for randomized people, study week 1 starts the day after
  #   randomization. We originally had this to start the day of randomization.
  #   In short, we re-contextualized the randomization day as the "end of
  #   pre-treatment" instead of the "start of treatment". Similarly, for the
  #   non-randomized people, we will count the day of consent as the "end of
  #   pre-treatment" (even though they received no treatment), instead of the
  #   "start of treatment".
  # UPDATE 2023-03-29. I disagree with my previous assessment. Because this week
  #   moves off of the consent date, there is no reason to add a `(week - 1)`
  #   adjustment
	# mutate(studyWeek = (when - 1) %/% 7 + 1) %>%
  mutate(studyWeek = when %/% 7 + 1) %>%
	group_by(who, studyWeek) %>%
	summarise(
		posUDS  = sum(udsOpioid == 1, na.rm = TRUE) > 0
	)


###  Create 2 Quinary Words  ###
weeklyNonRandPatterns_df <-
  timelineNonRandUDS_df %>%
  mutate(
    udsStatus = case_when(
      # If they are positive, they are positive
      posUDS ~ "+",
      # If they aren't positive and it's after the consent week, they are
      #   missing (because they weren't randomized)
      !posUDS & studyWeek >= 1 ~ "o",
      # If they aren't positive and it's on or before the consent week, then
      #   leave it blank (pre-study)
      !posUDS & studyWeek < 1 ~ "_"
    )
  ) %>%
  mutate(
    phase = case_when(
      studyWeek <  1 ~ "Baseline",
      studyWeek >= 1 ~ "Phase_1"
    )
  ) %>%
  group_by(who, phase) %>%
  summarise(
    usePattern = paste0(udsStatus, collapse = "")
  ) %>%
  pivot_wider(names_from = "phase", values_from = "usePattern") %>%
  ungroup()


###  Weekly Opioid Patterns by Phase  ###
derived_weeklyOpioidPatternNonRand <-
  timelineNonRandUDS_df %>%
  group_by(who) %>%
  summarise(
    startWeek  = min(studyWeek),
    randWeek1  = NA_real_,
    randWeek2  = NA_real_,
    endWeek    = max(studyWeek)
  ) %>%
  left_join(weeklyNonRandPatterns_df, by = "who")



######  Combine and Save  #####################################################
derived_weeklyOpioidPattern <-
	derived_weeklyOpioidPatternRand %>%
	bind_rows(derived_weeklyOpioidPatternNonRand) %>%
	arrange(who) %>%
  replace_na(list(Phase_2 = ""))

# If you are re-running these scripts, change the name of the old version in
#   data/ (include the date of the script used to make it), move it to
#   inst/extdata/, then run this code to write the new data version into data/.
#   DO NOT overwrite the older versions (so we have a record of what changed).
usethis::use_data(derived_weeklyOpioidPattern)
