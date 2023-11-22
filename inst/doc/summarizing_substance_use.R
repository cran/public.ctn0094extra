## ----setup, echo=FALSE--------------------------------------------------------
knitr::opts_chunk$set(
	echo = TRUE,
	message = FALSE,
	warning = FALSE,
	collapse = TRUE,
  comment = "#>"
)

## ----packages-----------------------------------------------------------------
library(public.ctn0094data)
library(public.ctn0094extra)
library(dplyr)
library(purrr)
library(tibble)
library(tidyr)
library(stringr)

## -----------------------------------------------------------------------------
examplePeople_int <- c(1L, 163L, 210L, 242L, 4L, 17L, 13L, 1103L, 233L, 2089L)

## ----trim-data-1--------------------------------------------------------------
data_ls <- loadRawData(c("randomization", "treatment"))

data_ls$randomization <-
  data_ls$randomization %>%
  select(who, when, treatment, randomized = which) %>%
  # Remove second randomization events
  filter(randomized != 2) %>% 
  # Retain example participants
  filter(who %in% examplePeople_int)

data_ls$treatment <- 
  data_ls$treatment %>% 
  # Retain example participants
  filter(who %in% examplePeople_int)

## ----view-data-1--------------------------------------------------------------
data_ls$randomization

data_ls$treatment

## ----mark-study-drug-days-----------------------------------------------------
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

# Inspect results
treatTimeLong_df

## ----calculate-induction-delay------------------------------------------------
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
  ungroup()

# Inspect results
inductDelay_df

## ----echo=FALSE---------------------------------------------------------------
eg_df <-
  inductDelay_df %>% 
  filter(inductDelay > 0)

## ----echo=FALSE---------------------------------------------------------------
rm(eg_df)

## ----clean-up-1---------------------------------------------------------------
rm(data_ls, treatTimeLong_df)

## ----backbone-2751------------------------------------------------------------
start_int <- c(`27` = -30L, `51` = -30L)
end_int   <- c(`27` = 168L, `51` = 168L)
backbone2751_df <- 
  CreateProtocolHistory(
	  start_vec = start_int,
	  end_vec = end_int
	) %>% 
  filter(who %in% examplePeople_int)

# Inspect
 backbone2751_df

## ----backbone-30--------------------------------------------------------------
backbone30_df <-
	randomization %>%
	full_join(everybody, by = "who") %>%
	filter(project == "30") %>%
  filter(who %in% examplePeople_int) %>% 
	CreateCTN30ProtocolHistory() %>%
	mutate(project = "30") %>%
	select(who, project, when)

# Inspect
backbone30_df

backbone30_df %>% 
  group_by(who) %>% 
  summarise(lastDay = max(when))

## ----backbone-first-----------------------------------------------------------
backbone_df <-
  bind_rows(backbone2751_df, backbone30_df) %>%
	arrange(who)

rm(backbone2751_df, backbone30_df, start_int, end_int)

## ----trim-data-2--------------------------------------------------------------
data_ls <- loadRawData(c("randomization", "visit"))

data_ls$randomization <-
  data_ls$randomization %>%
  select(who, when, treatment, randomized = which) %>%
  # Remove second randomization events
  filter(randomized != 2) %>% 
  # Retain example participants
  filter(who %in% examplePeople_int)

data_ls$visit <- 
  data_ls$visit %>% 
  filter(who %in% examplePeople_int)

## ----add-randomization-day----------------------------------------------------
timelineRand1_df <-
	data_ls$randomization %>%
	mutate(randomized = randomized == "1") %>%
	# Join to backbone and arrange within subject by day
	full_join(backbone_df, by = c("who", "when")) %>%
	group_by(who) %>%
	arrange(when, .by_group = TRUE) %>%
	select(who, project, when, randomized)

# Inspect
timelineRand1_df

## ----add-visit-days-----------------------------------------------------------
timelineVisit1_df <-
	data_ls$visit %>%
	select(who, when, visit, status = what) %>%
	filter(status %in% c("visit", "final")) %>%
	mutate(visit = TRUE) %>%
	select(who, when, visit) %>%
	left_join(timelineRand1_df, ., by = c("who", "when"))

# Inspect
timelineVisit1_df

## ----mark-missing-visits------------------------------------------------------
timelineMissing1_df <- MarkMissing(timelineVisit1_df) 

## ----tidy-missing-visit-marks-------------------------------------------------
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

# Inspect
derived_visitImputed

## ----clean-up-2---------------------------------------------------------------
rm(
  backbone_df, data_ls, timelineMissing1_df, timelineRand1_df, timelineVisit1_df
)

## ----backbone-again-----------------------------------------------------------
# CTN-0027 and CTN-0051
start_int <- c(`27` = -30L, `51` = -30L)
end_int   <- c(`27` = 168L, `51` = 168L) # 24 weeks
backbone2751_df <- CreateProtocolHistory(
	start_vec = start_int, end_vec = end_int
)

# CTN-0030
backbone30_df <-
	randomization %>%
	full_join(everybody, by = "who") %>%
	filter(project == "30") %>%
	CreateCTN30ProtocolHistory() %>%
	mutate(project = "30") %>%
	select(who, project, when)

# All Days
backbone_df <- bind_rows(
	backbone2751_df, backbone30_df
) %>%
	arrange(who) %>%
  mutate(project = factor(project, levels = c("27", "30", "51"))) %>% 
  filter(who %in% examplePeople_int)

rm(backbone2751_df, backbone30_df, start_int, end_int)

## -----------------------------------------------------------------------------
randomized_df <-
  randomization %>%
  filter(who %in% examplePeople_int) %>% 
  mutate(randomized = as.integer(as.character(which))) %>%
  select(who, when, randomized) %>%
  left_join(everybody, by = "who") %>%
  filter( !(randomized == 2 & project %in% c("27", "51")) ) %>%
  select(-project)

udsUse2_df <-
	backbone_df %>%
	left_join(randomized_df, by = c("who", "when")) %>%
	left_join(derived_visitImputed, by = c("who", "when")) %>%
	left_join(uds, by = c("who", "when")) %>%
	# So we can use MarkUse() with UDS data (instead of all_drugs)
	mutate(source = "UDS")

## ----eg-visit-day-substances--------------------------------------------------
udsUse2_df %>% 
  filter(visitImputed == "Present") %>% 
  filter(!is.na(what)) %>% 
  filter(who == 17) %>% 
  print(n = nrow(.))

## ----good-drugs-bad-drugs-----------------------------------------------------
nonStudyOpioids_ls <- list(
  "Buprenorphine" = c("Opioid", "Methadone"),
  "Methadone"     = c("Opioid", "Buprenorphine"),
  "Naltrexone"    = c("Opioid", "Methadone", "Buprenorphine"),
  "Not treated"   = c("Opioid", "Methadone", "Buprenorphine")
)

## ----treatment-groups---------------------------------------------------------
treatGroups_ls <-
  public.ctn0094data::randomization %>%
  filter(who %in% examplePeople_int) %>% 
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

# Inspect
treatGroups_ls

## ----mark-non-study-drug-use, warning=TRUE------------------------------------
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
        drugs_df = .x,
        # because we have participants with no recorded UDS; in practice DO NOT
        #   use this command
        retainEmptyRows = TRUE
      ) 
    }
  ) %>%
  bind_rows() %>%
  mutate(
    udsOpioid = case_when(
       is.na(when) ~ NA,
      !is.na(when) ~ TRUE
    )
  ) %>% 
  select(who, when, udsOpioid)

## ----timeline-to-one-row-per-day----------------------------------------------
timelineUDS_df <-
	udsUse2_df %>%
	left_join(opioidUse_df, by = c("who", "when")) %>%
	select(-what, -source) %>%
  # 2,089 rows to 1,994
	distinct()

rm(
  derived_visitImputed, opioidUse_df, randomized_df, treatGroups_ls, udsUse2_df
)

## ----sets-of-randomized-people------------------------------------------------
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

# Was randomized:
wasRandomized_int

# Wasn't
notRandomized_int

## ----study-day-ticker-randomized----------------------------------------------
timelineUDS2_df <-
	timelineUDS_df %>%
	filter(who %in% wasRandomized_int) %>%
	group_by(who) %>%
  filter(!is.na(randomized)) %>%
	mutate(
		whenRandomized1 = case_when(randomized == 1 ~ when),
		whenRandomized2 = case_when(randomized == 2 ~ when)
	) %>%
	select(who, when, whenRandomized1, whenRandomized2) %>%
	left_join(timelineUDS_df, ., by = c("who", "when")) %>%
	filter(who %in% wasRandomized_int) %>%
	# Add back in the groupings BEFORE the fill()
	group_by(who) %>%
	fill(whenRandomized1, .direction = "updown") %>%
	fill(whenRandomized2, .direction = "updown") %>%
	mutate(daysSinceRand1 = when - whenRandomized1) %>%
	mutate(daysSinceRand2 = when - whenRandomized2) %>%
	select(-whenRandomized1, -whenRandomized2)

## ----weekly-use-summary-------------------------------------------------------
weeklyUse_df <-
  timelineUDS2_df %>%
  # The (daysSinceRand1 - 1) adjustment is to ensure that the first study week
  #   is a full 7 days, since "day 0" is the day before randomization. The "+1"
  #   at the end is to shift the study week label such that "week 0" is the 
  #   week *BEFORE* treatment, rather than the first week of treatment. So, the
  #   randomization day is the last day of "week 0" (the pre-treatment period).
  mutate(studyWeek1 = (daysSinceRand1 - 1) %/% 7 + 1) %>%
  mutate(studyWeek2 = (daysSinceRand2 - 1) %/% 7 + 1) %>%
  group_by(who, studyWeek1) %>%
  # There are some study weeks with multiple UDS, so we count the number of
  #   positive and negative UDS per week.
  summarise(
    nPosUDS  = sum(udsOpioid == 1, na.rm = TRUE),
    nNegUDS  = sum(visitImputed == "Present" & is.na(udsOpioid), na.rm = TRUE),
    nMissing = sum(visitImputed == "Missing", na.rm = TRUE),
    randWk1  = sum(randomized == 1, na.rm = TRUE) > 0,
    randWk2  = sum(randomized == 2 & project == "30", na.rm = TRUE) > 0
  ) %>%
  ungroup()

## ----single-week-symbols------------------------------------------------------
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
  # For CTN-0030, Phase II could have started on any day of the week, even in
  #   the middle of a treatment week. If we try to start counting Phase II
  #   weeks the day after treatment arms are switched, we can end up with the
  #   last "week" of Phase I not having 7 days. I'm going to leave the first
  #   week of Phase II as whatever week the switch happened in.
  mutate(
    rand1Active = studyWeek1 > 0,
    # This returns 0 for any week before the Phase II randomization, and 1 for
    #   the Phase II randomization week and all subsequent weeks (because the
    #   randWk2 column is 1 only for the week of second randomization and 0 
    #   all other rows).
    rand2Active = cumsum(randWk2),
    trialPhase  = rand1Active + rand2Active
  ) %>%
	select(
	  who, studyWeek = studyWeek1, randWk1, randWk2, udsStatus, trialPhase
	)

## ----use-patterns-by-phase----------------------------------------------------
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

weeklyOpioidPatterns_df

## ----use-pattern-phases-start-and-end-----------------------------------------
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
  # View this smaller data set before joining. In practice, you can comment out
  #   this print() command.
  print() %>% 
  left_join(weeklyOpioidPatterns_df, by = "who")

## ----mark-use-non-randomized--------------------------------------------------
nonRandUDS_df <-
	backbone_df %>%
	filter(who %in% notRandomized_int) %>%
	left_join(uds) %>%
	# MarkUse() requires a "source" column
	mutate(source = "UDS") %>%
	MarkUse(
		targetDrugs_char = nonStudyOpioids_ls$`Not treated`,
		drugs_df = .
	) %>%
  mutate(
    udsOpioid = case_when(
      !is.na(when) ~ TRUE,
       is.na(when) ~ NA
    )
  ) %>% 
	select(who, when, udsOpioid) %>%
  # Uneccessary here, but some UDS records are duplicated
	distinct()

## ----non-randomized-study-week------------------------------------------------
timelineNonRandUDS_df <-
	backbone_df %>%
	filter(who %in% notRandomized_int) %>%
	left_join(nonRandUDS_df, by = c("who", "when")) %>%
  # Because this week moves off of the consent date, there is no reason to add
  #   a `(week - 1)` adjustment
  mutate(studyWeek = when %/% 7 + 1) %>%
	group_by(who, studyWeek) %>%
	summarise(
		posUDS  = sum(udsOpioid == 1, na.rm = TRUE) > 0
	)

## ----single-week-symbols-non-randomized---------------------------------------
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
  # Again, this print is unecessary, but here it make clear what we are doing
  print() %>% 
  group_by(who, phase) %>%
  summarise(
    usePattern = paste0(udsStatus, collapse = "")
  ) %>%
  pivot_wider(names_from = "phase", values_from = "usePattern") %>%
  ungroup()

weeklyNonRandPatterns_df

## ----non-randomized-start-and-end---------------------------------------------
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

## ----final-product------------------------------------------------------------
derived_weeklyOpioidPattern <-
	derived_weeklyOpioidPatternRand %>%
	bind_rows(derived_weeklyOpioidPatternNonRand) %>%
	arrange(who) %>%
  replace_na(list(Phase_2 = ""))

derived_weeklyOpioidPattern

## -----------------------------------------------------------------------------
sessionInfo()

