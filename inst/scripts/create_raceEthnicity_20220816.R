# Add Derived "Race+Ethnicity" Feature to Demographics
# Gabriel Odom
# 2022-08-16

# Our health disparities results depend on a clear definition of participant
#   race and ethinicity. In the paper, we refer to "Non-Hispanic White"
#   (n_W = 2484), "Non-Hispanic Black" (n_B = 347), "Hispanic" (n_H = 507), and
#   "Other" (n_O = 222). We should make this data-driven instead of typed.

library(public.ctn0094extra)
library(tidyverse)

derived_raceEthnicity <-
  public.ctn0094data::demographics %>%
  mutate(
    raceEth = case_when(
      race == "White" & is_hispanic == "No" ~ "NHW",
      race == "Black" & is_hispanic == "No" ~ "NHB",
      is_hispanic == "Yes" ~ "Hisp",
      TRUE ~ "Other"
    )
  ) %>%
  mutate(
    race_ethnicity = factor(
      raceEth,
      levels = c("NHW", "NHB", "Hisp", "Other"),
      labels = c(
        "Non-Hispanic White", "Non-Hispanic Black", "Hispanic", "Other"
      )
    )
  ) %>%
  select(who, race, is_hispanic, race_ethnicity)

table(derived_raceEthnicity$race_ethnicity)

usethis::use_data(derived_raceEthnicity)
