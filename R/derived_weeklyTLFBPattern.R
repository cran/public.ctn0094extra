#' @title Patient TLFB Opioid Weekly Pattern Data
#'
#' @description Show the pattern of positive and negative patient self-report
#'    (timeline follow-back, TLFB) results for opioids by patient over the study
#'    protocol. Study "Week 1" starts the day after randomization (for patients
#'    who were randomized) or the day after signed consent (for patients who
#'    were not randomized).
#'
#' @details This data set contains a "word" describing weekly non-study opioid
#'    use patterns as reported by the subject in TLFB. Based on the substances
#'    reported by the subjects, our list of substances classified as an opioid
#'    is: non-study Buprenorphine, non-study Methadone, heroin, and "opioids"
#'    (which includes Oxymorphone, Opium, Fentanyl, Hydromorphone, Codeine,
#'    Suboxone, Tramadol, Morphine, Hydrocodone, and Oxycodone). TLFB reporting
#'    indicating the presence of two or more use days of these substances will
#'    be marked with \code{"+"} for that week. TLFB results positive for these
#'    substances for 0 days will be marked with \code{"-"}. TLFB results
#'    positive for these substances for 1 day in the week will be marked with
#'    \code{"*"} for that week. This data set is a derived data set; the script
#'    used to create it is
#'    `"scripts/create_weeklyTLFBPattern_20220511.R"`.
#'
#'    NOTE: some studies collected more TLFB data than others. Also, all times
#'    are marked starting with the week of randomization. We represent the weeks
#'    before randomization with \code{"_"} if no TLFB data was collected. For
#'    subjects who were never randomized, all subsequent protocol weeks are
#'    marked as missing (\code{"o"}).
#'
#'
#' @docType data
#'
#' @usage data(derived_weeklyTLFBPattern)
#'
#' @format A tibble with `r scales::comma(nrow(derived_weeklyTLFBPattern))`
#'    rows and columns:
#'    \describe{
#'      \item{who}{Patient ID}
#'      \item{startWeek}{The start of the "word" is how many weeks before
#'        randomization? This should be -4 for most people, but can be as high
#'        as -8. Note that week 0 is included, so a value of -4 represents data
#'        in the 5th week before randomization; that is, 29-35 days prior to
#'        randomization. Most subjects have timeline follow-back data 30 days
#'        before consent, and delays between consent and randomization are
#'        common.}
#'      \item{randWeek1}{Week of first randomization (1, if randomized; NA if
#'        not)}
#'      \item{randWeek2}{Week of second randomization (only for CTN-0030)}
#'      \item{endWeek}{The end of the "word" is how many weeks after
#'        randomization? This depends on the study protocol, but should be
#'        close to 16 or 24 weeks for most subjects.}
#'      \item{Baseline}{A character string of symbols from \code{startWeek} to
#'        the last week before \code{randWeek1}. Symbols are as defined in
#'        \code{Phase_1}.
#'      }
#'      \item{Phase_1}{A character string of symbols from \code{randWeek1}
#'        to \code{endWeek} (for subjects from CTN-0027 and CTN-0051) or the
#'        last week before \code{randWeek2} (for CTN-0030). These symbols are:
#'        \code{"+"} = the subject reported the use of an opioid for two or more
#'        days in that week; \code{"-"} = the subject did not report use of an
#'        opioid in that week; \code{"*"} = the subject reported one day of
#'        opioid use in that week; \code{"o"} if the subject was supposed to
#'        report TLFB but did not; and \code{"_"} to represent weeks wherein the
#'        subject was not scheduled to provide TLFB (for example, more than 30
#'        days before randomization).
#'      }
#'      \item{Phase_2}{A character string of symbols from \code{randWeek2}
#'        to \code{endWeek} for subjects from CTN-0030 only. Symbols are as
#'        defined in \code{Phase_1}.
#'      }
#' }
"derived_weeklyTLFBPattern"
