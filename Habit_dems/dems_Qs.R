#!/usr/bin/env Rscript

# 20241017WF - copy of VD's Habit_demsandqs.Rmd

library("tidyr")
library("dplyr")
library("lubridate")

# DOB responses from survey were unrestricted. we get all sorts of formats
freerresponse_to_date <- function(date_in) {
 date_in %>%
  as.character() %>%
  parse_date_time(orders = c("%m/%d/%Y", "%m-%d-%Y", "%m/%d/%y", "%m-%d-%y", "%B %d, %Y", "%m%d%Y", "%M%d%Y", "%M/%d/%y", "%M-%d-%y"))
}

All_dem_Habit <- read.csv("/Volumes/Hera/Victoria/Habit_dems/All_dem_Habit.csv")

#rename ID column
All_dem_Habit <- relocate(All_dem_Habit, ExternalReference) # not needed?
Demsandqs <- select(All_dem_Habit,
                    lunaid=ExternalReference,
                    Testdate=INTRO.Q1_1,
                    DOB=INTRO.Q1_2,
                    Gender=INTRO.Q1_3,
                    starts_with ("UPP"))
#human error in entering dates, so a few cases missing
Demsandqs <- Demsandqs %>%
  mutate(form_Testdate = freerresponse_to_date(Testdate),
         form_DOB = freerresponse_to_date(DOB),
         Age = round(as.numeric((form_Testdate - form_DOB) / 365.25), 2))
#re-order columns
# TODO: use relocate(matches()) instead?
# WARNING: orig var overwrite is not idempotent! running more than once changes Demsandqs
#          also not needed?
Demsandqs_uppsplast <- Demsandqs[, c(1:4, 64, 65, 66, 5:63)] # relocate form_* and Age columns to the front
uppspcols <- grep("UPPS.P", names(Demsandqs_uppsplast), value=T)
uppsp <- uppsp_scoring(Demsandqs_uppsplast, uppspcols)
uppsp$lunaid <- Demsandqs_uppsplast$lunaid
uppsp$Age <- Demsandqs_uppsplast$Age
final_uppsp <- uppsp[, c(7, 8, 1:6)] # relocate again to the front

#write.csv(final_uppsp, "final_uppsp_scored.csv")
