# R Script: 02_DataMerge.R
# Description: 
# 1. Using the working dataset from 01_DataPrep, left join on identified key reference tables
# 2. ..\Data\Reference\REF_AthleticClubs_Venues_PerfAdjust.xlsx
# Author: Bree McLennan
# Date: 03/01/2018
#
# ======================================================================================================================== #

library(XLConnect) #comprehensive and cross-platform R package for manipulating Microsoft Excel files from within R
library(dplyr)
library(purrr)
library(data.table)
library(glue)
library(feather)
library(rprojroot)

# Define a function that computes file paths relative to where root .git folder is located
F <- is_git_root$make_fix_file() 
# Example usage: F("Data/Raw") , F("Data/Processed")

# Load feather data
wrk.02_DataMerge_00 <- setDT(read_feather(glue(F("Data/Processed/wrk.01DataPrep_4.feather"))))

#Cleanup for ref table merge:
wrk.02_DataMerge_00 <- wrk.02_DataMerge_00 %>%
  mutate(CATCompetitionVenue = as.character(CATCompetitionVenue)) %>% #unformatting back to character to perform data correction (as factor causes weird issues)
  mutate(CATCompetitionVenue = ifelse(CATCompetitionVenue == "Meadow Glen", "Meadowglen", CATCompetitionVenue)) %>% #Meadowglen is the correct name for the venue.
  mutate(CATCompetitionVenue = ifelse(CATCompetitionVenue == "Casey", "Casey Fields", CATCompetitionVenue)) %>% #Casey Fields is the correct name for the venue.
  mutate(CATCompetitionVenue = ifelse(CATCompetitionVenue == "Ballam Park", "Frankston", CATCompetitionVenue)) %>% #Using Frankston for consistency.
  mutate(CATCompetitionVenue = as.factor(CATCompetitionVenue)) #return to factor format

#Load the reference workbook containing the reference tables
REF.Workbook_01 <- loadWorkbook(F("Data/Reference/REF_AthleticClubs_Venues_PerfAdjust.xlsx"))
                           
REF.AthleticClubLookup <- readWorksheet(REF.Workbook_01, sheet = "Clubs") %>%
  mutate(CATAthleticClubCode = as.factor(CATAthleticClubCode)) %>%
  select(-CATAthleticClubHomeVenueSuburb)

  
REF.AthleticTrackVenueLookup <- readWorksheet(REF.Workbook_01, sheet = "Venues") %>%
  mutate(CATVenueShortName = as.factor(CATVenueShortName)) 


REF.PerformanceAdjustmentLookup <- setDT(readWorksheet(REF.Workbook_01, sheet = "PerformanceAdjustment")) 
REF.00PerfAdjustLookup <- REF.PerformanceAdjustmentLookup %>%
  melt.data.table(id.vars = c("CATEventName","CATEventDistance","CATGender"), na.rm = TRUE, variable.name = "CATAdjustCategory", value.name = "NUMAdjustFactor") %>%
  mutate(CATAdjustCategory = gsub(c("NUMAgeGroup_"), "", CATAdjustCategory)) %>%
  mutate(CATAdjustCategory = gsub(c("NUMAWDClass_"), "", CATAdjustCategory)) %>%
  mutate(CATEventDistance = "is.na<-"(CATEventDistance, CATEventDistance == 0)) %>%
  mutate(CATEventDistance = as.factor(CATEventDistance))
  

#Left join reference tables onto source data.
## ATHLETICS CLUBS
wrk.02_DataMerge_REFClubs <- list(wrk.02_DataMerge_00, REF.AthleticClubLookup) %>%
  reduce(left_join, by = c("CATAthleteRegisteredClub" = "CATAthleticClubCode")) 

## VENUES
wrk.02_DataMerge_REFVenues <- list(wrk.02_DataMerge_REFClubs, REF.AthleticTrackVenueLookup) %>%
  reduce(left_join, by = c("CATCompetitionVenue" = "CATVenueShortName")) 

## Performance adjustments.
#left_join(d1, d2, by = c("x" = "x2", "y" = "y2"))
wrk.02_DataMerge_REFAdjust <- list(wrk.02_DataMerge_REFVenues, REF.00PerfAdjustLookup ) %>%
  reduce(left_join, by = c("CATEventDiscipline" = "CATEventName",
                           "CATDistance" = "CATEventDistance",
                           "CATGender" = "CATGender",
                           "CATAthleteAWDClass" = "CATAdjustCategory")) %>% # This will work for AWD ONLY, need another column on LHS for 40/50/60+ 
  rename("NUMAdjustFactorAWD" = "NUMAdjustFactor")
# Merge in 40/50/60+ adjustments as a separate column
wrk.02_DataMerge_REFAdjust_2 <- list(wrk.02_DataMerge_REFAdjust , REF.00PerfAdjustLookup ) %>%
  reduce(left_join, by = c("CATEventDiscipline" = "CATEventName",
                           "CATDistance" = "CATEventDistance",
                           "CATGender" = "CATGender",
                           "CATAgeGroupKey" = "CATAdjustCategory")) %>% # left join on 40/50/60+ adjustments
  rename("NUMAdjustFactorVET" = "NUMAdjustFactor")
  
# Check the data again:
#wrd_3 = GetNewWrd()
#Desc(wrk.02_DataMerge_REFAdjust_2, wrd = wrd_3)

# Tidy up data types and column selections for next step:
wrk.02_DataMerge_01 <- wrk.02_DataMerge_REFAdjust_2 %>%
  select(-CATAthleteFullName) %>% #remove athlete name from dataset
  mutate(CATCompetitionVenue = factor(CATCompetitionVenue),
         KEYRegistrationNumber = factor(KEYRegistrationNumber),
         CATGender = factor(CATGender),
         CATAthleteRegisteredClub = factor(CATAthleteRegisteredClub),
         CATEventDiscipline = factor(CATEventDiscipline),
         CATDistance = factor(CATDistance),
         CATAgeGroupKey = factor(CATAgeGroupKey),
         CATAthleteAWDClass = factor(gsub("NA","None",CATAthleteAWDClass)),
         CATClubDistrict = factor(CATClubDistrict),
         CATClubZoneName = factor(CATClubZoneName),
         CATAthleticClubName = factor(CATAthleticClubName),
         CATAthleticTrackVenueName = factor(CATAthleticTrackVenueName),
         CATVenueTrackType = factor(CATVenueTrackType),
         CATVenueDistrict = factor(CATVenueDistrict),
         CATVenueZone = factor(CATVenueZone),
         CATVenueVictoriaRegion = factor(CATVenueVictoriaRegion)) %>%
  mutate(BINAthleteCompeteAwayVenue = as.factor(ifelse(CATAthleteRegisteredZone == CATVenueZone, 1, 0))) %>%
  mutate(CATAthleteEventStatus = as.factor(ifelse(BINValidEventAttempt  == 0 & TXTPerformanceFormatted %in% c("DQ","DNS", "DNF", "NM", "NT"), TXTPerformanceFormatted , "OK"))) %>%
  mutate(NUMPerformanceAWDAdjusted = ifelse(is.na(NUMAdjustFactorAWD), NUMPerformance, round((NUMPerformance * NUMAdjustFactorAWD), digits = 2 ))) %>%
  group_by(ORDCompetitionRound, CATAgeGroup, CATEventDiscipline, CATDistance) %>% #GROUP BY ROUND
  #Calculate round finishing order with AWD adjust applied (all other athletes across vic who competed in same event in same round)
  mutate(ORDRoundEventFinishOrderAWDAdj = ifelse(CATEventGroupL1 == "Track", order(order(NUMPerformanceAWDAdjusted, decreasing = FALSE)), # Calculate finishing order for track events
                                         (ifelse(CATEventGroupL1 == "Field", order(order(NUMPerformanceAWDAdjusted, decreasing = TRUE)),0)))) %>% # Calculate finishing order for field events
  #Calculate registered athlete finishing order
  mutate(ORDRoundEventFinishOrderPointsAWDAdj = ifelse(CATEventGroupL1 == "Track" & BINInvitationEventOrAthlete == 0, order(order(BINInvitationEventOrAthlete, NUMPerformanceAWDAdjusted, decreasing = FALSE)), # Calculate finishing order for track events
                                               (ifelse(CATEventGroupL1 == "Field" & BINInvitationEventOrAthlete == 0, order(order(rev(BINInvitationEventOrAthlete), NUMPerformanceAWDAdjusted, decreasing = TRUE)),0)))) %>%
  ungroup() #if you use group_by, also use ungroup() to save heartache later. Return to LONG form.
  
# Save Feather file to store progress:
write_feather(wrk.02_DataMerge_01, F("Data/Processed/wrk.02_DataMerge_01.feather"))

# ======================================================================================================================== #
# END OF PROGRAM #
# ======================================================================================================================== #