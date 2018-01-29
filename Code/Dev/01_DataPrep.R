# R Script: 01_DataPrep.R
# Description: 
# 1. Load in data created by 00_DataLoad.R
# 2. datatype conversions, rename variables to data science convention: KEY, FOR, TXT, NUM, BIN, ORD, CAT, DAT, TIM
# 3. data integrity checks
# 4. Add in features
# 5. Apply risk treatments & controls: DATA SENSITIVITY AND SECURITY
# 6. Consider applying scaling
# Author: Bree McLennan
# Date: 03/01/2018
#
# ======================================================================================================================== #

# Load packages
library(data.table)
library(feather)
library(dplyr)
library(forcats)
library(lubridate)
library(glue)
library(DescTools)
library(RDCOMClient)
library(stringr)
`%ni%` <- Negate(`%in%`)

# Load feather data
wrk.data <- setDT(read_feather(glue("D:/Data Science/Athletics Data/Project Files/athletics_data_analysis/Data/Raw/raw.AllInterclubResults.feather")))

# Check over the dataset and identify the state of variables in the dataset
#wrd=GetNewWrd()
#Desc(wrk.data$notes, wrd=wrd)

# Data Type configurations ==============================================================================================#
wrk.01DataPrep_1 <- wrk.data %>%
  mutate(CATAthleteRegisteredZone = as.factor(zone),
         ORDCompetitionRound = factor(round, ordered = TRUE, levels = c("1","2","3","4","5","6","8","9","10","11","12")), 
         CATCompetitionVenue = factor(venue),
         CATGender = case_when(sex == "M" ~ "Male", sex == "F" ~ "Female"),
         CATGender = factor(CATGender),
         KEYRegistrationNumber = as.character(`bib-nbr`),
         CATAthleteFullName = paste(`first-name`, `last-name`, sep = " "),
         CATAthleteRegisteredClub = as.factor(club),
         CATAgeGroup = as.factor(`age group`),
         CATAthleteDivision = as.factor(division),
         CATAthleteTeamNumber = as.factor(`team-nbr`),
         CATEventDiscipline = as.factor(discipline),
         NUMDistance = "is.na<-"(distance, distance == 0), #set NA for distances of zero
         CATDistance = as.factor(NUMDistance),
         NUMStandardDistance = "is.na<-"(`std-distance`, `std-distance` == 0), #set NA for distances of zero
         CATStandardDistance = as.factor(NUMStandardDistance),
         CATEventSpecification = as.factor(spec),
         CATEventHeat = as.factor(heat), # we can order this, but theres up to 33 levels.
         NUMPerformance = "is.na<-"(perf, perf == 0), #set NA for distances of zero
         TXTPerformanceFormatted = as.character(`std-perf`),
         NUMWindReading = as.numeric(wind),
         NUMPointsAwarded = "is.na<-"(points, points == 0), # When points are zero this is usually due to INV or invalid attempt
         CATEventNote = as.character(ifelse(notes %in% c(" ", ""), "None", notes))
         )

#check the dataset again
#wrd_2=GetNewWrd()
#Desc(wrk.01DataPrep_1, wrd=wrd_2)

# Data Type configurations ==============================================================================================#
# Apply risk treatments, controls, data integrity checks, add in useful features #
#Set up AWD classifiers as search strings
# Search for pattern: with conditional OR: "TF##", "T/##", "F/##", "T/F##", "T/F ##"
AWDTFGroupings_Regex <- "(TF\\d{2})|(T/\\d{2})|(F/\\d{2})|(T/F\\d{2})|(T/F\\s\\d{2})" 
AWDCleanupFind.List <- list("/", " ") #for cleaning up final grouping, for consistency
AWDCleanupFind.String <- paste(unlist(AWDCleanupFind.List), collapse = "|") # | is OR
# ========#

wrk.01DataPrep_2 <- wrk.01DataPrep_1 %>%
  select(starts_with("KEY"), starts_with("BIN"), starts_with("NUM"), starts_with("CAT"), starts_with("ORD"), starts_with("TXT")) %>%
  mutate(BINValidEventAttempt = as.factor(ifelse(NUMPerformance %ni% c("NA") & TXTPerformanceFormatted %ni% c("DQ","DNS", "DNF", "NM", "NT", "00.00"), 1, 0))) %>%
  mutate(BINInvitationEventOrAthlete = as.factor(ifelse(grepl("INV", CATEventNote), 1, 0))) %>%
  mutate(CATEventNote = factor(CATEventNote)) %>%
  mutate(BINTeamEvent = as.factor(ifelse(KEYRegistrationNumber == "0", 1,0  ))) %>%
  mutate(CATEventFullName = as.factor(trimws(paste(CATDistance, CATEventDiscipline, CATEventSpecification, sep = " " )))) %>%
  mutate(CATEventFullName = factor(gsub("NA ","",CATEventFullName))) %>%
  mutate(CATEventGroupL1 = as.factor(case_when(CATEventDiscipline %in% c("Shot Put", "Discus","Hammer","Javelin","High Jump","Pole Vault","Long Jump","Triple Jump") ~ "Field", CATEventDiscipline %in% c("Run", "Walk", "Hurdles", "Steeple", "Relay" ) ~ "Track"))) %>%
  mutate(CATEventGroupL2 = as.factor(case_when(CATEventDiscipline %in% c("Shot Put", "Discus","Hammer","Javelin") ~ "Throw", CATEventDiscipline %in% c("High Jump","Pole Vault","Long Jump","Triple Jump") ~ "Jump", CATEventDiscipline %in% c("Run", "Walk", "Hurdles", "Steeple", "Relay" ) ~ "Run"))) %>%
  mutate(CATAgeGroupLeveL1 = as.factor(case_when(CATAgeGroup %in% c("MOP","FOP") ~ "Open", CATAgeGroup %in% c("F14","M14","F16","M16","F18","M18","F20","M20") ~ "Juniors", CATAgeGroup %in% c("F40","M40","F50","M50","F60","M60") ~ "Veteran"))) %>% 
  mutate(CATAgeGroupKey = case_when(CATAgeGroup %in% c("F40","M40") ~ "40", CATAgeGroup %in% c("F50","M50") ~ "50", CATAgeGroup %in% c("F60","M60") ~ "60", TRUE ~ "")) %>%
  mutate(BINAthleteWithDisability = as.factor(ifelse(str_detect(CATEventNote, AWDTFGroupings_Regex) == TRUE | str_detect(CATAthleteFullName, AWDTFGroupings_Regex) == TRUE, 1, 0))) %>%
  mutate(CATAthleteAWDClass = str_extract(CATEventNote, AWDTFGroupings_Regex)) %>% #use string extract to return a character vector, compatible with dataframe
  mutate(CATAthleteAWDClass2 = str_extract(CATAthleteFullName, AWDTFGroupings_Regex), CATAthleteAWDClass) %>% #check in full name for pattern as well
  mutate(CATAthleteAWDClass = ifelse(is.na(CATAthleteAWDClass),CATAthleteAWDClass2, CATAthleteAWDClass)) %>%
  mutate(CATAthleteAWDClass = factor(gsub(AWDCleanupFind.String, replacement = "", x = CATAthleteAWDClass))) %>%
  select(-CATAthleteAWDClass2) #drop the temporary calc column

# Data Cleanup on event specifications. Fill in missing specifications when the event is hurdles, steeple, or any throws except for hammer.
# REF http://athsvic.org.au/wp-content/uploads/AV_SummerHandbook2017-18_WEB02.pdf
wrk.01DataPrep_3 <- wrk.01DataPrep_2 %>%
  mutate(CATEventSpecification = as.character(CATEventSpecification)) %>%
  mutate(CATEventSpecification = ifelse(CATEventSpecification == "" & CATEventFullName == "100 Hurdles" & CATAgeGroup %in% c("F14", "F16", "F18", "F40"), "76cm", 
                                 ifelse(CATEventSpecification == "" & CATEventFullName == "100 Hurdles" & CATAgeGroup %in% c("F20", "FOP", "M14", "M16"), "84cm",
                                 ifelse(CATEventSpecification == "" & CATEventFullName == "110 Hurdles" & CATAgeGroup %in% c("M16"), "84cm",
                                 ifelse(CATEventSpecification == "" & CATEventFullName == "110 Hurdles" & CATAgeGroup %in% c("M18"), "91cm",
                                 ifelse(CATEventSpecification == "" & CATEventFullName == "200 Hurdles" & CATAgeGroup %in% c("M18"), "76cm",
                                 ifelse(CATEventSpecification == "" & CATEventFullName == "300 Hurdles" & CATAgeGroup %in% c("F40", "F50","F60", "M40", "M50", "M60"), "76cm",
                                 ifelse(CATEventSpecification == "" & CATEventFullName == "400 Hurdles" & CATAgeGroup %in% c("F16", "F18", "F20", "FOP", "F40"), "76cm",
                                 ifelse(CATEventSpecification == "" & CATEventFullName == "400 Hurdles" & CATAgeGroup %in% c("M16", "M18"), "84cm",
                                 ifelse(CATEventSpecification == "" & CATEventFullName == "90 Hurdles" & CATAgeGroup %in% c("M16", "F14", "F16"), "76cm",
                                 ifelse(CATEventSpecification == "" & CATEventFullName == "3000 Steeple" & CATAgeGroup %in% c("M18", "MOP"), "91cm",
                                 ifelse(CATEventSpecification == "" & CATEventFullName == "3000 Steeple" & CATAgeGroup %in% c("F16", "F18", "F40", "FOP"), "76cm",
                                 ifelse(CATEventSpecification == "" & CATEventFullName == "Discus" & CATAgeGroup %in% c("M14","M16", "M60", "F14", "F16", "F18", "F20", "FOP", "F40", "F50", "F60"), "1kg",
                                 ifelse(CATEventSpecification == "" & CATEventFullName == "Discus" & CATAgeGroup %in% c("M18", "M50"), "1.5kg",
                                 ifelse(CATEventSpecification == "" & CATEventFullName == "Discus" & CATAgeGroup %in% c("M20"), "1.75kg",
                                 ifelse(CATEventSpecification == "" & CATEventFullName == "Discus" & CATAgeGroup %in% c("MOP", "M40"), "2kg",
                                 ifelse(CATEventSpecification == "" & CATEventFullName == "Javelin" & CATAgeGroup %in% c("F14"), "400g",
                                 ifelse(CATEventSpecification == "" & CATEventFullName == "Javelin" & CATAgeGroup %in% c("F16", "F18", "F50", "F60"), "500g",
                                 ifelse(CATEventSpecification == "" & CATEventFullName == "Javelin" & CATAgeGroup %in% c("M14", "M60", "F20", "FOP", "F40"), "600g",
                                 ifelse(CATEventSpecification == "" & CATEventFullName == "Javelin" & CATAgeGroup %in% c("M16", "M18", "M50"), "700g",
                                 ifelse(CATEventSpecification == "" & CATEventFullName == "Javelin" & CATAgeGroup %in% c("M20", "MOP", "M40"), "800g",
                                 ifelse(CATEventSpecification == "" & CATEventFullName == "Shot Put" & CATAgeGroup %in% c("M14", "F14", "F16", "F18", "F50", "F60"), "3kg",
                                 ifelse(CATEventSpecification == "" & CATEventFullName == "Shot Put" & CATAgeGroup %in% c("M16", "F20", "FOP", "F40"), "4kg",
                                 ifelse(CATEventSpecification == "" & CATEventFullName == "Shot Put" & CATAgeGroup %in% c("M18", "M60"), "5kg",
                                 ifelse(CATEventSpecification == "" & CATEventFullName == "Shot Put" & CATAgeGroup %in% c("M20", "M50"), "6kg",
                                 ifelse(CATEventSpecification == "" & CATEventFullName == "Shot Put" & CATAgeGroup %in% c("MOP", "M40"), "7.26kg", 
                                 CATEventSpecification )))))))))))))))))))))))))) %>%
  # recalc CATEventFullName now spec is fixed.
  mutate(CATEventFullName = as.factor(trimws(paste(CATDistance, CATEventDiscipline, CATEventSpecification, sep = " " )))) %>%
  mutate(CATEventFullName = factor(gsub("NA ","", CATEventFullName))) 

# Calculated results using group wise
wrk.01DataPrep_4 <- wrk.01DataPrep_3 %>%
  group_by(ORDCompetitionRound, CATCompetitionVenue, CATAgeGroup, CATEventDiscipline, CATDistance) %>%
  #Calculate "real" finishing order
  mutate(ORDEventFinishOrder = ifelse(CATEventGroupL1 == "Track", order(order(NUMPerformance, decreasing = FALSE)), # Calculate finishing order for track events
                               (ifelse(CATEventGroupL1 == "Field", order(order(NUMPerformance, decreasing = TRUE)),0)))) %>% # Calculate finishing order for field events
  #Calculate registered athlete finishing order
  mutate(ORDEventFinishOrderPoints = ifelse(CATEventGroupL1 == "Track" & BINInvitationEventOrAthlete == 0, order(order(BINInvitationEventOrAthlete, NUMPerformance, decreasing = FALSE)), # Calculate finishing order for track events
                                    (ifelse(CATEventGroupL1 == "Field" & BINInvitationEventOrAthlete == 0, order(order(rev(BINInvitationEventOrAthlete), NUMPerformance, decreasing = TRUE)),0)))) %>%
  #Calclate total registered athletes in grouping
  mutate(NUMTotAthletesInVenueAgeEventPoints = max(ORDEventFinishOrderPoints)) %>%
  add_tally() %>% #count the total number of rows in the by group assign to variable "n"
  rename(NUMTotAthletesInVenueAgeEvent = n) %>% # RENAME PARAMETERS (NEW NAME = OLD NAME).
  ungroup() %>% #if you use group_by, also use ungroup() to save heartache later. Return to LONG form.
  group_by(ORDCompetitionRound, CATAgeGroup, CATEventDiscipline, CATDistance) %>% #GROUP BY ROUND
  #Calculate round finishing order (all other athletes across vic who competed in same event in same round)
  mutate(ORDRoundEventFinishOrder = ifelse(CATEventGroupL1 == "Track", order(order(NUMPerformance, decreasing = FALSE)), # Calculate finishing order for track events
                                      (ifelse(CATEventGroupL1 == "Field", order(order(NUMPerformance, decreasing = TRUE)),0)))) %>% # Calculate finishing order for field events
  #Calculate registered athlete finishing order
  mutate(ORDRoundEventFinishOrderPoints = ifelse(CATEventGroupL1 == "Track" & BINInvitationEventOrAthlete == 0, order(order(BINInvitationEventOrAthlete, NUMPerformance, decreasing = FALSE)), # Calculate finishing order for track events
                                         (ifelse(CATEventGroupL1 == "Field" & BINInvitationEventOrAthlete == 0, order(order(rev(BINInvitationEventOrAthlete), NUMPerformance, decreasing = TRUE)),0)))) %>%
  #Calclate total registered athletes in grouping
  mutate(NUMTotAthletesInRoundAgeEventPoints = max(ORDRoundEventFinishOrderPoints)) %>%
  add_tally() %>% #count the total number of rows in the by group assign to variable "n"
  rename(NUMTotAthletesInRoundAgeEvent = n) %>% # RENAME PARAMETERS (NEW NAME = OLD NAME).
  ungroup() #if you use group_by, also use ungroup() to save heartache later. Return to LONG form.
  
  

# Save Feather file to store progress:
write_feather(wrk.01DataPrep_4, "D:/Data Science/Athletics Data/Project Files/athletics_data_analysis/Data/Processed/wrk.01DataPrep_4.feather")

# ======================================================================================================================== #
# END OF PROGRAM #
# ======================================================================================================================== #