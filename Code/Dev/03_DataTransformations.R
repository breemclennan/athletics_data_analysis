# R Script: 03_DataTransformations.R
# Description: Summary datasets ready for descriptive stats & analysis
# 1. Using the working dataset from 02_DataMerge
# 2. ..\Data\Processed\wrk.02_DataMerge_01.feather
# Author: Bree McLennan
# Date: 03/01/2018
#
# ======================================================================================================================== #

options(scipen = 10000) #prevent y axis from scaling
library(dplyr)
library(purrr)
library(data.table)
library(feather)
library(DescTools)
library(RDCOMClient)
library(glue)
library(rprojroot)
`%ni%` <- Negate(`%in%`)


# Define a function that computes file paths relative to where root .git folder is located
F <- is_git_root$make_fix_file() 
# Example usage: F("Data/Raw") , F("Data/Processed")

# Load feather data
wrk.03DataTrans_00 <- setDT(read_feather(glue(F("Data/Processed/wrk.02_DataMerge_01.feather"))))

#check the data before we begin to summarise
wrd_4 = GetNewWrd(header=TRUE)
Desc(wrk.03DataTrans_00, plotit = TRUE, maxrows = Inf, wrd = wrd_4)

# Groupings for calculations ===========
# 1. [Complete EVENT FINISHING ORDER] ROUND > VENUE > AGE GROUP/gender > EVENT > Distance(if track)   [FOR AGE GROUP specific finish order]
# 2. [Heat EVENT FINISHING ORDER] ROUND > VENUE > AGE GROUP/gender > EVENT > Distance(if track) > HEAT [FOR RACE SPECIFIC FINISH ORDER]
# 3. ROUND > VENUE > GENDER > EVENT > DISTANCE                            [FOR overall ranking by gender]

# FOR POINTS CALC: CHECK BUSINESS RULES.
# Consider heat event finishing order + check on eligibility for athletes competing at away venue vs home.
# REF: http://athsvic.org.au/wp-content/uploads/Scoring-2017-18-Shield.pdf
# 1st = 11 points
# 2nd to 9th = 11 minus finishing place = points
# 10th and over = 1 point

## 1A Apply alternative point scoring mechanism. First place in age group for round/venue/event/agegroup = 11 points.
wrk.03DataTrans_01 <- wrk.03DataTrans_00 %>%
  group_by(ORDCompetitionRound, CATCompetitionVenue, CATAgeGroup, CATEventDiscipline, CATDistance) %>%
  mutate(NUMEventFinishOrderPoints11 = case_when(ORDEventFinishOrderPoints == 1 ~ 11, ORDEventFinishOrderPoints %in% c(2,3,4,5,6,7,8,9) ~ 11-ORDEventFinishOrderPoints, ORDEventFinishOrderPoints >=10 ~ 1)) %>%
  mutate(NUMEventFinishOrderPoints11 = ifelse(BINValidEventAttempt == 0 | ORDEventFinishOrderPoints == 0, 0, NUMEventFinishOrderPoints11)) %>% #if invalid attempt or invitation, award no points.
  #if athlete competing invitationally, award no points.
  ungroup()  #if you use group_by, also use ungroup() to save heartache later. Return to LONG form.
  
## 1B Apply alternative point scoring mechanism. First place in age group for round/event/agegroup = 11 points.
wrk.03DataTrans_02 <- wrk.03DataTrans_01 %>%
  group_by(ORDCompetitionRound, CATAgeGroup, CATEventDiscipline, CATDistance) %>% #GROUP BY ROUND
  mutate(NUMRoundEventFinishOrderPoints11 = case_when(ORDRoundEventFinishOrderPoints == 1 ~ 11, ORDRoundEventFinishOrderPoints %in% c(2,3,4,5,6,7,8,9) ~ 11-ORDRoundEventFinishOrderPoints, ORDRoundEventFinishOrderPoints >=10 ~ 1)) %>%
  mutate(NUMRoundEventFinishOrderPoints11 = ifelse(BINValidEventAttempt == 0 | ORDRoundEventFinishOrderPoints == 0, 0, NUMRoundEventFinishOrderPoints11)) %>% #if invalid attempt or invitation, award no points.
  #if athlete competing invitationally, award no points.
  ungroup() 

## 1C Apply alternative point scoring mechanism. WITH AWD Adjustment applied to performance. First place in age group for round/event/agegroup = 11 points.
wrk.03DataTrans_03 <- wrk.03DataTrans_02 %>%
  group_by(ORDCompetitionRound, CATAgeGroup, CATEventDiscipline, CATDistance) %>% #GROUP BY ROUND
  mutate(NUMRoundEventFinishOrderPoints11AWDAdj = case_when(ORDRoundEventFinishOrderPointsAWDAdj == 1 ~ 11, ORDRoundEventFinishOrderPointsAWDAdj %in% c(2,3,4,5,6,7,8,9) ~ 11-ORDRoundEventFinishOrderPointsAWDAdj, ORDRoundEventFinishOrderPointsAWDAdj >=10 ~ 1)) %>%
  mutate(NUMRoundEventFinishOrderPoints11AWDAdj = ifelse(BINValidEventAttempt == 0 | ORDRoundEventFinishOrderPointsAWDAdj == 0, 0, NUMRoundEventFinishOrderPoints11AWDAdj)) %>% #if invalid attempt or invitation, award no points.
  #if athlete competing invitationally, award no points.
  ungroup() 

# ======================================================================================================= #
# who is the highest point scoring athlete, club, zone? and vs alt methods of point calc?
wrk.03DataTrans_SUMM01A <- wrk.03DataTrans_03 %>%
  filter(KEYRegistrationNumber %ni% c("0")) %>% #remove teams
  group_by(KEYRegistrationNumber, CATAgeGroup, CATAthleticClubName, CATClubZoneName) %>%
  summarise(NUMTotalPoints11 = sum(NUMEventFinishOrderPoints11, na.rm = TRUE), 
            NUMTotalAVPointsAwarded = sum(NUMPointsAwarded, na.rm = TRUE), 
            NUMTotalRoundPoints11 = sum(NUMRoundEventFinishOrderPoints11, na.rm = TRUE),
            NUMTotalRoundPoints11AWDAdj = sum(NUMRoundEventFinishOrderPoints11AWDAdj, na.rm = TRUE)) %>%
  arrange(desc(NUMTotalPoints11))

    par(mfrow = c(2,2))
    ### BY ATHLETE / REGISTRATION ID
    # Horizontal Barplot [Method 11]:
    barplot(rev(wrk.03DataTrans_SUMM01A$NUMTotalPoints11[1:20]),
            main = "AV Interclub 2017-18: Points awarded [11-1st]",
            col = rgb(0.2,0.4,0.6,0.6), horiz = TRUE , las = 1 ,
            xlab = "Total Points Awarded [11]",
            names.arg = rev(wrk.03DataTrans_SUMM01A$KEYRegistrationNumber[1:20]))
    head(wrk.03DataTrans_SUMM01A, n = 20)
    
    # Horizontal Barplot [Method Decathlon WR, AV]:
    wrk.03DataTrans_SUMM01B <- arrange(wrk.03DataTrans_SUMM01A, desc(NUMTotalAVPointsAwarded))
    
    barplot(rev(wrk.03DataTrans_SUMM01B$NUMTotalAVPointsAwarded[1:20]),
            main = "AV Interclub 2017-18: Points awarded [AV Shield]",
            col = rgb(0.2,0.4,0.6,0.6), horiz = TRUE , las = 1 ,
            xlab = "Total Points Awarded [AV Shield]",
            names.arg = rev(wrk.03DataTrans_SUMM01B$NUMTotalAVPointsAwarded[1:20]))
    head(wrk.03DataTrans_SUMM01B, n = 20)
    
    # Horizontal Barplot [Method 11 by Round]:
    wrk.03DataTrans_SUMM01C <- arrange(wrk.03DataTrans_SUMM01A, desc(NUMTotalRoundPoints11))
    
    barplot(rev(wrk.03DataTrans_SUMM01C$NUMTotalRoundPoints11[1:20]),
            main = "AV Interclub 2017-18: Points awarded [11-1st by round]",
            col = rgb(0.2,0.4,0.6,0.6), horiz = TRUE , las = 1 ,
            xlab = "Total Points Awarded [11 by round]",
            names.arg = rev(wrk.03DataTrans_SUMM01C$NUMTotalRoundPoints11[1:20]))
    head(wrk.03DataTrans_SUMM01C, n = 20)
    
    # Horizontal Barplot [Method 11 with AWD performance Adjust by Round]:
    wrk.03DataTrans_SUMM01D <- arrange(wrk.03DataTrans_SUMM01A, desc(NUMTotalRoundPoints11AWDAdj))
    
    barplot(rev(wrk.03DataTrans_SUMM01D$NUMTotalRoundPoints11AWDAdj[1:20]),
            main = "AV Interclub 2017-18: Points awarded [AWD Adjust & 11-1st by round]",
            col = rgb(0.2,0.4,0.6,0.6), horiz = TRUE , las = 1 ,
            xlab = "Total Points Awarded [AWD Adjust & 11 by round]",
            names.arg = rev(wrk.03DataTrans_SUMM01D$NUMTotalRoundPoints11AWDAdj[1:20]))
    head(wrk.03DataTrans_SUMM01D, n = 20)
    
  # ============ #
  ##### Total points by club and zone overall, 4 different point scoring methods:
  wrk.03DataTrans_SUMM02A <- wrk.03DataTrans_03 %>%
    group_by(CATAthleticClubName, CATClubZoneName) %>%
    summarise(NUMTotalPoints11 = sum(NUMEventFinishOrderPoints11, na.rm = TRUE), 
              NUMTotalAVPointsAwarded = sum(NUMPointsAwarded, na.rm = TRUE), 
              NUMTotalRoundPoints11 = sum(NUMRoundEventFinishOrderPoints11, na.rm = TRUE),
              NUMTotalRoundPoints11AWDAdj = sum(NUMRoundEventFinishOrderPoints11AWDAdj, na.rm = TRUE))
  
 par(mfrow = c(2,2), mar = c(5,6,4,1) + .1) 
 # CLUBS TOTAL: Horizontal Barplot [Method 11 by Round]:
  wrk.03DataTrans_SUMM02A <- arrange(wrk.03DataTrans_SUMM02A, desc(NUMTotalPoints11))
  barplot(rev(wrk.03DataTrans_SUMM02A$NUMTotalPoints11[1:10]), #TOP 10
          main = "AV Interclub 2017-18: Points awarded [11-1st]",
          col = rgb(0.2,0.4,0.6,0.6), horiz = TRUE , las = 1 ,
          xlab = "Total Points Awarded [11]",
          names.arg = rev(wrk.03DataTrans_SUMM02A$CATAthleticClubName[1:10]))
  head(wrk.03DataTrans_SUMM02A, n = 10)
  
  # CLUBS TOTAL: Horizontal Barplot [Method Decathlon WR, AV]:
  wrk.03DataTrans_SUMM02B <- arrange(wrk.03DataTrans_SUMM02A, desc(NUMTotalAVPointsAwarded)) %>%
    mutate(NUMTotalAVPointsAwarded = as.double(NUMTotalAVPointsAwarded))
  barplot(rev(wrk.03DataTrans_SUMM02B$NUMTotalAVPointsAwarded[1:10]), #TOP 10
          main = "AV Interclub 2017-18: Points awarded [AV Shield]",
          col = rgb(0.2,0.4,0.6,0.6), horiz = TRUE , las = 1 ,
          xlab = "Total Points Awarded [AV Shield]",
          names.arg = rev(wrk.03DataTrans_SUMM02B$CATAthleticClubName[1:10]))
  head(wrk.03DataTrans_SUMM02B, n = 10)
  
  # CLUBS TOTAL: Horizontal Barplot [Method 11 by Round]:
  wrk.03DataTrans_SUMM02C <- arrange(wrk.03DataTrans_SUMM02A, desc(NUMTotalRoundPoints11))
  barplot(rev(wrk.03DataTrans_SUMM02C$NUMTotalRoundPoints11[1:10]), #TOP 10
          main = "AV Interclub 2017-18: Points awarded [11-1st by round]",
          col = rgb(0.2,0.4,0.6,0.6), horiz = TRUE , las = 1 ,
          xlab = "Total Points Awarded [11 by round]",
          names.arg = rev(wrk.03DataTrans_SUMM02C$CATAthleticClubName[1:10]))
  head(wrk.03DataTrans_SUMM02C, n = 10)
  
  # CLUBS TOTAL: Horizontal Barplot [Method 11 with AWD performance Adjust by Round]:
  wrk.03DataTrans_SUMM02D <- arrange(wrk.03DataTrans_SUMM02A, desc(NUMTotalRoundPoints11AWDAdj))
  barplot(rev(wrk.03DataTrans_SUMM02D$NUMTotalRoundPoints11AWDAdj[1:10]),
          main = "AV Interclub 2017-18: Points awarded [AWD Adjust & 11-1st by round]",
          col = rgb(0.2,0.4,0.6,0.6), horiz = TRUE , las = 1 ,
          xlab = "Total Points Awarded [AWD Adjust & 11 by round]",
          names.arg = rev(wrk.03DataTrans_SUMM02D$CATAthleticClubName[1:10]))
  head(wrk.03DataTrans_SUMM02D, n = 10)


# ======================================================================================================= #

# how many competitions per athlete [round & venue as one key]. Total number and rolling total.

# how many events per athlete, for round & season. Total number and rolling total.

# how many athletes per round, per season. Is this congruent with "registered athletes"

# how many athletes competed in at minimum, one round?

# how many athletes competed at away venues (ignoring metro zone v zone)

# do athletes compete at more than one venue per round of competition?

# do athletes compete twice in the same event in a round of competition?

# what is the most windy venue and event?

# what is the most common event status when not "OK" ?, for which event?

# what is the most participated-in track event, field event?

# which athlete has the highest rate of participation?

# unique rego IDs by round


# ======================================================================================================================== #
# END OF PROGRAM #
# ======================================================================================================================== #