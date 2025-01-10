# Information ----
# Reformatting CASTOR Data to better fit R
# Author: Diego Rodríguez Esperante
# Date of creation: 06/12/2024
# Last edited: 06/12/2024

# Packages ----
require(rstudioapi)
require(ggplot2)
require(stringr)
require(dplyr)
require(datetime)
require(nlme)
require(lme4)

# Load data ----
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

survey_folder = paste0(c(getwd(), "Data", "Castor Data"), collapse = "/")
files = list.files(survey_folder)

survey_files = list(survey = c("TRACK", "ISAAC", "TAPQOL", "EQ-5D-Y", "EQ-5D-5L", "HCSRU", "ADEM2_export"),
                    file = rep("", 6),
                    id = rep(0, 6))
surveys = list()

for (i in 1:length(survey_files$survey)){
  survey_files$file[i] = files[match(T, grepl(survey_files$survey[i], files))]
  survey_files$id[i] = match(T, grepl(survey_files$survey[i], files))
  
  surveys[[i]] = read.csv(paste0(c(survey_folder, survey_files$file[i]), collapse = "/"), sep = ";")
}

names(surveys) = gsub("-", "_", survey_files$survey)

study_key = read.csv(paste0(c(survey_folder, "study_variablelist.csv"), collapse = "/"), sep = ";")
survey_key = read.csv(paste0(c(survey_folder, "survey_variablelist.csv"), collapse = "/"), sep = ";")

rm(i)
# Cleaning each survey ----
## Common cleaning ----
for (i in 1:length(surveys)){
  rm_col = c("Survey.Instance.Id", "Survey.Package.Id")
  surveys[[i]] = surveys[[i]] %>% select(!contains(rm_col))
  
  datecols = c("Survey.Creation.Date", "Survey.Sent.Date", "Survey.Completed.On", "Participant.Creation.Date",
                study_key$Variable.name[(study_key$Original.field.type == "date" | study_key$Original.field.type == "datetime")])
  
  repcols = datecols[datecols %in% colnames(surveys[[i]])]
  for (j in 1:length(repcols)){
    surveys[[i]][[repcols[j]]] = as.Date(surveys[[i]][[repcols[j]]], format = "%d-%m-%Y")
    
    invDate = surveys[[i]][[repcols[j]]] > Sys.Date()
    invDate[is.na(invDate)] = FALSE
    surveys[[i]][invDate,repcols[j]] = NA
  }
}

rm(invDate)
# Bin out time
daterangecols = c("V1_Incl_VisDat", "V2_Visit_Dat", "V3_Visit_Dat", "V4_Visit_Dat", "V5_Visit_Dat", "Survey.Sent.Date")
date_range = c(as.Date(surveys$TRACK$Survey.Creation.Date[1], format = "%d-%m-%Y"),
               as.Date(surveys$TRACK$Survey.Creation.Date[1], format = "%d-%m-%Y"))
for (i in 1:length(surveys)){
  maxmin = c(min(as.matrix(surveys[[i]][, colnames(surveys[[i]])%in% daterangecols]), na.rm = T),
             max(as.matrix(surveys[[i]][, colnames(surveys[[i]])%in% daterangecols]), na.rm = T))
  if(maxmin[1] < date_range[1]){
    date_range[1] = maxmin[1]
  }
  if((maxmin[2] > date_range[1])&(maxmin[2] < Sys.Date())){
    date_range[2] = maxmin[2]
  }
}

rm(rm_col, datecols, repcols, maxmin, i, j)
## Cleaning TRACK Survey ----
# Add total TRACK score
surveys$TRACK$TRACKSUM = rowSums(surveys$TRACK %>% select(c(TRACK1:TRACK5)))

surveys$TRACK$TRACKTHRESH = surveys$TRACK$TRACKSUM >= 80
surveys$TRACK$TRACKTHRESH = as.factor(surveys$TRACK$TRACKTHRESH)
levels(surveys$TRACK$TRACKTHRESH) = c("NotWellControlled", "WellControlled")

# Merge data frames of surveys
TRACK_sub = surveys$TRACK[,c(1, 3, 6, 8, 14, 15)]
ADEM2_sub = surveys$ADEM2_export %>% select(Participant.Id, V1_Rand_RandGroup, V1_Rand_BreathResult)

TRACK_merge = merge(ADEM2_sub, TRACK_sub, by.x = "Participant.Id", by.y = "Castor.Participant.ID")

TRACK_merge$V1_Rand_RandGroup = as.factor(TRACK_merge$V1_Rand_RandGroup)
levels(TRACK_merge$V1_Rand_RandGroup) = c("UsualCare", "Intervention")

ADEM2_sub$V1_Rand_RandGroup = as.factor(ADEM2_sub$V1_Rand_RandGroup)
levels(ADEM2_sub$V1_Rand_RandGroup) = c("UsualCare", "Intervention")

TRACK_merge$V1_Rand_BreathResult = as.factor(TRACK_merge$V1_Rand_BreathResult)
levels(TRACK_merge$V1_Rand_BreathResult) = c("Asthma", "TransWheeze", "UsualCare")
TRACK_merge$V1_Rand_BreathResult = relevel(TRACK_merge$V1_Rand_BreathResult, "UsualCare")

ADEM2_sub$V1_Rand_BreathResult = as.factor(ADEM2_sub$V1_Rand_BreathResult)
levels(ADEM2_sub$V1_Rand_BreathResult) = c("Asthma", "TransWheeze", "UsualCare")
ADEM2_sub$V1_Rand_BreathResult = relevel(ADEM2_sub$V1_Rand_BreathResult, "UsualCare")

TRACK_merge$Survey.Package.Name = t(as.data.frame(str_split(TRACK_merge$Survey.Package.Name, " : ")))[,2]

# Replicating Moniek's analysis ----
date_options = list(day_bwidth = 90,
                    days = c(0, 180, 360, 520),
                    start_day = 0,
                    end_day = 520,
                    MoniekMethod = FALSE)

TRACK_merge$DaysSince1stVisit = rep(NA, nrow(TRACK_merge))
TRACK_merge$StartEndVisit = rep(NA, nrow(TRACK_merge))

pat_list = ADEM2_sub$Participant.Id

pat_startend = data.frame(pat = pat_list,
                          group = ADEM2_sub$V1_Rand_RandGroup,
                          breathRes = ADEM2_sub$V1_Rand_BreathResult,
                          start = rep(NA, length(pat_list)),
                          end = rep(NA, length(pat_list)),
                          change = rep(NA, length(pat_list)))

pat_days = as.data.frame(matrix(nrow = length(pat_list), ncol = length(date_options$days)))
colnames(pat_days) = paste0("day", date_options$days)

pat_startend = cbind(pat_startend, pat_days)

## Start-End analysis ----
# Sticking to Moniek's method
if(date_options$MoniekMethod){
  TRACK_merge$StartEndVisit = rep(NA, nrow(TRACK_merge))
  incl_start = (abs(TRACK_merge$DaysSince1stVisit - date_options$start_day) <= date_options$day_bwidth)
  TRACK_merge$StartEndVisit[incl_start] = "Start"
  
  incl_end = (abs(TRACK_merge$DaysSince1stVisit - date_options$end_day) <= date_options$day_bwidth)
  TRACK_merge$StartEndVisit[incl_end] = "End"
  
  for (i in 1:length(pat_list)){
    this_pat = TRACK_merge$Participant.Id == pat_list[i]
    
    pat_startend$start[i] = mean(TRACK_merge$TRACKSUM[this_pat & TRACK_merge$StartEndVisit %in% "Start"])
    pat_startend$end[i] = mean(TRACK_merge$TRACKSUM[this_pat & TRACK_merge$StartEndVisit %in% "End"])
    pat_startend$change[i] = pat_startend$end[i] - pat_startend$start[i]
    
    for (j in 1:length(date_options$days)){
      incl_day = (abs(TRACK_merge$DaysSince1stVisit - date_options$days[j]) <= date_options$day_bwidth)
      pat_startend[i, 6+j] = mean(TRACK_merge$TRACKSUM[this_pat & incl_day])
    }
  }
  
  rm(i, incl_start, incl_end, this_pat, incl_day)
}else{ # My own method
  for (i in 1:length(pat_list)){
    this_pat = TRACK_merge$Participant.Id == pat_list[i]
    
    incl_dates = TRACK_merge$Survey.Creation.Date[this_pat]
    TRACK_merge$DaysSince1stVisit[this_pat] = incl_dates - min(incl_dates)
    
    # Diverges from Moniek, she takes all inclusions within 90 days of the first and last, I think it's a mistake
    close_start = which.min(abs((incl_dates - min(incl_dates)) - date_options$start_day))
    close_end = which.min(abs((incl_dates - min(incl_dates)) - date_options$end_day))
    
    TRACK_merge$StartEndVisit[this_pat][close_start] = "Start"
    TRACK_merge$StartEndVisit[this_pat][close_end] = "End"
    
    pat_startend$start[i] = TRACK_merge$TRACKSUM[this_pat][close_start]
    pat_startend$end[i] = TRACK_merge$TRACKSUM[this_pat][close_end]
    pat_startend$change[i] = pat_startend$end[i] - pat_startend$start[i]
    
    for (j in 1:length(date_options$days)){
      incl_day = which.min(abs(TRACK_merge$DaysSince1stVisit - date_options$days[j]) <= date_options$day_bwidth)
      pat_startend[i, 6+j] = mean(TRACK_merge$TRACKSUM[this_pat][incl_day])
    }
  }
  
  rm(i, j, incl_dates, close_start, close_end, this_pat)
}

TRACK_merge$StartEndVisit = as.factor(TRACK_merge$StartEndVisit)

# Data frame with one row per patient, only first and last visit
TRACK_pat = ADEM2_sub
TRACK_pat = merge(TRACK_pat, TRACK_merge[TRACK_merge$StartEndVisit %in% "Start", c(1, 7)])
colnames(TRACK_pat)[colnames(TRACK_pat) == "TRACKSUM"] = "TRACKSUM_Start"
TRACK_pat = merge(TRACK_pat, TRACK_merge[TRACK_merge$StartEndVisit %in% "End", c(1, 7, 9)])
colnames(TRACK_pat)[colnames(TRACK_pat) == "TRACKSUM"] = "TRACKSUM_End"
colnames(TRACK_pat)[colnames(TRACK_pat) == "DaysSince1stVisit"] = "LastVisit"

# By Group
grp = 1
mean(pat_startend$change[pat_startend$group == levels(pat_startend$group)[grp]], na.rm = T)
sd(pat_startend$change[pat_startend$group == levels(pat_startend$group)[grp]], na.rm = T)

# By Care
care = 1
mean(pat_startend$change[pat_startend$breathRes == levels(pat_startend$breathRes)[care]], na.rm = T)
sd(pat_startend$change[pat_startend$breathRes == levels(pat_startend$breathRes)[care]], na.rm = T)

# By day
day = 1
care = 1
mean(pat_startend[pat_startend$breathRes == levels(pat_startend$breathRes)[care], 6+day], na.rm = T)
sd(pat_startend[pat_startend$breathRes == levels(pat_startend$breathRes)[care], 6+day], na.rm = T)

rm(pat_days, grp, day, care)

# Statistical tests ----
# Chosen tests from https://www.scribbr.com/statistics/statistical-tests/
## Statistical tests ----
# Checking statistical assumptions to choose parametric or non-parametric test
shapiro.test(TRACK_merge$TRACKSUM) # Normality of TRACK results
fligner.test(TRACKSUM ~ V1_Rand_BreathResult, data = TRACK_merge, na.action = "na.omit") # Variance-independence
# We expect variables to be correlated, since they are different indicators for the health of the child.
# Because distribution is not normally distributed, we are not confident that variance is homogeneous,
# and we expect variables to be correlated we must take non-parametric tests.

# All tests, patients repeated
# Categorical predictor with three groups of quantitative outcome: Kruskal-Wallis
kruskal.test(TRACKSUM ~ V1_Rand_BreathResult, data = TRACK_merge, na.action = "na.omit")
# Categorical predictor with two groups of quantitative outcome: Sign test
binom.test(sum(TRACK_merge$TRACKSUM >= 80, na.rm = T), sum(!is.na(TRACK_merge$TRACKSUM)), alternative = "two.sided")

# Test if difference between start and end
wilcox.test(TRACK_pat$TRACKSUM_Start, TRACK_pat$TRACKSUM_End, paired = TRUE, na.action = "na.omit")
wilcox.test(TRACK_pat$TRACKSUM_Start[TRACK_pat$V1_Rand_BreathResult == "Asthma"], TRACK_pat$TRACKSUM_End[TRACK_pat$V1_Rand_BreathResult == "Asthma"], paired = TRUE, na.action = "na.omit")
wilcox.test(TRACK_pat$TRACKSUM_Start[TRACK_pat$V1_Rand_BreathResult == "TransWheeze"], TRACK_pat$TRACKSUM_End[TRACK_pat$V1_Rand_BreathResult == "TransWheeze"], paired = TRUE, na.action = "na.omit")

## Linear mixed-effect models ----
