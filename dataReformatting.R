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

rm(invDate, rm_col, datecols, repcols, i, j)
## Cleaning TRACK Survey ----
# Add total TRACK score
surveys$TRACK$TRACKSUM = rowSums(surveys$TRACK %>% select(c(TRACK1:TRACK5)))

surveys$TRACK$TRACKTHRESH = surveys$TRACK$TRACKSUM >= 80
surveys$TRACK$TRACKTHRESH = as.factor(surveys$TRACK$TRACKTHRESH)
levels(surveys$TRACK$TRACKTHRESH) = c("NotWellControlled", "WellControlled")

# Merge data frames of surveys
TRACK_sub = surveys$TRACK[,c(1, 3, 6, 8, 14, 15)]
ADEM2_sub = surveys$ADEM2_export %>% select(Participant.Id, V1_Rand_RandGroup, V1_Rand_BreathInformDate, V1_Demo_DOB)

breathresults = read.csv("Data/Breathtest_result.csv")
breathresults$Breath.test.result[breathresults$Breath.test.result %in% "ASTMA"] = "ASTHMA"
breathresults$Breath.test.result = as.factor(breathresults$Breath.test.result)
levels(breathresults$Breath.test.result) = c("Asthma", "TransWheeze")

ADEM2_sub = merge(ADEM2_sub, breathresults, by.x = "Participant.Id", by.y = "Study.ID", all = T)
ADEM2_sub = ADEM2_sub[!is.na(ADEM2_sub$V1_Rand_RandGroup),]
colnames(ADEM2_sub)[colnames(ADEM2_sub) %in% "Breath.test.result"] = "V1_Rand_BreathResult"

TRACK_merge = merge(ADEM2_sub, TRACK_sub, by.x = "Participant.Id", by.y = "Castor.Participant.ID")

TRACK_merge$V1_Rand_RandGroup = as.factor(TRACK_merge$V1_Rand_RandGroup)
levels(TRACK_merge$V1_Rand_RandGroup) = c("UsualCare", "Intervention")

ADEM2_sub$V1_Rand_RandGroup = as.factor(ADEM2_sub$V1_Rand_RandGroup)
levels(ADEM2_sub$V1_Rand_RandGroup) = c("UsualCare", "Intervention")

TRACK_merge$V1_Rand_BreathResult = as.factor(TRACK_merge$V1_Rand_BreathResult)
levels(TRACK_merge$V1_Rand_BreathResult) = c("Asthma", "TransWheeze")
TRACK_merge$V1_Rand_BreathResult = relevel(TRACK_merge$V1_Rand_BreathResult, "TransWheeze")

ADEM2_sub$V1_Rand_BreathResult = as.factor(ADEM2_sub$V1_Rand_BreathResult)
levels(ADEM2_sub$V1_Rand_BreathResult) = c("Asthma", "TransWheeze")
ADEM2_sub$V1_Rand_BreathResult = relevel(ADEM2_sub$V1_Rand_BreathResult, "TransWheeze")

TRACK_merge$Survey.Package.Name = t(as.data.frame(str_split(TRACK_merge$Survey.Package.Name, " : ")))[,2]

TRACK_merge$AgeAtVisit = as.numeric(TRACK_merge$Survey.Creation.Date - TRACK_merge$V1_Demo_DOB)/365

rm(breathresults)

## Cleaning ISAAC Survey ----
ISAAC_qs = list(questions = c("ISAACa02"),
                levels = list(c("No", "Yes")))

names(ISAAC_qs$levels) = ISAAC_qs$questions

ISAAC_sub = surveys$ISAAC[,c(1,3,8:50)]

ISAAC_sub$Survey.Package.Name = t(as.data.frame(str_split(ISAAC_sub$Survey.Package.Name, " : ")))[,2]

# Replicating Moniek's analysis ----
date_options = list(day_bwidth = 90,
                    days = c(0, 180, 360, 520),
                    start_day = 0,
                    end_day = 730,
                    MoniekMethod = FALSE)

## Start-End analysis ----
startend_analysis = function(TRACK_merge, date_options){
  TRACK_merge$DaysSince1stVisit = rep(NA, nrow(TRACK_merge))
  TRACK_merge$StartEndVisit = rep(NA, nrow(TRACK_merge))
  
  pat_list = ADEM2_sub$Participant.Id
  
  pat_startend = data.frame(pat = pat_list,
                            group = ADEM2_sub$V1_Rand_RandGroup,
                            breathRes = ADEM2_sub$V1_Rand_BreathResult,
                            start = rep(NA, length(pat_list)),
                            end = rep(NA, length(pat_list)),
                            change = rep(NA, length(pat_list)))
  
  if(date_options$MoniekMethod){
    pat_startend = data.frame(pat = pat_list,
                              group = ADEM2_sub$V1_Rand_RandGroup,
                              breathRes = ADEM2_sub$V1_Rand_BreathResult,
                              start = rep(NA, length(pat_list)),
                              end = rep(NA, length(pat_list)),
                              change = rep(NA, length(pat_list)))
    
    pat_days = as.data.frame(matrix(nrow = length(pat_list), ncol = length(date_options$days)))
    colnames(pat_days) = paste0("day", date_options$days)
    
    pat_startend = cbind(pat_startend, pat_days)
    
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
      start = abs((incl_dates - min(incl_dates)) - date_options$start_day)
      close_start = which.min(start)
      end = abs((incl_dates - min(incl_dates)) - date_options$end_day)
      close_end = which.min(end)
      
      if(close_start %in% which(start<date_options$day_bwidth)){
        TRACK_merge$StartEndVisit[this_pat][close_start] = "Start"
      }
      if(close_end %in% which(end<date_options$day_bwidth)){
        TRACK_merge$StartEndVisit[this_pat][close_end] = "End"
      }
      
      # I end up not using pat_startend for anything, but I'll keep it here just in case
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
  
  return(TRACK_merge)
}

TRACK_merge_se = startend_analysis(TRACK_merge, date_options)

# Data frame with one row per patient, only first and last visit
startend_ByPatient = function(survey_df, patient_df){
  surveypatient_df = patient_df
  surveypatient_df = merge(surveypatient_df, survey_df[survey_df$StartEndVisit %in% "Start", c("Participant.Id", "TRACKSUM", "AgeAtVisit")])
  colnames(surveypatient_df)[colnames(surveypatient_df) == "TRACKSUM"] = "TRACKSUM_Start"
  colnames(surveypatient_df)[colnames(surveypatient_df) == "AgeAtVisit"] = "Age1stVisit"
  surveypatient_df = merge(surveypatient_df, survey_df[survey_df$StartEndVisit %in% "End", c("Participant.Id", "TRACKSUM", "DaysSince1stVisit")])
  colnames(surveypatient_df)[colnames(surveypatient_df) == "TRACKSUM"] = "TRACKSUM_End"
  colnames(surveypatient_df)[colnames(surveypatient_df) == "DaysSince1stVisit"] = "LastVisit"
  
  surveypatient_df$TRACKSUM_Diff = surveypatient_df$TRACKSUM_End - surveypatient_df$TRACKSUM_Start
  
  return(surveypatient_df)
}

TRACK_pat = startend_ByPatient(TRACK_merge_se, ADEM2_sub)

# Incorporate ISAAC QoL results
TRACK_pat = merge(TRACK_pat, ISAAC_sub[ISAAC_sub$Survey.Package.Name == "baselinevisite", c("Castor.Participant.ID", ISAAC_qs$questions)], by.x = "Participant.Id", by.y = "Castor.Participant.ID")
for (i in 1:length(ISAAC_qs$questions)){
  TRACK_pat[,ISAAC_qs$questions[i]] = as.factor(TRACK_pat[,ISAAC_qs$questions[i]])
  levels(TRACK_pat[,ISAAC_qs$questions[i]]) = ISAAC_qs$levels[[i]]
}

# Statistical tests ----
# Chosen tests from https://www.scribbr.com/statistics/statistical-tests/
## Statistical tests ----
# Checking normality
patbools = list(UsualCare = TRACK_pat$V1_Rand_RandGroup %in% "UsualCare",
                Intervention = TRACK_pat$V1_Rand_RandGroup %in% "Intervention",
                Asthma = TRACK_pat$V1_Rand_BreathResult %in% "Asthma",
                TransWheeze = TRACK_pat$V1_Rand_BreathResult %in% "TransWheeze")

histbins = seq(min(TRACK_pat$TRACKSUM_Diff, na.rm = T), max(TRACK_pat$TRACKSUM_Diff, na.rm = T), by = 5)

hist_txt = list(main = "Histogram of TRACK score differences", 
                xlab = "Score difference between start and end TRACK results")

hist(TRACK_pat$TRACKSUM_Diff, breaks =  histbins, main = hist_txt$main, xlab = hist_txt$xlab)
hist(TRACK_pat$TRACKSUM_Diff[patbools$UsualCare], breaks =  histbins, main = paste0(hist_txt$main, "\n for control patients (n = ", sum(patbools$UsualCare), ")"), xlab = hist_txt$xlab)
hist(TRACK_pat$TRACKSUM_Diff[patbools$Intervention], breaks =  histbins, main = paste0(hist_txt$main, "\n for intervention patients (n = ", sum(patbools$Intervention), ")"), xlab = hist_txt$xlab)
hist(TRACK_pat$TRACKSUM_Diff[patbools$UsualCare & patbools$Asthma], breaks =  histbins, main = paste0(hist_txt$main, "\n for control patients with Asthma (n = ", sum(patbools$UsualCare & patbools$Asthma), ")"), xlab = hist_txt$xlab)
hist(TRACK_pat$TRACKSUM_Diff[patbools$Intervention & patbools$Asthma], breaks =  histbins, main = paste0(hist_txt$main, "\n for intervention patients with Asthma (n = ", sum(patbools$Intervention & patbools$Asthma), ")"), xlab = hist_txt$xlab)
hist(TRACK_pat$TRACKSUM_Diff[patbools$UsualCare & patbools$TransWheeze], breaks =  histbins, main = paste0(hist_txt$main, "\n for control patients with TransWheeze (n = ", sum(patbools$UsualCare & patbools$TransWheeze), ")"), xlab = hist_txt$xlab)
hist(TRACK_pat$TRACKSUM_Diff[patbools$Intervention & patbools$TransWheeze], breaks =  histbins, main = paste0(hist_txt$main, "\n for intervention patients with TransWheeze (n = ", sum(patbools$Intervention & patbools$TransWheeze), ")"), xlab = hist_txt$xlab)

# Non-parametric tests of difference results
wilcox.test(TRACK_pat$TRACKSUM_Start, TRACK_pat$TRACKSUM_End, paired = TRUE, na.action = "na.omit")
wilcox.test(TRACK_pat$TRACKSUM_Start[patbools$Asthma], TRACK_pat$TRACKSUM_End[patbools$Asthma], paired = TRUE, na.action = "na.omit")
wilcox.test(TRACK_pat$TRACKSUM_Start[patbools$TransWheeze], TRACK_pat$TRACKSUM_End[patbools$TransWheeze], paired = TRUE, na.action = "na.omit")

# Parametric tests of difference results
t.test(TRACK_pat$TRACKSUM_Diff[patbools$UsualCare], TRACK_pat$TRACKSUM_Diff[patbools$Intervention], na.action = "na.omit")
t.test(TRACK_pat$TRACKSUM_Diff[patbools$Asthma], TRACK_pat$TRACKSUM_Diff[patbools$TransWheeze], na.action = "na.omit")

t.test(TRACK_pat$TRACKSUM_Diff[patbools$UsualCare & patbools$Asthma], TRACK_pat$TRACKSUM_Diff[patbools$Intervention & patbools$Asthma], na.action = "na.omit")
t.test(TRACK_pat$TRACKSUM_Diff[patbools$UsualCare & patbools$TransWheeze], TRACK_pat$TRACKSUM_Diff[patbools$Intervention & patbools$TransWheeze], na.action = "na.omit")

## Linear regression ----
TRACK_lm = lm(TRACKSUM_Diff ~ V1_Rand_BreathResult*V1_Rand_RandGroup + Age1stVisit + ISAACa02, data = TRACK_pat)
summary(TRACK_lm)

plot(TRACKSUM_Diff ~ V1_Rand_BreathResult*V1_Rand_RandGroup)
