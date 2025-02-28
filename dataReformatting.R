# Information ----
# Reformatting CASTOR Data to better fit R
# Author: Diego Rodríguez Esperante
# Date of creation: 06/12/2024
# Last edited: 06/12/2024

# Packages ----
require(rstudioapi)
require(ggplot2)
require(stringr)
require(tidyr)
require(dplyr)
require(datetime)
require(lme4)
require(nlme)
require(scales)

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
TRACK_qs = list(questions = c("Castor.Participant.ID", "Survey.Creation.Date", "Survey.Completed.On", "Survey.Package.Name", "TRACKSUM"))

# Add total TRACK score
surveys$TRACK$TRACKSUM = rowSums(surveys$TRACK %>% select(c(TRACK1:TRACK5)))

surveys$TRACK$TRACKTHRESH = surveys$TRACK$TRACKSUM >= 80
surveys$TRACK$TRACKTHRESH = as.factor(surveys$TRACK$TRACKTHRESH)
levels(surveys$TRACK$TRACKTHRESH) = c("NotWellControlled", "WellControlled")

# Merge data frames of surveys
TRACK_sub = surveys$TRACK[,TRACK_qs$questions]
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

ISAAC_sub = surveys$ISAAC[,c("Castor.Participant.ID","Survey.Creation.Date","Survey.Package.Name", ISAAC_qs$questions)]

ISAAC_sub$Survey.Package.Name = t(as.data.frame(str_split(ISAAC_sub$Survey.Package.Name, " : ")))[,2]
colnames(ISAAC_sub)[colnames(ISAAC_sub) %in% "Castor.Participant.ID"] = "Participant.Id"

## Cleaning HCSRU Survey ----
HCSRU_qs = list(questions = c("HCSRU13a2", "HCSRU14a2", "HCSRU15a2", "HCSRU16a2", "HCSRU21_01"), # paste0("HCSRU19_", 1:10, "a")
                costs = c(33, 259, 116, 476, 34.75))

HCSRU_sub = surveys$HCSRU[,c("Castor.Participant.ID","Survey.Creation.Date","Survey.Package.Name", HCSRU_qs$questions)]

for (i in 1:length(HCSRU_qs$questions)){
  HCSRU_sub[is.na(HCSRU_sub[,HCSRU_qs$questions[i]]), HCSRU_qs$questions[i]] = 0
}

HCSRU_sub$TotalCost = as.numeric(as.matrix(HCSRU_sub[, HCSRU_qs$questions]) %*% as.matrix(HCSRU_qs$costs))
colnames(HCSRU_sub)[colnames(HCSRU_sub) %in% "Castor.Participant.ID"] = "Participant.Id"

# Replicating Moniek's analysis ----
date_options = list(day_bwidth = 90,
                    days = c(0, 180, 360, 520),
                    start_day = 0,
                    end_day = c(180, 360, 540, 720),
                    MoniekMethod = FALSE)

## Start-End analysis ----
startend_analysis = function(TRACK_merge, date_options){
  TRACK_merge$DaysSince1stVisit = rep(NA, nrow(TRACK_merge))
  TRACK_merge$StartEndVisit = rep(NA, nrow(TRACK_merge))
  
  pat_list = ADEM2_sub$Participant.Id
  pat_col = colnames(TRACK_merge)[first(which(grepl("Participant", colnames(TRACK_merge), ignore.case = T)))]
  
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
    TRACK_merge$StartEndVisit[incl_start] = "Day 0"
    
    incl_end = (abs(TRACK_merge$DaysSince1stVisit - date_options$end_day) <= date_options$day_bwidth)
    TRACK_merge$StartEndVisit[incl_end] = paste("Day", date_options$end_day)
    
    for (i in 1:length(pat_list)){
      this_pat = TRACK_merge$Participant.Id == pat_list[i]
      
      pat_startend$start[i] = mean(TRACK_merge$TRACKSUM[this_pat & TRACK_merge$StartEndVisit %in% "Day 0"])
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
      this_pat = TRACK_merge[,pat_col] == pat_list[i]
      
      incl_dates = TRACK_merge$Survey.Creation.Date[this_pat]
      TRACK_merge$DaysSince1stVisit[this_pat] = incl_dates - min(incl_dates)
      
      # Diverges from Moniek, she takes all inclusions within 90 days of the first and last, I think it's a mistake
      start = abs((incl_dates - min(incl_dates)) - date_options$start_day)
      close_start = which.min(start)
      
      if(close_start %in% which(start<date_options$day_bwidth)){
        TRACK_merge$StartEndVisit[this_pat][close_start] = "Day 0"
      }
      
      for (j in 1:length(date_options$end_day)){
        end = abs((incl_dates - min(incl_dates)) - date_options$end_day[j])
        close_end = which.min(end)
        if(close_end %in% which(end<date_options$day_bwidth)){
          TRACK_merge$StartEndVisit[this_pat][close_end] = paste("Day", date_options$end_day[j])
        }
      }
      
      # I end up not using pat_startend for anything, but I'll keep it here just in case
      # pat_startend$start[i] = TRACK_merge$TRACKSUM[this_pat][close_start]
      # pat_startend$end[i] = TRACK_merge$TRACKSUM[this_pat][close_end]
      # pat_startend$change[i] = pat_startend$end[i] - pat_startend$start[i]
      
      # for (j in 1:length(date_options$days)){
      #   incl_day = which.min(abs(TRACK_merge$DaysSince1stVisit - date_options$days[j]) <= date_options$day_bwidth)
      #   pat_startend[i, 6+j] = mean(TRACK_merge$TRACKSUM[this_pat][incl_day])
      # }
    }
    
    rm(i, j, incl_dates, close_start, close_end, this_pat)
  }
  
  TRACK_merge$StartEndVisit = as.factor(TRACK_merge$StartEndVisit)
  TRACK_merge$StartEndVisit = relevel(TRACK_merge$StartEndVisit, ref = "Day 0")
  
  return(TRACK_merge)
}

TRACK_merge_se = startend_analysis(TRACK_merge, date_options)
ISAAC_sub = startend_analysis(ISAAC_sub, date_options)

# Data frame with one row per patient, only first and last visit
startend_reformat = function(survey_df, patient_df, format, extraSurvey_list = NULL){
  surveypatient_df = patient_df
  startends = levels(survey_df$StartEndVisit)
  if (format == "long"){ # Long formatting
    surveypatient_df = merge(surveypatient_df, survey_df[survey_df$StartEndVisit %in% "Day 0", c("Participant.Id", "TRACKSUM", "AgeAtVisit")])
    colnames(surveypatient_df)[colnames(surveypatient_df) == "TRACKSUM"] = "TRACKSUM_Start"
    colnames(surveypatient_df)[colnames(surveypatient_df) == "AgeAtVisit"] = "Age1stVisit"
    template_df = surveypatient_df
    
    surveypatient_df$StartEndVisit = "Day 0"
    surveypatient_df$TRACKSUM_End = surveypatient_df$TRACKSUM_Start
    surveypatient_df$DaysSince1stVisit = 0
    #surveypatient_df$DayBin = "Start"
    for (i in 1:length(startends)){
      if (startends[i] != "Day 0"){
        temp_df = merge(template_df, survey_df[survey_df$StartEndVisit %in% startends[i], c("Participant.Id", "TRACKSUM", "DaysSince1stVisit", "StartEndVisit")])
        colnames(temp_df)[colnames(temp_df) == "TRACKSUM"] = "TRACKSUM_End"
        #colnames(surveypatient_df)[colnames(surveypatient_df) == "DaysSince1stVisit"] = paste0("VisitDay", startends[i])
        
        #surveypatient_df[paste0("TRACKSUM_Diff", startends[i])] = surveypatient_df[paste0("TRACKSUM_", startends[i])] - surveypatient_df$TRACKSUM_Start
        surveypatient_df = rbind(surveypatient_df, temp_df)
      }
    }
    surveypatient_df$StartEndVisit = as.factor(surveypatient_df$StartEndVisit)
    surveypatient_df$StartEndVisit = relevel(surveypatient_df$StartEndVisit, "Day 0")
    surveypatient_df$TRACKSUM_Diff = surveypatient_df$TRACKSUM_End - surveypatient_df$TRACKSUM_Start
    surveypatient_df$UnderControl = surveypatient_df$TRACKSUM_End >= 80
  }else{ # Wide formatting
    for (i in 1:length(startends)){
      if (startends[i]=="Day 0"){
        surveypatient_df = merge(surveypatient_df, survey_df[survey_df$StartEndVisit %in% "Day 0", c("Participant.Id", "TRACKSUM", "AgeAtVisit")])
        colnames(surveypatient_df)[colnames(surveypatient_df) == "TRACKSUM"] = "TRACKSUM_Start"
        colnames(surveypatient_df)[colnames(surveypatient_df) == "AgeAtVisit"] = "Age1stVisit"
      }else{
        surveypatient_df = merge(surveypatient_df, survey_df[survey_df$StartEndVisit %in% startends[i], c("Participant.Id", "TRACKSUM", "DaysSince1stVisit")])
        colnames(surveypatient_df)[colnames(surveypatient_df) == "TRACKSUM"] = paste0("TRACKSUM_", startends[i])
        colnames(surveypatient_df)[colnames(surveypatient_df) == "DaysSince1stVisit"] = paste0("VisitDay", startends[i])
        
        surveypatient_df[paste0("TRACKSUM_Diff", startends[i])] = surveypatient_df[paste0("TRACKSUM_", startends[i])] - surveypatient_df$TRACKSUM_Start
      }
    }
  }
  
  # Handle extra surveys
  if(!is.null(extraSurvey_list)){
    # ISAAC matching
    if(any(names(extraSurvey_list$df) %in% "ISAAC")){
      ISAAC_qs = extraSurvey_list$qs$ISAAC
      ISAAC_sub = extraSurvey_list$df$ISAAC
      ISAAC_sub = ISAAC_sub[!is.na(ISAAC_sub$StartEndVisit),]
      if(format == "long"){
        surveypatient_df = merge(surveypatient_df, ISAAC_sub[, c("Participant.Id", "StartEndVisit",ISAAC_qs$questions)])
        for (k in 1:length(ISAAC_qs$questions)){
          surveypatient_df[,ISAAC_qs$questions[k]] = as.factor(surveypatient_df[,ISAAC_qs$questions[k]])
          levels(surveypatient_df[,ISAAC_qs$questions[k]]) = ISAAC_qs$levels[[k]]
        }
      }else{
        isaac_startends = levels(ISAAC_sub$StartEndVisit)
        for (j in 1:length(isaac_startends)){
          surveypatient_df = merge(surveypatient_df, ISAAC_sub[ISAAC_sub$StartEndVisit %in% isaac_startends[j] ,c("Participant.Id", ISAAC_qs$questions)], all.x = T)
          for (k in 1:length(ISAAC_qs$questions)){
            surveypatient_df[,ISAAC_qs$questions[k]] = as.factor(surveypatient_df[,ISAAC_qs$questions[k]])
            levels(surveypatient_df[,ISAAC_qs$questions[k]]) = ISAAC_qs$levels[[k]]
          }
          
          colnames(surveypatient_df)[colnames(surveypatient_df) %in% ISAAC_qs$questions] = paste0(ISAAC_qs$questions, isaac_startends[j])
          }
      }
    }
    
    # HCSRU matching
    if(any(names(extraSurvey_list$df) %in% "HCSRU")){
      HCSRU_sub = extraSurvey_list$df$HCSRU
      surveypatient_df = merge(surveypatient_df, HCSRU_sub[, c("Participant.Id", "TotalCost")])
    }
  }
  
  return(surveypatient_df)
}

extrasurveylist = list(df = list(ISAAC = ISAAC_sub,
                                 HCSRU = HCSRU_sub),
                       qs = list(ISAAC = ISAAC_qs,
                                 HCSRU = HCSRU_qs))

TRACK_long = startend_reformat(TRACK_merge_se, ADEM2_sub, format = "long", extraSurvey_list = extrasurveylist)
TRACK_wide = startend_reformat(TRACK_merge_se, ADEM2_sub, format = "wide", extraSurvey_list = extrasurveylist)

# Tests and plots ----
## Plots ----
# Remove NAs
# Combine columns for plots
TRACK_plot = TRACK_plot %>% unite(GroupTest, c(V1_Rand_RandGroup, V1_Rand_BreathResult), sep = " ", remove = F)

customggsave = function(plot, upscale = 1.5, save_path = '', name = plot$labels$title) {
  save_path = paste0('./Plots', save_path)
  if (is.null(name)) {
    name = deparse(substitute(plot))
  }
  ggsave(
    paste0(name, ".png"),
    plot = plot,
    device = 'png',
    width = round(1920 * upscale),
    height = round(1080 * upscale),
    units = 'px',
    path = save_path
  )
}

cutie_layer = function(title, subtitle = NULL) {
  list(theme_bw(),
       ggtitle(label = title, subtitle = subtitle),
       theme(plot.title = element_text(hjust = 0.5),
             plot.subtitle = element_text(hjust = 0.5),
             legend.position = 'bottom'))
}

plotbools = list(UsualCare = TRACK_plot$V1_Rand_RandGroup %in% "UsualCare",
                Intervention = TRACK_plot$V1_Rand_RandGroup %in% "Intervention",
                Asthma = TRACK_plot$V1_Rand_BreathResult %in% "Asthma",
                TransWheeze = TRACK_plot$V1_Rand_BreathResult %in% "TransWheeze")

group_colors = list(V1_Rand_BreathResult = c("#00BFC4", "#F8766D"),
                    V1_Rand_RandGroup = c("#7CAE00","#C77CFF"),
                    ISAACa02 = c("#B2C900", "#4D36FF"))
                    
euro_label = label_currency(
  accuracy = 1,
  prefix = "",
  suffix = " €",
  big.mark = ".",
  decimal.mark = ","
)

# Long-format plots
TRACK_plot_long = TRACK_long
TRACK_plot_long = subset(TRACK_plot_long, !is.na(V1_Rand_RandGroup))
TRACK_plot_long = subset(TRACK_plot_long, !is.na(V1_Rand_BreathResult))
# Cost between groups
ggplot(data = TRACK_plot_long, aes(x = V1_Rand_RandGroup, y = TotalCost, color = V1_Rand_BreathResult)) +
  geom_boxplot() +
  stat_summary(mapping = aes(label = euro_label(..y..)), fun = "median", geom = "label", position = position_dodge(width = 0.75)) +
  scale_x_discrete(name = "Experimental group", labels = paste0(c("Control (n = ", "Intervention (n = "), count(TRACK_plot_long, V1_Rand_RandGroup)$n, ")")) +
  scale_y_continuous(name = "Total cost", labels = euro_label) +
  scale_color_manual(name = "Breath test results", labels = paste0(c("Transient Wheeze (n = ", "Asthma (n = "), count(TRACK_plot_long, V1_Rand_BreathResult)$n, ")"), , values = group_colors$V1_Rand_BreathResult) +
  cutie_layer(title = "Difference in total cost to family and healthcare system per patient")
# TRACK score differences over time
ggplot(data = subset(TRACK_plot_long, !(StartEndVisit %in% "Day 0")), aes(x = StartEndVisit, y = TRACKSUM_Diff, color = V1_Rand_BreathResult)) +
  geom_boxplot() +
  stat_summary(mapping = aes(label = ..y..), fun = "median", geom = "label", position = position_dodge(width = 0.75)) +
  scale_x_discrete(name = "Day of visit", labels = date_options$end_day) +
  scale_y_continuous(name = "Difference in TRACK score") +
  scale_color_manual(name = "Breath test results", labels = paste0(c("Transient Wheeze (n = ", "Asthma (n = "), count(TRACK_plot_long, V1_Rand_BreathResult)$n, ")"), values = group_colors$V1_Rand_BreathResult) +
  cutie_layer(title = "Difference in TRACK score compared to baseline")
# Plot asthma attacks over time (yes/no)
ggplot(data = subset(TRACK_plot_long, !is.na(ISAACa02)), aes(x = StartEndVisit, fill = ISAACa02)) +
  geom_bar(position = "fill") + 
  scale_x_discrete(name = "Day of visit", labels = paste0(levels(TRACK_plot_long$StartEndVisit), " (n = ", count(TRACK_plot_long, StartEndVisit)$n, ")")) +
  scale_y_continuous(name = "Fraction") +
  scale_fill_manual(name = "Asthma attacks in the past 12 months", values = group_colors$ISAACa02) +
  cutie_layer(title = "Proportion of patients that had asthma attacks in the past 12 months")

# Wide-format plots
TRACK_plot_wide = TRACK_wide
TRACK_plot_wide = subset(TRACK_plot_wide, !is.na(V1_Rand_RandGroup))
TRACK_plot_wide = subset(TRACK_plot_wide, !is.na(V1_Rand_BreathResult))


# Past asthma attacks over time

customggsave(plot)

## Statistical tests ----
# Chosen tests from https://www.scribbr.com/statistics/statistical-tests/

## Linear regression ----
TRACK_lm = lm(TotalCost ~ V1_Rand_BreathResult*V1_Rand_RandGroup + Age1stVisit, data = TRACK_pat, na.action = "na.omit")
summary(TRACK_lm)

## Logistic regression ----
lgm = glm(UnderControl ~ V1_Rand_BreathResult + V1_Rand_RandGroup + Age1stVisit + DaysSince1stVisit, family = "binomial", data = TRACK_pat)
summary(lgm)

## Linear/logistic mixed models ----
GLM = glm(TRACKSUM_DiffEnd720 ~ V1_Rand_BreathResult + Age1stVisit + ISAACa02, data = TRACK_pat, na.action = "na.omit")
lmm180 = lme(TRACKSUM_DiffEnd180 ~ Age1stVisit + V1_Rand_BreathResult + ISAACa02, data = TRACK_pat, 
                random = ~ 1|V1_Rand_RandGroup, na.action = "na.omit")
lmm360 = lme(TRACKSUM_DiffEnd360 ~ Age1stVisit + V1_Rand_BreathResult + ISAACa02, data = TRACK_pat, 
             random = ~1|V1_Rand_RandGroup, na.action = "na.omit")
lmm540 = lme(TRACKSUM_DiffEnd540 ~ Age1stVisit + V1_Rand_BreathResult + ISAACa02, data = TRACK_pat, 
             random = ~1|V1_Rand_RandGroup, na.action = "na.omit")
lmm720 = lme(TRACKSUM_DiffEnd720 ~ Age1stVisit + V1_Rand_BreathResult + ISAACa02, data = TRACK_pat, 
             random = ~1|V1_Rand_RandGroup, na.action = "na.omit")

anova(GLM, lmm180, lmm360, lmm540, lmm720)

plot(TRACKSUM_Diff ~ V1_Rand_BreathResult*V1_Rand_RandGroup)
