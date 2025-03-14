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
ISAAC_qs = list(questions = c("ISAACa02", "ISAACa03"),
                vartype = c("factor", "numeric"), 
                levels = list(c("No", "Yes"), NULL))

names(ISAAC_qs$levels) = ISAAC_qs$questions

ISAAC_sub = surveys$ISAAC[,c("Castor.Participant.ID","Survey.Creation.Date","Survey.Package.Name", ISAAC_qs$questions)]

ISAAC_sub$Survey.Package.Name = t(as.data.frame(str_split(ISAAC_sub$Survey.Package.Name, " : ")))[,2]
colnames(ISAAC_sub)[colnames(ISAAC_sub) %in% "Castor.Participant.ID"] = "Participant.Id"

## Cleaning HCSRU Survey ----
HCSRU_qs = list(questions = c("HCSRU13b1", "HCSRU13b2", "HCSRU13b3", "HCSRU14a2", "HCSRU15a2", "HCSRU16a2", "HCSRU21_01"), # paste0("HCSRU19_", 1:10, "a")
                source = c("Direct", "Direct", "Direct", "Direct", "Direct", "Direct", "Indirect"),
                costs = c(30.87, 43.31, 30.87, 258, 138, 537, 39.88*8))

HCSRU_sub = surveys$HCSRU[,c("Castor.Participant.ID","Survey.Creation.Date","Survey.Package.Name", HCSRU_qs$questions)]

for (i in 1:length(HCSRU_qs$questions)){
  HCSRU_sub[is.na(HCSRU_sub[,HCSRU_qs$questions[i]]), HCSRU_qs$questions[i]] = 0
}

# Splitting direct and indirect costs
HCSRU_sub$DirectCost = as.numeric(as.matrix(HCSRU_sub[, HCSRU_qs$questions[HCSRU_qs$source %in% "Direct"]]) %*% as.matrix(HCSRU_qs$costs[HCSRU_qs$source %in% "Direct"]))
HCSRU_sub$IndirectCost = as.numeric(as.matrix(HCSRU_sub[, HCSRU_qs$questions[HCSRU_qs$source %in% "Indirect"]]) %*% as.matrix(HCSRU_qs$costs[HCSRU_qs$source %in% "Indirect"]))

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
    TRACK_merge$StartEndVisit[incl_start] = "Day0"
    
    incl_end = (abs(TRACK_merge$DaysSince1stVisit - date_options$end_day) <= date_options$day_bwidth)
    TRACK_merge$StartEndVisit[incl_end] = paste0("Day", date_options$end_day)
    
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
        TRACK_merge$StartEndVisit[this_pat][close_start] = "Day0"
      }
      
      for (j in 1:length(date_options$end_day)){
        end = abs((incl_dates - min(incl_dates)) - date_options$end_day[j])
        close_end = which.min(end)
        if(close_end %in% which(end<date_options$day_bwidth)){
          TRACK_merge$StartEndVisit[this_pat][close_end] = paste0("Day", date_options$end_day[j])
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
  TRACK_merge$StartEndVisit = relevel(TRACK_merge$StartEndVisit, ref = "Day0")
  
  return(TRACK_merge)
}

TRACK_merge_se = startend_analysis(TRACK_merge, date_options)
ISAAC_sub = startend_analysis(ISAAC_sub, date_options)

# Data frame with one row per patient, only first and last visit
startend_reformat = function(survey_df, patient_df, format, extraSurvey_list = NULL){
  surveypatient_df = patient_df
  startends = levels(survey_df$StartEndVisit)
  if (format == "long"){ # Long formatting
    surveypatient_df = merge(surveypatient_df, survey_df[survey_df$StartEndVisit %in% "Day0", c("Participant.Id", "TRACKSUM", "AgeAtVisit")])
    colnames(surveypatient_df)[colnames(surveypatient_df) == "TRACKSUM"] = "TRACKSUM_Start"
    colnames(surveypatient_df)[colnames(surveypatient_df) == "AgeAtVisit"] = "Age1stVisit"
    template_df = surveypatient_df
    
    surveypatient_df$StartEndVisit = "Day0"
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
    surveypatient_df$StartEndVisit = relevel(surveypatient_df$StartEndVisit, "Day0")
    surveypatient_df$TRACKSUM_Diff = surveypatient_df$TRACKSUM_End - surveypatient_df$TRACKSUM_Start
    surveypatient_df$UnderControl = surveypatient_df$TRACKSUM_End >= 80
  }else{ # Wide formatting
    for (i in 1:length(startends)){
      if (startends[i]=="Day0"){
        surveypatient_df = merge(surveypatient_df, survey_df[survey_df$StartEndVisit %in% "Day0", c("Participant.Id", "TRACKSUM", "AgeAtVisit")])
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
        surveypatient_df = merge(surveypatient_df, ISAAC_sub[, c("Participant.Id", "StartEndVisit", ISAAC_qs$questions)])
        for (k in 1:length(ISAAC_qs$questions)){
          if (ISAAC_qs$vartype[k] == "factor"){
            surveypatient_df[,ISAAC_qs$questions[k]] = as.factor(surveypatient_df[,ISAAC_qs$questions[k]])
            levels(surveypatient_df[,ISAAC_qs$questions[k]]) = ISAAC_qs$levels[[k]]
          }
        }
      }else{
        isaac_startends = levels(ISAAC_sub$StartEndVisit)
        for (j in 1:length(isaac_startends)){
          surveypatient_df = merge(surveypatient_df, ISAAC_sub[ISAAC_sub$StartEndVisit %in% isaac_startends[j] ,c("Participant.Id", ISAAC_qs$questions)], all.x = T)
          for (k in 1:length(ISAAC_qs$questions)){
            if (ISAAC_qs$vartype[k] == "factor"){
              surveypatient_df[,ISAAC_qs$questions[k]] = as.factor(surveypatient_df[,ISAAC_qs$questions[k]])
              levels(surveypatient_df[,ISAAC_qs$questions[k]]) = ISAAC_qs$levels[[k]]
            }
          }
          
          colnames(surveypatient_df)[colnames(surveypatient_df) %in% ISAAC_qs$questions] = paste0(ISAAC_qs$questions, isaac_startends[j])
          }
      }
    }
    
    # HCSRU matching
    if(any(names(extraSurvey_list$df) %in% "HCSRU")){
      HCSRU_sub = extraSurvey_list$df$HCSRU
      surveypatient_df = merge(surveypatient_df, HCSRU_sub[, c("Participant.Id", "DirectCost", "IndirectCost")])
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
             legend.position = 'bottom',
             strip.text = element_text(size = 11)))
}

makeGroupValues = function(df, format){
  if (format == "long"){
    group_values = list(Days = list(name = "Day of visit (± 90 days)",
                                    labels = paste0(gsub("([0-9]+){1}", " \\1", count(df, StartEndVisit)$StartEndVisit), " (n = ", count(df, StartEndVisit)$n, ")"),
                                    labelsabr = gsub("([0-9]+){1}", " \\1", count(df, StartEndVisit)$StartEndVisit)),
                        V1_Rand_BreathResult = list(name = "Breath test results",
                                                    labels = paste0(c("Transient Wheeze (n = ", "Asthma (n = "), (df %>% subset(!is.na(V1_Rand_BreathResult)) %>% count(V1_Rand_BreathResult))$n, ")"),
                                                    labelsabr = c("Transient Wheeze", "Asthma"),
                                                    Colors = c("#00BFC4", "#F8766D"),
                                                    LineType = c("solid", "longdash")),
                        V1_Rand_RandGroup = list(name = "Experimental group",
                                                 labels = paste0(c("Control (n = ", "Intervention (n = "), (df %>% subset(!is.na(V1_Rand_RandGroup)) %>% count(V1_Rand_RandGroup))$n, ")"),
                                                 labelsabr = c("Control", "Intervention"),
                                                 Colors = c("#7CAE00","#C77CFF"),
                                                 LineType = c("solid", "longdash")))
    names(group_values$Days$labels) = count(df, StartEndVisit)$StartEndVisit
    
    if ("ISAACa02" %in% colnames(df)){
      group_values$ISAACa02 = list(name = "Previous asthma attacks",
                                   labels = paste0(c("Yes (n = ", "No (n = "), (df %>% subset(!is.na(ISAACa02)) %>% count(ISAACa02))$n, ")"),
                                   labelsabr = c("Yes", "No"),
                                   Colors = c("pink", "red3"), 
                                   LineType = c("solid", "longdash"))
      names(group_values$ISAACa02$labels) = levels(df$ISAACa02)
    }
    if ("ISAACa03" %in% colnames(df)){
      colramp <- colorRampPalette(c("pink", "red3"))
      group_values$ISAACa03 = list(name = "Number of previous asthma attacks",
                                   labels = paste0((df %>% subset(!is.na(ISAACa03)) %>% count(ISAACa03))$ISAACa03, " (n = ", (df %>% subset(!is.na(ISAACa03)) %>% count(ISAACa03))$n, ")"),
                                   labelsabr = (df %>% subset(!is.na(ISAACa03)) %>% count(ISAACa03))$ISAACa03,
                                   Colors = colramp(df %>% subset(!is.na(ISAACa03)) %>% .$ISAACa03 %>% unique %>% length), 
                                   LineType = c("solid", "longdash"))
      names(group_values$ISAACa03$labels) = levels(df$ISAACa03)
    }
    
    if (any(grepl("Cost", colnames(TRACK_plot_long), ignore.case = T))){
      group_values$HCSRU = list(name = "Type of cost",
                                labels = c("Direct", "Indirect"),
                                labelsabr = c("Direct", "Indirect"),
                                Colors = c("#7da4ff", "#B78000"),
                                LineType = c("solid", "longdash"))
    }
  }else{
    group_values = list(V1_Rand_BreathResult = list(name = "Breath test results",
                                                    labels = paste0(c("Transient Wheeze (n = ", "Asthma (n = "), (df %>% subset(!is.na(V1_Rand_BreathResult)) %>% count(V1_Rand_BreathResult))$n, ")"),
                                                    labelsabr = c("Transient Wheeze", "Asthma"),
                                                    Colors = c("#00BFC4", "#F8766D"),
                                                    LineType = c("solid", "longdash")),
                        V1_Rand_RandGroup = list(name = "Experimental group",
                                                 labels = paste0(c("Control (n = ", "Intervention (n = "), (df %>% subset(!is.na(V1_Rand_RandGroup)) %>% count(V1_Rand_RandGroup))$n, ")"),
                                                 labelsabr = c("Control", "Intervention"),
                                                 Colors = c("#7CAE00","#C77CFF"),
                                                 LineType = c("solid", "longdash")),
                        ISAACa02 = list(name = "Previous asthma attacks",
                                        labels = c("Yes", "No"),
                                        Colors = c("pink", "red3"), 
                                        LineType = c("solid", "longdash")),
                        HCSRU = list(name = "Type of cost",
                                     labels = c("Direct cost", "Indirect cost"),
                                     Colors = c("#7da4ff", "#B78000"),
                                     LineType = c("solid", "longdash")))
    names(group_values$HCSRU$labels) = c("DirectCost", "IndirectCost")
    
  }
  names(group_values$V1_Rand_BreathResult$labels) = levels(df$V1_Rand_BreathResult)
  names(group_values$V1_Rand_BreathResult$labelsabr) = levels(df$V1_Rand_BreathResult)
  names(group_values$V1_Rand_RandGroup$labels) = levels(df$V1_Rand_RandGroup)
  names(group_values$V1_Rand_RandGroup$labelsabr) = levels(df$V1_Rand_RandGroup)
  return(group_values)
}
                    
euro_label = label_currency(accuracy = 1, prefix = "", suffix = " €", big.mark = ".", decimal.mark = ",")

# Long-format plots
TRACK_plot_long = TRACK_long
TRACK_plot_long = subset(TRACK_plot_long, !is.na(V1_Rand_RandGroup))
TRACK_plot_long = subset(TRACK_plot_long, !is.na(V1_Rand_BreathResult))
gv_long = makeGroupValues(TRACK_plot_long, "long")

# TRACK score differences over time
TRACK_plot_long %>%
  subset(!(StartEndVisit %in% "Day0")) %>%
  ggplot(aes(x = V1_Rand_BreathResult, y = TRACKSUM_Diff, fill = V1_Rand_BreathResult, linetype = V1_Rand_BreathResult)) +
    geom_boxplot() +
    stat_summary(mapping = aes(label = ..y..), fun = "median", geom = "label", position = position_dodge(width = 0.75), show.legend = F) +
    scale_x_discrete(name = gv_long$V1_Rand_BreathResult$name, labels = gv_long$V1_Rand_BreathResult$labelsabr) +
    scale_y_continuous(name = "Difference in TRACK score") +
    scale_fill_manual(name = gv_long$V1_Rand_BreathResult$name, labels = gv_long$V1_Rand_BreathResult$labels, values = gv_long$V1_Rand_BreathResult$Colors) +
    scale_linetype_manual(name = gv_long$V1_Rand_BreathResult$name, labels = gv_long$V1_Rand_BreathResult$labels, values = gv_long$V1_Rand_BreathResult$LineType) +
    facet_wrap(~StartEndVisit, nrow = 1, labeller = labeller(StartEndVisit = gv_long$Days$labels), strip.position = "bottom") +  
    cutie_layer(title = "Difference in TRACK score compared to baseline")
# Plot asthma attacks over time (yes/no)
TRACK_plot_long %>% 
  subset(!is.na(ISAACa02)) %>% 
  subset(!(StartEndVisit %in% c("Day180","Day540"))) %>%
  ggplot(aes(x = V1_Rand_RandGroup, fill = ISAACa02, color = V1_Rand_RandGroup, linetype = V1_Rand_RandGroup)) +
  geom_bar(linewidth = 1, position = "fill") +
  scale_x_discrete(name = gv_long$V1_Rand_RandGroup$name, labels = c("Control", "Intervention")) +
  scale_y_continuous(name = "Proportion", labels = percent) +
  scale_fill_manual(name = gv_long$ISAACa02$name, labels = gv_long$ISAACa02$labels, values = gv_long$ISAACa02$Colors) +
  scale_color_manual(name = gv_long$V1_Rand_RandGroup$name, labels = gv_long$V1_Rand_RandGroup$labels, values = gv_long$V1_Rand_RandGroup$Colors, guide = "none") + 
  scale_linetype_manual(name = gv_long$V1_Rand_RandGroup$name, labels = gv_long$V1_Rand_RandGroup$labels, values = gv_long$V1_Rand_RandGroup$LineType, guide = "none") +  
  facet_wrap(~StartEndVisit, nrow = 1, labeller = labeller(StartEndVisit = gv_long$Days$labels)) +
  cutie_layer(title = "Proportion of patients that had asthma attacks in the past 12 months", strip.position = "bottom")
# Plot asthma attacks over time (number)
plot = TRACK_plot_long %>% 
  subset(!is.na(ISAACa03)) %>% 
  subset(!(StartEndVisit %in% c("Day180","Day540"))) %>%
  ggplot(aes(x = V1_Rand_RandGroup, fill = as.factor(ISAACa03), linetype = V1_Rand_RandGroup)) +
    geom_bar(color = "black", linewidth = 1, position = "fill") +
    geom_text(aes(label = after_stat(count)), stat = "count", position = position_fill(vjust = .5), color = "black") + 
    scale_x_discrete(name = gv_long$V1_Rand_RandGroup$name, labels = c("Control", "Intervention")) +
    scale_y_continuous(name = "Proportion", labels = percent) +
    scale_fill_manual(name = gv_long$ISAACa03$name, labels = gv_long$ISAACa03$labels, values = gv_long$ISAACa03$Colors) +
    scale_linetype_manual(name = gv_long$V1_Rand_RandGroup$name, labels = gv_long$V1_Rand_RandGroup$labels, values = gv_long$V1_Rand_RandGroup$LineType, guide = "none") +  
    facet_wrap(~StartEndVisit, nrow = 1, labeller = labeller(StartEndVisit = gv_long$Days$labels), strip.position = "bottom") +
    cutie_layer(title = "Distribution of patients according to the amount of asthma attacks in the past 12 months")

# Wide-format plots
TRACK_plot_wide = TRACK_wide
TRACK_plot_wide = subset(TRACK_plot_wide, !is.na(V1_Rand_RandGroup))
TRACK_plot_wide = subset(TRACK_plot_wide, !is.na(V1_Rand_BreathResult))
gv_wide = makeGroupValues(TRACK_plot_wide, "wide")

# Direct and indirect costs per group
TRACK_plot_wide %>% 
  pivot_longer(cols = c("DirectCost", "IndirectCost"), names_to = "SourceCost", values_to = "Cost") %>%
  ggplot(aes(x = V1_Rand_RandGroup, y = Cost, fill = V1_Rand_RandGroup, linetype = V1_Rand_RandGroup)) +
    geom_boxplot(linewidth = 1) +
    stat_summary(mapping = aes(label = euro_label(..y..)), fun = "median", geom = "label", position = position_dodge(width = 0.75), show.legend = F) +
    scale_x_discrete(name = gv_wide$V1_Rand_RandGroup$name, labels = gv_wide$V1_Rand_RandGroup$labels) +
    scale_y_continuous(name = "Total cost", labels = euro_label) +
    scale_fill_manual(name = gv_wide$V1_Rand_RandGroup$name, labels = gv_wide$V1_Rand_RandGroup$labels, values = gv_wide$V1_Rand_RandGroup$Colors, guide = "none") +
    scale_linetype_manual(name = gv_wide$V1_Rand_RandGroup$name, labels = gv_wide$V1_Rand_RandGroup$labels, values = gv_wide$V1_Rand_RandGroup$LineType, guide = "none") +
    facet_wrap(~SourceCost, nrow = 1, labeller = labeller(SourceCost = gv_wide$HCSRU$labels)) + 
    cutie_layer(title = "Distribution of direct and indirect costs across patients")

# Total cost per group
TRACK_plot_wide %>% 
  pivot_longer(cols = c("DirectCost", "IndirectCost"), names_to = "SourceCost", values_to = "Cost") %>%
  ggplot(aes(x = V1_Rand_RandGroup, y = Cost, fill = V1_Rand_RandGroup, linetype = V1_Rand_RandGroup)) +
  geom_boxplot(linewidth = 1) +
  stat_summary(mapping = aes(label = euro_label(..y..)), fun = "median", geom = "label", position = position_dodge(width = 0.75), show.legend = F) +
  scale_x_discrete(name = gv_wide$V1_Rand_RandGroup$name, labels = gv_wide$V1_Rand_RandGroup$labelsabr) +
  scale_y_continuous(name = "Total cost", labels = euro_label) +
  scale_fill_manual(name = gv_wide$V1_Rand_RandGroup$name, labels = gv_wide$V1_Rand_RandGroup$labels, values = gv_wide$V1_Rand_RandGroup$Colors, guide = "none") +
  scale_linetype_manual(name = gv_wide$V1_Rand_RandGroup$name, labels = gv_wide$V1_Rand_RandGroup$labels, values = gv_wide$V1_Rand_RandGroup$LineType, guide = "none") +
  facet_grid(V1_Rand_BreathResult~SourceCost, labeller = labeller(V1_Rand_BreathResult = gv_wide$V1_Rand_BreathResult$labelsabr, SourceCost = gv_wide$HCSRU$labels)) + 
  cutie_layer(title = "Distribution of direct and indirect costs across patients")

# Save
customggsave(plot, name = "number of previous asthma attacks per experimental group")

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
