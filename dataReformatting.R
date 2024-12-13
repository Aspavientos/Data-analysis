# Information ----
# Reformatting CASTOR Data to better fit R
# Author: Diego Rodríguez Esperante
# Date of creation: 06/12/2024
# Last edited: 06/12/2024

# Packages ----
require(rstudioapi)
require(stringr)
require(dplyr)

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
    surveys[[i]][[repcols[j]]] = as.Date(surveys[[i]][[repcols[j]]], format = "%d-%m-%y")
    
    invDate = surveys[[i]][[repcols[j]]] > Sys.Date()
    invDate[is.na(invDate)] = FALSE
    surveys[[i]][invDate,repcols[j]] = NA
  }
}

# Bin out time
daterangecols = c("V1_Incl_VisDat", "V2_Visit_Dat", "V3_Visit_Dat", "V4_Visit_Dat", "V5_Visit_Dat", "Survey.Sent.Date")
date_range = c(as.Date(surveys$TRACK$Survey.Creation.Date[1], format = "%d-%m-%y"),
               as.Date(surveys$TRACK$Survey.Creation.Date[1], format = "%d-%m-%y"))
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

# Replicating Moniek's analysis ----
