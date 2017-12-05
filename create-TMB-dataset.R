##################################################################################
## Program          : create-TMB-dataset.R                                
## Project          : Sleep Innovate                                            
## Author           : ng700                                                     
## Date created     : 20171116                                                  
## Purpose          : combine all the processed TMB files and generate a dataset
## Revision History :                                                           
##    Date       Author     Ref       Revision                            
##                                                                
##################################################################################

# Clear existing data and graphics
rm(list=ls())
graphics.off

# Load relevant libraries
library(dplyr)

# Load existing TMB dataset 
load("//rfawin/BWH-SLEEPEPI-R35/Data/R/_datasets/R35TMB.RData")

# Sets directory to program's filepath
setwd("//rfawin/BWH-SLEEPEPI-R35/Data/TMB/processed")

#Loop through all files in the processed folder
processed_TMB_files <- as.vector(list.files(path = ".", pattern = NULL, all.files = FALSE, full.names = FALSE, recursive = FALSE))
processed_TMB_files <- as.data.frame(processed_TMB_files)
processed_TMB_files$date <- as.Date(substr(processed_TMB_files$processed_TMB_files,20,29))

archive_datasets <- as.vector(list.files(path = "//rfawin/BWH-SLEEPEPI-R35/Data/R/_archive", pattern = NULL, all.files = FALSE, full.names = FALSE, recursive = FALSE))
archive_datasets <- as.data.frame(archive_datasets)
archive_TMB_datasets <- archive_datasets %>% select(archive_datasets) %>% filter(grepl("TMB",archive_datasets))
archive_TMB_datasets$date <- as.Date(substr(archive_TMB_datasets$archive_datasets,8,17))
last_TMB_date <- archive_TMB_datasets[order(archive_TMB_datasets$date,decreasing = T),]$date

for (i in 1:dim(processed_TMB_files)[1]){
  if (processed_TMB_files$date[i] > as.Date(last_TMB_date[1])){
    processed_TMB <-  read.csv(file = as.character(processed_TMB_files$processed_TMB_files[i]), header = T)
    names(processed_TMB)[names(processed_TMB) == 'subject'] <- 'Subject'
    names(processed_TMB)[names(processed_TMB) == 'event'] <- 'Event'
    processed_TMB$Event <- ifelse(processed_TMB$Event == "BASELINE","Baseline",processed_TMB$Event)
    processed_TMB$Event <- ifelse(processed_TMB$Event == "Followup-3month","3-Month Follow-up",processed_TMB$Event)
    processed_TMB$Event <- ifelse(processed_TMB$Event == "Followup-6month","6-Month Follow-up",processed_TMB$Event)
    processed_TMB$Event <- ifelse(processed_TMB$Event == "Followup-12month","12-Month Follow-up",processed_TMB$Event)
    R35TMB <- rbind.fill(R35TMB,processed_TMB)
  }
}

# Delete duplicates
R35TMB <- R35TMB %>% distinct(Subject,Event,testName,dateTime,sittingId, .keep_all = T)

# Save dataset
R35TMB <- R35TMB %>% arrange(Subject,Event,dateTime)
archive_path <- paste("//rfawin/BWH-SLEEPEPI-R35/Data/R/_archive/","R35TMB_",as.character(Sys.Date()),".RData",sep="")
dataset_path <- paste("//rfawin/BWH-SLEEPEPI-R35/Data/R/_datasets/","R35TMB.RData",sep="")
save(R35TMB,file = archive_path)
save(R35TMB,file = dataset_path)