##################################################################################
## Program          : import-R35-PSG-report.R                                
## Project          : Sleep Innovate                                            
## Author           : ng700                                                     
## Date created     : 20171120                                                  
## Purpose          : read in PSG report for consented participants
## Revision History :                                                           
##    Date       Author     Ref       Revision                            
##                                                                
##################################################################################

# Clear existing data and graphics
rm(list=ls())
graphics.off

# Load relevant packages
library(readtext)
library(dplyr)
library(rex)
library(cwhmisc)
library(plyr)
library(qdapRegex)

# Set working directory
setwd("//rfawin/BWH-SLEEPEPI-R35/Data/PSG/SleepINNOVATE")

# Retrieve folder names that start with slice subject code  
psg_folder <- as.vector(list.files(path = ".", pattern = NULL, all.files = FALSE, full.names = FALSE, recursive = FALSE))
psg_folder <- psg_folder[grepl("INN",psg_folder)]

# Loop through psg_folder 
psgfiles <- data.frame(matrix(ncol = 0, nrow = 0))
for (i in 1:length(psg_folder)){
   filenames <- list.files(path = paste("./",psg_folder[i],sep = ""), pattern = ".txt", all.files = FALSE, full.names = FALSE, recursive = FALSE)
   if (length(filenames) !=0){
     filenames <- as.data.frame(filenames)
     filenames <- cbind(psg_folder[i],filenames)
     colnames(filenames) <- c("Subject","filename")
     psgfiles <- rbind.fill(psgfiles,filenames)
   }
}

psg_list <- list()
for (i in 1:dim(psgfiles)[1]){
  psg_text<- readLines(file(paste("./",psgfiles$Subject[i], "/",psgfiles$filename[i], sep = "")))
  psg_text <- psg_text[psg_text != ""] 
  psg_text <- gsub(' +',' ',psg_text)
  psg_text <- c(as.character(psgfiles$filename[i]),psg_text)
  psg_list[[i]] <- as.vector(psg_text)
}

# specify variable names for each report

## polysomnography report
polysomnography_names <- c("subject","filename","study_date","lightsoff","sleep_efficiency","lightson","stage_N1","sleep_latency",
                           "stage_N2","REM_latency","stage_N3","total_sleep_time","stage_REM","total_sleep_period","sleep_time_supine",
                           "total_recording_time","sleep_time_prone","waso","sleep_time_lateral","total_REM_period","sleep_time_upright",
                           "total_awakenings_count","total_awakenings_index","total_arousals_count","total_arousals_index",
                           "spontaneous_arousals_count","spontaneous_arousals_index","total_PLMs_count","total_PLMs_index",
                           "total_PLMs_arousals_count","total_PLMs_arousals_index","baseline_pulse","min_pulse","mean_pulse",
                           "max_pulse","baseline_SpO2","min_SpO2","psleeplt88","mean_SpO2","ODI","max_SpO2","ahi_a0h4_total",
                           "ahi_a0h4_supine","ahi_a0h4_lateral","ahi_a0h4_prone","ahi_a0h4_REM","ahi_a0h4_NREM","ahi_a0h4_supine_REM",
                           "ahi_total","ahi_supine","ahi_lateral","ahi_prone","ahi_REM","ahi_NREM","ahi_supine_REM","rdi_total",
                           "rdi_supine","rdi_lateral","rdi_prone","rdi_REM","rdi_NREM","rdi_supine_REM","oai_total","oai_supine",
                           "oai_lateral","oai_prone","oai_REM","oai_NREM","oai_supine_REM","cai_total","cai_supine","cai_lateral",
                           "cai_prone","cai_REM","cai_NREM","cai_supine_REM","mai_total","mai_supine","mai_lateral","mai_prone",
                           "mai_REM","mai_NREM","mai_supine_REM","ohi_total","ohi_supine","ohi_lateral","ohi_prone","ohi_REM",
                           "ohi_NREM","ohi_supine_REM","chi_total","chi_supine","chi_lateral","chi_prone","chi_REM",
                           "chi_NREM","chi_supine_REM","RERA_total","RERA_supine","RERA_lateral","RERA_prone","RERA_REM",
                           "RERA_NREM","RERA_supine_REM","periodic_stokes_time")
sleep_architecture <- c("Lights Off:","Sleep Efficiency:","Lights On:","Stage N1:","Sleep Latency:","Stage N2:","Stage REM Latency:",
                        "Stage N3:","Total Sleep Time:","Stage REM:","Total Sleep Period:","Supine Position:",
                        "Total Recording Time:","Prone Position:","Wake After Sleep Onset:","Lateral Position:",
                        "Total REM Periods:","Upright Position:","AROUSAL EVENTS:")
arousal_events <- c("Total Awakenings:","Total Arousals:","Spontaneous Arousals:","Count")
movement_events <- c("Total PLMs:","Total PLMs with Arousals:","CARDIAC ANALYSIS:")
cardiac_analysis <- c("Baseline Pulse:","Minimum Pulse:","Mean Pulse:","Maximum Pulse:","OXYGEN ANALYSIS:")
oxygen_analysis <- c("Baseline SpO2%:","Minimum SpO2%:","% Time Spent < or equal to 88%:","Mean SpO2%:","O2 Desaturation Index:",
                     "Maximum SpO2%:","Patient Name:")
respiratory <- c("AHI 4%:","AHI:","RDI:","Obs. Apnea Index:","Central Apnea Index:","Mixed Apnea Index:",
                 "Obs. Hypopnea Index:","Central Hypopneas Index:","RERA Index:","Periodic Breathing/Cheyne Stokes Time:")
psg_report <- data.frame(matrix(nrow = 0, ncol = 105))
colnames(psg_report) <- polysomnography_names

## Split titration report
split_titration_names <- c("subject","filename","study_date", "titration_note",
                           "lightsoff_dia","lightsoff_trt","lightson_dia","lightson_trt",
                           "total_recording_time_dia","total_recording_time_trt","total_sleep_time_dia","total_sleep_time_trt",
                           "total_sleep_period_dia","total_sleep_period_trt","sleep_latency_dia","sleep_latency_trt",
                           "REM_latency_dia","REM_latency_trt","stage_N1_dia","stage_N1_trt","stage_N2_dia","stage_N2_trt",
                           "stage_N3_dia","stage_N3_trt","stage_REM_dia","stage_REM_trt",
                           "total_REM_period_dia","total_REM_period_trt","waso_dia","waso_trt",
                           "sleep_efficiency_dia","sleep_efficiency_trt","total_awakenings_count","total_awakenings_index",
                           "total_arousals_count","total_arousals_index","spontaneous_arousals_count","spontaneous_arousals_index",
                           "total_PLMs_count","total_PLMs_index",
                           "total_PLMs_arousals_count","total_PLMs_arousals_index","baseline_pulse","min_pulse","mean_pulse",
                           "max_pulse","baseline_SpO2","min_SpO2","psleeplt88","mean_SpO2","ODI","max_SpO2","ahi_a0h4_total",
                           "ahi_a0h4_supine","ahi_a0h4_lateral","ahi_a0h4_prone","ahi_a0h4_REM","ahi_a0h4_NREM","ahi_a0h4_supine_REM",
                           "ahi_total","ahi_supine","ahi_lateral","ahi_prone","ahi_REM","ahi_NREM","ahi_supine_REM","rdi_total",
                           "rdi_supine","rdi_lateral","rdi_prone","rdi_REM","rdi_NREM","rdi_supine_REM","oai_total","oai_supine",
                           "oai_lateral","oai_prone","oai_REM","oai_NREM","oai_supine_REM","cai_total","cai_supine","cai_lateral",
                           "cai_prone","cai_REM","cai_NREM","cai_supine_REM","mai_total","mai_supine","mai_lateral","mai_prone",
                           "mai_REM","mai_NREM","mai_supine_REM","ohi_total","ohi_supine","ohi_lateral","ohi_prone","ohi_REM",
                           "ohi_NREM","ohi_supine_REM","chi_total","chi_supine","chi_lateral","chi_prone","chi_REM",
                           "chi_NREM","chi_supine_REM","RERA_total","RERA_supine","RERA_lateral","RERA_prone","RERA_REM",
                           "RERA_NREM","RERA_supine_REM","periodic_stokes_time")

sleep_titration <- c("Lights Off:","Lights On:","Total Recording Time:","Total Sleep Time:","Total Sleep Period:",
                     "Sleep Latency:","Stage REM Latency:","Stage N1:","Stage N2:","Stage N3:","Stage REM:","Total REM Periods:",
                     "Wake After Sleep Onset:","Sleep Efficiency:","AROUSAL EVENTS:")
arousal_titration <- c("Total Awakenings:","Total Arousals:","Spontaneous Arousals:","MOVEMENT EVENTS:")
movement_titration <- c("Total PLMs:","Total PLMs with Arousals:","CARDIAC ANALYSIS:")
cardiac_titration <- c("Baseline Pulse:","Minimum Pulse:","Mean Pulse:","Maximum Pulse:","Patient Name:")
oxygen_titration <- c("Baseline SpO2%:","Minimum SpO2%:","% Time Spent < or equal to 88%:","Mean SpO2%:","O2 Desaturation Index:",
                     "Maximum SpO2%:","RESPIRATORY ANALYSIS")
respiratory_titration <- c("AHI 4%:","AHI:","RDI:","Obs. Apnea Index:","Central Apnea Index:","Mixed Apnea Index:",
                 "Hypopnea Index:","Central Hypopnea Index:","RERA Index:","TITRATION SUMMARY:")

titration_report <- data.frame(matrix(nrow = 0, ncol = 116))
colnames(titration_report) <- split_titration_names

for (p in 1:length(psg_list))
{
  type <- length(which(grepl("titration notes", tolower(paste(psg_list[[p]],collapse = " "))))) 
  if(type == 0) # Process polysomnograhy report
  { 
    text <- psg_list[[p]]
    psg_report[p,1] <- substr(text[1],1,8)
    psg_report[p,2] <- rm_between(text[1],"_",".txt",extract = T)[[1]]
    
    startline <- which(grepl("sleep architecture",tolower(text)))
    endline <- which(grepl("periodic breathing/cheyne stokes time",tolower(text)))
    text <- text[startline:endline]
    text <- paste(text,collapse = " ")
    
    text <- gsub("min","",text)
    text <- gsub(" *\\(.*?\\) *","",text)
    text <- gsub(' +',' ',text)
    
    a <- cpos(text,"Study Date:") + nchar("Study Date:")
    psg_report[p,3] <- strsplit(substr(text,start = a+1, stop = a+10)," ")[[1]][1]
    a <- cpos(text,"Periodic Breathing/Cheyne Stokes Time:") + nchar("Periodic Breathing/Cheyne Stokes Time:")
    psg_report[p,105] <- substr(text,start = a+1, stop = a+9)
    
    for (i in 4:21)
      {
        j <- i - 3
        psg_report[p,i] <- rm_between(text,sleep_architecture[j],sleep_architecture[j+1],extract = T)[[1]]
      }
    
    arousal <- vector()
    for (j in 1:3)
      {
        arousal <- c(arousal,strsplit(rm_between(text,arousal_events[j],arousal_events[j+1],extract = T)[[1]]," ")[[1]])
      }
    
    for (i in 22:27)
      {
        j <- i - 21
        psg_report[p,i] <- as.numeric(arousal[j])
      }
    
    movement <- vector()
    for (j in 1:2)
      {
        movement <- c(movement,strsplit(rm_between(text,movement_events[j],movement_events[j+1],extract = T)[[1]]," ")[[1]])
      }
    
    for (i in 28:31)
      {
        j <- i - 27
        psg_report[p,i] <- as.numeric(movement[j])
      }
    
    for (i in 32:35)
      {
        j <- i - 31
       psg_report[p,i] <- rm_between(text,cardiac_analysis[j],cardiac_analysis[j+1],extract = T)[[1]]
      }
    
    for (i in 36:41)
      {
        j <- i - 35
        psg_report[p,i] <- rm_between(text,oxygen_analysis[j],oxygen_analysis[j+1],extract = T)[[1]]
      }
    
    apneas <- vector()
    for (j in 1:9)
      {
        apneas <- c(apneas,strsplit(rm_between(text,respiratory[j],respiratory[j+1],extract = T)[[1]]," ")[[1]])
      }
    
    for (i in 42:104)
      {
        j <- i - 41
        psg_report[p,i] <- apneas[j]
      }
  }
  
  if(type > 0) # Process split titration report
  { 
    text <- psg_list[[p]]
    titration_report[p,1] <- substr(text[1],1,8)
    titration_report[p,2] <- rm_between(text[1],"_",".txt",extract = T)[[1]]
    text <- paste(text,collapse = " ")
    titration_report[p,4] <- rm_between(text,"Titration Notes:","EKG Findings:",extract = T)[[1]]
    
    text <- psg_list[[p]]
    startline <- which(grepl("sleep architecture",tolower(text)))
    endline <- which(grepl("periodic breathing/cheyne stokes time",tolower(text)))
    text <- text[startline:endline]
    text <- paste(text,collapse = " ")
    
    text <- gsub("min","",text)
    text <- gsub(" *\\(.*?\\) *","",text)
    text <- gsub(' +',' ',text)
    
    a <- cpos(text,"Study Date:") + nchar("Study Date:")
    titration_report[p,3] <- strsplit(substr(text,start = a+1, stop = a+10)," ")[[1]][1]
    
    a <- cpos(text,"Periodic Breathing/Cheyne Stokes Time:") + nchar("Periodic Breathing/Cheyne Stokes Time:")
    titration_report[p,116] <- substr(text,start = a+1, stop = a+9)
    
    sleep <- vector()
    for (j in 1:14)
    {
      sleep <- c(sleep,strsplit(rm_between(text,sleep_titration[j],sleep_titration[j+1],extract = T)[[1]]," ")[[1]])
    }
    
    for (k in c(1,3,5,7))
    {
      sleep[k] <- c(paste(sleep[k],sleep[k + 1], collapse = ""))
    }
    sleep <- sleep[-c(2,4,6,8)]
    
    for (i in 5:32)
    {
      j <- i - 4
      titration_report[p,i] <- sleep[j]
    }
    
    arousal <- vector()
    for (j in 1:3)
    {
      arousal <- c(arousal,strsplit(rm_between(text,arousal_titration[j],arousal_titration[j+1],extract = T)[[1]]," ")[[1]])
    }
    
    for (i in 33:38)
    {
      j <- i - 32
      titration_report[p,i] <- as.numeric(arousal[j])
    }
    
    movement <- vector()
    for (j in 1:2)
    {
      movement <- c(movement,strsplit(rm_between(text,movement_titration[j],movement_titration[j+1],extract = T)[[1]]," ")[[1]])
    }
    
    for (i in 39:42)
    {
      j <- i - 38
      titration_report[p,i] <- as.numeric(movement[j])
    }
    
    for (i in 43:46)
    {
      j <- i - 42
      titration_report[p,i] <- rm_between(text,cardiac_titration[j],cardiac_titration[j+1],extract = T)[[1]]
    }
    
    for (i in 47:52)
    {
      j <- i - 46
      titration_report[p,i] <- rm_between(text,oxygen_titration[j],oxygen_titration[j+1],extract = T)[[1]]
    }
    
    apneas <- vector()
    for (j in 1:9)
    {
      apneas <- c(apneas,strsplit(rm_between(text,respiratory_titration[j],respiratory_titration[j+1],extract = T)[[1]]," ")[[1]])
    }
    
    for (i in 53:115)
    {
      j <- i - 52
      titration_report[p,i] <- apneas[j]
    }
  }
}

# Create a R35 PSG download checking dataset 
R35_psg_report <- psg_report %>% filter(!is.na(subject)) %>%
  mutate(report_type = "Polysomnography Report",study_date = as.Date(study_date,"%m/%d/%Y")) %>% 
  select(subject,filename,report_type,study_date) %>% arrange(subject,study_date)

R35_titration_report <- titration_report %>% filter(!is.na(subject)) %>% 
  mutate(report_type = "Split Titration Report",study_date = as.Date(study_date,"%m/%d/%Y")) %>% 
  select(subject,filename,report_type,study_date) %>% arrange(subject,study_date)

R35_psg_download<- rbind(R35_psg_report,R35_titration_report) %>% arrange(subject,report_type,study_date)

# Save R35 psg download dataset
archive_path <- paste("//rfawin/BWH-SLEEPEPI-R35/Data/R/_archive/","R35PSGdownload_",as.character(Sys.Date()),".RData",sep="")
dataset_path <- paste("//rfawin/BWH-SLEEPEPI-R35/Data/R/_datasets/","R35PSGdownload.RData",sep="")
save(R35_psg_download,file = archive_path)
save(R35_psg_download,file = dataset_path)


