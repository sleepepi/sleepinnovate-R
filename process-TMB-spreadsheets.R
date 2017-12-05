##################################################################################
## Program          : process-TMB-spreadsheets.R                                
## Project          : Sleep Innovate                                            
## Author           : ng700                                                     
## Date created     : 20171114                                                  
## Purpose          : Import all CSVs in TMB folder and generate meaning export 
## Revision History :                                                           
##    Date       Author     Ref       Revision                            
##                                                                
##################################################################################

# Clear existing data and graphics
rm(list=ls())
graphics.off

# Load relevant libraries
library(dplyr)
library(RJSONIO)
library(jsonlite)
library(plyr)

# Sets directory to program's filepath
setwd("//rfawin/BWH-SLEEPEPI-R35/Data/TMB")

#Loop through all years and months in the archive
Years <- as.vector(list.files(path = "./archive", pattern = NULL, all.files = FALSE, full.names = FALSE, recursive = FALSE))

foldernames <- vector()
for (i in 1:length(Years)){
  Months <- as.vector(list.files(path = paste("./archive/", Years[i],sep = ""), pattern = NULL, all.files = FALSE,
                                 full.names = FALSE, recursive = FALSE))
  
  for (j in 1:length(Months)){
    folder <- as.vector(paste("./archive/", Years[i],"/",Months[j], sep = ""))
    foldernames <- as.vector(cbind(foldernames, folder))
  }
}

foldersets <- as.data.frame(foldernames)
foldersets$year <- substr(foldersets$foldernames,11,14)
foldersets$month <- as.numeric(substr(foldersets$foldernames,16,17)) + 1 
foldersets$date<- as.Date(paste(foldersets$year, foldersets$month,"01",sep="-"), "%Y-%m-%d")-1
foldersets$date[foldersets$month == 13] <- as.Date(paste(foldersets$year,"12","31",sep="-"),"%Y-%m-%d")


#Select files to be processed 
files_to_process <- function(a)
  {
  folderpaths <- foldersets %>% filter(date > as.Date(a)) %>% select(foldernames)
  filenames <- as.data.frame(as.character(folderpaths$foldernames))
  files <- data.frame(matrix(nrow = 0, ncol = 2))
  for (i in 1:dim(filenames)[1]){
    file <- list.files(path = as.character(filenames[i,1]), pattern = NULL, all.files = FALSE,
                           full.names = FALSE, recursive = FALSE)
    file <- as.data.frame(file)
    file$path <- as.character(filenames[i,1])
    files <- rbind(files,file)
  }
  for (j in 1:dim(files)[1]){
    files$file_date[j] <- as.Date(gsub(".csv","",(strsplit(as.character(files$file[j]),"_")[[1]][4])))
    files$stay[j] <- ifelse(files$file_date[j] > as.Date(a),1,0)
  } 
  files<- files %>% filter(stay == 1)
  TMB_to_be_processed <- data.frame(matrix(ncol = 11, nrow = 0))
  for (k in 1:dim(files)[1]){
    if (file.info(as.character(paste(files$path[k],"/",files$file[k],sep = "")))$size > 0) {
    TMB_files <-  read.csv(file = as.character(paste(files$path[k],"/",files$file[k],sep = "")), header = T)
    TMB_files <- TMB_files %>% filter(grepl("INN",userId))
    TMB_to_be_processed <- rbind(TMB_to_be_processed,TMB_files)
    }
  }
  TMB_dataset <<- TMB_to_be_processed
}

#Specify the date after which the TMB tests will be processed
processed_dates <- as.vector(list.files(path = "./processed", pattern = NULL, all.files = FALSE, full.names = FALSE, recursive = FALSE))
processed_dates <- as.Date(strsplit(processed_dates,"_")[[1]][4])
last_p_date <- processed_dates[order(processed_dates,decreasing = T)]
files_to_process(last_p_date)

if(dim(TMB_dataset)[1] > 0){
  #Process TMB files
  TMB_dataset$event <- substr(TMB_dataset$userId,9,nchar(as.character(TMB_dataset$userId)))
  TMB_dataset$event <- gsub("dash","-",TMB_dataset$event)
  TMB_dataset$subject <- substr(TMB_dataset$userId,1,8)
  TMB_dataset$testName <- gsub("_sleep_innovate","",as.character(TMB_dataset$testName))
  
  TMB_dataset_list <- list()
  TMB_outcomes_list <- list()
  counter <- 0
  
  for (i in c("dsc","trailsA","trailsB","vocab","choiceRT","vpa_test","gradCPT","forwardSpan","MOT")){
    counter <- counter + 1
    TMB_dataset_list[[counter]] <- TMB_dataset %>% filter(testName == i)
    
    TMB_outcomes_list[[counter]] <- do.call(rbind.data.frame, 
                                            lapply(as.character(TMB_dataset_list[[counter]]$outcomes),
                                                   FUN = function(x){as.list(fromJSON(x))}))
    if (i == "dsc"){
      names(TMB_outcomes_list[[counter]])[names(TMB_outcomes_list[[counter]]) == 'score'] <- 'DSC_score'
    }
    if (i == "trailsA"){
      names(TMB_outcomes_list[[counter]])[names(TMB_outcomes_list[[counter]]) == 'score'] <- 'traisA_score'
    }
    if (i == "trailsB"){
      names(TMB_outcomes_list[[counter]])[names(TMB_outcomes_list[[counter]]) == 'score'] <- 'traisB_score'
    }
    if (i == "vocab"){
      names(TMB_outcomes_list[[counter]])[names(TMB_outcomes_list[[counter]]) == 'score'] <- 'vocab_score'
    }
    if (i == "choiceRT"){
      names(TMB_outcomes_list[[counter]])[names(TMB_outcomes_list[[counter]]) == 'score'] <- 'choiceRT_score'
    }
    if (i == "vpa_test"){
      names(TMB_outcomes_list[[counter]])[names(TMB_outcomes_list[[counter]]) == 'score'] <- 'vpa_test_score'
    }
    if (i == "gradCPT"){
      names(TMB_outcomes_list[[counter]])[names(TMB_outcomes_list[[counter]]) == 'score'] <- 'gradCPT_score'
    }
    if (i == "forwardSpan"){
      names(TMB_outcomes_list[[counter]])[names(TMB_outcomes_list[[counter]]) == 'score'] <- 'forwardSpan_score'
    }
    if (i == "MOT"){
      names(TMB_outcomes_list[[counter]])[names(TMB_outcomes_list[[counter]]) == 'score'] <- 'MOT_score'
    }
    rownames(TMB_dataset_list[[counter]]) <- NULL
    rownames(TMB_outcomes_list[[counter]]) <- NULL
    TMB_dataset_list[[counter]] <- cbind(TMB_dataset_list[[counter]],TMB_outcomes_list[[counter]])
  }
  
  TMB_dataset <- data.frame(matrix(nrow=0, ncol=0))
  for (j in 1:9){
    TMB_dataset <- rbind.fill(TMB_dataset,TMB_dataset_list[[j]])
  }
  
  TMB_dataset <- TMB_dataset %>% select(-c(userId,userAgent,reloaded,outcomes,data))
  TMB_dataset <- TMB_dataset %>% select(c(subject,event,testName,dateTime,everything())) %>% arrange(subject,dateTime)
  TMB_dataset$dateTime <- as.character(TMB_dataset$dateTime)
  

  testdate <- TMB_dataset %>% mutate(testdate = as.Date(dateTime)) %>% select(testdate) 
  lastdate <- testdate[order(testdate$testdate,decreasing = T),][1]
  
  savepath <- "//rfawin/BWH-SLEEPEPI-R35/Data/TMB/processed/"
  
  #Save the most recently processed TMB dataset 
  write.table(TMB_dataset,file=as.character(paste(savepath,"R35_TMB_LatestDate_",lastdate,".csv",sep="")),row.names = FALSE, na = "",sep=",")
}


