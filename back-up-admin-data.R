##################################################################################
## Program          : back-up-admin-data.R                                
## Project          : Sleep Innovate                                            
## Author           : ng700                                                     
## Date created     : 20171128                                                 
## Purpose          : save data in admin interface periodically into archive folder
## Revision History :                                                           
##    Date       Author     Ref       Revision                            
##                                                                
##################################################################################

# Clear existing data and graphics
rm(list=ls())
graphics.off

# Load relevant libraries
library(dplyr)
library(plyr)
library(tidyr)
library(RODBC)

# set up connection to SQL server
conn <- odbcDriverConnect('driver={SQL Server};server=phssql2023;database=sleepinnovate;trusted_connection=true')

# create two tables
AdminSubjectProfile <- sqlQuery(conn, 'select * from dbo.tblSubjectProfile')
AdminCommunication <- sqlQuery(conn, 'select * from dbo.tblCommunication')

# save 
profile_path <- paste("//rfawin/BWH-SLEEPEPI-R35/Data/Administrative Interface/Archive/","R35profile_",as.character(Sys.Date()),".RData",sep="")
communication_path <- paste("//rfawin/BWH-SLEEPEPI-R35/Data/Administrative Interface/Archive/","R35communication_",as.character(Sys.Date()),".RData",sep="")
save(AdminSubjectProfile,file = profile_path)
save(AdminCommunication,file = communication_path)