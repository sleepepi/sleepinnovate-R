sleepinnovate-R
===============

A collection of R scripts used to import, process, and back up SleepInnovate data.  


#### back-up-admin-data.R
Generate periodical back-up R data for subject profiles and communication records housed in the administrative interface.

#### import-R35-PSG-report.R
Convert downloaded PSG summary text files into data tables and create a PSG download tracking table for administrative report.

#### process-TMB-spreadsheets.R 
Process recent test-my-brain exports in the TMB\archive folder and save the processed test-my-brain data as a CSV file in the TMB\processed folder. 

#### create-TMB-dataset.R
Merge the new processed test-my-brain data with the existing data table. 

## Required R packages

### Data Wrangling
``` 
install.packages("dplyr")
install.packages("plyr")
install.packages("tidyr")
```

### Connecting R with Microsoft SQL Server 
```
install.packages("RODBC")
```

### Text Processing
```
install.packages("readtext")
install.packages("rex")
install.packages("cwhmisc")
install.packages("qdapRegex")
install.packages("RJSONIO")
install.packages("jsonlite")
```











