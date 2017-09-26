#RHC+BNP+NYHA merge.R
#
#R code to merge NYHA, BNP and RHC data into Camphor data table using HospNo and nearest neighbour Date using 
#
#DEPENDENCIES (automatically installed)
#
#pacman - package manager that installs other packages: https://cran.r-project.org/web/packages/pacman/index.html
#readXL - used to import Excel data: https://cran.r-project.org/web/packages/readxl/
#lubridate - functions to work with date-times and time-spans: https://cran.r-project.org/web/packages/lubridate/
#data.table - fast aggregation and joins of large data: https://cran.r-project.org/web/packages/data.table/index.html
#
#NOTEs
#Check for errors in some merges, too much data returned

#Check if packages are installed, if not install them, then load the liraries:
if (!require("pacman")) install.packages("pacman")
pacman::p_load(readxl, lubridate, data.table)

#set working directory
setwd("C:/Users/Edward/PowerFolders/MISC/Katherine R and Excel/InputFiles")
#setwd("C:/Users/Katherine/Desktop/")			

#read in data
RHCData <- read_excel("RHC_data.xlsx", sheet = "Sheet1")
NYHAData <- read_excel("NYHA_data.xlsx", sheet = "Sheet1")
BNPData <- read_excel("BNP_data.xlsx", sheet = "Sheet1")
CAMPHORData <- read_excel ("Camphor_raw.xlsx", sheet = "Sheet1")
MWDData <- read_excel ("6MWD _raw.xlsx", sheet = "Sheet1")

#make the Excel data into data.tables
setDT(RHCData)
setDT(NYHAData)
setDT(BNPData)
setDT(CAMPHORData)
setDT(MWDData)

#Possibility to set dates to IDate class for use in data.table
#Date calculations work anyway using Lubridate
#This data class doesn't work in PowerBI, would have to set back to POSIXct
  #RHCData<-RHCData[, dtmDate:=as.IDate(dtmDate)]
  #NYHAData<-NYHAData[, Date:=as.IDate(Date)]
  #BNPData<-BNPData[, Date:=as.IDate(Date)]
  #CAMPHORData<-CAMPHORData[, Date:=as.IDate(Date)]
  #MWDData<-MWDData[, TestDate:=as.IDate(TestDate)]

#sort all the same way (not necessary as long as merge table is sorted)
setkey(RHCData, strHospNo, dtmDate)
setkey(NYHAData,HospNo, Date)
setkey(BNPData,HospNo, Date)
setkey(CAMPHORData,HospNo, Date)
setkey(MWDData,HospNo, TestDate)

#remove any rows where same patient is seen twice on same day
#this is important for preventing multiple match lookups
RHCData <- unique(RHCData, by=c("strHospNo", "dtmDate"))
NYHAData <- unique(NYHAData, by=c("HospNo", "Date"))
BNPData <- unique(BNPData, by=c("HospNo", "Date"))
CAMPHORData <- unique(CAMPHORData, by=c("HospNo", "Date"))
MWDData <- unique(MWDData, by=c("HospNo", "TestDate"))

#make an copy of CAMPHOR data to merge into, and sort it
MergedTable <- CAMPHORData

#merge in MWDData
indx_MWD <- MWDData[CAMPHORData,
                    on = c(HospNo = "HospNo", TestDate = "Date"), 
                    roll = "nearest",
                    which = TRUE]

#make list of old and new column names, then read these into the merged table
#written as separate lines for clarity, works as single line when code substituted
MWD_new_col_names<-c("MWD_Matched_Date",colnames(MWDData)[-c(1,2)])
MWD_old_col_names <- colnames(MWDData)[-1]
MergedTable[, (MWD_new_col_names) := MWDData[indx_MWD, MWD_old_col_names, with=FALSE]]


########################################
#merge in BNPData
#NOTE: if patient not in BNPData, NA is returned 	
indx_BNP <- BNPData[CAMPHORData,
                    on = c(HospNo = "HospNo", Date = "Date"), 
                    roll = "nearest",
                    which = TRUE]

#Add matched BNP results to the output file: MergedTable
MergedTable[, BNP_Matched_Date := BNPData[indx_BNP,Date]]
MergedTable[, BNP := BNPData[indx_BNP,ProBNP]]


########################################
#merge in NYHA data	
indx_NYHA <- NYHAData[CAMPHORData,
                      on = c(HospNo = "HospNo", Date = "Date"), 
                      roll = "nearest",
                      which = TRUE]


#Add matched BNP results to the output file: MergedTable
MergedTable[, NYHA_Matched_Date := NYHAData[indx_NYHA,Date]]
MergedTable[, NYHA := NYHAData[indx_NYHA,NYHA]]


#######################################
#edit RHCData column names to remove data identifiers 'str' and 'dtm' ('boo' is kept)
#would be more elegant to edit in just the output list, but this way the merge is easier
Orig_RHC_Colnames<-colnames(RHCData)
Orig_RHC_Colnames<-gsub("str","",Orig_RHC_Colnames)
Orig_RHC_Colnames<-gsub("dtmDate","Date",Orig_RHC_Colnames)
colnames(RHCData)<-Orig_RHC_Colnames

#merge in RHC data
indx_RHC <- RHCData[CAMPHORData,
                    on = c(HospNo = "HospNo", Date = "Date"), 
                    roll = "nearest",
                    which = TRUE]

#Make list of destination columns, omitting HospNo and changing Date to RHC_Date
Orig_RHC_Colnames<-Orig_RHC_Colnames[-1]
Merged_RHC_Colnames<-gsub("Date","RHC_Matched_Date",Orig_RHC_Colnames)
MergedTable[, (Merged_RHC_Colnames) := RHCData[indx_RHC, Orig_RHC_Colnames, with=FALSE]]




#Filter to find matches within 3 months (92 days) only
Date_Threshold=92
#subset of i rows, where each date is less than 6 months
MergedTable_filtered<-MergedTable[abs(ymd(Date)-ymd(BNP_Matched_Date)) < Date_Threshold & 
                                    abs(ymd(Date)-ymd(NYHA_Matched_Date)) < Date_Threshold &
                                    abs(ymd(Date)-ymd(RHC_Matched_Date)) < Date_Threshold &
                                    abs(ymd(Date)-ymd(MWD_Matched_Date)) < Date_Threshold ]
                                    

#Write out
write.csv(MergedTable, file = "RHC+BNP+NYHA merged data.V6.csv")
write.csv(MergedTable_filtered, file = "RHC+BNP+NYHA merged filtered data.V6.csv")

#Remove indices, vectors, and original files used
rm(indx_BNP,indx_NYHA, indx_RHC, indx_MWD)
rm(Merged_RHC_Colnames, Orig_RHC_Colnames, MWD_new_col_names, MWD_old_col_names)
rm(BNPData,CAMPHORData,MWDData,NYHAData,RHCData)
