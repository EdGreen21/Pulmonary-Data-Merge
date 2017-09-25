#RHC+BNP+NYHA merge.R
#
#R code to merge NYHA and BNP data into RHC data table
#Merging done by HospNo and Date, where Date is same or earlier than Date in RHC data table
#Code based on: https://stackoverflow.com/questions/33357341/data-table-join-on-id-and-date-key-but-want-closest-date-before-or-equal-to
#
#DEPENDENCIES (automatically installed)
#
#pacman - package manager that installs other packages: https://cran.r-project.org/web/packages/pacman/index.html
#readXL - used to import Excel data: https://cran.r-project.org/web/packages/readxl/
#lubridate - functions to work with date-times and time-spans: https://cran.r-project.org/web/packages/lubridate/
#data.table - fast aggregation and joins of large data: https://cran.r-project.org/web/packages/data.table/index.html
#
#NOTEs
# merge roll to Infinity is used, i.e. return closest session before.  This could be substituted for a rolling window, but current approach allows windowing to be done in Excel/BI

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
CAMPHORData <- read_excel ("Camphor_data.xlsx", sheet = "Sheet1")

#make the Excel data into data tables
setDT(RHCData)
setDT(NYHAData)
setDT(BNPData)
setDT (CAMPHORData)

#remove any duplicate rows
BNPData <- unique(BNPData)
NYHAData <- unique(NYHAData)
RHCData <- unique(RHCData)
CAMPHORData <- unique (CAMPHORData)



########################################
#make an copy of RHC table to merge into, ordered by HospNo and then Date
#THIS MEANS THAT RHCData IS THE BASE TABLE, this may not be optimal
MergedTable <- CAMPHORData[order(HospNo, Date)]
########################################


########################################
#For every patient (HospNo) and Date in RHCData data table, find the index of _
#the Date in BNPdata date table that is on or BEFORE the Date in RHCData 
#NOTE: if patient not in BNPdata, NA is returned 	
indx_BNP <- BNPData[CAMPHORData,
                    on = c(HospNo = "HospNo", 
                           Date = "Date"), 
                    roll = "nearest",
                    which = TRUE]

#Add matched BNP results to the output file: MergedTable
MergedTable[, BNP_Matched_Date := BNPData[indx_BNP,Date]]
MergedTable[, BNP := BNPData[indx_BNP,ProBNP]]


########################################
#For every patient (HospNo) and Date in NYHAData data table, find the index of _
#the Date in NYHAData date table that is on or BEFORE the Date in RHCData 
#NOTE: if patient not in NYHAData, NA is returned 	
indx_NYHA <- NYHAData[CAMPHORData,
                      on = c(HospNo = "HospNo", 
                             Date = "Date"), 
                      roll = "nearest",
                      which = TRUE]

#Add matched BNP results to the output file: MergedTable
MergedTable[, NYHA_Matched_Date := NYHAData[indx_NYHA,Date]]
MergedTable[, NYHA := NYHAData[indx_NYHA,NYHA]]

#######################################


#edit RHCData column names to remove data identifiers 'str' and 'dtm' ('boo' is kept)

#would be more elegant to edit in just the output list, but this way the merge is easier
RHCData_Colnames<-colnames(RHCData)
RHCData_Colnames<-gsub("str","",RHCData_Colnames)
RHCData_Colnames<-gsub("dtmDate","Date",RHCData_Colnames)
colnames(RHCData)<-RHCData_Colnames

#For every patient (HospNo) and Date in NYHAData data table, find the index of _
#the Date in NYHAData date table that is on or BEFORE the Date in RHCData 
#NOTE: if patient not in NYHAData, NA is returned 	
indx_RHC <- RHCData[CAMPHORData,
                    on = c(HospNo = "HospNo", 
                           Date = "Date"), 
                    roll = "nearest",
                    which = TRUE]

#Make list of destination columns, omitting HospNo and changing Date to RHC_Date
RHCData_Colnames<-RHCData_Colnames[-1]
Merged_RHC_Colnames<-gsub("Date","RHC_Matched_Date",RHCData_Colnames)
MergedTable[, (Merged_RHC_Colnames) := RHCData[indx_RHC, RHCData_Colnames, with=FALSE]]




#Filter to find matches within 6 months only
Date_Threshold=178 #half a year
#subset of i rows, where each date is less than 6 months
MergedTable_filtered<-MergedTable[abs(ymd(Date)-ymd(BNP_Matched_Date)) < Date_Threshold & abs(ymd(Date)-ymd(NYHA_Matched_Date)) < Date_Threshold & abs(ymd(Date)-ymd(RHC_Matched_Date)) < Date_Threshold]

#Write output
write.csv(MergedTable, file = "RHC+BNP+NYHA merged data.V4.csv")


#Remove indices and vectors used
rm(indx_BNP)
rm(indx_NYHA)
rm(indx_RHC)
rm(Merged_RHC_Colnames)
rm(RHCData_Colnames)

