#Camphor MWD only.R
#09/10/2017
#_________________________________________________________________________________________
#Script re-using much of the data joining code that combines  NYHA, BNP and RHC data 
#into Camphor data table using HospNo and nearest neighbour Date.  Here only MWD is used 
##_________________________________________________________________________________________
#DEPENDENCIES (automatically installed)
#
#pacman - package manager that installs other packages: https://cran.r-project.org/web/packages/pacman/index.html
#readXL - used to import Excel data: https://cran.r-project.org/web/packages/readxl/
#lubridate - functions to work with date-times and time-spans: https://cran.r-project.org/web/packages/lubridate/
#data.table - fast aggregation and joins of large data: https://cran.r-project.org/web/packages/data.table/index.html


#Check if packages are installed, if not install them, then load the liraries:
if (!require("pacman")) install.packages("pacman")
pacman::p_load(readxl, lubridate, data.table)
#_________________________________________________________________________________________

#set working directory
setwd("C:/Users/Edward/PowerFolders/MISC/Katherine R and Excel/InputFiles")
#setwd("C:/Users/Katherine/Desktop/")			

#read in data

CAMPHORData <- read_excel ("Camphor_raw.xlsx", sheet = "Sheet1")
MWDData <- read_excel ("6MWD _raw.xlsx", sheet = "Sheet1")

#make the Excel data into data.tables
setDT(CAMPHORData)
setDT(MWDData)

#sort all the same way (not necessary as long as merge table is sorted)
setkey(CAMPHORData,HospNo, Date)
setkey(MWDData,HospNo, TestDate)

#remove any rows where same patient (HospNo) is seen more than once on same date
#so that the unique function works on just strHospNo and dtmDate (but NOT score) we 
#have to put these two together using the concatenate function  c()
#this is important for preventing multiple match lookups.
#unclear why a patient would get two scores on the same day - data input error
CAMPHORData <- unique(CAMPHORData, by=c("HospNo", "Date"))
MWDData <- unique(MWDData, by=c("HospNo", "TestDate"))

#make an copy of CAMPHOR data to merge into
MergedTable <- CAMPHORData

#Make an index that describes how the tables should be joined together
indx_MWD <- MWDData[CAMPHORData,
                    on = c(HospNo = "HospNo", TestDate = "Date"), 
                    roll = "nearest",
                    which = TRUE] # which = true means an index is returned

#make list of old and new column names, then read these into the merged table
#written as separate lines for clarity, works as single line when code substituted

#make a list of the new column names by:
#1 take the column names from MWDDate: colnames(MWDData)
#2 make a list containing the numbers 1 and 2: c(1,2)
#3 chain [] the lines above to remove (-) the  first two column titles from the list:  
#4 add a new first value using c c("MWD_Matched_Date"
MWD_new_col_names<-c("MWD_Matched_Date",colnames(MWDData)[-c(1,2)])

#old column names without the first one (which is HospID)
MWD_old_col_names <- colnames(MWDData)[-1]

#perform the join, simultaneously renmaing the columns
MergedTable[, (MWD_new_col_names) := MWDData[indx_MWD, MWD_old_col_names, with=FALSE]]


#Filter to find matches within 3 months (92 days) only
Date_Threshold=92

#subset of i rows, where each date is less than 6 months
MergedTable_filtered<-MergedTable[abs(ymd(Date)-ymd(MWD_Matched_Date)) < Date_Threshold ]


#Write out
write.csv(MergedTable, file = "RHC+MWD data.V7.csv")
write.csv(MergedTable_filtered, file = "RHC+MWD data_filtered.V7.csv")

#Remove indices, vectors, and original files used
#rm(indx_BNP,indx_NYHA, indx_RHC, indx_MWD)
#rm(Merged_RHC_Colnames, Orig_RHC_Colnames, MWD_new_col_names, MWD_old_col_names)
#rm(BNPData,CAMPHORData,MWDData,NYHAData,RHCData)
