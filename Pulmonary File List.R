#Pulmonary File List
#
#
#script to load list of *.xlsx files containing blood test results, perform extensive
#data cleaning, then join the results to a list of patients.
#
#Joined data is then summarised as a table of BNP scores over time
#
#based on https://stackoverflow.com/questions/32888757/reading-multiple-files-into-r-best-practice

#________________________________________________________

if (!require("pacman")) install.packages("pacman")
pacman::p_load(readxl, lubridate, data.table)

setwd("C:/Users/Edward/PowerFolders/MISC/Katherine R and Excel/InputFiles/Newest_Data")

#________________________________________________________

#get list of xlsx files in the folder
file.list <- list.files(pattern='*.xlsx')

#for each file in the list, load it with read_excel, skipping the first 2 rows
#this still throws a number of errors: poor quality input data
data.list <- lapply(file.list, read_excel, skip=2,trim_ws = TRUE)

#combine lists to make a single data.table
BloodData <- rbindlist(data.list)

#rmove intermediate steps
rm(data.list,file.list)

#rename column
setnames(BloodData,"Hosp No","HospNo")                      #rename column

#data is very poor quality - lots of empty rows
#when we  de-duplicate using data.table::unique only the first occurance is kept
#sort table by descending BNP value, so the row that is kept should have data
#This does NOT remove text values from what should be numeric data only, i.e.
# an entry like "less than 30" is kept
BloodData<-BloodData[order(-BNP)]   
BloodData<-BloodData[BNP!="",]                          #remove blanks from BNP
BloodData <- unique(BloodData, by=c("HospNo", "Date"))  #remove duplicates

#some dummy patients added - e.g. P123456789.
#although they shouldn't match real patient IDs in lookups, we'll remove
#them by only keeping HospNo entries that are 7 characers long and start with "P"
BloodData<-BloodData[nchar(BloodData$"HospNo") == 7,]
BloodData<-BloodData[substr(BloodData$"HospNo",1,1) == "P",]

#________________________________________________________
#IMPORT PATIENT OPERATION DATES
#read list of target patients and make into data table with POSIX date values
#surprisingly difficult for this date format - have to make a new class from the date string 
#might have been easier if using read.csv with the 'stringAsFactors = FALSE' option
setClass('myDate')
setAs("character","myDate", function(from) as.POSIXct(from, format="%d/%m/%Y") )
Target_Patients <- read.csv("Blood_Data_Merged.csv", colClasses=c('numeric','myDate'), header=TRUE)

setDT(Target_Patients)                                        #make table
setnames(Target_Patients,"RGM","HospNo")                      #rename column
Target_Patients$HospNo=paste0('P', Target_Patients$HospNo)    #Add 'P' and rename column
Target_Patients<-unique(Target_Patients)

#________________________________________________________

#sort both data tables by HospNo
setkey(BloodData,"HospNo", Date)
setkey(Target_Patients,"HospNo")

#________________________________________________________


TestMerge_indx<-Target_Patients[BloodData,
                          on = c("HospNo" = "HospNo", "pea_date" = "Date"),
                          roll="nearest",
                          which=TRUE]

#Match operation dates to blood date, and remove all records without a match (i.e. NA)
Patients_BNP<-BloodData[, Operation_Date := Target_Patients[TestMerge_indx,pea_date]]
Patients_BNP<-Patients_BNP[is.na(Operation_Date) == 0,]

#Express dates relative to operation date 
#this generates continuous data, which can then be used for correlation work
Patients_BNP[, Time_since_operation := as.numeric(ymd(Date)-ymd(Operation_Date))]


#For statistical analysis require data to be binned:
#1 - up to 1 week before operation
#2 - 3 months (11-14 weeks) after operation
#3 - 6 months (24-28 weeks) after operation
#4 - 12 months (49-55 weeks) after operation

#simplistically we can simply make groups:
#Patients_BNP_3mnths<-Patients_BNP[Time_since_operation<113 & Time_since_operation > 70,]
#better to use a function for making bins; either use;
#cut, binr, findInterval or the data.table based solution here:
#https://github.com/ben519/mltools/blob/master/R/bin_data.R
#https://cran.r-project.org/web/packages/binr/binr.pdf
  
#in this case we will extract the columns HospNo, BNP, Operation_date and time since operation
#we also filter out rows containing BNP for times more than a week before operation
Patients_BNP_short<-Patients_BNP[Time_since_operation>=-7,.(HospNo,BNP,Time_since_operation)]

#make a list of how to group the data:
#1 week to Op, Oper to 3 weeks later, 3 weeks to 1.5 months (bin) week after op, 3 months, then years
sequence=c(-7,0,20,40,120,(seq(300,2200,365)))

#make a new column detailing which bin the data falls in to
Patients_BNP_short[,Time_Bin := cut(Time_since_operation, sequence)]

#This could now be used to make summary table ######HOWEVER######
#some BNP values are non-numeric, e.g. 'less than 30'. For summarizing to work
#we have to replace these with 183 (the median of the dataset)

#Can be removed by hard coded replacement of text:
#set(Patients_BNP_short, i=Patients_BNP_short[BNP=="LESS THAN 30",which=TRUE], j="BNP", value= 183)
#set(Patients_BNP_short, i=Patients_BNP_short[BNP=="Less than 10",which=TRUE], j="BNP", value= 183)

#Better: replace all rows where the contents can't be expressed as a number 
#this uses the set notation - here the simpler i, j form is used for clarity
set(Patients_BNP_short, i=Patients_BNP_short[is.na(as.numeric(BNP))==1,which=TRUE], j="BNP", value= 183)

#To replace all remaining non-numeric with NA:
#This line could be altered to simply set BNP as numeric
Patients_BNP_short$BNP <- as.numeric(as.character(Patients_BNP_short$BNP))

#Mke summary table using dcast (a bit like Excel Pivot Table)
Summary_Table<-dcast(Patients_BNP_short, HospNo ~ Time_Bin, fun = mean, value.var = "BNP")

#Rename columns
#setnames(Test,c(HospNo,Pre-Op,

write.csv(Summary_Table, file = "BNP_changes_over_time.csv")

