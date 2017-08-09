---
title: "Phil Mosley PD subjects: Read in data and run data imputation"
author: "David Smith"
date: "8 July 2017"
output: html_document
---

#Hi Phil, here is code to read in your data from your spreadsheets to the longitudinal analysis file that I use for analysis (one of the main analysis files).


#First step is to take your Excel files and save them as csvs.  This is necessary if you are running this at QIMR.  The issue is that most of the techniques for reading in Excel files into R require java and java is forbidden at QIMR.  So we save each of your worksheets from Excel as csv files with filenames like this:

  #Pre-DBS Data csv.csv
  #FU1 data csv.csv
  #FU2 data csv.csv
  #FU3 data csv.csv
  #FU4 data csv.csv

#save these somewhere and then change the path below to where you saved them
setwd("E:/Documents and Settings/Administrator/Desktop/Phil_Mosley/Syntax")

#attempt to use read_excel to read in Excel files
library(readxl)
#imputation package
library(mice)
#visualize missing values
library(VIM)


#read them in
PreDBS<-read.csv("E:/Documents and Settings/Administrator/Desktop/Phil_Mosley/WorkingData/Pre-DBS Data csv.csv")

#I had "Gender" in my models for this covariate from before and so this is a quick fix instead of a fiddly search/replace
names(PreDBS)[3]<-"Gender"

#this next line and ones like it remove blank lines that Excel sometimes leaves
PreDBS<-PreDBS[!is.na(PreDBS$ID),]


FU1DBS<-data.frame(read.csv("E:/Documents and Settings/Administrator/Desktop/Phil_Mosley/WorkingData/FU1 Data csv.csv"))
#exclude 19 because 19 has no data anywhere; this was changed in the csv file but not in the excel file
FU1DBS<-FU1DBS[!is.na(FU1DBS$ID),]

FU2DBS<-data.frame(read.csv("E:/Documents and Settings/Administrator/Desktop/Phil_Mosley/WorkingData/FU2 Data csv.csv"))
FU2DBS<-FU2DBS[!is.na(FU2DBS$ID),]

FU3DBS<-data.frame(read.csv("E:/Documents and Settings/Administrator/Desktop/Phil_Mosley/WorkingData/FU3 Data csv.csv"))
FU3DBS<-FU3DBS[!is.na(FU3DBS$ID),]

FU4DBS<-data.frame(read.csv("E:/Documents and Settings/Administrator/Desktop/Phil_Mosley/WorkingData/FU4 Data csv.csv"))
FU4DBS<-FU4DBS[!is.na(FU4DBS$ID),]

#this may work for you, not sure.  if not, you'll have to export as csv
anatomical <- read_excel('E:/Documents and Settings/Administrator/Desktop/Phil_Mosley/WorkingData/Anatomical_Data_Normalised_Lead_DBS_v3.xlsx')
anatomical<-anatomical[!is.na(anatomical$ID),]
names(anatomical)<-make.names(names(anatomical))
names(anatomical)[2]<-"Signif.Psych"
anatomical$Signif.Psych<-factor(anatomical$Signif.Psych,labels=c(FALSE,TRUE))

#make a new variable for pts with signif psych and merge that onto PreDBS as a covariate
pts.with.signif.psych<-anatomical[,1:2]

PreDBS<-merge(PreDBS,pts.with.signif.psych, all=T)

#########
#########
#these are preprocessing steps to wipe out data that is missing and have zeroes in there.  The zeroes are inserted instead of missing when Excel derives a formula with a missing value; this is a problem because a 0 is real data.  This next set of steps overwrites missing data 0's with NA (R's missing value)

#######          
#       # #    # 
#       #  #  #  
#####   #   ##   
#       #   ##   
#       #  #  #  
#       # #    #

#     #                                    ######                      
##   ## #  ####   ####  # #    #  ####     #     #   ##   #####   ##   
# # # # # #      #      # ##   # #    #    #     #  #  #    #    #  #  
#  #  # #  ####   ####  # # #  # #         #     # #    #   #   #    # 
#     # #      #      # # #  # # #  ###    #     # ######   #   ###### 
#     # # #    # #    # # #   ## #    #    #     # #    #   #   #    # 
#     # #  ####   ####  # #    #  ####     ######  #    #   #   #    #


#take out pt 19: 
PreDBS<-PreDBS[PreDBS$ID!=19,]
FU1DBS<-FU1DBS[FU1DBS$ID!=19,]
FU2DBS<-FU2DBS[FU2DBS$ID!=19,]
FU3DBS<-FU3DBS[FU3DBS$ID!=19,]
FU4DBS<-FU4DBS[FU4DBS$ID!=19,]
anatomical<-anatomical[anatomical$ID!=19,]

#take out pt 43: 
#PreDBS<-PreDBS[PreDBS$ID!=43,]  this is okay because it is complete at Pre
#no data for these two
FU1DBS<-FU1DBS[FU1DBS$ID!=43,]
FU2DBS<-FU2DBS[FU2DBS$ID!=43,]
#data for this one?  Weird
#FU3DBS<-FU3DBS[FU3DBS$ID!=43,]
FU4DBS<-FU4DBS[FU4DBS$ID!=43,]




#Fill in NAs according to this from Phil:

#For the other data sets:
# FU1 - ID 13 (partial - patient data), 14 (partial - partner data) and 43 (total) will remain missing
# FU2 - ID 20 (total), 27 (total), 30 (total), 36 (partial - partner data) and 38 (total)
# FU3 - ID 1 (total)
# FU4 - Hope to get a complete set (excepting 19)

###We do it this way because Phil gives pt data in the form of EXcel files.
###When an Excel file has an operation on empty, it gives 0.  There are a ton of
###zeroes in the Excel files that should be missing.  Since updates come often, 
###this is a way to nuke the bad zeroes in one swoop instead of changing them by hand after each update.

FU3DBS[FU3DBS$ID==1,paste("FU3_",c("Apathy_Total",
                                   "BIS_Total",
                                   "BIS_Attentional",
                                   "BIS_Motor",
                                   "BIS_NonPlanning",
                                   "EQ_Total",
                                   "Gambling",
                                   "Sex",
                                   "Buying",
                                   "Eating",
                                   "Hobbyism.Punding",
                                   "Medication.Use",
                                   "ICD.Total",
                                   "QUIP.Total",
                                   "BDI_Total",
                                   "GAI_Total",
                                   "CarerBIS_Total",
                                   "CarerBIS_Attentional",
                                   "CarerBIS_Motor",
                                   "CarerBIS_NonPlanning",
                                   "CarerEQ_Total",
                                   "NPI_Total",
                                   "ZBI_Total",
                                   "RQI_Total",
                                   "UPDRS_Total",
                                   "MOCA_Total",
                                    "MMSE_Total",
                                    "DelayDiscount_K"),sep="")]<-NA

FU1DBS[FU1DBS$ID==13,paste("FU1_",c("Apathy_Total",
                                    "BIS_Total",
                                    "BIS_Attentional",
                                    "BIS_Motor",
                                    "BIS_NonPlanning",
                                    "EQ_Total",
                                    "Gambling",
                                    "Sex",
                                    "Buying",
                                    "Eating",
                                    "Hobbyism.Punding",
                                    "Medication.Use",
                                    "ICD.Total",
                                    "QUIP.Total",
                                    "BDI_Total",
                                    "GAI_Total"),sep="")]<-NA

FU1DBS[FU1DBS$ID==14,paste("FU1_",c("CarerBIS_Total",
                                    "CarerBIS_Attentional",
                                    "CarerBIS_Motor",
                                    "CarerBIS_NonPlanning",
                                    "CarerEQ_Total",
                                    "NPI_Total",
                                    "ZBI_Total",
                                    "RQI_Total"),sep="")]<-NA


FU2DBS[FU2DBS$ID==20,paste("FU2_",c("Apathy_Total",
                                    "BIS_Total",
                                    "BIS_Attentional",
                                    "BIS_Motor",
                                    "BIS_NonPlanning",
                                    "EQ_Total",
                                    "Gambling",
                                    "Sex",
                                    "Buying",
                                    "Eating",
                                    "Hobbyism.Punding",
                                    "Medication.Use",
                                    "ICD.Total",
                                    "QUIP.Total",
                                    "BDI_Total",
                                    "GAI_Total",
                                    "CarerBIS_Total",
                                    "CarerBIS_Attentional",
                                    "CarerBIS_Motor",
                                    "CarerBIS_NonPlanning",
                                    "CarerEQ_Total",
                                    "NPI_Total",
                                    "ZBI_Total",
                                    "RQI_Total",
                                    "UPDRS_Total",
                                    "MOCA_Total",
                                    "MMSE_Total",
                                    "DelayDiscount_K"),sep="")]<-NA


FU4DBS[FU4DBS$ID==26,paste("FU4_",c("CarerBIS_Total",
                                    "CarerBIS_Attentional",
                                    "CarerBIS_Motor",
                                    "CarerBIS_NonPlanning",
                                    "CarerEQ_Total",
                                    "NPI_Total",
                                    "ZBI_Total",
                                    "RQI_Total"),sep="")]<-NA



FU2DBS[FU2DBS$ID==27,paste("FU2_",c("Apathy_Total",
                                    "BIS_Total",
                                    "BIS_Attentional",
                                    "BIS_Motor",
                                    "BIS_NonPlanning",
                                    "EQ_Total",
                                    "Gambling",
                                    "Sex",
                                    "Buying",
                                    "Eating",
                                    "Hobbyism.Punding",
                                    "Medication.Use",
                                    "ICD.Total",
                                    "QUIP.Total",
                                    "BDI_Total",
                                    "GAI_Total",
                                    "CarerBIS_Total",
                                    "CarerBIS_Attentional",
                                    "CarerBIS_Motor",
                                    "CarerBIS_NonPlanning",
                                    "CarerEQ_Total",
                                    "NPI_Total",
                                    "ZBI_Total",
                                    "RQI_Total",
                                    "UPDRS_Total",
                                    "MOCA_Total",
                                    "MMSE_Total",
                                    "DelayDiscount_K"),sep="")]<-NA

FU2DBS[FU2DBS$ID==30,paste("FU2_",c("Apathy_Total",
                                    "BIS_Total",
                                    "BIS_Attentional",
                                    "BIS_Motor",
                                    "BIS_NonPlanning",
                                    "EQ_Total",
                                    "Gambling",
                                    "Sex",
                                    "Buying",
                                    "Eating",
                                    "Hobbyism.Punding",
                                    "Medication.Use",
                                    "ICD.Total",
                                    "QUIP.Total",
                                    "BDI_Total",
                                    "GAI_Total",
                                    "CarerBIS_Total",
                                    "CarerBIS_Attentional",
                                    "CarerBIS_Motor",
                                    "CarerBIS_NonPlanning",
                                    "CarerEQ_Total",
                                    "NPI_Total",
                                    "ZBI_Total",
                                    "RQI_Total",
                                    "UPDRS_Total",
                                    "MOCA_Total",
                                    "MMSE_Total",
                                    "DelayDiscount_K"),sep="")]<-NA

FU2DBS[FU2DBS$ID==36,paste("FU2_",c("CarerBIS_Total",
                                    "CarerBIS_Attentional",
                                    "CarerBIS_Motor",
                                    "CarerBIS_NonPlanning",
                                    "CarerEQ_Total",
                                    "NPI_Total",
                                    "ZBI_Total",
                                    "RQI_Total"),sep="")]<-NA

FU2DBS[FU2DBS$ID==38,paste("FU2_",c("Apathy_Total",
                                    "BIS_Total",
                                    "BIS_Attentional",
                                    "BIS_Motor",
                                    "BIS_NonPlanning",
                                    "EQ_Total",
                                    "Gambling",
                                    "Sex",
                                    "Buying",
                                    "Eating",
                                    "Hobbyism.Punding",
                                    "Medication.Use",
                                    "ICD.Total",
                                    "QUIP.Total",
                                    "BDI_Total",
                                    "GAI_Total",
                                    "CarerBIS_Total",
                                    "CarerBIS_Attentional",
                                    "CarerBIS_Motor",
                                    "CarerBIS_NonPlanning",
                                    "CarerEQ_Total",
                                    "NPI_Total",
                                    "ZBI_Total",
                                    "RQI_Total",
                                    "UPDRS_Total",
                                    "MOCA_Total",
                                    "MMSE_Total",
                                    "DelayDiscount_K"),sep="")]<-NA



FU3DBS[FU3DBS$ID==43,paste("FU3_",c("CarerBIS_Total",
                                    "CarerBIS_Attentional",
                                    "CarerBIS_Motor",
                                    "CarerBIS_NonPlanning",
                                    "CarerEQ_Total",
                                    "NPI_Total",
                                    "ZBI_Total",
                                    "RQI_Total"),sep="")]<-NA

FU3DBS[FU3DBS$ID==47,paste("FU3_",c("Gambling",
                                    "Sex",
                                    "Buying",
                                    "Eating",
                                    "Hobbyism.Punding",
                                    "Medication.Use",
                                    "ICD.Total",
                                    "QUIP.Total"),sep="")]<-NA


FU2DBS[FU2DBS$ID==50,paste("FU2_",c("CarerBIS_Total",
                                    "CarerBIS_Attentional",
                                    "CarerBIS_Motor",
                                    "CarerBIS_NonPlanning",
                                    "CarerEQ_Total",
                                    "NPI_Total",
                                    "ZBI_Total",
                                    "RQI_Total"),sep="")]<-NA


FU2DBS[FU2DBS$ID==51,paste("FU2_",c("CarerBIS_Total",
                                    "CarerBIS_Attentional",
                                    "CarerBIS_Motor",
                                    "CarerBIS_NonPlanning",
                                    "CarerEQ_Total"),sep="")]<-NA


FU4DBS[FU4DBS$ID==51,paste("FU4_",c("Apathy_Total",
                                    "BIS_Total",
                                    "BIS_Attentional",
                                    "BIS_Motor",
                                    "BIS_NonPlanning",
                                    "EQ_Total",
                                    "Gambling",
                                    "Sex",
                                    "Buying",
                                    "Eating",
                                    "Hobbyism.Punding",
                                    "Medication.Use",
                                    "ICD.Total",
                                    "QUIP.Total",
                                    "BDI_Total",
                                    "GAI_Total",
                                    "CarerBIS_Total",
                                    "CarerBIS_Attentional",
                                    "CarerBIS_Motor",
                                    "CarerBIS_NonPlanning",
                                    "CarerEQ_Total",
                                    "NPI_Total",
                                    "ZBI_Total",
                                    "RQI_Total",
                                    "UPDRS_Total",
                                    "MOCA_Total",
                                    "MMSE_Total",
                                    "DelayDiscount_K"),sep="")]<-NA

FU2DBS[FU2DBS$ID==59,paste("FU2_",c("Apathy_Total",
                                    "BIS_Total",
                                    "BIS_Attentional",
                                    "BIS_Motor",
                                    "BIS_NonPlanning",
                                    "EQ_Total",
                                    "Gambling",
                                    "Sex",
                                    "Buying",
                                    "Eating",
                                    "Hobbyism.Punding",
                                    "Medication.Use",
                                    "ICD.Total",
                                    "QUIP.Total",
                                    "BDI_Total",
                                    "GAI_Total",
                                    "CarerBIS_Total",
                                    "CarerBIS_Attentional",
                                    "CarerBIS_Motor",
                                    "CarerBIS_NonPlanning",
                                    "CarerEQ_Total",
                                    "NPI_Total",
                                    "ZBI_Total",
                                    "RQI_Total",
                                    "UPDRS_Total",
                                    "MOCA_Total",
                                    "MMSE_Total",
                                    "DelayDiscount_K"),sep="")]<-NA

FU2DBS[FU2DBS$ID==64,paste("FU2_",c("Apathy_Total",
                                    "BIS_Total",
                                    "BIS_Attentional",
                                    "BIS_Motor",
                                    "BIS_NonPlanning",
                                    "EQ_Total",
                                    "Gambling",
                                    "Sex",
                                    "Buying",
                                    "Eating",
                                    "Hobbyism.Punding",
                                    "Medication.Use",
                                    "ICD.Total",
                                    "QUIP.Total",
                                    "BDI_Total",
                                    "GAI_Total"),sep="")]<-NA

FU3DBS[FU3DBS$ID==60,paste("FU3_",c("CarerBIS_Total",
                                    "CarerBIS_Attentional",
                                    "CarerBIS_Motor",
                                    "CarerBIS_NonPlanning",
                                    "CarerEQ_Total",
                                    "NPI_Total",
                                    "ZBI_Total",
                                    "RQI_Total"),sep="")]<-NA

FU3DBS[FU3DBS$ID==60,paste("FU3_",c("CarerBIS_Total",
                                    "CarerBIS_Attentional",
                                    "CarerBIS_Motor",
                                    "CarerBIS_NonPlanning",
                                    "CarerEQ_Total",
                                    "NPI_Total",
                                    "ZBI_Total",
                                    "RQI_Total"),sep="")]<-NA


PreDBS[PreDBS$ID==62,paste("Pre_",c("BDI_Total"),sep="")]<-NA


FU1DBS[FU1DBS$ID==62,paste("FU1_",c("Apathy_Total",
                                    "Gambling",
                                    "Sex",
                                    "Buying",
                                    "Eating",
                                    "Hobbyism.Punding",
                                    "Medication.Use",
                                    "ICD.Total",
                                    "QUIP.Total",
                                    "GAI_Total",
                                    "CarerBIS_Total",
                                    "CarerBIS_Attentional",
                                    "CarerBIS_Motor",
                                    "CarerBIS_NonPlanning",
                                    "CarerEQ_Total",
                                    "NPI_Total",
                                    "ZBI_Total",
                                    "RQI_Total",
                                    "UPDRS_Total",
                                    "MOCA_Total",
                                    "MMSE_Total",
                                    "DelayDiscount_K"),sep="")]<-NA

FU2DBS[FU2DBS$ID==62,paste("FU2_",c("UPDRS_Total",
                                    "MOCA_Total",
                                    "MMSE_Total",
                                    "DelayDiscount_K"),sep="")]<-NA


#Get rid of the RQI for ID==50 as caregiver was the child and not the spouse
PreDBS[PreDBS$ID==50,paste("Pre_",c("RQI_Total"),sep="")]<-NA
FU1DBS[FU1DBS$ID==50,paste("FU1_",c("RQI_Total"),sep="")]<-NA
FU2DBS[FU2DBS$ID==50,paste("FU2_",c("RQI_Total"),sep="")]<-NA
FU3DBS[FU3DBS$ID==50,paste("FU3_",c("RQI_Total"),sep="")]<-NA
FU4DBS[FU4DBS$ID==50,paste("FU4_",c("RQI_Total"),sep="")]<-NA


#Get rid of the RGI for ID==64 FU1 onwards as we stopped doing it
FU1DBS[FU1DBS$ID==64,paste("FU1_",c("RQI_Total"),sep="")]<-NA
FU2DBS[FU2DBS$ID==64,paste("FU2_",c("RQI_Total"),sep="")]<-NA
FU3DBS[FU3DBS$ID==64,paste("FU3_",c("RQI_Total"),sep="")]<-NA
FU4DBS[FU4DBS$ID==64,paste("FU4_",c("RQI_Total"),sep="")]<-NA

PreDBS[PreDBS$ID==65,paste("Pre_",c("RQI_Total"),sep="")]<-NA
FU1DBS[FU1DBS$ID==65,paste("FU1_",c("RQI_Total"),sep="")]<-NA
FU2DBS[FU2DBS$ID==65,paste("FU2_",c("RQI_Total"),sep="")]<-NA
FU3DBS[FU3DBS$ID==65,paste("FU3_",c("RQI_Total"),sep="")]<-NA
FU4DBS[FU4DBS$ID==65,paste("FU4_",c("RQI_Total"),sep="")]<-NA

PreDBS[PreDBS$ID==66,paste("Pre_",c("RQI_Total"),sep="")]<-NA
FU1DBS[FU1DBS$ID==66,paste("FU1_",c("RQI_Total"),sep="")]<-NA
FU2DBS[FU2DBS$ID==66,paste("FU2_",c("RQI_Total"),sep="")]<-NA
FU3DBS[FU3DBS$ID==66,paste("FU3_",c("RQI_Total"),sep="")]<-NA
FU4DBS[FU4DBS$ID==66,paste("FU4_",c("RQI_Total"),sep="")]<-NA

PreDBS[PreDBS$ID==67,paste("Pre_",c("RQI_Total"),sep="")]<-NA
FU1DBS[FU1DBS$ID==67,paste("FU1_",c("RQI_Total"),sep="")]<-NA
FU2DBS[FU2DBS$ID==67,paste("FU2_",c("RQI_Total"),sep="")]<-NA
FU3DBS[FU3DBS$ID==67,paste("FU3_",c("RQI_Total"),sep="")]<-NA
FU4DBS[FU4DBS$ID==67,paste("FU4_",c("RQI_Total"),sep="")]<-NA



#tag the timepoint data char
PreDBS$Timepoint<-"00 BaseLine Prior to DBS"
FU1DBS$Timepoint<-"01 +2wks post DBS"
FU2DBS$Timepoint<-"02 +6wks post DBS"
FU3DBS$Timepoint<-"03 +3mos post DBS"
FU4DBS$Timepoint<-"04 +6mos post DBS"

#tag the timepoint data num
PreDBS$TimepointNum<-0
FU1DBS$TimepointNum<-2
FU2DBS$TimepointNum<-6
FU3DBS$TimepointNum<-13
FU4DBS$TimepointNum<-26

names(PreDBS)[4:8]<-c("ClinicalSubtype","TremorAkinesiaSubtype","HoehnandYahrStage", "YearsSinceDiagnosis","SideofOnset")


#########
#########
#formatting for setting up a longitudinal data set

list.of.vars.for.plotting.no.prefix<-c(
  "_Apathy_Total"         ,"_BIS_Total"            ,"_BIS_Attentional"    ,
  "_BIS_Motor"            ,"_BIS_NonPlanning"      ,"_EQ_Total"             ,"_Gambling"           ,
  "_Sex"                  ,"_Buying"               ,"_Eating"               ,"_Hobbyism.Punding"   ,
  "_Medication.Use"       ,"_ICD.Total"            ,"_QUIP.Total"           ,"_BDI_Total"          , 
  "_GAI_Total"            ,"_CarerBIS_Total"       ,"_CarerBIS_Attentional" ,"_CarerBIS_Motor"     , 
  "_CarerBIS_NonPlanning" ,"_CarerEQ_Total"        ,"_NPI_Total"            ,"_ZBI_Total"          , 
  "_RQI_Total"            ,"_UPDRS_Total"          ,"_MOCA_Exec"            ,"_MOCA_Naming"        , 
  "_MOCA_Atten"           ,"_MOCA_Lang"            ,"_MOCA_Abst"            ,"_MOCA_Recall"        , 
  "_MOCA_Orient"          ,"_MOCA_Total"           ,"_MMSE_Total"           ,"_Hayling_BoxA"       , 
  "_Hayling_BoxB"         ,"_Hayling_BoxC"         ,"_Hayling_Overall"      ,"_Hayling_CatAErrors" , 
  "_Hayling_CatBErrors"   ,"_Hayling_ABErrorScore" ,"_ELF_TotalCorrect"     ,"_ELF_RuleViolations" , 
  "_ELF_Repetitions"      ,"_DelayDiscount_K"      ,"_LEDD"
)

PreDBS.stack<-PreDBS[,c("ID","Timepoint","TimepointNum", paste("Pre",list.of.vars.for.plotting.no.prefix,sep=""))]
FU1DBS.stack<-FU1DBS[,c("ID","Timepoint","TimepointNum", paste("FU1",list.of.vars.for.plotting.no.prefix,sep=""))]
FU2DBS.stack<-FU2DBS[,c("ID","Timepoint","TimepointNum", paste("FU2",list.of.vars.for.plotting.no.prefix,sep=""))]
FU3DBS.stack<-FU3DBS[,c("ID","Timepoint","TimepointNum", paste("FU3",list.of.vars.for.plotting.no.prefix,sep=""))]
FU4DBS.stack<-FU4DBS[,c("ID","Timepoint","TimepointNum", paste("FU4",list.of.vars.for.plotting.no.prefix,sep=""))]

#this picks off the names of the important endpoints from the first PreDBS.stack data set 
unified.names<-substring(names(PreDBS.stack[4:length(names(PreDBS.stack))]),first=5)
names(PreDBS.stack)<-names(FU1DBS.stack)<-names(FU2DBS.stack)<-names(FU3DBS.stack)<-names(FU4DBS.stack)<-c("ID","Timepoint","TimepointNum",unified.names)

#make a longitudinal data set
stacked.long.unified<-rbind(PreDBS.stack,
                            FU1DBS.stack,
                            FU2DBS.stack,
                            FU3DBS.stack,
                            FU4DBS.stack)

#propagate the other BL vars to each ID
#this also includes the significant psych problems variable

#stacked.long.unified
#This is the long version of the data set - stacked, so that ID is repeated and time point is given on each new line
stacked.long.unified<-merge(PreDBS[,c("ID", "Age","ClinicalSubtype","TremorAkinesiaSubtype",  "Gender", "Signif.Psych", "SideofOnset", "HoehnandYahrStage", "YearsSinceDiagnosis")],stacked.long.unified, by="ID")


#make formatted vars for splitting the plots
stacked.long.unified$ClinicalSubtype<-factor(stacked.long.unified$ClinicalSubtype)
stacked.long.unified$TremorAkinesiaSubtype<-factor(stacked.long.unified$TremorAkinesiaSubtype)
stacked.long.unified$Gender<-factor(stacked.long.unified$Gender)
stacked.long.unified$ID<-factor(stacked.long.unified$ID)
stacked.long.unified$Signif.Psych<-factor(stacked.long.unified$Signif.Psych)
stacked.long.unified$Signif.Psych.CaseYN<-stacked.long.unified$Signif.Psych
levels(stacked.long.unified$Signif.Psych.CaseYN)<-c("Not a Case","Case")

stacked.long.unified<-stacked.long.unified[order(stacked.long.unified$ID,stacked.long.unified$TimepointNum),]





###                                                         
 #  #    # #####  #    # #####   ##   ##### #  ####  #    # 
 #  ##  ## #    # #    #   #    #  #    #   # #    # ##   # 
 #  # ## # #    # #    #   #   #    #   #   # #    # # #  # 
 #  #    # #####  #    #   #   ######   #   # #    # #  # # 
 #  #    # #      #    #   #   #    #   #   # #    # #   ## 
### #    # #       ####    #   #    #   #   #  ####  #    # 



#not bad missingness plot
aggr_plot<-aggr(stacked.long.unified, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(stacked.long.unified), cex.axis=.4, gap=3, ylab=c("Histogram of missing data","Pattern"))

#CART linear regression
#***this will take a long time***
cart.impute.stacked.long.unified.mids<-
  mice(stacked.long.unified[!is.na(stacked.long.unified$Signif.Psych),],
       m=50,maxit=50,meth='cart',seed=500)
cart.impute.stacked.long.unified<-complete(cart.impute.stacked.long.unified.mids,1)

#when it is done, write it out as csv
write.csv(cart.impute.stacked.long.unified, file="E:/Documents and Settings/Administrator/Desktop/Phil_Mosley/WorkingData/stacked_long_unified_with_cart_imputation.csv")

