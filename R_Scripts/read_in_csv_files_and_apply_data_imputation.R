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
setwd('Dropbox/Philip/Professional/Statistical Analyses Parkinsons Disease and DBS/Data sets/09 2017 DATA SET/')

#attempt to use read_excel to read in Excel files
library(readxl)
#imputation package
library(mice)
#visualize missing values
library(VIM)


#Read in data, remove extra lines
PreDBS<-read_excel("Pre-DBS_09_2017.xlsx")
FU1DBS<-read_excel("FU1_Data_09_2017.xlsx")
FU2DBS<-read_excel("FU2_Data_09_2017.xlsx")
FU3DBS<-read_excel("FU3_Data_09_2017.xlsx")
FU4DBS<-read_excel("FU4_Data_09_2017.xlsx")
names(PreDBS)[3]<-"Gender"
PreDBS<-PreDBS[!is.na(PreDBS$ID),]
FU1DBS<-FU1DBS[!is.na(FU1DBS$ID),]
FU2DBS<-FU2DBS[!is.na(FU2DBS$ID),]
FU3DBS<-FU3DBS[!is.na(FU3DBS$ID),]
FU4DBS<-FU4DBS[!is.na(FU4DBS$ID),]
anatomical<-read_excel("Anatomical_Cases.xlsx")
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


#take out pt 03 as his leads are misplaced:
PreDBS<-PreDBS[PreDBS$ID!=3,]
FU1DBS<-FU1DBS[FU1DBS$ID!=3,]
FU2DBS<-FU2DBS[FU2DBS$ID!=3,]
FU3DBS<-FU3DBS[FU3DBS$ID!=3,]
FU4DBS<-FU4DBS[FU4DBS$ID!=3,]
anatomical<-anatomical[anatomical$ID!=3,]

#take out pt 19 as she withdrew from the study after baseline:
PreDBS<-PreDBS[PreDBS$ID!=19,]
FU1DBS<-FU1DBS[FU1DBS$ID!=19,]
FU2DBS<-FU2DBS[FU2DBS$ID!=19,]
FU3DBS<-FU3DBS[FU3DBS$ID!=19,]
FU4DBS<-FU4DBS[FU4DBS$ID!=19,]
anatomical<-anatomical[anatomical$ID!=19,]

#take out pt 33 as he has no anatomical data (had a pacemaker so DBS done using CT images):
PreDBS<-PreDBS[PreDBS$ID!=33,]
FU1DBS<-FU1DBS[FU1DBS$ID!=33,]
FU2DBS<-FU2DBS[FU2DBS$ID!=33,]
FU3DBS<-FU3DBS[FU3DBS$ID!=33,]
FU4DBS<-FU4DBS[FU4DBS$ID!=33,]
anatomical<-anatomical[anatomical$ID!=33,]

#now remove those participants who missed an entire follow up (as data imputation may not be reliable):
#FU1 Total missing: 43, 62
#FU2 Total missing: 20, 27, 30, 38, 43, 59
#FU3 Total missing: 1
#FU4 Total missing: 43, 51, 59

FU1DBS<-FU1DBS[FU1DBS$ID!=43,]
FU1DBS<-FU1DBS[FU1DBS$ID!=62,]

FU2DBS<-FU2DBS[FU2DBS$ID!=20,]
FU2DBS<-FU2DBS[FU2DBS$ID!=27,]
FU2DBS<-FU2DBS[FU2DBS$ID!=30,]
FU2DBS<-FU2DBS[FU2DBS$ID!=38,]
FU2DBS<-FU2DBS[FU2DBS$ID!=43,]
FU2DBS<-FU2DBS[FU2DBS$ID!=59,]

FU3DBS<-FU3DBS[FU3DBS$ID!=1,]

FU4DBS<-FU4DBS[FU4DBS$ID!=43,]
FU4DBS<-FU4DBS[FU4DBS$ID!=51,]
FU4DBS<-FU4DBS[FU4DBS$ID!=59,]

#Fill in NAs for data that is missing within an otherwise complete follow up:

#For the other data sets:
# Pre-DBS - 62 (partial - depression scale)
# FU1 - 13 (partial - patient data), 14 (partial - partner data)
# FU2 - 36 (partial - partner data), 50 (partial - partner data), 51 (partial - partner data), 62 (partial - examination data), 64 (partial - patient data)
# FU3 - 43 (partial - partner data), 47 (partial - patient data), 60 (partial - partner data)
# FU4 - 26 (partial - partner data), 60 (partial - partner data)

###We do it this way because Phil gives pt data in the form of EXcel files.
###When an Excel file has an operation on empty, it gives 0.  There are a ton of
###zeroes in the Excel files that should be missing.  Since updates come often, 
###this is a way to nuke the bad zeroes in one swoop instead of changing them by hand after each update.

PreDBS[PreDBS$ID==62,paste("Pre_",c("BDI_Total"),sep="")]<-NA

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


FU4DBS[FU4DBS$ID==26,paste("FU4_",c("CarerBIS_Total",
                                    "CarerBIS_Attentional",
                                    "CarerBIS_Motor",
                                    "CarerBIS_NonPlanning",
                                    "CarerEQ_Total",
                                    "NPI_Total",
                                    "ZBI_Total",
                                    "RQI_Total"),sep="")]<-NA

FU2DBS[FU2DBS$ID==36,paste("FU2_",c("CarerBIS_Total",
                                    "CarerBIS_Attentional",
                                    "CarerBIS_Motor",
                                    "CarerBIS_NonPlanning",
                                    "CarerEQ_Total",
                                    "NPI_Total",
                                    "ZBI_Total",
                                    "RQI_Total"),sep="")]<-NA

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

FU2DBS[FU2DBS$ID==62,paste("FU2_",c("UPDRS_Total",
                            "UPDRS_Left",
                            "UPDRS_Right",
                            "UPDRS_Axial",
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

FU3DBS[FU3DBS$ID==60,paste("FU3_",c("CarerBIS_Total",
                                    "CarerBIS_Attentional",
                                    "CarerBIS_Motor",
                                    "CarerBIS_NonPlanning",
                                    "CarerEQ_Total",
                                    "NPI_Total",
                                    "ZBI_Total",
                                    "RQI_Total"),sep="")]<-NA

FU4DBS[FU4DBS$ID==60,paste("FU4_",c("CarerBIS_Total",
                                    "CarerBIS_Attentional",
                                    "CarerBIS_Motor",
                                    "CarerBIS_NonPlanning",
                                    "CarerEQ_Total",
                                    "NPI_Total",
                                    "ZBI_Total",
                                    "RQI_Total"),sep="")]<-NA


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
  "_ELF_Repetitions"      ,"_DelayDiscount_K"      ,"_LEDD"                 ,"_UPDRS_Left"         ,
  "_UPDRS_Right"          ,"_UPDRS_Total"
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
stacked.long.unified<-merge(PreDBS[,c("ID", "Age", "ClinicalSubtype","TremorAkinesiaSubtype",  "Gender", "Signif.Psych", "SideofOnset", "HoehnandYahrStage", "YearsSinceDiagnosis")],stacked.long.unified, by="ID")


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
write.csv(cart.impute.stacked.long.unified, file="stacked_long_unified_with_cart_imputation.csv")

#Remember to insert timepoint with NAs at 13 weeks for patient 01 otherwise it messes up the rest of the calcs

