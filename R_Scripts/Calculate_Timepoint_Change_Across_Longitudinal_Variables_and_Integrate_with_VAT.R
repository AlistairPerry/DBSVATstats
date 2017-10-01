---
title: "Phil Mosley PD subjects: Calculate Timepoint Change Across Longitudinal Variables"
author: "Phil Mosley"
date: "24 September 2017"
output: html_document
---  
  
#load libraries
library(ggplot2)
library(readxl)


setwd("~/Dropbox/Statistical Analyses Parkinsons Disease and DBS/Data sets/08 2017 DATA SET/")

#read in imputed data set

Imputed<-read.csv("stacked_long_unified_with_cart_imputation.csv")

#and anatomical
anatomical<-read_excel("Anatomical_Longitudinal.xlsx")
anatomical<-anatomical[!is.na(anatomical$ID),]
names(anatomical)<-make.names(names(anatomical))
names(anatomical)[2]<-"Signif.Psych"
anatomical$Signif.Psych<-factor(anatomical$Signif.Psych,labels=c(FALSE,TRUE))
anatomical<-anatomical[order(anatomical$ID),]

#alistair's version - overlap calculated differently
anatomical2<-read.table("VATstats.txt",header = TRUE)

#Remove ID 33 as this patient has no anatomical data (no MRI due to pacemaker in situ)
Imputed<-Imputed[Imputed$ID!=33,]
anatomical<-anatomical[anatomical$ID!=33,]

#Remove ID 03 as this patient has misplaced leads
Imputed<-Imputed[Imputed$ID!=3,]
anatomical<-anatomical[anatomical$ID!=3,]

### CREATE BASELINE FACTORS ###

Baseline<-Imputed[,c("ID","TimepointNum",
"ClinicalSubtype", "TremorAkinesiaSubtype", "Gender", "Age", "Signif.Psych",
"Signif.Psych.CaseYN")]

Baseline_wide<-reshape(Baseline, idvar = "ID", timevar = "TimepointNum", direction = "wide")

Baseline_Factors<-data.frame("ID" = Baseline_wide$ID, "ClinicalSubtype" = Baseline_wide$ClinicalSubtype.0, "TremorAkinesiaSubtype" = Baseline_wide$TremorAkinesiaSubtype.0, "Gender" = Baseline_wide$Gender.0, "Signif.Psych" = Baseline_wide$Signif.Psych.0, "Signif.Psych.CaseYN" = Baseline_wide$Signif.Psych.CaseYN.0)

### CAREGIVER-RATED EQ ###

CarerEQ<-Imputed[,c("ID","TimepointNum",
"ClinicalSubtype", "TremorAkinesiaSubtype", "Gender", "Age", "Signif.Psych",
"Signif.Psych.CaseYN", "CarerEQ_Total")]

CarerEQ_wide<-reshape(CarerEQ, idvar = "ID", timevar = "TimepointNum", direction = "wide",
drop = c("ClinicalSubtype", "TremorAkinesiaSubtype", "Gender", "Age", "Signif.Psych", "Signif.Psych.CaseYN"))

#Create difference matrix and z-score matrix

CarerEQ<-CarerEQ_wide[,2:6]
CarerEQsc<-CarerEQ
CarerEQsc[,1]<-scale(CarerEQ[,1], center = TRUE, scale = TRUE)
CarerEQsc[,2]<-scale(CarerEQ[,2], center = TRUE, scale = TRUE)
CarerEQsc[,3]<-scale(CarerEQ[,3], center = TRUE, scale = TRUE)
CarerEQsc[,4]<-scale(CarerEQ[,4], center = TRUE, scale = TRUE)
CarerEQsc[,5]<-scale(CarerEQ[,5], center = TRUE, scale = TRUE)

diff.matrix.CarerEQ<-CarerEQsc[,2:5]-CarerEQsc[,1]

#z.matrix<-data.frame(scale(diff.matrix[,1:4], center = TRUE, scale = TRUE))

#Create a data frame with derived variables

CarerEQ_Total<-data.frame("ID" = CarerEQ_wide$ID, "CarerEQ_FU1Diff" = diff.matrix.CarerEQ[,1], "CarerEQ_FU2Diff" = diff.matrix.CarerEQ[,2], "CarerEQ_FU3Diff" = diff.matrix.CarerEQ[,3], "CarerEQ_FU4Diff" = diff.matrix.CarerEQ[,4], "CarerEQ_FU1.z" = CarerEQsc[,2], "CarerEQ_FU2.z" = CarerEQsc[,3], "CarerEQ_FU3.z" = CarerEQsc[,4], "CarerEQ_FU4.z" = CarerEQsc[,5])


### PATIENT-RATED EQ ###

EQ<-Imputed[,c("ID","TimepointNum",
               "ClinicalSubtype", "TremorAkinesiaSubtype", "Gender", "Age", "Signif.Psych",
               "Signif.Psych.CaseYN", "EQ_Total")]

EQ_wide<-reshape(EQ, idvar = "ID", timevar = "TimepointNum", direction = "wide",
                 drop = c("ClinicalSubtype", "TremorAkinesiaSubtype", "Gender", "Age", "Signif.Psych", "Signif.Psych.CaseYN"))

#Create difference matrix and z-score matrix

EQ<-EQ_wide[,2:6]
EQsc<-EQ
EQsc[,1]<-scale(EQ[,1], center = TRUE, scale = TRUE)
EQsc[,2]<-scale(EQ[,2], center = TRUE, scale = TRUE)
EQsc[,3]<-scale(EQ[,3], center = TRUE, scale = TRUE)
EQsc[,4]<-scale(EQ[,4], center = TRUE, scale = TRUE)
EQsc[,5]<-scale(EQ[,5], center = TRUE, scale = TRUE)

diff.matrix.EQ<-EQsc[,2:5]-EQsc[,1]

#Create a data frame with derived variables

EQ_Total<-data.frame("ID" = EQ_wide$ID, "EQ_FU1Diff" = diff.matrix.EQ[,1], "EQ_FU2Diff" = diff.matrix.EQ[,2], "EQ_FU3Diff" = diff.matrix.EQ[,3], "EQ_FU4Diff" = diff.matrix.EQ[,4], "EQ_FU1.z" = EQsc[,2], "EQ_FU2.z" = EQsc[,3], "EQ_FU3.z" = EQsc[,4], "EQ_FU4.z" = EQsc[,5])

### CAREGIVER-RATED BIS ###

CarerBIS<-Imputed[,c("ID","TimepointNum",
                      "ClinicalSubtype", "TremorAkinesiaSubtype", "Gender", "Age", "Signif.Psych",
                      "Signif.Psych.CaseYN", "CarerBIS_Total")]

CarerBIS_wide<-reshape(CarerBIS, idvar = "ID", timevar = "TimepointNum", direction = "wide",
                        drop = c("ClinicalSubtype", "TremorAkinesiaSubtype", "Gender", "Age", "Signif.Psych", "Signif.Psych.CaseYN"))

#Create difference matrix and z-score matrix

CBIS<-CarerBIS_wide[,2:6]
CBISsc<-CBIS
CBISsc[,1]<-scale(CBIS[,1], center = TRUE, scale = TRUE)
CBISsc[,2]<-scale(CBIS[,2], center = TRUE, scale = TRUE)
CBISsc[,3]<-scale(CBIS[,3], center = TRUE, scale = TRUE)
CBISsc[,4]<-scale(CBIS[,4], center = TRUE, scale = TRUE)
CBISsc[,5]<-scale(CBIS[,5], center = TRUE, scale = TRUE)

diff.matrix.CarerBIS<-CBISsc[,2:5]-CBISsc[,1]

#Create a data frame with derived variables

CarerBIS_Total<-data.frame("ID" = CarerBIS_wide$ID, "CarerBIS_FU1Diff" = diff.matrix.CarerBIS[,1], "CarerBIS_FU2Diff" = diff.matrix.CarerBIS[,2], "CarerBIS_FU3Diff" = diff.matrix.CarerBIS[,3], "CarerBIS_FU4Diff" = diff.matrix.CarerBIS[,4], "CarerBIS_FU1.z" = CBISsc[,2], "CarerBIS_FU2.z" = CBISsc[,3], "CarerBIS_FU3.z" = CBISsc[,4], "CarerBIS_FU4.z" = CBISsc[,5])
# 
# 
# 
 ### PATIENT-RATED BIS ###

BIS<-Imputed[,c("ID","TimepointNum",
                 "ClinicalSubtype", "TremorAkinesiaSubtype", "Gender", "Age", "Signif.Psych",
                 "Signif.Psych.CaseYN", "BIS_Total")]

 BIS_wide<-reshape(BIS, idvar = "ID", timevar = "TimepointNum", direction = "wide",
                   drop = c("ClinicalSubtype", "TremorAkinesiaSubtype", "Gender", "Age", "Signif.Psych", "Signif.Psych.CaseYN"))

#Create difference matrix and z-score matrix

BIS<-BIS_wide[,2:6]
BISsc<-BIS
BISsc[,1]<-scale(BIS[,1], center = TRUE, scale = TRUE)
BISsc[,2]<-scale(BIS[,2], center = TRUE, scale = TRUE)
BISsc[,3]<-scale(BIS[,3], center = TRUE, scale = TRUE)
BISsc[,4]<-scale(BIS[,4], center = TRUE, scale = TRUE)
BISsc[,5]<-scale(BIS[,5], center = TRUE, scale = TRUE)

diff.matrix.BIS<-BISsc[,2:5]-BISsc[,1]

#Create a data frame with derived variables

BIS_Total<-data.frame("ID" = BIS_wide$ID, "BIS_FU1Diff" = diff.matrix.BIS[,1], "BIS_FU2Diff" = diff.matrix.BIS[,2], "BIS_FU3Diff" = diff.matrix.BIS[,3], "BIS_FU4Diff" = diff.matrix.BIS[,4], "BIS_FU1.z" = BISsc[,2], "BIS_FU2.z" = BISsc[,3], "BIS_FU3.z" = BISsc[,4], "BIS_FU4.z" = BISsc[,5])



### HAYLING AB ERROR SCORE ###

Hayling<-Imputed[,c("ID","TimepointNum",
                    "ClinicalSubtype", "TremorAkinesiaSubtype", "Gender", "Age", "Signif.Psych",
                    "Signif.Psych.CaseYN", "Hayling_ABErrorScore")]

Hayling_wide<-reshape(Hayling, idvar = "ID", timevar = "TimepointNum", direction = "wide",
                      drop = c("ClinicalSubtype", "TremorAkinesiaSubtype", "Gender", "Age", "Signif.Psych", "Signif.Psych.CaseYN"))

#Create difference matrix and z-score matrix

Hayling<-Hayling_wide[,2:6]
Haylingsc<-Hayling
Haylingsc[,1]<-scale(Hayling[,1], center = TRUE, scale = TRUE)
Haylingsc[,2]<-scale(Hayling[,2], center = TRUE, scale = TRUE)
Haylingsc[,3]<-scale(Hayling[,3], center = TRUE, scale = TRUE)
Haylingsc[,4]<-scale(Hayling[,4], center = TRUE, scale = TRUE)
Haylingsc[,5]<-scale(Hayling[,5], center = TRUE, scale = TRUE)

diff.matrix.Hayling<-Haylingsc[,2:5]-Haylingsc[,1]

#Create a data frame with derived variables

Hayling_ABErrorScore<-data.frame("ID" = Hayling_wide$ID, "Hayling_FU1Diff" = diff.matrix.Hayling[,1], "Hayling_FU2Diff" = diff.matrix.Hayling[,2], "Hayling_FU3Diff" = diff.matrix.Hayling[,3], "Hayling_FU4Diff" = diff.matrix.Hayling[,4], "Hayling_FU1.z" = Haylingsc[,2], "Hayling_FU2.z" = Haylingsc[,3], "Hayling_FU3.z" = Haylingsc[,4], "Hayling_FU4.z" = Haylingsc[,5])



### EXCLUDED LETTER FLUENCY RULE VIOLATIONS ###

ELF<-Imputed[,c("ID","TimepointNum",
                "ClinicalSubtype", "TremorAkinesiaSubtype", "Gender", "Age", "Signif.Psych",
                "Signif.Psych.CaseYN", "ELF_RuleViolations")]

ELF_wide<-reshape(ELF, idvar = "ID", timevar = "TimepointNum", direction = "wide",
                  drop = c("ClinicalSubtype", "TremorAkinesiaSubtype", "Gender", "Age", "Signif.Psych", "Signif.Psych.CaseYN"))

#Create difference matrix and z-score matrix

ELF<-ELF_wide[,2:6]
ELFsc<-ELF
ELFsc[,1]<-scale(ELF[,1], center = TRUE, scale = TRUE)
ELFsc[,2]<-scale(ELF[,2], center = TRUE, scale = TRUE)
ELFsc[,3]<-scale(ELF[,3], center = TRUE, scale = TRUE)
ELFsc[,4]<-scale(ELF[,4], center = TRUE, scale = TRUE)
ELFsc[,5]<-scale(ELF[,5], center = TRUE, scale = TRUE)

diff.matrix.ELF<-ELFsc[,2:5]-ELFsc[,1]

#Create a data frame with derived variables

ELF_RuleViolations<-data.frame("ID" = ELF_wide$ID, "ELF_FU1Diff" = diff.matrix.ELF[,1], "ELF_FU2Diff" = diff.matrix.ELF[,2], "ELF_FU3Diff" = diff.matrix.ELF[,3], "ELF_FU4Diff" = diff.matrix.ELF[,4], "ELF_FU1.z" = ELFsc[,2], "ELF_FU2.z" = ELFsc[,3], "ELF_FU3.z" = ELFsc[,4], "ELF_FU4.z" = ELFsc[,5])



### LEVODOPA EQUIVALENT DAILY DOSE ###

 LEDD<-Imputed[,c("ID","TimepointNum",
                "ClinicalSubtype", "TremorAkinesiaSubtype", "Gender", "Age", "Signif.Psych",
                "Signif.Psych.CaseYN", "LEDD")]

LEDD_wide<-reshape(LEDD, idvar = "ID", timevar = "TimepointNum", direction = "wide",
                     drop = c("ClinicalSubtype", "TremorAkinesiaSubtype", "Gender", "Age", "Signif.Psych", "Signif.Psych.CaseYN"))

#Create difference matrix and z-score matrix

LEDD<-LEDD_wide[,2:6]
LEDDsc<-LEDD
LEDDsc[,1]<-scale(LEDD[,1], center = TRUE, scale = TRUE)
LEDDsc[,2]<-scale(LEDD[,2], center = TRUE, scale = TRUE)
LEDDsc[,3]<-scale(LEDD[,3], center = TRUE, scale = TRUE)
LEDDsc[,4]<-scale(LEDD[,4], center = TRUE, scale = TRUE)
LEDDsc[,5]<-scale(LEDD[,5], center = TRUE, scale = TRUE)

diff.matrix.LEDD<-LEDDsc[,2:5]-LEDDsc[,1]

#Create a data frame with derived variables

LEDD_Total<-data.frame("ID" = LEDD_wide$ID, "LEDD_FU1Diff" = diff.matrix.LEDD[,1], "LEDD_FU2Diff" = diff.matrix.LEDD[,2], "LEDD_FU3Diff" = diff.matrix.LEDD[,3], "LEDD_FU4Diff" = diff.matrix.LEDD[,4], "LEDD_FU1.z" = LEDDsc[,2], "LEDD_FU2.z" = LEDDsc[,3], "LEDD_FU3.z" = LEDDsc[,4], "LEDD_FU4.z" = LEDDsc[,5])



### UPDRS TOTAL ###

UPDRS<-Imputed[,c("ID","TimepointNum",
            "ClinicalSubtype", "TremorAkinesiaSubtype", "Gender", "Age", "Signif.Psych",
            "Signif.Psych.CaseYN", "UPDRS_Total")]

UPDRS_wide<-reshape(UPDRS, idvar = "ID", timevar = "TimepointNum", direction = "wide",
                    drop = c("ClinicalSubtype", "TremorAkinesiaSubtype", "Gender", "Age", "Signif.Psych", "Signif.Psych.CaseYN"))

#Create difference matrix and z-score matrix

UPDRS<-UPDRS_wide[,2:6]
UPDRSsc<-UPDRS
UPDRSsc[,1]<-scale(UPDRS[,1], center = TRUE, scale = TRUE)
UPDRSsc[,2]<-scale(UPDRS[,2], center = TRUE, scale = TRUE)
UPDRSsc[,3]<-scale(UPDRS[,3], center = TRUE, scale = TRUE)
UPDRSsc[,4]<-scale(UPDRS[,4], center = TRUE, scale = TRUE)
UPDRSsc[,5]<-scale(UPDRS[,5], center = TRUE, scale = TRUE)

diff.matrix.UPDRS<-UPDRSsc[,2:5]-UPDRSsc[,1]

#Create a data frame with derived variables

UPDRS_Total<-data.frame("ID" = UPDRS_wide$ID, "UPDRS_FU1Diff" = diff.matrix.UPDRS[,1], "UPDRS_FU2Diff" = diff.matrix.UPDRS[,2], "UPDRS_FU3Diff" = diff.matrix.UPDRS[,3], "UPDRS_FU4Diff" = diff.matrix.UPDRS[,4], "UPDRS_FU1.z" = UPDRSsc[,2], "UPDRS_FU2.z" = UPDRSsc[,3], "UPDRS_FU3.z" = UPDRSsc[,4], "UPDRS_FU4.z" = UPDRSsc[,5])



### UPDRS LEFT (RIGHT STN) ###

UPDRS_Left<-Imputed[,c("ID","TimepointNum",
                    "ClinicalSubtype", "TremorAkinesiaSubtype", "Gender", "Age",
                    "Signif.Psych","Signif.Psych.CaseYN", "UPDRS_Left")]

UPDRS_Left_wide<-reshape(UPDRS_Left, idvar = "ID", timevar = "TimepointNum", direction = "wide",
                        drop = c("ClinicalSubtype", "TremorAkinesiaSubtype", "Gender", "Age", "Signif.Psych", "Signif.Psych.CaseYN"))

#Create difference matrix and z-score matrix

UPDRS_Left<-UPDRS_Left_wide[,2:6]
UPDRS_Leftsc<-UPDRS_Left
UPDRS_Leftsc[,1]<-scale(UPDRS_Left[,1], center = TRUE, scale = TRUE)
UPDRS_Leftsc[,2]<-scale(UPDRS_Left[,2], center = TRUE, scale = TRUE)
UPDRS_Leftsc[,3]<-scale(UPDRS_Left[,3], center = TRUE, scale = TRUE)
UPDRS_Leftsc[,4]<-scale(UPDRS_Left[,4], center = TRUE, scale = TRUE)
UPDRS_Leftsc[,5]<-scale(UPDRS_Left[,5], center = TRUE, scale = TRUE)

diff.matrix.UPDRS_Left<-UPDRS_Leftsc[,2:5]-UPDRS_Leftsc[,1]

#Create a data frame with derived variables

UPDRS_Left_Total<-data.frame("ID" = UPDRS_Left_wide$ID, "UPDRS_Left_FU1Diff" = diff.matrix.UPDRS_Left[,1], "UPDRS_Left_FU2Diff" = diff.matrix.UPDRS_Left[,2], "UPDRS_Left_FU3Diff" = diff.matrix.UPDRS_Left[,3], "UPDRS_Left_FU4Diff" = diff.matrix.UPDRS_Left[,4], "UPDRS_Left_FU1.z" = UPDRS_Leftsc[,2], "UPDRS_Left_FU2.z" = UPDRS_Leftsc[,3], "UPDRS_Left_FU3.z" = UPDRS_Leftsc[,4], "UPDRS_Left_FU4.z" = UPDRS_Leftsc[,5])



### UPDRS RIGHT (LEFT STN) ###

UPDRS_Right<-Imputed[,c("ID","TimepointNum",
            "ClinicalSubtype", "TremorAkinesiaSubtype", "Gender", "Age", "Signif.Psych",
            "Signif.Psych.CaseYN", "UPDRS_Right")]

UPDRS_Right_wide<-reshape(UPDRS_Right, idvar = "ID", timevar = "TimepointNum", direction = "wide",
                        drop = c("ClinicalSubtype", "TremorAkinesiaSubtype", "Gender", "Age", "Signif.Psych", "Signif.Psych.CaseYN"))

#Create difference matrix and z-score matrix

UPDRS_Right<-UPDRS_Right_wide[,2:6]
UPDRS_Rightsc<-UPDRS_Right
UPDRS_Rightsc[,1]<-scale(UPDRS_Right[,1], center = TRUE, scale = TRUE)
UPDRS_Rightsc[,2]<-scale(UPDRS_Right[,2], center = TRUE, scale = TRUE)
UPDRS_Rightsc[,3]<-scale(UPDRS_Right[,3], center = TRUE, scale = TRUE)
UPDRS_Rightsc[,4]<-scale(UPDRS_Right[,4], center = TRUE, scale = TRUE)
UPDRS_Rightsc[,5]<-scale(UPDRS_Right[,5], center = TRUE, scale = TRUE)

diff.matrix.UPDRS_Right<-UPDRS_Rightsc[,2:5]-UPDRS_Rightsc[,1]

#Create a data frame with derived variables

UPDRS_Right_Total<-data.frame("ID" = UPDRS_Right_wide$ID, "UPDRS_Right_FU1Diff" = diff.matrix.UPDRS_Right[,1], "UPDRS_Right_FU2Diff" = diff.matrix.UPDRS_Right[,2], "UPDRS_Right_FU3Diff" = diff.matrix.UPDRS_Right[,3], "UPDRS_Right_FU4Diff" = diff.matrix.UPDRS_Right[,4], "UPDRS_Right_FU1.z" = UPDRS_Rightsc[,2], "UPDRS_Right_FU2.z" = UPDRS_Rightsc[,3], "UPDRS_Right_FU3.z" = UPDRS_Rightsc[,4], "UPDRS_Right_FU4.z" = UPDRS_Rightsc[,5])



### CREATE A SUMMARY CSV ###

Timepoint_Change<-merge(Baseline_Factors, CarerEQ_Total, by="ID")
Timepoint_Change<-merge(Timepoint_Change, EQ_Total, by="ID")
Timepoint_Change<-merge(Timepoint_Change, CarerBIS_Total, by="ID")
Timepoint_Change<-merge(Timepoint_Change, BIS_Total, by="ID")
Timepoint_Change<-merge(Timepoint_Change, Hayling_ABErrorScore, by="ID")
Timepoint_Change<-merge(Timepoint_Change, ELF_RuleViolations, by="ID")
Timepoint_Change<-merge(Timepoint_Change, LEDD_Total, by="ID")
Timepoint_Change<-merge(Timepoint_Change, UPDRS_Total, by="ID")
#Timepoint_Change<-merge(Timepoint_Change, UPDRS_Left_Total, by="ID")
#Timepoint_Change<-merge(Timepoint_Change, UPDRS_Right_Total, by="ID")



### SUBSET APPROPRIATE ANATOMICAL DATA ###

Anatomical_Subset<-anatomical[,-c(2, 3, 8, 11, 14, 17, 20, 21, 24, 27)]

### MERGE ANATOMICAL AND TIMEPOINT CHANGE ###

TimepointPlusAnatomical<-merge(Timepoint_Change, Anatomical_Subset, by="ID")

### TIDY UP NAs FOR ANALYSES ###

#Replace any missing values with the mean of the sample at each timepoint

TimepointPlusAnatomical$CarerEQ_FU1Diff[is.na(TimepointPlusAnatomical$CarerEQ_FU1Diff)] <- mean(TimepointPlusAnatomical$CarerEQ_FU1Diff, na.rm=T)
TimepointPlusAnatomical$CarerEQ_FU2Diff[is.na(TimepointPlusAnatomical$CarerEQ_FU2Diff)] <- mean(TimepointPlusAnatomical$CarerEQ_FU2Diff, na.rm=T)
TimepointPlusAnatomical$CarerEQ_FU3Diff[is.na(TimepointPlusAnatomical$CarerEQ_FU3Diff)] <- mean(TimepointPlusAnatomical$CarerEQ_FU3Diff, na.rm=T)
TimepointPlusAnatomical$CarerEQ_FU4Diff[is.na(TimepointPlusAnatomical$CarerEQ_FU4Diff)] <- mean(TimepointPlusAnatomical$CarerEQ_FU4Diff, na.rm=T)

TimepointPlusAnatomical$CarerEQ_FU1.z[is.na(TimepointPlusAnatomical$CarerEQ_FU1.z)] <- mean(TimepointPlusAnatomical$CarerEQ_FU1.z, na.rm=T)
TimepointPlusAnatomical$CarerEQ_FU2.z[is.na(TimepointPlusAnatomical$CarerEQ_FU2.z)] <- mean(TimepointPlusAnatomical$CarerEQ_FU2.z, na.rm=T)
TimepointPlusAnatomical$CarerEQ_FU3.z[is.na(TimepointPlusAnatomical$CarerEQ_FU3.z)] <- mean(TimepointPlusAnatomical$CarerEQ_FU3.z, na.rm=T)
TimepointPlusAnatomical$CarerEQ_FU4.z[is.na(TimepointPlusAnatomical$CarerEQ_FU4.z)] <- mean(TimepointPlusAnatomical$CarerEQ_FU4.z, na.rm=T)

TimepointPlusAnatomical$EQ_FU1Diff[is.na(TimepointPlusAnatomical$EQ_FU1Diff)] <- mean(TimepointPlusAnatomical$EQ_FU1Diff, na.rm=T)
TimepointPlusAnatomical$EQ_FU2Diff[is.na(TimepointPlusAnatomical$EQ_FU2Diff)] <- mean(TimepointPlusAnatomical$EQ_FU2Diff, na.rm=T)
TimepointPlusAnatomical$EQ_FU3Diff[is.na(TimepointPlusAnatomical$EQ_FU3Diff)] <- mean(TimepointPlusAnatomical$EQ_FU3Diff, na.rm=T)
TimepointPlusAnatomical$EQ_FU4Diff[is.na(TimepointPlusAnatomical$EQ_FU4Diff)] <- mean(TimepointPlusAnatomical$EQ_FU4Diff, na.rm=T)

TimepointPlusAnatomical$EQ_FU1.z[is.na(TimepointPlusAnatomical$EQ_FU1.z)] <- mean(TimepointPlusAnatomical$EQ_FU1.z, na.rm=T)
TimepointPlusAnatomical$EQ_FU2.z[is.na(TimepointPlusAnatomical$EQ_FU2.z)] <- mean(TimepointPlusAnatomical$EQ_FU2.z, na.rm=T)
TimepointPlusAnatomical$EQ_FU3.z[is.na(TimepointPlusAnatomical$EQ_FU3.z)] <- mean(TimepointPlusAnatomical$EQ_FU3.z, na.rm=T)
TimepointPlusAnatomical$EQ_FU4.z[is.na(TimepointPlusAnatomical$EQ_FU4.z)] <- mean(TimepointPlusAnatomical$EQ_FU4.z, na.rm=T)

TimepointPlusAnatomical$CarerBIS_FU1Diff[is.na(TimepointPlusAnatomical$CarerBIS_FU1Diff)] <- mean(TimepointPlusAnatomical$CarerBIS_FU1Diff, na.rm=T)
TimepointPlusAnatomical$CarerBIS_FU2Diff[is.na(TimepointPlusAnatomical$CarerBIS_FU2Diff)] <- mean(TimepointPlusAnatomical$CarerBIS_FU2Diff, na.rm=T)
TimepointPlusAnatomical$CarerBIS_FU3Diff[is.na(TimepointPlusAnatomical$CarerBIS_FU3Diff)] <- mean(TimepointPlusAnatomical$CarerBIS_FU3Diff, na.rm=T)
TimepointPlusAnatomical$CarerBIS_FU4Diff[is.na(TimepointPlusAnatomical$CarerBIS_FU4Diff)] <- mean(TimepointPlusAnatomical$CarerBIS_FU4Diff, na.rm=T)

TimepointPlusAnatomical$CarerBIS_FU1.z[is.na(TimepointPlusAnatomical$CarerBIS_FU1.z)] <- mean(TimepointPlusAnatomical$CarerBIS_FU1.z, na.rm=T)
TimepointPlusAnatomical$CarerBIS_FU2.z[is.na(TimepointPlusAnatomical$CarerBIS_FU2.z)] <- mean(TimepointPlusAnatomical$CarerBIS_FU2.z, na.rm=T)
TimepointPlusAnatomical$CarerBIS_FU3.z[is.na(TimepointPlusAnatomical$CarerBIS_FU3.z)] <- mean(TimepointPlusAnatomical$CarerBIS_FU3.z, na.rm=T)
TimepointPlusAnatomical$CarerBIS_FU4.z[is.na(TimepointPlusAnatomical$CarerBIS_FU4.z)] <- mean(TimepointPlusAnatomical$CarerBIS_FU4.z, na.rm=T)

TimepointPlusAnatomical$BIS_FU1Diff[is.na(TimepointPlusAnatomical$BIS_FU1Diff)] <- mean(TimepointPlusAnatomical$BIS_FU1Diff, na.rm=T)
TimepointPlusAnatomical$BIS_FU2Diff[is.na(TimepointPlusAnatomical$BIS_FU2Diff)] <- mean(TimepointPlusAnatomical$BIS_FU2Diff, na.rm=T)
TimepointPlusAnatomical$BIS_FU3Diff[is.na(TimepointPlusAnatomical$BIS_FU3Diff)] <- mean(TimepointPlusAnatomical$BIS_FU3Diff, na.rm=T)
TimepointPlusAnatomical$BIS_FU4Diff[is.na(TimepointPlusAnatomical$BIS_FU4Diff)] <- mean(TimepointPlusAnatomical$BIS_FU4Diff, na.rm=T)

TimepointPlusAnatomical$BIS_FU1.z[is.na(TimepointPlusAnatomical$BIS_FU1.z)] <- mean(TimepointPlusAnatomical$BIS_FU1.z, na.rm=T)
TimepointPlusAnatomical$BIS_FU2.z[is.na(TimepointPlusAnatomical$BIS_FU2.z)] <- mean(TimepointPlusAnatomical$BIS_FU2.z, na.rm=T)
TimepointPlusAnatomical$BIS_FU3.z[is.na(TimepointPlusAnatomical$BIS_FU3.z)] <- mean(TimepointPlusAnatomical$BIS_FU3.z, na.rm=T)
TimepointPlusAnatomical$BIS_FU4.z[is.na(TimepointPlusAnatomical$BIS_FU4.z)] <- mean(TimepointPlusAnatomical$BIS_FU4.z, na.rm=T)

TimepointPlusAnatomical$Hayling_FU1Diff[is.na(TimepointPlusAnatomical$Hayling_FU1Diff)] <- mean(TimepointPlusAnatomical$Hayling_FU1Diff, na.rm=T)
TimepointPlusAnatomical$Hayling_FU2Diff[is.na(TimepointPlusAnatomical$Hayling_FU2Diff)] <- mean(TimepointPlusAnatomical$Hayling_FU2Diff, na.rm=T)
TimepointPlusAnatomical$Hayling_FU3Diff[is.na(TimepointPlusAnatomical$Hayling_FU3Diff)] <- mean(TimepointPlusAnatomical$Hayling_FU3Diff, na.rm=T)
TimepointPlusAnatomical$Hayling_FU4Diff[is.na(TimepointPlusAnatomical$Hayling_FU4Diff)] <- mean(TimepointPlusAnatomical$Hayling_FU4Diff, na.rm=T)

TimepointPlusAnatomical$Hayling_FU1.z[is.na(TimepointPlusAnatomical$Hayling_FU1.z)] <- mean(TimepointPlusAnatomical$Hayling_FU1.z, na.rm=T)
TimepointPlusAnatomical$Hayling_FU2.z[is.na(TimepointPlusAnatomical$Hayling_FU2.z)] <- mean(TimepointPlusAnatomical$Hayling_FU2.z, na.rm=T)
TimepointPlusAnatomical$Hayling_FU3.z[is.na(TimepointPlusAnatomical$Hayling_FU3.z)] <- mean(TimepointPlusAnatomical$Hayling_FU3.z, na.rm=T)
TimepointPlusAnatomical$Hayling_FU4.z[is.na(TimepointPlusAnatomical$Hayling_FU4.z)] <- mean(TimepointPlusAnatomical$Hayling_FU4.z, na.rm=T)

TimepointPlusAnatomical$ELF_FU1Diff[is.na(TimepointPlusAnatomical$ELF_FU1Diff)] <- mean(TimepointPlusAnatomical$ELF_FU1Diff, na.rm=T)
TimepointPlusAnatomical$ELF_FU2Diff[is.na(TimepointPlusAnatomical$ELF_FU2Diff)] <- mean(TimepointPlusAnatomical$ELF_FU2Diff, na.rm=T)
TimepointPlusAnatomical$ELF_FU3Diff[is.na(TimepointPlusAnatomical$ELF_FU3Diff)] <- mean(TimepointPlusAnatomical$ELF_FU3Diff, na.rm=T)
TimepointPlusAnatomical$ELF_FU4Diff[is.na(TimepointPlusAnatomical$ELF_FU4Diff)] <- mean(TimepointPlusAnatomical$ELF_FU4Diff, na.rm=T)

TimepointPlusAnatomical$ELF_FU1.z[is.na(TimepointPlusAnatomical$ELF_FU1.z)] <- mean(TimepointPlusAnatomical$ELF_FU1.z, na.rm=T)
TimepointPlusAnatomical$ELF_FU2.z[is.na(TimepointPlusAnatomical$ELF_FU2.z)] <- mean(TimepointPlusAnatomical$ELF_FU2.z, na.rm=T)
TimepointPlusAnatomical$ELF_FU3.z[is.na(TimepointPlusAnatomical$ELF_FU3.z)] <- mean(TimepointPlusAnatomical$ELF_FU3.z, na.rm=T)
TimepointPlusAnatomical$ELF_FU4.z[is.na(TimepointPlusAnatomical$ELF_FU4.z)] <- mean(TimepointPlusAnatomical$ELF_FU4.z, na.rm=T)

TimepointPlusAnatomical$UPDRS_FU1Diff[is.na(TimepointPlusAnatomical$UPDRS_FU1Diff)] <- mean(TimepointPlusAnatomical$UPDRS_FU1Diff, na.rm=T)
TimepointPlusAnatomical$UPDRS_FU2Diff[is.na(TimepointPlusAnatomical$UPDRS_FU2Diff)] <- mean(TimepointPlusAnatomical$UPDRS_FU2Diff, na.rm=T)
TimepointPlusAnatomical$UPDRS_FU3Diff[is.na(TimepointPlusAnatomical$UPDRS_FU3Diff)] <- mean(TimepointPlusAnatomical$UPDRS_FU3Diff, na.rm=T)
TimepointPlusAnatomical$UPDRS_FU4Diff[is.na(TimepointPlusAnatomical$UPDRS_FU4Diff)] <- mean(TimepointPlusAnatomical$UPDRS_FU4Diff, na.rm=T)

TimepointPlusAnatomical$UPDRS_FU1.z[is.na(TimepointPlusAnatomical$UPDRS_FU1.z)] <- mean(TimepointPlusAnatomical$UPDRS_FU1.z, na.rm=T)
TimepointPlusAnatomical$UPDRS_FU2.z[is.na(TimepointPlusAnatomical$UPDRS_FU2.z)] <- mean(TimepointPlusAnatomical$UPDRS_FU2.z, na.rm=T)
TimepointPlusAnatomical$UPDRS_FU3.z[is.na(TimepointPlusAnatomical$UPDRS_FU3.z)] <- mean(TimepointPlusAnatomical$UPDRS_FU3.z, na.rm=T)
TimepointPlusAnatomical$UPDRS_FU4.z[is.na(TimepointPlusAnatomical$UPDRS_FU4.z)] <- mean(TimepointPlusAnatomical$UPDRS_FU4.z, na.rm=T)

TimepointPlusAnatomical$LEDD_FU1Diff[is.na(TimepointPlusAnatomical$LEDD_FU1Diff)] <- mean(TimepointPlusAnatomical$LEDD_FU1Diff, na.rm=T)
TimepointPlusAnatomical$LEDD_FU2Diff[is.na(TimepointPlusAnatomical$LEDD_FU2Diff)] <- mean(TimepointPlusAnatomical$LEDD_FU2Diff, na.rm=T)
TimepointPlusAnatomical$LEDD_FU3Diff[is.na(TimepointPlusAnatomical$LEDD_FU3Diff)] <- mean(TimepointPlusAnatomical$LEDD_FU3Diff, na.rm=T)
TimepointPlusAnatomical$LEDD_FU4Diff[is.na(TimepointPlusAnatomical$LEDD_FU4Diff)] <- mean(TimepointPlusAnatomical$LEDD_FU4Diff, na.rm=T)

TimepointPlusAnatomical$LEDD_FU1.z[is.na(TimepointPlusAnatomical$LEDD_FU1.z)] <- mean(TimepointPlusAnatomical$LEDD_FU1.z, na.rm=T)
TimepointPlusAnatomical$LEDD_FU2.z[is.na(TimepointPlusAnatomical$LEDD_FU2.z)] <- mean(TimepointPlusAnatomical$LEDD_FU2.z, na.rm=T)
TimepointPlusAnatomical$LEDD_FU3.z[is.na(TimepointPlusAnatomical$LEDD_FU3.z)] <- mean(TimepointPlusAnatomical$LEDD_FU3.z, na.rm=T)
TimepointPlusAnatomical$LEDD_FU4.z[is.na(TimepointPlusAnatomical$LEDD_FU4.z)] <- mean(TimepointPlusAnatomical$LEDD_FU4.z, na.rm=T)

### WRITE OUT CSV ###

write.csv(TimepointPlusAnatomical, file = "TimepointPlusAnatomical.csv", row.names = FALSE)
