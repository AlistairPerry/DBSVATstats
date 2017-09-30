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
anatomical <- read_excel("~/Dropbox/Statistical Analyses Parkinsons Disease and DBS/Data sets/08 2017 DATA SET/Anatomical_Longitudinal.xlsx")

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

diff.matrix.CarerEQ<-CarerEQsc[,2:4]-CarerEQsc[,1]

#z.matrix<-data.frame(scale(diff.matrix[,1:4], center = TRUE, scale = TRUE))

#Create a data frame with derived variables

CarerEQ_Total<-data.frame("ID" = CarerEQ_wide$ID, "CarerEQ_FU1Diff" = diff.matrix.CarerEQ[,1], "CarerEQ_FU2Diff" = diff.matrix.CarerEQ[,2], "CarerEQ_FU3Diff" = diff.matrix.CarerEQ[,3], "CarerEQ_FU4Diff" = diff.matrix.CarerEQ[,4], "CarerEQ_FU1.z" = CarerEQsc[,1], "CarerEQ_FU2.z" = CarerEQsc[,2], "CarerEQ_FU3.z" = CarerEQsc[,3], "CarerEQ_FU4.z" = CarerEQsc[,4])


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

#z.matrix<-data.frame(scale(diff.matrix[,1:4], center = TRUE, scale = TRUE))

#Create a data frame with derived variables

EQ_Total<-data.frame("ID" = EQ_wide$ID, "EQ_FU1Diff" = diff.matrix.EQ[,1], "EQ_FU2Diff" = diff.matrix.EQ[,2], "EQ_FU3Diff" = diff.matrix.EQ[,3], "EQ_FU4Diff" = diff.matrix.EQ[,4], "EQ_FU1.z" = EQsc[,1], "EQ_FU2.z" = EQsc[,2], "EQ_FU3.z" = EQsc[,3], "EQ_FU4.z" = EQsc[,4])

# ### CAREGIVER-RATED BIS ###
# 
# CarerBIS<-Imputed[,c("ID","TimepointNum",
#                      "ClinicalSubtype", "TremorAkinesiaSubtype", "Gender", "Age", "Signif.Psych",
#                      "Signif.Psych.CaseYN", "CarerBIS_Total")]
# 
# CarerBIS_wide<-reshape(CarerBIS, idvar = "ID", timevar = "TimepointNum", direction = "wide",
#                        drop = c("ClinicalSubtype", "TremorAkinesiaSubtype", "Gender", "Age", "Signif.Psych", "Signif.Psych.CaseYN"))
# 
# #Create difference matrix and z-score matrix
# 
# diff.matrix<-CarerBIS_wide[,3:6]-CarerBIS_wide[,2]
# z.matrix<-data.frame(scale(diff.matrix[,1:4], center = TRUE, scale = TRUE))
# 
# #Create a data frame with derived variables
# 
# Caregiver_BIS<-data.frame("ID" = CarerBIS_wide$ID, "CarerBIS_FU1Diff" = diff.matrix$CarerBIS_Total.2, "CarerBIS_FU2Diff" = diff.matrix$CarerBIS_Total.6, "CarerBIS_FU3Diff" = diff.matrix$CarerBIS_Total.13, "CarerBIS_FU4Diff" = diff.matrix$CarerBIS_Total.26, "CarerBIS_FU1.z" = z.matrix$CarerBIS_Total.2, "CarerBIS_FU2.z" = z.matrix$CarerBIS_Total.6, "CarerBIS_FU3.z" = z.matrix$CarerBIS_Total.13, "CarerBIS_FU4.z" = z.matrix$CarerBIS_Total.26)
# 
# 
# 
# ### PATIENT-RATED BIS ###
# 
# BIS<-Imputed[,c("ID","TimepointNum",
#                 "ClinicalSubtype", "TremorAkinesiaSubtype", "Gender", "Age", "Signif.Psych",
#                 "Signif.Psych.CaseYN", "BIS_Total")]
# 
# BIS_wide<-reshape(BIS, idvar = "ID", timevar = "TimepointNum", direction = "wide",
#                   drop = c("ClinicalSubtype", "TremorAkinesiaSubtype", "Gender", "Age", "Signif.Psych", "Signif.Psych.CaseYN"))
# 
# #Create difference matrix and z-score matrix
# 
# diff.matrix<-BIS_wide[,3:6]-BIS_wide[,2]
# z.matrix<-data.frame(scale(diff.matrix[,1:4], center = TRUE, scale = TRUE))
# 
# #Create a data frame with derived variables
# 
# Patient_BIS<-data.frame("ID" = BIS_wide$ID, "BIS_FU1Diff" = diff.matrix$BIS_Total.2, "BIS_FU2Diff" = diff.matrix$BIS_Total.6, "BIS_FU3Diff" = diff.matrix$BIS_Total.13, "BIS_FU4Diff" = diff.matrix$BIS_Total.26, "BIS_FU1.z" = z.matrix$BIS_Total.2, "BIS_FU2.z" = z.matrix$BIS_Total.6, "BIS_FU3.z" = z.matrix$BIS_Total.13, "BIS_FU4.z" = z.matrix$BIS_Total.26)


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

#z.matrix<-data.frame(scale(diff.matrix[,1:4], center = TRUE, scale = TRUE))

#Create a data frame with derived variables

Hayling_ABErrorScore<-data.frame("ID" = Hayling_wide$ID, "Hayling_FU1Diff" = diff.matrix.Hayling[,1], "Hayling_FU2Diff" = diff.matrix.Hayling[,2], "Hayling_FU3Diff" = diff.matrix.Hayling[,3], "Hayling_FU4Diff" = diff.matrix.Hayling[,4], "Hayling_FU1.z" = Haylingsc[,1], "Hayling_FU2.z" = Haylingsc[,2], "Hayling_FU3.z" = Haylingsc[,3], "Hayling_FU4.z" = Haylingsc[,4])

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

#z.matrix<-data.frame(scale(diff.matrix[,1:4], center = TRUE, scale = TRUE))

#Create a data frame with derived variables

ELF_RuleViolations<-data.frame("ID" = ELF_wide$ID, "ELF_FU1Diff" = diff.matrix.ELF[,1], "ELF_FU2Diff" = diff.matrix.ELF[,2], "ELF_FU3Diff" = diff.matrix.ELF[,3], "ELF_FU4Diff" = diff.matrix.ELF[,4], "ELF_FU1.z" = ELFsc[,1], "ELF_FU2.z" = ELFsc[,2], "ELF_FU3.z" = ELFsc[,3], "ELF_FU4.z" = ELFsc[,4])

### LEVODOPA EQUIVALENT DAILY DOSE ###

# LEDD<-Imputed[,c("ID","TimepointNum",
# "ClinicalSubtype", "TremorAkinesiaSubtype", "Gender", "Age", "Signif.Psych",
# "Signif.Psych.CaseYN", "LEDD")]
# 
# LEDD_wide<-reshape(LEDD, idvar = "ID", timevar = "TimepointNum", direction = "wide",
# drop = c("ClinicalSubtype", "TremorAkinesiaSubtype", "Gender", "Age", "Signif.Psych", "Signif.Psych.CaseYN"))
# 
# #Create difference matrix and z-score matrix
# 
# diff.matrix<-log1p(LEDD_wide[,3:6])-log1p(LEDD_wide[,2])
# 
# #Calculate the z-scores on the un-transformed data
# diff.matrix.nolog<-LEDD_wide[,3:6]-LEDD_wide[,2]
# z.matrix<-data.frame(scale(diff.matrix.nolog[,1:4], center = TRUE, scale = TRUE))
# 
# #Create a data frame with derived variables
# 
# Drug<-data.frame("ID" = LEDD_wide$ID, "LEDD_FU1Diff" = diff.matrix$LEDD.2, "LEDD_FU2Diff" = diff.matrix$LEDD.6, "LEDD_FU3Diff" = diff.matrix$LEDD.13, "LEDD_FU4Diff" = diff.matrix$LEDD.26, "LEDD_FU1.z" = z.matrix$LEDD.2, "LEDD_FU2.z" = z.matrix$LEDD.6, "LEDD_FU3.z" = z.matrix$LEDD.13, "LEDD_FU4.z" = z.matrix$LEDD.26)



### UPDRS TOTAL ###

# UPDRS<-Imputed[,c("ID","TimepointNum",
# "ClinicalSubtype", "TremorAkinesiaSubtype", "Gender", "Age", "Signif.Psych",
# "Signif.Psych.CaseYN", "UPDRS_Total")]
# 
# UPDRS_wide<-reshape(UPDRS, idvar = "ID", timevar = "TimepointNum", direction = "wide",
# drop = c("ClinicalSubtype", "TremorAkinesiaSubtype", "Gender", "Age", "Signif.Psych", "Signif.Psych.CaseYN"))
# 
# #Create difference matrix and z-score matrix
# 
# diff.matrix<-UPDRS_wide[,3:6]-UPDRS_wide[,2]
# z.matrix<-data.frame(scale(diff.matrix[,1:4], center = TRUE, scale = TRUE))
# 
# #Create a data frame with derived variables
# 
# UPDRS_Total<-data.frame("ID" = UPDRS_wide$ID, "UPDRS_FU1Diff" = diff.matrix$UPDRS_Total.2, "UPDRS_FU2Diff" = diff.matrix$UPDRS_Total.6, "UPDRS_FU3Diff" = diff.matrix$UPDRS_Total.13, "UPDRS_FU4Diff" = diff.matrix$UPDRS_Total.26, "UPDRS_FU1.z" = z.matrix$UPDRS_Total.2, "UPDRS_FU2.z" = z.matrix$UPDRS_Total.6, "UPDRS_FU3.z" = z.matrix$UPDRS_Total.13, "UPDRS_FU4.z" = z.matrix$UPDRS_Total.26)


### UPDRS LEFT (RIGHT STN) ###

#UPDRS_Left<-Imputed[,c("ID","TimepointNum","ClinicalSubtype", "TremorAkinesiaSubtype", "Gender", "Age", "Signif.Psych","Signif.Psych.CaseYN", "UPDRS_Left")]

#UPDRS_Left_wide<-reshape(UPDRS_Left, idvar = "ID", timevar = "TimepointNum", direction = "wide",drop = c("ClinicalSubtype", "TremorAkinesiaSubtype", "Gender", "Age", "Signif.Psych", "Signif.Psych.CaseYN"))

#Create difference matrix and z-score matrix

#diff.matrix<-UPDRS_Left_wide[,3:6]-UPDRS_Left_wide[,2]
#z.matrix<-data.frame(scale(diff.matrix[,1:4], center = TRUE, scale = TRUE))

#Create a data frame with derived variables
#Recreate wide data frame without dropping clinical & demographic data
#UPDRS_Left_wide<-reshape(UPDRS_Left, idvar = "ID", timevar = "TimepointNum", direction = "wide")

#UPDRS_Left<-data.frame("ID" = UPDRS_Left_wide$ID, "ClinicalSubtype" = UPDRS_Left_wide$ClinicalSubtype.0, "TremorAkinesiaSubtype" = UPDRS_Left_wide$TremorAkinesiaSubtype.0, "Gender" = UPDRS_Left_wide$Gender.0, "Signif.Psych" = UPDRS_Left_wide$Signif.Psych.0, "Signif.Psych.CaseYN" = UPDRS_Left_wide$Signif.Psych.CaseYN.0,
#"UPDRS_Left_FU1Diff" = diff.matrix$UPDRS_Left.2, "UPDRS_Left_FU2Diff" = diff.matrix$UPDRS_Left.6, "UPDRS_Left_FU3Diff" = diff.matrix$UPDRS_Left.13, "UPDRS_Left_FU4Diff" = diff.matrix$UPDRS_Left.26, "UPDRS_Left_FU1.z" = z.matrix$UPDRS_Left.2, "UPDRS_Left_FU2.z" = z.matrix$UPDRS_Left.6, "UPDRS_Left_FU3.z" = z.matrix$UPDRS_Left.13, "UPDRS_Left_FU4.z" = z.matrix$UPDRS_Left.26)



### UPDRS RIGHT (LEFT STN) ###

#UPDRS_Right<-Imputed[,c("ID","TimepointNum",
#"ClinicalSubtype", "TremorAkinesiaSubtype", "Gender", "Age", "Signif.Psych",
#"Signif.Psych.CaseYN", "UPDRS_Right")]

#UPDRS_Right_wide<-reshape(UPDRS_Right, idvar = "ID", timevar = "TimepointNum", direction = "wide",
#drop = c("ClinicalSubtype", "TremorAkinesiaSubtype", "Gender", "Age", "Signif.Psych", "Signif.Psych.CaseYN"))

#Create difference matrix and z-score matrix

#diff.matrix<-UPDRS_Right_wide[,3:6]-UPDRS_Right_wide[,2]
#z.matrix<-data.frame(scale(diff.matrix[,1:4], center = TRUE, scale = TRUE))

#Create a data frame with derived variables
#Recreate wide data frame without dropping clinical & demographic data
#UPDRS_Right_wide<-reshape(UPDRS_Right, idvar = "ID", timevar = "TimepointNum", direction = "wide")

#UPDRS_Right<-data.frame("ID" = UPDRS_Right_wide$ID, "ClinicalSubtype" = UPDRS_Right_wide$ClinicalSubtype.0, "TremorAkinesiaSubtype" = UPDRS_Right_wide$TremorAkinesiaSubtype.0, "Gender" = UPDRS_Right_wide$Gender.0, "Signif.Psych" = UPDRS_Right_wide$Signif.Psych.0, "Signif.Psych.CaseYN" = UPDRS_Right_wide$Signif.Psych.CaseYN.0,
#"UPDRS_Right_FU1Diff" = diff.matrix$UPDRS_Right.2, "UPDRS_Right_FU2Diff" = diff.matrix$UPDRS_Right.6, "UPDRS_Right_FU3Diff" = diff.matrix$UPDRS_Right.13, "UPDRS_Right_FU4Diff" = diff.matrix$UPDRS_Right.26, "UPDRS_Right_FU1.z" = z.matrix$UPDRS_Right.2, "UPDRS_Right_FU2.z" = z.matrix$UPDRS_Right.6, "UPDRS_Right_FU3.z" = z.matrix$UPDRS_Right.13, "UPDRS_Right_FU4.z" = z.matrix$UPDRS_Right.26)



### CREATE A SUMMARY CSV ###

a<-merge(Baseline_Factors, Caregiver_EQ, by="ID")
b<-merge(a, Patient_EQ, by="ID")
c<-merge(b, Caregiver_BIS, by="ID")
d<-merge(c, Patient_BIS, by="ID")
e<-merge(d, Hayling_ABErrorScore, by="ID")
Timepoint_Change<-merge(e, ELF_RuleViolations, by="ID")
#Timepoint_Change<-merge(f, Drug, by="ID")
#h<-merge(g, UPDRS_Total, by="ID")
#i<-merge(h, UPDRS_Left, by="ID")
#Timepoint_Change<-merge(i, UPDRS_Right, by="ID")



### SUBSET APPROPRIATE ANATOMICAL DATA ###

Anatomical_Subset<-anatomical[,-c(2, 3, 8, 11, 14, 17, 20, 21, 24, 27)]

### MERGE ANATOMICAL AND TIMEPOINT CHANGE ###

TimepointPlusAnatomical<-merge(Timepoint_Change, Anatomical_Subset, by="ID")
write.csv(TimepointPlusAnatomical, file = "TimepointPlusAnatomical.csv", row.names = FALSE)

### Merge with Alistairs
TimepointPlusAnatomical2<-merge(Timepoint_Change, anatomical2, by="ID")


