---
title: "Phil Mosley PD subjects: Calculate Timepoint Change Across Longitudinal Variables"
author: "Phil Mosley"
date: "24 September 2017"
output: html_document
---

library(ggplot2)

setwd("/Users/philipm/Dropbox/Philip/Professional/Statistical Analyses Parkinsons Disease and DBS/Data sets/08 2017 DATA SET/")

#read in imputed data set

Imputed<-read.csv("stacked_long_unified_with_cart_imputation.csv")

#Remove ID 33 as this patient has no anatomical data (no MRI due to pacemaker in situ)
Imputed<-Imputed[Imputed$ID!=33,]
anatomical<-anatomical[anatomcial$ID!=33,]

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

diff.matrix<-CarerEQ_wide[,3:6]-CarerEQ_wide[,2]
z.matrix<-data.frame(scale(diff.matrix[,1:4], center = TRUE, scale = TRUE))

#Create a data frame with derived variables

Caregiver_EQ<-data.frame("ID" = CarerEQ_wide$ID, "CarerEQ_FU1Diff" = diff.matrix$CarerEQ_Total.2, "CarerEQ_FU2Diff" = diff.matrix$CarerEQ_Total.6, "CarerEQ_FU3Diff" = diff.matrix$CarerEQ_Total.13, "CarerEQ_FU4Diff" = diff.matrix$CarerEQ_Total.26, "CarerEQ_FU1.z" = z.matrix$CarerEQ_Total.2, "CarerEQ_FU2.z" = z.matrix$CarerEQ_Total.6, "CarerEQ_FU3.z" = z.matrix$CarerEQ_Total.13, "CarerEQ_FU4.z" = z.matrix$CarerEQ_Total.26)



### PATIENT-RATED EQ ###

EQ<-Imputed[,c("ID","TimepointNum",
"ClinicalSubtype", "TremorAkinesiaSubtype", "Gender", "Age", "Signif.Psych",
"Signif.Psych.CaseYN", "EQ_Total")]

EQ_wide<-reshape(EQ, idvar = "ID", timevar = "TimepointNum", direction = "wide",
drop = c("ClinicalSubtype", "TremorAkinesiaSubtype", "Gender", "Age", "Signif.Psych", "Signif.Psych.CaseYN"))

#Create difference matrix and z-score matrix

diff.matrix<-EQ_wide[,3:6]-EQ_wide[,2]
z.matrix<-data.frame(scale(diff.matrix[,1:4], center = TRUE, scale = TRUE))

#Create a data frame with derived variables

Patient_EQ<-data.frame("ID" = EQ_wide$ID, "EQ_FU1Diff" = diff.matrix$EQ_Total.2, "EQ_FU2Diff" = diff.matrix$EQ_Total.6, "EQ_FU3Diff" = diff.matrix$EQ_Total.13, "EQ_FU4Diff" = diff.matrix$EQ_Total.26, "EQ_FU1.z" = z.matrix$EQ_Total.2, "EQ_FU2.z" = z.matrix$EQ_Total.6, "EQ_FU3.z" = z.matrix$EQ_Total.13, "EQ_FU4.z" = z.matrix$EQ_Total.26)



### CAREGIVER-RATED BIS ###

CarerBIS<-Imputed[,c("ID","TimepointNum",
"ClinicalSubtype", "TremorAkinesiaSubtype", "Gender", "Age", "Signif.Psych",
"Signif.Psych.CaseYN", "CarerBIS_Total")]

CarerBIS_wide<-reshape(CarerBIS, idvar = "ID", timevar = "TimepointNum", direction = "wide",
drop = c("ClinicalSubtype", "TremorAkinesiaSubtype", "Gender", "Age", "Signif.Psych", "Signif.Psych.CaseYN"))

#Create difference matrix and z-score matrix

diff.matrix<-CarerBIS_wide[,3:6]-CarerBIS_wide[,2]
z.matrix<-data.frame(scale(diff.matrix[,1:4], center = TRUE, scale = TRUE))

#Create a data frame with derived variables

Caregiver_BIS<-data.frame("ID" = CarerBIS_wide$ID, "CarerBIS_FU1Diff" = diff.matrix$CarerBIS_Total.2, "CarerBIS_FU2Diff" = diff.matrix$CarerBIS_Total.6, "CarerBIS_FU3Diff" = diff.matrix$CarerBIS_Total.13, "CarerBIS_FU4Diff" = diff.matrix$CarerBIS_Total.26, "CarerBIS_FU1.z" = z.matrix$CarerBIS_Total.2, "CarerBIS_FU2.z" = z.matrix$CarerBIS_Total.6, "CarerBIS_FU3.z" = z.matrix$CarerBIS_Total.13, "CarerBIS_FU4.z" = z.matrix$CarerBIS_Total.26)



### PATIENT-RATED BIS ###

BIS<-Imputed[,c("ID","TimepointNum",
"ClinicalSubtype", "TremorAkinesiaSubtype", "Gender", "Age", "Signif.Psych",
"Signif.Psych.CaseYN", "BIS_Total")]

BIS_wide<-reshape(BIS, idvar = "ID", timevar = "TimepointNum", direction = "wide",
drop = c("ClinicalSubtype", "TremorAkinesiaSubtype", "Gender", "Age", "Signif.Psych", "Signif.Psych.CaseYN"))

#Create difference matrix and z-score matrix

diff.matrix<-BIS_wide[,3:6]-BIS_wide[,2]
z.matrix<-data.frame(scale(diff.matrix[,1:4], center = TRUE, scale = TRUE))

#Create a data frame with derived variables

Patient_BIS<-data.frame("ID" = BIS_wide$ID, "BIS_FU1Diff" = diff.matrix$BIS_Total.2, "BIS_FU2Diff" = diff.matrix$BIS_Total.6, "BIS_FU3Diff" = diff.matrix$BIS_Total.13, "BIS_FU4Diff" = diff.matrix$BIS_Total.26, "BIS_FU1.z" = z.matrix$BIS_Total.2, "BIS_FU2.z" = z.matrix$BIS_Total.6, "BIS_FU3.z" = z.matrix$BIS_Total.13, "BIS_FU4.z" = z.matrix$BIS_Total.26)



### HAYLING AB ERROR SCORE ###

Hayling<-Imputed[,c("ID","TimepointNum",
"ClinicalSubtype", "TremorAkinesiaSubtype", "Gender", "Age", "Signif.Psych",
"Signif.Psych.CaseYN", "Hayling_ABErrorScore")]

Hayling_wide<-reshape(Hayling, idvar = "ID", timevar = "TimepointNum", direction = "wide",
drop = c("ClinicalSubtype", "TremorAkinesiaSubtype", "Gender", "Age", "Signif.Psych", "Signif.Psych.CaseYN"))

#Create difference matrix and z-score matrix

diff.matrix<-log1p(Hayling_wide[,3:6])-log1p(Hayling_wide[,2])

#Calculate the z-scores on the un-transformed data
diff.matrix.nolog<-Hayling_wide[,3:6]-Hayling_wide[,2]
z.matrix<-data.frame(scale(diff.matrix.nolog[,1:4], center = TRUE, scale = TRUE))

#Create a data frame with derived variables

Hayling_ABErrorScore<-data.frame("ID" = Hayling_wide$ID, "Hayling_FU1Diff" = diff.matrix$Hayling_ABErrorScore.2, "Hayling_FU2Diff" = diff.matrix$Hayling_ABErrorScore.6, "Hayling_FU3Diff" = diff.matrix$Hayling_ABErrorScore.13, "Hayling_FU4Diff" = diff.matrix$Hayling_ABErrorScore.26, "Hayling_FU1.z" = z.matrix$Hayling_ABErrorScore.2, "Hayling_FU2.z" = z.matrix$Hayling_ABErrorScore.6, "Hayling_FU3.z" = z.matrix$Hayling_ABErrorScore.13, "Hayling_FU4.z" = z.matrix$Hayling_ABErrorScore.26)



### EXCLUDED LETTER FLUENCY RULE VIOLATIONS ###

ELF<-Imputed[,c("ID","TimepointNum",
"ClinicalSubtype", "TremorAkinesiaSubtype", "Gender", "Age", "Signif.Psych",
"Signif.Psych.CaseYN", "ELF_RuleViolations")]

ELF_wide<-reshape(ELF, idvar = "ID", timevar = "TimepointNum", direction = "wide",
drop = c("ClinicalSubtype", "TremorAkinesiaSubtype", "Gender", "Age", "Signif.Psych", "Signif.Psych.CaseYN"))

#Create difference matrix and z-score matrix

diff.matrix<-log1p(ELF_wide[,3:6])-log1p(ELF_wide[,2])

#Calculate the z-scores on the un-transformed data
diff.matrix.nolog<-ELF_wide[,3:6]-ELF_wide[,2]
z.matrix<-data.frame(scale(diff.matrix.nolog[,1:4], center = TRUE, scale = TRUE))

#Create a data frame with derived variables

ELF_RuleViolations<-data.frame("ID" = ELF_wide$ID, "ELF_FU1Diff" = diff.matrix$ELF_RuleViolations.2, "ELF_FU2Diff" = diff.matrix$ELF_RuleViolations.6, "ELF_FU3Diff" = diff.matrix$ELF_RuleViolations.13, "ELF_FU4Diff" = diff.matrix$ELF_RuleViolations.26, "ELF_FU1.z" = z.matrix$ELF_RuleViolations.2, "ELF_FU2.z" = z.matrix$ELF_RuleViolations.6, "ELF_FU3.z" = z.matrix$ELF_RuleViolations.13, "ELF_FU4.z" = z.matrix$ELF_RuleViolations.26)



### LEVODOPA EQUIVALENT DAILY DOSE ###

LEDD<-Imputed[,c("ID","TimepointNum",
"ClinicalSubtype", "TremorAkinesiaSubtype", "Gender", "Age", "Signif.Psych",
"Signif.Psych.CaseYN", "LEDD")]

LEDD_wide<-reshape(LEDD, idvar = "ID", timevar = "TimepointNum", direction = "wide",
drop = c("ClinicalSubtype", "TremorAkinesiaSubtype", "Gender", "Age", "Signif.Psych", "Signif.Psych.CaseYN"))

#Create difference matrix and z-score matrix

diff.matrix<-log1p(LEDD_wide[,3:6])-log1p(LEDD_wide[,2])

#Calculate the z-scores on the un-transformed data
diff.matrix.nolog<-LEDD_wide[,3:6]-LEDD_wide[,2]
z.matrix<-data.frame(scale(diff.matrix.nolog[,1:4], center = TRUE, scale = TRUE))

#Create a data frame with derived variables

Drug<-data.frame("ID" = LEDD_wide$ID, "LEDD_FU1Diff" = diff.matrix$LEDD.2, "LEDD_FU2Diff" = diff.matrix$LEDD.6, "LEDD_FU3Diff" = diff.matrix$LEDD.13, "LEDD_FU4Diff" = diff.matrix$LEDD.26, "LEDD_FU1.z" = z.matrix$LEDD.2, "LEDD_FU2.z" = z.matrix$LEDD.6, "LEDD_FU3.z" = z.matrix$LEDD.13, "LEDD_FU4.z" = z.matrix$LEDD.26)



### UPDRS TOTAL ###

UPDRS<-Imputed[,c("ID","TimepointNum",
"ClinicalSubtype", "TremorAkinesiaSubtype", "Gender", "Age", "Signif.Psych",
"Signif.Psych.CaseYN", "UPDRS_Total")]

UPDRS_wide<-reshape(UPDRS, idvar = "ID", timevar = "TimepointNum", direction = "wide",
drop = c("ClinicalSubtype", "TremorAkinesiaSubtype", "Gender", "Age", "Signif.Psych", "Signif.Psych.CaseYN"))

#Create difference matrix and z-score matrix

diff.matrix<-UPDRS_wide[,3:6]-UPDRS_wide[,2]
z.matrix<-data.frame(scale(diff.matrix[,1:4], center = TRUE, scale = TRUE))

#Create a data frame with derived variables

UPDRS_Total<-data.frame("ID" = UPDRS_wide$ID, "UPDRS_FU1Diff" = diff.matrix$UPDRS_Total.2, "UPDRS_FU2Diff" = diff.matrix$UPDRS_Total.6, "UPDRS_FU3Diff" = diff.matrix$UPDRS_Total.13, "UPDRS_FU4Diff" = diff.matrix$UPDRS_Total.26, "UPDRS_FU1.z" = z.matrix$UPDRS_Total.2, "UPDRS_FU2.z" = z.matrix$UPDRS_Total.6, "UPDRS_FU3.z" = z.matrix$UPDRS_Total.13, "UPDRS_FU4.z" = z.matrix$UPDRS_Total.26)



### UPDRS LEFT (RIGHT STN) ###

UPDRS_Left<-Imputed[,c("ID","TimepointNum",
"ClinicalSubtype", "TremorAkinesiaSubtype", "Gender", "Age", "Signif.Psych",
"Signif.Psych.CaseYN", "UPDRS_Left")]

UPDRS_Left_wide<-reshape(UPDRS_Left, idvar = "ID", timevar = "TimepointNum", direction = "wide",
drop = c("ClinicalSubtype", "TremorAkinesiaSubtype", "Gender", "Age", "Signif.Psych", "Signif.Psych.CaseYN"))

#Create difference matrix and z-score matrix

diff.matrix<-UPDRS_Left_wide[,3:6]-UPDRS_Left_wide[,2]
z.matrix<-data.frame(scale(diff.matrix[,1:4], center = TRUE, scale = TRUE))

#Create a data frame with derived variables
#Recreate wide data frame without dropping clinical & demographic data
UPDRS_Left_wide<-reshape(UPDRS_Left, idvar = "ID", timevar = "TimepointNum", direction = "wide")

UPDRS_Left<-data.frame("ID" = UPDRS_Left_wide$ID, "ClinicalSubtype" = UPDRS_Left_wide$ClinicalSubtype.0, "TremorAkinesiaSubtype" = UPDRS_Left_wide$TremorAkinesiaSubtype.0, "Gender" = UPDRS_Left_wide$Gender.0, "Signif.Psych" = UPDRS_Left_wide$Signif.Psych.0, "Signif.Psych.CaseYN" = UPDRS_Left_wide$Signif.Psych.CaseYN.0,
"UPDRS_Left_FU1Diff" = diff.matrix$UPDRS_Left.2, "UPDRS_Left_FU2Diff" = diff.matrix$UPDRS_Left.6, "UPDRS_Left_FU3Diff" = diff.matrix$UPDRS_Left.13, "UPDRS_Left_FU4Diff" = diff.matrix$UPDRS_Left.26, "UPDRS_Left_FU1.z" = z.matrix$UPDRS_Left.2, "UPDRS_Left_FU2.z" = z.matrix$UPDRS_Left.6, "UPDRS_Left_FU3.z" = z.matrix$UPDRS_Left.13, "UPDRS_Left_FU4.z" = z.matrix$UPDRS_Left.26)



### UPDRS RIGHT (LEFT STN) ###

UPDRS_Right<-Imputed[,c("ID","TimepointNum",
"ClinicalSubtype", "TremorAkinesiaSubtype", "Gender", "Age", "Signif.Psych",
"Signif.Psych.CaseYN", "UPDRS_Right")]

UPDRS_Right_wide<-reshape(UPDRS_Right, idvar = "ID", timevar = "TimepointNum", direction = "wide",
drop = c("ClinicalSubtype", "TremorAkinesiaSubtype", "Gender", "Age", "Signif.Psych", "Signif.Psych.CaseYN"))

#Create difference matrix and z-score matrix

diff.matrix<-UPDRS_Right_wide[,3:6]-UPDRS_Right_wide[,2]
z.matrix<-data.frame(scale(diff.matrix[,1:4], center = TRUE, scale = TRUE))

#Create a data frame with derived variables
#Recreate wide data frame without dropping clinical & demographic data
UPDRS_Right_wide<-reshape(UPDRS_Right, idvar = "ID", timevar = "TimepointNum", direction = "wide")

UPDRS_Right<-data.frame("ID" = UPDRS_Right_wide$ID, "ClinicalSubtype" = UPDRS_Right_wide$ClinicalSubtype.0, "TremorAkinesiaSubtype" = UPDRS_Right_wide$TremorAkinesiaSubtype.0, "Gender" = UPDRS_Right_wide$Gender.0, "Signif.Psych" = UPDRS_Right_wide$Signif.Psych.0, "Signif.Psych.CaseYN" = UPDRS_Right_wide$Signif.Psych.CaseYN.0,
"UPDRS_Right_FU1Diff" = diff.matrix$UPDRS_Right.2, "UPDRS_Right_FU2Diff" = diff.matrix$UPDRS_Right.6, "UPDRS_Right_FU3Diff" = diff.matrix$UPDRS_Right.13, "UPDRS_Right_FU4Diff" = diff.matrix$UPDRS_Right.26, "UPDRS_Right_FU1.z" = z.matrix$UPDRS_Right.2, "UPDRS_Right_FU2.z" = z.matrix$UPDRS_Right.6, "UPDRS_Right_FU3.z" = z.matrix$UPDRS_Right.13, "UPDRS_Right_FU4.z" = z.matrix$UPDRS_Right.26)



### CREATE A SUMMARY CSV ###

a<-merge(Baseline_Factors, Caregiver_EQ, by="ID")
b<-merge(a, Patient_EQ, by="ID")
c<-merge(b, Caregiver_BIS, by="ID")
d<-merge(c, Patient_BIS, by="ID")
e<-merge(d, Hayling_ABErrorScore, by="ID")
f<-merge(e, ELF_RuleViolations, by="ID")
g<-merge(f, Drug, by="ID")
h<-merge(g, UPDRS_Total, by="ID")
i<-merge(h, UPDRS_Left, by="ID")
Timepoint_Change<-merge(i, UPDRS_Right, by="ID")



### SUBSET APPROPRIATE ANATOMICAL DATA ###

Anatomical_Subset<-anatomical[,-c(2, 3, 8, 11, 14, 17, 20, 21, 24, 27)]



### MERGE ANATOMICAL AND TIMEPOINT CHANGE ###

TimepointPlusAnatomical<-merge(Timepoint_Change, Anatomical_Subset, by+"ID")
write.csv(TimepointPlusAnatomical, file = "TimepointPlusAnatomical.csv", row.names = FALSE)
