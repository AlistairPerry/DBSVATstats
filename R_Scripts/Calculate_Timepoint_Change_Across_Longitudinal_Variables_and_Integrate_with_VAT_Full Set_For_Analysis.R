---
title: "Phil Mosley PD subjects: Calculate Timepoint Change Across Longitudinal Variables"
author: "Phil Mosley"
date: "24 September 2017"
output: html_document
---  
  
#load libraries
library(ggplot2)
library(readxl)


setwd("~/Dropbox/Statistical Analyses Parkinsons Disease and DBS/Data sets/09 2017 DATA SET/")

#read in imputed data set

Imputed<-read.csv("stacked_long_unified_with_cart_imputation.csv")

#and anatomical data for each follow up timepoint
anatomical_FU2<-read_excel("Anatomical_FU2.xlsx")
anatomical_FU2<-anatomical_FU2[!is.na(anatomical_FU2$ID),]
names(anatomical_FU2)<-make.names(names(anatomical_FU2))
names(anatomical_FU2)[2]<-"Signif.Psych.FU2"
anatomical_FU2$Signif.Psych.FU2<-factor(anatomical_FU2$Signif.Psych.FU2,labels=c(FALSE,TRUE))
anatomical_FU2<-anatomical_FU2[order(anatomical_FU2$ID),]

anatomical_FU3<-read_excel("Anatomical_FU3.xlsx")
anatomical_FU3<-anatomical_FU3[!is.na(anatomical_FU3$ID),]
names(anatomical_FU3)<-make.names(names(anatomical_FU3))
names(anatomical_FU3)[2]<-"Signif.Psych.FU3"
anatomical_FU3$Signif.Psych.FU3<-factor(anatomical_FU3$Signif.Psych.FU3,labels=c(FALSE,TRUE))
anatomical_FU3<-anatomical_FU3[order(anatomical_FU3$ID),]

anatomical_FU4<-read_excel("Anatomical_FU4.xlsx")
anatomical_FU4<-anatomical_FU4[!is.na(anatomica_FU4l$ID),]
names(anatomical_FU4)<-make.names(names(anatomical_FU4))
names(anatomical_FU4)[2]<-"Signif.Psych"
anatomical_FU4$Signif.Psych<-factor(anatomical_FU4$Signif.Psych,labels=c(FALSE,TRUE))
anatomical_FU4<-anatomical_FU4[order(anatomical_FU4$ID),]

#Remove ID 33 as this patient has no anatomical data (no MRI due to pacemaker in situ)
Imputed<-Imputed[Imputed$ID!=33,]
anatomical_FU2<-anatomical_FU2[anatomical_FU2$ID!=33,]
anatomical_FU3<-anatomical_FU3[anatomical_FU3$ID!=33,]
anatomical_FU4<-anatomical_FU4[anatomical_FU4$ID!=33,]

#Remove ID 03 as this patient has misplaced leads
Imputed<-Imputed[Imputed$ID!=3,]
anatomical_FU2<-anatomical_FU2[anatomical_FU2$ID!=3,]
anatomical_FU3<-anatomical_FU3[anatomical_FU3$ID!=3,]
anatomical_FU4<-anatomical_FU4[anatomical_FU4$ID!=3,]

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



### ICD_Total ###

ICD<-Imputed[,c("ID","TimepointNum",
"ClinicalSubtype", "TremorAkinesiaSubtype", "Gender", "Age", "Signif.Psych",
"Signif.Psych.CaseYN", "ICD.Total")]

ICD_wide<-reshape(ICD, idvar = "ID", timevar = "TimepointNum", direction = "wide",
drop = c("ClinicalSubtype", "TremorAkinesiaSubtype", "Gender", "Age", "Signif.Psych", "Signif.Psych.CaseYN"))

#Create difference matrix and z-score matrix

ICD<-ICD_wide[,2:6]
ICDsc<-ICD
ICDsc[,1]<-scale(ICD[,1], center = TRUE, scale = TRUE)
ICDsc[,2]<-scale(ICD[,2], center = TRUE, scale = TRUE)
ICDsc[,3]<-scale(ICD[,3], center = TRUE, scale = TRUE)
ICDsc[,4]<-scale(ICD[,4], center = TRUE, scale = TRUE)
ICDsc[,5]<-scale(ICD[,5], center = TRUE, scale = TRUE)

diff.matrix.ICD<-ICDsc[,2:5]-ICDsc[,1]

#Create a data frame with derived variables

ICD_Total<-data.frame("ID" = ICD_wide$ID, "ICD_FU1Diff" = diff.matrix.ICD[,1], "ICD_FU2Diff" = diff.matrix.ICD[,2], "ICD_FU3Diff" = diff.matrix.ICD[,3], "ICD_FU4Diff" = diff.matrix.ICD[,4], "ICD_FU1.z" = ICDsc[,2], "ICD_FU2.z" = ICDsc[,3], "ICD_FU3.z" = ICDsc[,4], "ICD_FU4.z" = ICDsc[,5])


### QUIP_Total ###

QUIP<-Imputed[,c("ID","TimepointNum",
"ClinicalSubtype", "TremorAkinesiaSubtype", "Gender", "Age", "Signif.Psych",
"Signif.Psych.CaseYN", "QUIP.Total")]

QUIP_wide<-reshape(QUIP, idvar = "ID", timevar = "TimepointNum", direction = "wide",
drop = c("ClinicalSubtype", "TremorAkinesiaSubtype", "Gender", "Age", "Signif.Psych", "Signif.Psych.CaseYN"))

#Create difference matrix and z-score matrix

QUIP<-QUIP_wide[,2:6]
QUIPsc<-QUIP
QUIPsc[,1]<-scale(QUIP[,1], center = TRUE, scale = TRUE)
QUIPsc[,2]<-scale(QUIP[,2], center = TRUE, scale = TRUE)
QUIPsc[,3]<-scale(QUIP[,3], center = TRUE, scale = TRUE)
QUIPsc[,4]<-scale(QUIP[,4], center = TRUE, scale = TRUE)
QUIPsc[,5]<-scale(QUIP[,5], center = TRUE, scale = TRUE)

diff.matrix.QUIP<-QUIPsc[,2:5]-QUIPsc[,1]

#Create a data frame with derived variables

QUIP_Total<-data.frame("ID" = QUIP_wide$ID, "QUIP_FU1Diff" = diff.matrix.QUIP[,1], "QUIP_FU2Diff" = diff.matrix.QUIP[,2], "QUIP_FU3Diff" = diff.matrix.QUIP[,3], "QUIP_FU4Diff" = diff.matrix.QUIP[,4], "QUIP_FU1.z" = QUIPsc[,2], "QUIP_FU2.z" = QUIPsc[,3], "QUIP_FU3.z" = QUIPsc[,4], "QUIP_FU4.z" = QUIPsc[,5])


### BDI_Total ###

BDI<-Imputed[,c("ID","TimepointNum",
"ClinicalSubtype", "TremorAkinesiaSubtype", "Gender", "Age", "Signif.Psych",
"Signif.Psych.CaseYN", "BDI_Total")]

BDI_wide<-reshape(BDI, idvar = "ID", timevar = "TimepointNum", direction = "wide",
drop = c("ClinicalSubtype", "TremorAkinesiaSubtype", "Gender", "Age", "Signif.Psych", "Signif.Psych.CaseYN"))

#Create difference matrix and z-score matrix

BDI<-BDI_wide[,2:6]
BDIsc<-BDI
BDIsc[,1]<-scale(BDI[,1], center = TRUE, scale = TRUE)
BDIsc[,2]<-scale(BDI[,2], center = TRUE, scale = TRUE)
BDIsc[,3]<-scale(BDI[,3], center = TRUE, scale = TRUE)
BDIsc[,4]<-scale(BDI[,4], center = TRUE, scale = TRUE)
BDIsc[,5]<-scale(BDI[,5], center = TRUE, scale = TRUE)

diff.matrix.BDI<-BDIsc[,2:5]-BDIsc[,1]

#Create a data frame with derived variables

BDI_Total<-data.frame("ID" = BDI_wide$ID, "BDI_FU1Diff" = diff.matrix.BDI[,1], "BDI_FU2Diff" = diff.matrix.BDI[,2], "BDI_FU3Diff" = diff.matrix.BDI[,3], "BDI_FU4Diff" = diff.matrix.BDI[,4], "BDI_FU1.z" = BDIsc[,2], "BDI_FU2.z" = BDIsc[,3], "BDI_FU3.z" = BDIsc[,4], "BDI_FU4.z" = BDIsc[,5])



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
Timepoint_Change<-merge(Timepoint_Change, ICD_Total, by="ID")
Timepoint_Change<-merge(Timepoint_Change, QUIP_Total, by="ID")
Timepoint_Change<-merge(Timepoint_Change, BDI_Total, by="ID")
Timepoint_Change<-merge(Timepoint_Change, Hayling_ABErrorScore, by="ID")
Timepoint_Change<-merge(Timepoint_Change, ELF_RuleViolations, by="ID")
Timepoint_Change<-merge(Timepoint_Change, LEDD_Total, by="ID")
Timepoint_Change<-merge(Timepoint_Change, UPDRS_Total, by="ID")
Timepoint_Change<-merge(Timepoint_Change, UPDRS_Left_Total, by="ID")
Timepoint_Change<-merge(Timepoint_Change, UPDRS_Right_Total, by="ID")



### SUBSET APPROPRIATE ANATOMICAL DATA ###

anatomical_FU2_subset<-anatomical_FU2[,-c(3, 8, 11, 20, 21, 24, 27)]
anatomical_FU3_subset<-anatomical_FU3[,-c(3, 8, 11, 20, 21, 24, 27)]
anatomical_FU4_subset<-anatomical_FU4[,-c(3, 8, 11, 20, 21, 24, 27)]

### MERGE ANATOMICAL, TIMEPOINT CHANGE AND MAX CHANGE ###
#See the calculate max change script to derive max change

TimepointPlusMax<-merge(Timepoint_Change, Max_Change, by="ID")
TimepointMaxAnatomical<-merge(TimepointPlusMax, anatomical_FU2_subset, by="ID")
TimepointMaxAnatomical<-merge(TimepointMaxAnatomical, anatomical_FU3_subset, by="ID")
TimepointMaxAnatomical<-merge(TimepointMaxAnatomical, anatomical_FU4_subset, by="ID")

### TIDY UP NAs FOR ANALYSES ###

#Replace any missing values with the mean of the sample at each timepoint

TimepointMaxAnatomical$CarerEQ_FU1Diff[is.na(TimepointMaxAnatomical$CarerEQ_FU1Diff)] <- mean(TimepointMaxAnatomical$CarerEQ_FU1Diff, na.rm=T)
TimepointMaxAnatomical$CarerEQ_FU2Diff[is.na(TimepointMaxAnatomical$CarerEQ_FU2Diff)] <- mean(TimepointMaxAnatomical$CarerEQ_FU2Diff, na.rm=T)
TimepointMaxAnatomical$CarerEQ_FU3Diff[is.na(TimepointMaxAnatomical$CarerEQ_FU3Diff)] <- mean(TimepointMaxAnatomical$CarerEQ_FU3Diff, na.rm=T)
TimepointMaxAnatomical$CarerEQ_FU4Diff[is.na(TimepointMaxAnatomical$CarerEQ_FU4Diff)] <- mean(TimepointMaxAnatomical$CarerEQ_FU4Diff, na.rm=T)

TimepointMaxAnatomical$CarerEQ_FU1.z[is.na(TimepointMaxAnatomical$CarerEQ_FU1.z)] <- mean(TimepointMaxAnatomical$CarerEQ_FU1.z, na.rm=T)
TimepointMaxAnatomical$CarerEQ_FU2.z[is.na(TimepointMaxAnatomical$CarerEQ_FU2.z)] <- mean(TimepointMaxAnatomical$CarerEQ_FU2.z, na.rm=T)
TimepointMaxAnatomical$CarerEQ_FU3.z[is.na(TimepointMaxAnatomical$CarerEQ_FU3.z)] <- mean(TimepointMaxAnatomical$CarerEQ_FU3.z, na.rm=T)
TimepointMaxAnatomical$CarerEQ_FU4.z[is.na(TimepointMaxAnatomical$CarerEQ_FU4.z)] <- mean(TimepointMaxAnatomical$CarerEQ_FU4.z, na.rm=T)

TimepointMaxAnatomical$EQ_FU1Diff[is.na(TimepointMaxAnatomical$EQ_FU1Diff)] <- mean(TimepointMaxAnatomical$EQ_FU1Diff, na.rm=T)
TimepointMaxAnatomical$EQ_FU2Diff[is.na(TimepointMaxAnatomical$EQ_FU2Diff)] <- mean(TimepointMaxAnatomical$EQ_FU2Diff, na.rm=T)
TimepointMaxAnatomical$EQ_FU3Diff[is.na(TimepointMaxAnatomical$EQ_FU3Diff)] <- mean(TimepointMaxAnatomical$EQ_FU3Diff, na.rm=T)
TimepointMaxAnatomical$EQ_FU4Diff[is.na(TimepointMaxAnatomical$EQ_FU4Diff)] <- mean(TimepointMaxAnatomical$EQ_FU4Diff, na.rm=T)

TimepointMaxAnatomical$EQ_FU1.z[is.na(TimepointMaxAnatomical$EQ_FU1.z)] <- mean(TimepointMaxAnatomical$EQ_FU1.z, na.rm=T)
TimepointMaxAnatomical$EQ_FU2.z[is.na(TimepointMaxAnatomical$EQ_FU2.z)] <- mean(TimepointMaxAnatomical$EQ_FU2.z, na.rm=T)
TimepointMaxAnatomical$EQ_FU3.z[is.na(TimepointMaxAnatomical$EQ_FU3.z)] <- mean(TimepointMaxAnatomical$EQ_FU3.z, na.rm=T)
TimepointMaxAnatomical$EQ_FU4.z[is.na(TimepointMaxAnatomical$EQ_FU4.z)] <- mean(TimepointMaxAnatomical$EQ_FU4.z, na.rm=T)

TimepointMaxAnatomical$CarerBIS_FU1Diff[is.na(TimepointMaxAnatomical$CarerBIS_FU1Diff)] <- mean(TimepointMaxAnatomical$CarerBIS_FU1Diff, na.rm=T)
TimepointMaxAnatomical$CarerBIS_FU2Diff[is.na(TimepointMaxAnatomical$CarerBIS_FU2Diff)] <- mean(TimepointMaxAnatomical$CarerBIS_FU2Diff, na.rm=T)
TimepointMaxAnatomical$CarerBIS_FU3Diff[is.na(TimepointMaxAnatomical$CarerBIS_FU3Diff)] <- mean(TimepointMaxAnatomical$CarerBIS_FU3Diff, na.rm=T)
TimepointMaxAnatomical$CarerBIS_FU4Diff[is.na(TimepointMaxAnatomical$CarerBIS_FU4Diff)] <- mean(TimepointMaxAnatomical$CarerBIS_FU4Diff, na.rm=T)

TimepointMaxAnatomical$CarerBIS_FU1.z[is.na(TimepointMaxAnatomical$CarerBIS_FU1.z)] <- mean(TimepointMaxAnatomical$CarerBIS_FU1.z, na.rm=T)
TimepointMaxAnatomical$CarerBIS_FU2.z[is.na(TimepointMaxAnatomical$CarerBIS_FU2.z)] <- mean(TimepointMaxAnatomical$CarerBIS_FU2.z, na.rm=T)
TimepointMaxAnatomical$CarerBIS_FU3.z[is.na(TimepointMaxAnatomical$CarerBIS_FU3.z)] <- mean(TimepointMaxAnatomical$CarerBIS_FU3.z, na.rm=T)
TimepointMaxAnatomical$CarerBIS_FU4.z[is.na(TimepointMaxAnatomical$CarerBIS_FU4.z)] <- mean(TimepointMaxAnatomical$CarerBIS_FU4.z, na.rm=T)

TimepointMaxAnatomical$BIS_FU1Diff[is.na(TimepointMaxAnatomical$BIS_FU1Diff)] <- mean(TimepointMaxAnatomical$BIS_FU1Diff, na.rm=T)
TimepointMaxAnatomical$BIS_FU2Diff[is.na(TimepointMaxAnatomical$BIS_FU2Diff)] <- mean(TimepointMaxAnatomical$BIS_FU2Diff, na.rm=T)
TimepointMaxAnatomical$BIS_FU3Diff[is.na(TimepointMaxAnatomical$BIS_FU3Diff)] <- mean(TimepointMaxAnatomical$BIS_FU3Diff, na.rm=T)
TimepointMaxAnatomical$BIS_FU4Diff[is.na(TimepointMaxAnatomical$BIS_FU4Diff)] <- mean(TimepointMaxAnatomical$BIS_FU4Diff, na.rm=T)

TimepointMaxAnatomical$BIS_FU1.z[is.na(TimepointMaxAnatomical$BIS_FU1.z)] <- mean(TimepointMaxAnatomical$BIS_FU1.z, na.rm=T)
TimepointMaxAnatomical$BIS_FU2.z[is.na(TimepointMaxAnatomical$BIS_FU2.z)] <- mean(TimepointMaxAnatomical$BIS_FU2.z, na.rm=T)
TimepointMaxAnatomical$BIS_FU3.z[is.na(TimepointMaxAnatomical$BIS_FU3.z)] <- mean(TimepointMaxAnatomical$BIS_FU3.z, na.rm=T)
TimepointMaxAnatomical$BIS_FU4.z[is.na(TimepointMaxAnatomical$BIS_FU4.z)] <- mean(TimepointMaxAnatomical$BIS_FU4.z, na.rm=T)

TimepointMaxAnatomical$ICD_FU1Diff[is.na(TimepointMaxAnatomical$ICD_FU1Diff)] <- mean(TimepointMaxAnatomical$ICD_FU1Diff, na.rm=T)
TimepointMaxAnatomical$ICD_FU2Diff[is.na(TimepointMaxAnatomical$ICD_FU2Diff)] <- mean(TimepointMaxAnatomical$ICD_FU2Diff, na.rm=T)
TimepointMaxAnatomical$ICD_FU3Diff[is.na(TimepointMaxAnatomical$ICD_FU3Diff)] <- mean(TimepointMaxAnatomical$ICD_FU3Diff, na.rm=T)
TimepointMaxAnatomical$ICD_FU4Diff[is.na(TimepointMaxAnatomical$ICD_FU4Diff)] <- mean(TimepointMaxAnatomical$ICD_FU4Diff, na.rm=T)

TimepointMaxAnatomical$ICD_FU1.z[is.na(TimepointMaxAnatomical$ICD_FU1.z)] <- mean(TimepointMaxAnatomical$ICD_FU1.z, na.rm=T)
TimepointMaxAnatomical$ICD_FU2.z[is.na(TimepointMaxAnatomical$ICD_FU2.z)] <- mean(TimepointMaxAnatomical$ICD_FU2.z, na.rm=T)
TimepointMaxAnatomical$ICD_FU3.z[is.na(TimepointMaxAnatomical$ICD_FU3.z)] <- mean(TimepointMaxAnatomical$ICD_FU3.z, na.rm=T)
TimepointMaxAnatomical$ICD_FU4.z[is.na(TimepointMaxAnatomical$ICD_FU4.z)] <- mean(TimepointMaxAnatomical$ICD_FU4.z, na.rm=T)

TimepointMaxAnatomical$QUIP_FU1Diff[is.na(TimepointMaxAnatomical$QUIP_FU1Diff)] <- mean(TimepointMaxAnatomical$QUIP_FU1Diff, na.rm=T)
TimepointMaxAnatomical$QUIP_FU2Diff[is.na(TimepointMaxAnatomical$QUIP_FU2Diff)] <- mean(TimepointMaxAnatomical$QUIP_FU2Diff, na.rm=T)
TimepointMaxAnatomical$QUIP_FU3Diff[is.na(TimepointMaxAnatomical$QUIP_FU3Diff)] <- mean(TimepointMaxAnatomical$QUIP_FU3Diff, na.rm=T)
TimepointMaxAnatomical$QUIP_FU4Diff[is.na(TimepointMaxAnatomical$QUIP_FU4Diff)] <- mean(TimepointMaxAnatomical$QUIP_FU4Diff, na.rm=T)

TimepointMaxAnatomical$QUIP_FU1.z[is.na(TimepointMaxAnatomical$QUIP_FU1.z)] <- mean(TimepointMaxAnatomical$QUIP_FU1.z, na.rm=T)
TimepointMaxAnatomical$QUIP_FU2.z[is.na(TimepointMaxAnatomical$QUIP_FU2.z)] <- mean(TimepointMaxAnatomical$QUIP_FU2.z, na.rm=T)
TimepointMaxAnatomical$QUIP_FU3.z[is.na(TimepointMaxAnatomical$QUIP_FU3.z)] <- mean(TimepointMaxAnatomical$QUIP_FU3.z, na.rm=T)
TimepointMaxAnatomical$QUIP_FU4.z[is.na(TimepointMaxAnatomical$QUIP_FU4.z)] <- mean(TimepointMaxAnatomical$QUIP_FU4.z, na.rm=T)

TimepointMaxAnatomical$BDI_FU1Diff[is.na(TimepointMaxAnatomical$BDI_FU1Diff)] <- mean(TimepointMaxAnatomical$BDI_FU1Diff, na.rm=T)
TimepointMaxAnatomical$BDI_FU2Diff[is.na(TimepointMaxAnatomical$BDI_FU2Diff)] <- mean(TimepointMaxAnatomical$BDI_FU2Diff, na.rm=T)
TimepointMaxAnatomical$BDI_FU3Diff[is.na(TimepointMaxAnatomical$BDI_FU3Diff)] <- mean(TimepointMaxAnatomical$BDI_FU3Diff, na.rm=T)
TimepointMaxAnatomical$BDI_FU4Diff[is.na(TimepointMaxAnatomical$BDI_FU4Diff)] <- mean(TimepointMaxAnatomical$BDI_FU4Diff, na.rm=T)

TimepointMaxAnatomical$BDI_FU1.z[is.na(TimepointMaxAnatomical$BDI_FU1.z)] <- mean(TimepointMaxAnatomical$BDI_FU1.z, na.rm=T)
TimepointMaxAnatomical$BDI_FU2.z[is.na(TimepointMaxAnatomical$BDI_FU2.z)] <- mean(TimepointMaxAnatomical$BDI_FU2.z, na.rm=T)
TimepointMaxAnatomical$BDI_FU3.z[is.na(TimepointMaxAnatomical$BDI_FU3.z)] <- mean(TimepointMaxAnatomical$BDI_FU3.z, na.rm=T)
TimepointMaxAnatomical$BDI_FU4.z[is.na(TimepointMaxAnatomical$BDI_FU4.z)] <- mean(TimepointMaxAnatomical$BDI_FU4.z, na.rm=T)

TimepointMaxAnatomical$Hayling_FU1Diff[is.na(TimepointMaxAnatomical$Hayling_FU1Diff)] <- mean(TimepointMaxAnatomical$Hayling_FU1Diff, na.rm=T)
TimepointMaxAnatomical$Hayling_FU2Diff[is.na(TimepointMaxAnatomical$Hayling_FU2Diff)] <- mean(TimepointMaxAnatomical$Hayling_FU2Diff, na.rm=T)
TimepointMaxAnatomical$Hayling_FU3Diff[is.na(TimepointMaxAnatomical$Hayling_FU3Diff)] <- mean(TimepointMaxAnatomical$Hayling_FU3Diff, na.rm=T)
TimepointMaxAnatomical$Hayling_FU4Diff[is.na(TimepointMaxAnatomical$Hayling_FU4Diff)] <- mean(TimepointMaxAnatomical$Hayling_FU4Diff, na.rm=T)

TimepointMaxAnatomical$Hayling_FU1.z[is.na(TimepointMaxAnatomical$Hayling_FU1.z)] <- mean(TimepointMaxAnatomical$Hayling_FU1.z, na.rm=T)
TimepointMaxAnatomical$Hayling_FU2.z[is.na(TimepointMaxAnatomical$Hayling_FU2.z)] <- mean(TimepointMaxAnatomical$Hayling_FU2.z, na.rm=T)
TimepointMaxAnatomical$Hayling_FU3.z[is.na(TimepointMaxAnatomical$Hayling_FU3.z)] <- mean(TimepointMaxAnatomical$Hayling_FU3.z, na.rm=T)
TimepointMaxAnatomical$Hayling_FU4.z[is.na(TimepointMaxAnatomical$Hayling_FU4.z)] <- mean(TimepointMaxAnatomical$Hayling_FU4.z, na.rm=T)

TimepointMaxAnatomical$ELF_FU1Diff[is.na(TimepointMaxAnatomical$ELF_FU1Diff)] <- mean(TimepointMaxAnatomical$ELF_FU1Diff, na.rm=T)
TimepointMaxAnatomical$ELF_FU2Diff[is.na(TimepointMaxAnatomical$ELF_FU2Diff)] <- mean(TimepointMaxAnatomical$ELF_FU2Diff, na.rm=T)
TimepointMaxAnatomical$ELF_FU3Diff[is.na(TimepointMaxAnatomical$ELF_FU3Diff)] <- mean(TimepointMaxAnatomical$ELF_FU3Diff, na.rm=T)
TimepointMaxAnatomical$ELF_FU4Diff[is.na(TimepointMaxAnatomical$ELF_FU4Diff)] <- mean(TimepointMaxAnatomical$ELF_FU4Diff, na.rm=T)

TimepointMaxAnatomical$ELF_FU1.z[is.na(TimepointMaxAnatomical$ELF_FU1.z)] <- mean(TimepointMaxAnatomical$ELF_FU1.z, na.rm=T)
TimepointMaxAnatomical$ELF_FU2.z[is.na(TimepointMaxAnatomical$ELF_FU2.z)] <- mean(TimepointMaxAnatomical$ELF_FU2.z, na.rm=T)
TimepointMaxAnatomical$ELF_FU3.z[is.na(TimepointMaxAnatomical$ELF_FU3.z)] <- mean(TimepointMaxAnatomical$ELF_FU3.z, na.rm=T)
TimepointMaxAnatomical$ELF_FU4.z[is.na(TimepointMaxAnatomical$ELF_FU4.z)] <- mean(TimepointMaxAnatomical$ELF_FU4.z, na.rm=T)

TimepointMaxAnatomical$UPDRS_FU1Diff[is.na(TimepointMaxAnatomical$UPDRS_FU1Diff)] <- mean(TimepointMaxAnatomical$UPDRS_FU1Diff, na.rm=T)
TimepointMaxAnatomical$UPDRS_FU2Diff[is.na(TimepointMaxAnatomical$UPDRS_FU2Diff)] <- mean(TimepointMaxAnatomical$UPDRS_FU2Diff, na.rm=T)
TimepointMaxAnatomical$UPDRS_FU3Diff[is.na(TimepointMaxAnatomical$UPDRS_FU3Diff)] <- mean(TimepointMaxAnatomical$UPDRS_FU3Diff, na.rm=T)
TimepointMaxAnatomical$UPDRS_FU4Diff[is.na(TimepointMaxAnatomical$UPDRS_FU4Diff)] <- mean(TimepointMaxAnatomical$UPDRS_FU4Diff, na.rm=T)

TimepointMaxAnatomical$UPDRS_FU1.z[is.na(TimepointMaxAnatomical$UPDRS_FU1.z)] <- mean(TimepointMaxAnatomical$UPDRS_FU1.z, na.rm=T)
TimepointMaxAnatomical$UPDRS_FU2.z[is.na(TimepointMaxAnatomical$UPDRS_FU2.z)] <- mean(TimepointMaxAnatomical$UPDRS_FU2.z, na.rm=T)
TimepointMaxAnatomical$UPDRS_FU3.z[is.na(TimepointMaxAnatomical$UPDRS_FU3.z)] <- mean(TimepointMaxAnatomical$UPDRS_FU3.z, na.rm=T)
TimepointMaxAnatomical$UPDRS_FU4.z[is.na(TimepointMaxAnatomical$UPDRS_FU4.z)] <- mean(TimepointMaxAnatomical$UPDRS_FU4.z, na.rm=T)

TimepointMaxAnatomical$LEDD_FU1Diff[is.na(TimepointMaxAnatomical$LEDD_FU1Diff)] <- mean(TimepointMaxAnatomical$LEDD_FU1Diff, na.rm=T)
TimepointMaxAnatomical$LEDD_FU2Diff[is.na(TimepointMaxAnatomical$LEDD_FU2Diff)] <- mean(TimepointMaxAnatomical$LEDD_FU2Diff, na.rm=T)
TimepointMaxAnatomical$LEDD_FU3Diff[is.na(TimepointMaxAnatomical$LEDD_FU3Diff)] <- mean(TimepointMaxAnatomical$LEDD_FU3Diff, na.rm=T)
TimepointMaxAnatomical$LEDD_FU4Diff[is.na(TimepointMaxAnatomical$LEDD_FU4Diff)] <- mean(TimepointMaxAnatomical$LEDD_FU4Diff, na.rm=T)

TimepointMaxAnatomical$LEDD_FU1.z[is.na(TimepointMaxAnatomical$LEDD_FU1.z)] <- mean(TimepointMaxAnatomical$LEDD_FU1.z, na.rm=T)
TimepointMaxAnatomical$LEDD_FU2.z[is.na(TimepointMaxAnatomical$LEDD_FU2.z)] <- mean(TimepointMaxAnatomical$LEDD_FU2.z, na.rm=T)
TimepointMaxAnatomical$LEDD_FU3.z[is.na(TimepointMaxAnatomical$LEDD_FU3.z)] <- mean(TimepointMaxAnatomical$LEDD_FU3.z, na.rm=T)
TimepointMaxAnatomical$LEDD_FU4.z[is.na(TimepointMaxAnatomical$LEDD_FU4.z)] <- mean(TimepointMaxAnatomical$LEDD_FU4.z, na.rm=T)

### WRITE OUT CSV ###

write.csv(TimepointMaxAnatomical, file = "TimepointMaxAnatomical.csv", row.names = FALSE)
