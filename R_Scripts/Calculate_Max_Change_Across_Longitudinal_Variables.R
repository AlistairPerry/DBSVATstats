---
title: "Phil Mosley PD subjects: Calculate Max Change Across Longitudinal Variables"
author: "Phil Mosley"
date: "27 August 2017"
output: html_document
---

library(ggplot2)

setwd("/Users/philipm/Dropbox/Philip/Professional/Statistical Analyses Parkinsons Disease and DBS/Data sets/09 2017 DATA SET/")

#read in imputed data set

Imputed<-read.csv("stacked_long_unified_with_cart_imputation.csv")

#and anatomical
anatomical<-read_excel("Anatomical_Longitudinal.xlsx")
anatomical<-anatomical[!is.na(anatomical$ID),]
names(anatomical)<-make.names(names(anatomical))
names(anatomical)[2]<-"Signif.Psych"
anatomical$Signif.Psych<-factor(anatomical$Signif.Psych,labels=c(FALSE,TRUE))
anatomical<-anatomical[order(anatomical$ID),]

#Remove ID 33 as this patient has no anatomical data (no MRI due to pacemaker in situ)
Imputed<-Imputed[Imputed$ID!=33,]
anatomical<-anatomical[anatomical$ID!=33,]

#Remove ID 03 as this patient has misplaced leads
Imputed<-Imputed[Imputed$ID!=3,]
anatomical<-anatomical[anatomical$ID!=3,]



### CAREGIVER-RATED EQ ###

CarerEQ<-Imputed[,c("ID","TimepointNum",
    "ClinicalSubtype", "TremorAkinesiaSubtype", "Gender", "Signif.Psych",
    "Signif.Psych.CaseYN", "CarerEQ_Total")]

CarerEQ_wide<-reshape(CarerEQ, idvar = "ID", timevar = "TimepointNum", direction = "wide",
    drop = c("ClinicalSubtype", "TremorAkinesiaSubtype", "Gender", "Signif.Psych", "Signif.Psych.CaseYN"))

#Create difference matrix and z-score matrix

CarerEQ<-CarerEQ_wide[,2:6]
CarerEQsc<-CarerEQ
CarerEQsc[,1]<-scale(CarerEQ[,1], center = TRUE, scale = TRUE)
CarerEQsc[,2]<-scale(CarerEQ[,2], center = TRUE, scale = TRUE)
CarerEQsc[,3]<-scale(CarerEQ[,3], center = TRUE, scale = TRUE)
CarerEQsc[,4]<-scale(CarerEQ[,4], center = TRUE, scale = TRUE)
CarerEQsc[,5]<-scale(CarerEQ[,5], center = TRUE, scale = TRUE)

diff.matrix<-CarerEQsc[,2:5]-CarerEQsc[,1]

#Calculate minimum and maximum differences and absolute values

mins.and.maxes<-data.frame(CarerEQ_wide,PreDBS[,c("ID","Age", "ClinicalSubtype", "TremorAkinesiaSubtype")],
    anatomical[,c("ID","Signif.Psych")],
    min.diffs=apply(diff.matrix,1,min,na.rm=T),
    max.diffs=apply(diff.matrix,1,max,na.rm=T))

mins.and.maxes$abs.min.diffs<-abs(mins.and.maxes$min.diffs)
mins.and.maxes$abs.max.diffs<-abs(mins.and.maxes$max.diffs)

#Calculate the measurement furthest from zero

mins.and.maxes$closest.to.zero<-apply(mins.and.maxes[,c("abs.min.diffs","abs.max.diffs")],
1, max)
mins.and.maxes$closest.to.zero.min<-mins.and.maxes$closest.to.zero==mins.and.maxes$abs.min.diffs
mins.and.maxes$closest.to.zero.max<-mins.and.maxes$closest.to.zero==mins.and.maxes$abs.max.diffs

#If there is equal change in positive and negative direction, code this as zero, otherwise take the largest difference from baseline as the derived variable

mins.and.maxes$max.dist.from.zero<-ifelse(mins.and.maxes$abs.min.diffs==mins.and.maxes$abs.max.diffs,
0,mins.and.maxes$max.dist.from.zero<-ifelse(mins.and.maxes$closest.to.zero.max,
mins.and.maxes$max.diffs,
mins.and.maxes$min.diffs))

#Plot the largest decrease in EQ and perform one-way test for differences between caseness

CarerEQ_MaxImpairmentPlot<-ggplot(mins.and.maxes, aes(x=min.diffs, color=Signif.Psych)) +
labs(x="Max Decrease in Caregiver EQ", y="density") +
geom_density(alpha = .3) +
theme_bw(base_size = 15)

print(CarerEQ_MaxImpairmentPlot)
ggsave("CaregiverEQ_MaxDecrease.png", plot = CarerEQ_MaxImpairmentPlot)

MaxChange<-oneway_test(min.diffs~Signif.Psych, data=mins.and.maxes,
distribution=approximate(B=9999))
print(MaxChange)

#We can also calculate the "effect size" of the change using a within-subject standard deviation

mins.and.maxes$standard.deviation<-apply(mins.and.maxes[,c(2:6)],
    1, sd, na.rm=T)

mins.and.maxes$CaregiverEQ_EffectSize<-mins.and.maxes$min.diffs/mins.and.maxes$standard.deviation

CarerEQ_EffectSizePlot<-ggplot(mins.and.maxes, aes(x=CaregiverEQ_EffectSize, color=Signif.Psych)) +
    labs(x="CaregiverEQ Effect Size", y="density") +
    geom_density(alpha = .3) +
    theme_bw(base_size = 15)

print(CarerEQ_EffectSizePlot)
ggsave("CaregiverEQ_EffectSize.png", plot = CarerEQ_EffectSizePlot)

EffectSize<-oneway_test(CaregiverEQ_EffectSize~Signif.Psych, data=mins.and.maxes,
        distribution=approximate(B=9999))
print(EffectSize)

#Create a data frame with derived variables

CarerEQ_Summary<-data.frame("ID" = mins.and.maxes$ID, "CaregiverEQ_MaxDecrease" = mins.and.maxes$min.diffs, "CaregiverEQ_EffectSize" = mins.and.maxes$CaregiverEQ_EffectSize)



### PATIENT-RATED EQ ###

EQ<-Imputed[,c("ID","TimepointNum", "ClinicalSubtype", "TremorAkinesiaSubtype", "Gender", "Signif.Psych", "Signif.Psych.CaseYN", "EQ_Total")]

EQ_wide<-reshape(EQ, idvar = "ID", timevar = "TimepointNum", direction = "wide",
drop = c("ClinicalSubtype", "TremorAkinesiaSubtype", "Gender", "Signif.Psych", "Signif.Psych.CaseYN"))

#Create difference matrix and z-score matrix

EQ<-EQ_wide[,2:6]
EQsc<-EQ
EQsc[,1]<-scale(EQ[,1], center = TRUE, scale = TRUE)
EQsc[,2]<-scale(EQ[,2], center = TRUE, scale = TRUE)
EQsc[,3]<-scale(EQ[,3], center = TRUE, scale = TRUE)
EQsc[,4]<-scale(EQ[,4], center = TRUE, scale = TRUE)
EQsc[,5]<-scale(EQ[,5], center = TRUE, scale = TRUE)

diff.matrix<-EQsc[,2:5]-EQsc[,1]

#Calculate minimum and maximum differences and absolute values

mins.and.maxes<-data.frame(EQ_wide,PreDBS[,c("ID","Age", "ClinicalSubtype", "TremorAkinesiaSubtype")],
anatomical[,c("ID","Signif.Psych")],
min.diffs=apply(diff.matrix,1,min,na.rm=T),
max.diffs=apply(diff.matrix,1,max,na.rm=T))

mins.and.maxes$abs.min.diffs<-abs(mins.and.maxes$min.diffs)
mins.and.maxes$abs.max.diffs<-abs(mins.and.maxes$max.diffs)

#Calculate the measurement furthest from zero

mins.and.maxes$closest.to.zero<-apply(mins.and.maxes[,c("abs.min.diffs","abs.max.diffs")],
1, max)
mins.and.maxes$closest.to.zero.min<-mins.and.maxes$closest.to.zero==mins.and.maxes$abs.min.diffs
mins.and.maxes$closest.to.zero.max<-mins.and.maxes$closest.to.zero==mins.and.maxes$abs.max.diffs

#If there is equal change in positive and negative direction, code this as zero, otherwise take the largest difference from baseline as the derived variable

mins.and.maxes$max.dist.from.zero<-ifelse(mins.and.maxes$abs.min.diffs==mins.and.maxes$abs.max.diffs,
0,mins.and.maxes$max.dist.from.zero<-ifelse(mins.and.maxes$closest.to.zero.max,
mins.and.maxes$max.diffs,
mins.and.maxes$min.diffs))

#Plot the largest decrease in EQ and perform one-way test for differences between caseness

EQ_MaxImpairmentPlot<-ggplot(mins.and.maxes, aes(x=min.diffs, color=Signif.Psych)) +
labs(x="Max Decrease in Patient-Rated EQ", y="density") +
geom_density(alpha = .3) +
theme_bw(base_size = 15)

print(EQ_MaxImpairmentPlot)
ggsave("EQ_MaxDecrease.png", plot = EQ_MaxImpairmentPlot)

MaxChange<-oneway_test(min.diffs~Signif.Psych, data=mins.and.maxes,
distribution=approximate(B=9999))
print(MaxChange)

#We can also calculate the "effect size" of the change using a within-subject standard deviation

mins.and.maxes$standard.deviation<-apply(mins.and.maxes[,c(2:6)],
1, sd, na.rm=T)

mins.and.maxes$EQ_EffectSize<-mins.and.maxes$min.diffs/mins.and.maxes$standard.deviation

EQ_EffectSizePlot<-ggplot(mins.and.maxes, aes(x=EQ_EffectSize, color=Signif.Psych)) +
labs(x="Patient-Rated EQ Effect Size", y="density") +
geom_density(alpha = .3) +
theme_bw(base_size = 15)

print(EQ_EffectSizePlot)
ggsave("EQ_EffectSize.png", plot = EQ_EffectSizePlot)

EffectSize<-oneway_test(EQ_EffectSize~Signif.Psych, data=mins.and.maxes,
distribution=approximate(B=9999))
print(EffectSize)

#Create a data frame with derived variables

EQ_Summary<-data.frame("ID" = mins.and.maxes$ID, "EQ_MaxDecrease" = mins.and.maxes$min.diffs, "EQ_EffectSize" = mins.and.maxes$EQ_EffectSize)



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

diff.matrix<-CBISsc[,2:5]-CBISsc[,1]

#Calculate minimum and maximum differences and absolute values

mins.and.maxes<-data.frame(BIS_wide,PreDBS[,c("ID","Age", "ClinicalSubtype", "TremorAkinesiaSubtype")],
anatomical[,c("ID","Signif.Psych")],
min.diffs=apply(diff.matrix,1,min,na.rm=T),
max.diffs=apply(diff.matrix,1,max,na.rm=T))

mins.and.maxes$abs.min.diffs<-abs(mins.and.maxes$min.diffs)
mins.and.maxes$abs.max.diffs<-abs(mins.and.maxes$max.diffs)

#Calculate the measurement furthest from zero

mins.and.maxes$closest.to.zero<-apply(mins.and.maxes[,c("abs.min.diffs","abs.max.diffs")],
1, max)
mins.and.maxes$closest.to.zero.min<-mins.and.maxes$closest.to.zero==mins.and.maxes$abs.min.diffs
mins.and.maxes$closest.to.zero.max<-mins.and.maxes$closest.to.zero==mins.and.maxes$abs.max.diffs

#If there is equal change in positive and negative direction, code this as zero, otherwise take the largest difference from baseline as the derived variable

mins.and.maxes$max.dist.from.zero<-ifelse(mins.and.maxes$abs.min.diffs==mins.and.maxes$abs.max.diffs,
0,mins.and.maxes$max.dist.from.zero<-ifelse(mins.and.maxes$closest.to.zero.max,
mins.and.maxes$max.diffs,
mins.and.maxes$min.diffs))

#Plot the largest increase in CarerBIS and perform one-way test for differences between caseness

CarerBIS_MaxImpairmentPlot<-ggplot(mins.and.maxes, aes(x=max.diffs, color=Signif.Psych)) +
labs(x="Max Increase in Caregiver BIS", y="density") +
geom_density(alpha = .3) +
theme_bw(base_size = 15)

print(CarerBIS_MaxImpairmentPlot)
ggsave("CaregiverBIS_MaxIncrease.png", plot = CarerBIS_MaxImpairmentPlot)

MaxChange<-oneway_test(max.diffs~Signif.Psych, data=mins.and.maxes,
distribution=approximate(B=9999))
print(MaxChange)

#We can also calculate the "effect size" of the change using a within-subject standard deviation

mins.and.maxes$standard.deviation<-apply(mins.and.maxes[,c(2:6)],
1, sd, na.rm=T)

mins.and.maxes$CarerBIS_EffectSize<-mins.and.maxes$max.diffs/mins.and.maxes$standard.deviation

CarerBIS_EffectSizePlot<-ggplot(mins.and.maxes, aes(x=CarerBIS_EffectSize, color=Signif.Psych)) +
labs(x="Caregiver BIS Effect Size", y="density") +
geom_density(alpha = .3) +
theme_bw(base_size = 15)

print(CarerBIS_EffectSizePlot)
ggsave("CaregiverBIS_EffectSize.png", plot = CarerBIS_EffectSizePlot)

EffectSize<-oneway_test(CarerBIS_EffectSize~Signif.Psych, data=mins.and.maxes,
distribution=approximate(B=9999))
print(EffectSize)

#Create a data frame with derived variables

CarerBIS_Summary<-data.frame("ID" = mins.and.maxes$ID, "CaregiverBIS_MaxIncrease" = mins.and.maxes$max.diffs, "CaregiverBIS_EffectSize" = mins.and.maxes$CarerBIS_EffectSize)



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

diff.matrix<-BISsc[,2:5]-BISsc[,1]

#Calculate minimum and maximum differences and absolute values

mins.and.maxes<-data.frame(BIS_wide,PreDBS[,c("ID","Age", "ClinicalSubtype", "TremorAkinesiaSubtype")],
anatomical[,c("ID","Signif.Psych")],
min.diffs=apply(diff.matrix,1,min,na.rm=T),
max.diffs=apply(diff.matrix,1,max,na.rm=T))

mins.and.maxes$abs.min.diffs<-abs(mins.and.maxes$min.diffs)
mins.and.maxes$abs.max.diffs<-abs(mins.and.maxes$max.diffs)

#Calculate the measurement furthest from zero

mins.and.maxes$closest.to.zero<-apply(mins.and.maxes[,c("abs.min.diffs","abs.max.diffs")],
1, max)
mins.and.maxes$closest.to.zero.min<-mins.and.maxes$closest.to.zero==mins.and.maxes$abs.min.diffs
mins.and.maxes$closest.to.zero.max<-mins.and.maxes$closest.to.zero==mins.and.maxes$abs.max.diffs

#If there is equal change in positive and negative direction, code this as zero, otherwise take the largest difference from baseline as the derived variable

mins.and.maxes$max.dist.from.zero<-ifelse(mins.and.maxes$abs.min.diffs==mins.and.maxes$abs.max.diffs,
0,mins.and.maxes$max.dist.from.zero<-ifelse(mins.and.maxes$closest.to.zero.max,
mins.and.maxes$max.diffs,
mins.and.maxes$min.diffs))

#Plot the largest increase in BIS and perform one-way test for differences between caseness

BIS_MaxImpairmentPlot<-ggplot(mins.and.maxes, aes(x=max.diffs, color=Signif.Psych)) +
labs(x="Max Increase in BIS", y="density") +
geom_density(alpha = .3) +
theme_bw(base_size = 15)

print(BIS_MaxImpairmentPlot)
ggsave("BIS_MaxIncrease.png", plot = BIS_MaxImpairmentPlot)

MaxChange<-oneway_test(max.diffs~Signif.Psych, data=mins.and.maxes,
distribution=approximate(B=9999))
print(MaxChange)

#We can also calculate the "effect size" of the change using a within-subject standard deviation

mins.and.maxes$standard.deviation<-apply(mins.and.maxes[,c(2:6)],
1, sd, na.rm=T)

mins.and.maxes$BIS_EffectSize<-mins.and.maxes$max.diffs/mins.and.maxes$standard.deviation

BIS_EffectSizePlot<-ggplot(mins.and.maxes, aes(x=BIS_EffectSize, color=Signif.Psych)) +
labs(x="BIS Effect Size", y="density") +
geom_density(alpha = .3) +
theme_bw(base_size = 15)

print(BIS_EffectSizePlot)
ggsave("BIS_EffectSize.png", plot = BIS_EffectSizePlot)

EffectSize<-oneway_test(BIS_EffectSize~Signif.Psych, data=mins.and.maxes,
distribution=approximate(B=9999))
print(EffectSize)

#Create a data frame with derived variables

BIS_Summary<-data.frame("ID" = mins.and.maxes$ID, "BIS_MaxIncrease" = mins.and.maxes$max.diffs, "BIS_EffectSize" = mins.and.maxes$BIS_EffectSize)



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

diff.matrix<-ICDsc[,2:5]-ICDsc[,1]

#Calculate minimum and maximum differences and absolute values

mins.and.maxes<-data.frame(ICD_wide,PreDBS[,c("ID","Age", "ClinicalSubtype", "TremorAkinesiaSubtype")],
anatomical[,c("ID","Signif.Psych")],
min.diffs=apply(diff.matrix,1,min,na.rm=T),
max.diffs=apply(diff.matrix,1,max,na.rm=T))

mins.and.maxes$abs.min.diffs<-abs(mins.and.maxes$min.diffs)
mins.and.maxes$abs.max.diffs<-abs(mins.and.maxes$max.diffs)

#Calculate the measurement furthest from zero

mins.and.maxes$closest.to.zero<-apply(mins.and.maxes[,c("abs.min.diffs","abs.max.diffs")],
1, max)
mins.and.maxes$closest.to.zero.min<-mins.and.maxes$closest.to.zero==mins.and.maxes$abs.min.diffs
mins.and.maxes$closest.to.zero.max<-mins.and.maxes$closest.to.zero==mins.and.maxes$abs.max.diffs

#If there is equal change in positive and negative direction, code this as zero, otherwise take the largest difference from baseline as the derived variable

mins.and.maxes$max.dist.from.zero<-ifelse(mins.and.maxes$abs.min.diffs==mins.and.maxes$abs.max.diffs,
0,mins.and.maxes$max.dist.from.zero<-ifelse(mins.and.maxes$closest.to.zero.max,
mins.and.maxes$max.diffs,
mins.and.maxes$min.diffs))

#Plot the largest increase in ICD and perform one-way test for differences between caseness

ICD_MaxImpairmentPlot<-ggplot(mins.and.maxes, aes(x=max.diffs, color=Signif.Psych)) +
labs(x="Max Increase in ICD", y="density") +
geom_density(alpha = .3) +
theme_bw(base_size = 15)

print(ICD_MaxImpairmentPlot)
ggsave("ICD_MaxIncrease.png", plot = ICD_MaxImpairmentPlot)

MaxChange<-oneway_test(max.diffs~Signif.Psych, data=mins.and.maxes,
distribution=approximate(B=9999))
print(MaxChange)

#We can also calculate the "effect size" of the change using a within-subject standard deviation

mins.and.maxes$standard.deviation<-apply(mins.and.maxes[,c(2:6)],
1, sd, na.rm=T)

mins.and.maxes$ICD_EffectSize<-mins.and.maxes$max.diffs/mins.and.maxes$standard.deviation

ICD_EffectSizePlot<-ggplot(mins.and.maxes, aes(x=ICD_EffectSize, color=Signif.Psych)) +
labs(x="ICD Effect Size", y="density") +
geom_density(alpha = .3) +
theme_bw(base_size = 15)

print(ICD_EffectSizePlot)
ggsave("ICD_EffectSize.png", plot = ICD_EffectSizePlot)

EffectSize<-oneway_test(ICD_EffectSize~Signif.Psych, data=mins.and.maxes,
distribution=approximate(B=9999))
print(EffectSize)

#Create a data frame with derived variables

ICD_Summary<-data.frame("ID" = mins.and.maxes$ID, "ICD_MaxIncrease" = mins.and.maxes$max.diffs, "ICD_EffectSize" = mins.and.maxes$ICD_EffectSize)



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

diff.matrix<-QUIPsc[,2:5]-QUIPsc[,1]

#Calculate minimum and maximum differences and absolute values

mins.and.maxes<-data.frame(QUIP_wide,PreDBS[,c("ID","Age", "ClinicalSubtype", "TremorAkinesiaSubtype")],
anatomical[,c("ID","Signif.Psych")],
min.diffs=apply(diff.matrix,1,min,na.rm=T),
max.diffs=apply(diff.matrix,1,max,na.rm=T))

mins.and.maxes$abs.min.diffs<-abs(mins.and.maxes$min.diffs)
mins.and.maxes$abs.max.diffs<-abs(mins.and.maxes$max.diffs)

#Calculate the measurement furthest from zero

mins.and.maxes$closest.to.zero<-apply(mins.and.maxes[,c("abs.min.diffs","abs.max.diffs")],
1, max)
mins.and.maxes$closest.to.zero.min<-mins.and.maxes$closest.to.zero==mins.and.maxes$abs.min.diffs
mins.and.maxes$closest.to.zero.max<-mins.and.maxes$closest.to.zero==mins.and.maxes$abs.max.diffs

#If there is equal change in positive and negative direction, code this as zero, otherwise take the largest difference from baseline as the derived variable

mins.and.maxes$max.dist.from.zero<-ifelse(mins.and.maxes$abs.min.diffs==mins.and.maxes$abs.max.diffs,
0,mins.and.maxes$max.dist.from.zero<-ifelse(mins.and.maxes$closest.to.zero.max,
mins.and.maxes$max.diffs,
mins.and.maxes$min.diffs))

#Plot the largest increase in QUIP and perform one-way test for differences between caseness

QUIP_MaxImpairmentPlot<-ggplot(mins.and.maxes, aes(x=max.diffs, color=Signif.Psych)) +
labs(x="Max Increase in QUIP", y="density") +
geom_density(alpha = .3) +
theme_bw(base_size = 15)

print(QUIP_MaxImpairmentPlot)
ggsave("QUIP_MaxIncrease.png", plot = QUIP_MaxImpairmentPlot)

MaxChange<-oneway_test(max.diffs~Signif.Psych, data=mins.and.maxes,
distribution=approximate(B=9999))
print(MaxChange)

#We can also calculate the "effect size" of the change using a within-subject standard deviation

mins.and.maxes$standard.deviation<-apply(mins.and.maxes[,c(2:6)],
1, sd, na.rm=T)

mins.and.maxes$QUIP_EffectSize<-mins.and.maxes$max.diffs/mins.and.maxes$standard.deviation

QUIP_EffectSizePlot<-ggplot(mins.and.maxes, aes(x=QUIP_EffectSize, color=Signif.Psych)) +
labs(x="QUIP Effect Size", y="density") +
geom_density(alpha = .3) +
theme_bw(base_size = 15)

print(QUIP_EffectSizePlot)
ggsave("QUIP_EffectSize.png", plot = QUIP_EffectSizePlot)

EffectSize<-oneway_test(QUIP_EffectSize~Signif.Psych, data=mins.and.maxes,
distribution=approximate(B=9999))
print(EffectSize)

#Create a data frame with derived variables

QUIP_Summary<-data.frame("ID" = mins.and.maxes$ID, "QUIP_MaxIncrease" = mins.and.maxes$max.diffs, "QUIP_EffectSize" = mins.and.maxes$QUIP_EffectSize)



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

diff.matrix<-BDIsc[,2:5]-BDIsc[,1]

#Calculate minimum and maximum differences and absolute values

mins.and.maxes<-data.frame(BDI_wide,PreDBS[,c("ID","Age", "ClinicalSubtype", "TremorAkinesiaSubtype")],
anatomical[,c("ID","Signif.Psych")],
min.diffs=apply(diff.matrix,1,min,na.rm=T),
max.diffs=apply(diff.matrix,1,max,na.rm=T))

mins.and.maxes$abs.min.diffs<-abs(mins.and.maxes$min.diffs)
mins.and.maxes$abs.max.diffs<-abs(mins.and.maxes$max.diffs)

#Calculate the measurement furthest from zero

mins.and.maxes$closest.to.zero<-apply(mins.and.maxes[,c("abs.min.diffs","abs.max.diffs")],
1, max)
mins.and.maxes$closest.to.zero.min<-mins.and.maxes$closest.to.zero==mins.and.maxes$abs.min.diffs
mins.and.maxes$closest.to.zero.max<-mins.and.maxes$closest.to.zero==mins.and.maxes$abs.max.diffs

#If there is equal change in positive and negative direction, code this as zero, otherwise take the largest difference from baseline as the derived variable

mins.and.maxes$max.dist.from.zero<-ifelse(mins.and.maxes$abs.min.diffs==mins.and.maxes$abs.max.diffs,
0,mins.and.maxes$max.dist.from.zero<-ifelse(mins.and.maxes$closest.to.zero.max,
mins.and.maxes$max.diffs,
mins.and.maxes$min.diffs))

#Plot the largest increase in BDI and perform one-way test for differences between caseness

BDI_MaxImpairmentPlot<-ggplot(mins.and.maxes, aes(x=max.diffs, color=Signif.Psych)) +
labs(x="Max Increase in BDI", y="density") +
geom_density(alpha = .3) +
theme_bw(base_size = 15)

print(BDI_MaxImpairmentPlot)
ggsave("BDI_MaxIncrease.png", plot = BDI_MaxImpairmentPlot)

MaxChange<-oneway_test(max.diffs~Signif.Psych, data=mins.and.maxes,
distribution=approximate(B=9999))
print(MaxChange)

#We can also calculate the "effect size" of the change using a within-subject standard deviation

mins.and.maxes$standard.deviation<-apply(mins.and.maxes[,c(2:6)],
1, sd, na.rm=T)

mins.and.maxes$BDI_EffectSize<-mins.and.maxes$max.diffs/mins.and.maxes$standard.deviation

BDI_EffectSizePlot<-ggplot(mins.and.maxes, aes(x=BDI_EffectSize, color=Signif.Psych)) +
labs(x="BDI Effect Size", y="density") +
geom_density(alpha = .3) +
theme_bw(base_size = 15)

print(BDI_EffectSizePlot)
ggsave("BDI_EffectSize.png", plot = BDI_EffectSizePlot)

EffectSize<-oneway_test(BDI_EffectSize~Signif.Psych, data=mins.and.maxes,
distribution=approximate(B=9999))
print(EffectSize)

#Create a data frame with derived variables

BDI_Summary<-data.frame("ID" = mins.and.maxes$ID, "BDI_MaxIncrease" = mins.and.maxes$max.diffs, "BDI_EffectSize" = mins.and.maxes$BDI_EffectSize)



### HAYLING AB ERROR SCORE ###

Hayling<-Imputed[,c("ID","TimepointNum",
"ClinicalSubtype", "TremorAkinesiaSubtype", "Gender", "Signif.Psych",
"Signif.Psych.CaseYN", "Hayling_ABErrorScore")]

Hayling_wide<-reshape(Hayling, idvar = "ID", timevar = "TimepointNum", direction = "wide",
drop = c("ClinicalSubtype", "TremorAkinesiaSubtype", "Gender", "Signif.Psych", "Signif.Psych.CaseYN"))

#Create difference matrix and z-score matrix

Hayling<-Hayling_wide[,2:6]
Haylingsc<-Hayling
Haylingsc[,1]<-scale(Hayling[,1], center = TRUE, scale = TRUE)
Haylingsc[,2]<-scale(Hayling[,2], center = TRUE, scale = TRUE)
Haylingsc[,3]<-scale(Hayling[,3], center = TRUE, scale = TRUE)
Haylingsc[,4]<-scale(Hayling[,4], center = TRUE, scale = TRUE)
Haylingsc[,5]<-scale(Hayling[,5], center = TRUE, scale = TRUE)

diff.matrix<-Haylingsc[,2:5]-Haylingsc[,1]

#Calculate minimum and maximum differences and absolute values

mins.and.maxes<-data.frame(Hayling_wide,PreDBS[,c("ID","Age", "ClinicalSubtype", "TremorAkinesiaSubtype")],
anatomical[,c("ID","Signif.Psych")],
min.diffs=apply(diff.matrix,1,min,na.rm=T),
max.diffs=apply(diff.matrix,1,max,na.rm=T))

mins.and.maxes$abs.min.diffs<-abs(mins.and.maxes$min.diffs)
mins.and.maxes$abs.max.diffs<-abs(mins.and.maxes$max.diffs)

#Calculate the measurement furthest from zero

mins.and.maxes$closest.to.zero<-apply(mins.and.maxes[,c("abs.min.diffs","abs.max.diffs")],
1, max)
mins.and.maxes$closest.to.zero.min<-mins.and.maxes$closest.to.zero==mins.and.maxes$abs.min.diffs
mins.and.maxes$closest.to.zero.max<-mins.and.maxes$closest.to.zero==mins.and.maxes$abs.max.diffs

#If there is equal change in positive and negative direction, code this as zero, otherwise take the largest difference from baseline as the derived variable

mins.and.maxes$max.dist.from.zero<-ifelse(mins.and.maxes$abs.min.diffs==mins.and.maxes$abs.max.diffs,
0,mins.and.maxes$max.dist.from.zero<-ifelse(mins.and.maxes$closest.to.zero.max,
mins.and.maxes$max.diffs,
mins.and.maxes$min.diffs))

#Plot the largest increase in Hayling and perform one-way test for differences between caseness

Hayling_MaxImpairmentPlot<-ggplot(mins.and.maxes, aes(x=max.diffs, color=Signif.Psych)) +
labs(x="Max Increase in Hayling AB Error Score", y="density") +
geom_density(alpha = .3) +
theme_bw(base_size = 15)

print(Hayling_MaxImpairmentPlot)
ggsave("Hayling_MaxIncrease.png", plot = Hayling_MaxImpairmentPlot)

MaxChange<-oneway_test(max.diffs~Signif.Psych, data=mins.and.maxes,
distribution=approximate(B=9999))
print(MaxChange)

#We can also calculate the "effect size" of the change using a within-subject standard deviation

mins.and.maxes$standard.deviation<-apply(mins.and.maxes[,c(2:6)],
1, sd, na.rm=T)

mins.and.maxes$Hayling_EffectSize<-mins.and.maxes$max.diffs/mins.and.maxes$standard.deviation

Hayling_EffectSizePlot<-ggplot(mins.and.maxes, aes(x=Hayling_EffectSize, color=Signif.Psych)) +
labs(x="Hayling_EffectSize", y="density") +
geom_density(alpha = .3) +
theme_bw(base_size = 15)

print(Hayling_EffectSizePlot)
ggsave("Hayling Error Score Effect Size.png", plot = Hayling_EffectSizePlot)

EffectSize<-oneway_test(Hayling_EffectSize~Signif.Psych, data=mins.and.maxes,
distribution=approximate(B=9999))
print(EffectSize)

#Create a data frame with derived variables

Hayling_Summary<-data.frame("ID" = mins.and.maxes$ID, "Hayling_MaxIncrease" = mins.and.maxes$max.diffs, "Hayling_EffectSize" = mins.and.maxes$Hayling_EffectSize)



### EXCLUDED LETTER FLUENCY ###

ELF<-Imputed[,c("ID","TimepointNum",
"ClinicalSubtype", "TremorAkinesiaSubtype", "Gender", "Signif.Psych",
"Signif.Psych.CaseYN", "ELF_RuleViolations")]

ELF_wide<-reshape(ELF, idvar = "ID", timevar = "TimepointNum", direction = "wide",
drop = c("ClinicalSubtype", "TremorAkinesiaSubtype", "Gender", "Signif.Psych", "Signif.Psych.CaseYN"))

#Create difference matrix and z-score matrix

ELF<-ELF_wide[,2:6]
ELFsc<-ELF
ELFsc[,1]<-scale(ELF[,1], center = TRUE, scale = TRUE)
ELFsc[,2]<-scale(ELF[,2], center = TRUE, scale = TRUE)
ELFsc[,3]<-scale(ELF[,3], center = TRUE, scale = TRUE)
ELFsc[,4]<-scale(ELF[,4], center = TRUE, scale = TRUE)
ELFsc[,5]<-scale(ELF[,5], center = TRUE, scale = TRUE)

diff.matrix<-ELFsc[,2:5]-ELFsc[,1]

#Calculate minimum and maximum differences and absolute values

mins.and.maxes<-data.frame(ELF_wide,PreDBS[,c("ID","Age", "ClinicalSubtype", "TremorAkinesiaSubtype")],
anatomical[,c("ID","Signif.Psych")],
min.diffs=apply(diff.matrix,1,min,na.rm=T),
max.diffs=apply(diff.matrix,1,max,na.rm=T))

mins.and.maxes$abs.min.diffs<-abs(mins.and.maxes$min.diffs)
mins.and.maxes$abs.max.diffs<-abs(mins.and.maxes$max.diffs)

#Calculate the measurement furthest from zero

mins.and.maxes$closest.to.zero<-apply(mins.and.maxes[,c("abs.min.diffs","abs.max.diffs")],
1, max)
mins.and.maxes$closest.to.zero.min<-mins.and.maxes$closest.to.zero==mins.and.maxes$abs.min.diffs
mins.and.maxes$closest.to.zero.max<-mins.and.maxes$closest.to.zero==mins.and.maxes$abs.max.diffs

#If there is equal change in positive and negative direction, code this as zero, otherwise take the largest difference from baseline as the derived variable

mins.and.maxes$max.dist.from.zero<-ifelse(mins.and.maxes$abs.min.diffs==mins.and.maxes$abs.max.diffs,
0,mins.and.maxes$max.dist.from.zero<-ifelse(mins.and.maxes$closest.to.zero.max,
mins.and.maxes$max.diffs,
mins.and.maxes$min.diffs))

#Plot the largest increase in ELF and perform one-way test for differences between caseness

ELF_MaxImpairmentPlot<-ggplot(mins.and.maxes, aes(x=max.diffs, color=Signif.Psych)) +
labs(x="Max Increase in ELF Rule Violations", y="density") +
geom_density(alpha = .3) +
theme_bw(base_size = 15)

print(ELF_MaxImpairmentPlot)
ggsave("ELF_MaxIncrease.png", plot = ELF_MaxImpairmentPlot)

MaxChange<-oneway_test(max.diffs~Signif.Psych, data=mins.and.maxes,
distribution=approximate(B=9999))
print(MaxChange)

#We can also calculate the "effect size" of the change using a within-subject standard deviation

mins.and.maxes$standard.deviation<-apply(mins.and.maxes[,c(2:6)],
1, sd, na.rm=T)

mins.and.maxes$ELF_EffectSize<-mins.and.maxes$max.diffs/mins.and.maxes$standard.deviation

ELF_EffectSizePlot<-ggplot(mins.and.maxes, aes(x=ELF_EffectSize, color=Signif.Psych)) +
labs(x="ELF Rule Violations Effect Size", y="density") +
geom_density(alpha = .3) +
theme_bw(base_size = 15)

print(ELF_EffectSizePlot)
ggsave("ELF_EffectSize.png", plot = ELF_EffectSizePlot)

EffectSize<-oneway_test(ELF_EffectSize~Signif.Psych, data=mins.and.maxes,
distribution=approximate(B=9999))
print(EffectSize)

#Create a data frame with derived variables

ELF_Summary<-data.frame("ID" = mins.and.maxes$ID, "ELF_MaxIncrease" = mins.and.maxes$max.diffs, "ELF_EffectSize" = mins.and.maxes$ELF_EffectSize)



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

diff.matrix<-LEDDsc[,2:5]-LEDDsc[,1]

#Calculate minimum and maximum differences and absolute values

mins.and.maxes<-data.frame(LEDD_wide,PreDBS[,c("ID","Age", "ClinicalSubtype", "TremorAkinesiaSubtype")],
anatomical[,c("ID","Signif.Psych")],
min.diffs=apply(diff.matrix,1,min,na.rm=T),
max.diffs=apply(diff.matrix,1,max,na.rm=T))

mins.and.maxes$abs.min.diffs<-abs(mins.and.maxes$min.diffs)
mins.and.maxes$abs.max.diffs<-abs(mins.and.maxes$max.diffs)

#Calculate the measurement furthest from zero

mins.and.maxes$closest.to.zero<-apply(mins.and.maxes[,c("abs.min.diffs","abs.max.diffs")],
1, max)
mins.and.maxes$closest.to.zero.min<-mins.and.maxes$closest.to.zero==mins.and.maxes$abs.min.diffs
mins.and.maxes$closest.to.zero.max<-mins.and.maxes$closest.to.zero==mins.and.maxes$abs.max.diffs

#If there is equal change in positive and negative direction, code this as zero, otherwise take the largest difference from baseline as the derived variable

mins.and.maxes$max.dist.from.zero<-ifelse(mins.and.maxes$abs.min.diffs==mins.and.maxes$abs.max.diffs,
0,mins.and.maxes$max.dist.from.zero<-ifelse(mins.and.maxes$closest.to.zero.max,
mins.and.maxes$max.diffs,
mins.and.maxes$min.diffs))

#Plot the largest decrease in LEDD and perform one-way test for differences between caseness

LEDD_MaxDecrease<-ggplot(mins.and.maxes, aes(x=min.diffs, color=Signif.Psych)) +
labs(x="Max Decrease in LEDD", y="density") +
geom_density(alpha = .3) +
theme_bw(base_size = 15)

print(LEDD_MaxDecrease)
ggsave("LEDD_MaxDecrease.png", plot = LEDD_MaxDecrease)

MaxChange<-oneway_test(min.diffs~Signif.Psych, data=mins.and.maxes,
distribution=approximate(B=9999))
print(MaxChange)

#We can also calculate the "effect size" of the change using a within-subject standard deviation

mins.and.maxes$standard.deviation<-apply(mins.and.maxes[,c(2:6)],
1, sd, na.rm=T)

mins.and.maxes$LEDD_EffectSize<-mins.and.maxes$min.diffs/mins.and.maxes$standard.deviation

LEDD_EffectSizePlot<-ggplot(mins.and.maxes, aes(x=LEDD_EffectSize, color=Signif.Psych)) +
labs(x="LEDD Reduction Effect Size", y="density") +
geom_density(alpha = .3) +
theme_bw(base_size = 15)

print(LEDD_EffectSizePlot)
ggsave("LEDD_Reduction_EffectSize.png", plot = LEDD_EffectSizePlot)

EffectSize<-oneway_test(LEDD_EffectSize~Signif.Psych, data=mins.and.maxes,
distribution=approximate(B=9999))
print(EffectSize)

#Create a data frame with derived variables

LEDD_Summary<-data.frame("ID" = mins.and.maxes$ID, "LEDD_MaxDecrease" = mins.and.maxes$min.diffs, "LEDD_EffectSize" = mins.and.maxes$LEDD_EffectSize)



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

diff.matrix<-UPDRSsc[,2:5]-UPDRSsc[,1]

#Calculate minimum and maximum differences and absolute values

mins.and.maxes<-data.frame(UPDRS_wide,PreDBS[,c("ID","Age", "ClinicalSubtype", "TremorAkinesiaSubtype")],
anatomical[,c("ID","Signif.Psych")],
min.diffs=apply(diff.matrix,1,min,na.rm=T),
max.diffs=apply(diff.matrix,1,max,na.rm=T))

mins.and.maxes$abs.min.diffs<-abs(mins.and.maxes$min.diffs)
mins.and.maxes$abs.max.diffs<-abs(mins.and.maxes$max.diffs)

#Calculate the measurement furthest from zero

mins.and.maxes$closest.to.zero<-apply(mins.and.maxes[,c("abs.min.diffs","abs.max.diffs")],
1, max)
mins.and.maxes$closest.to.zero.min<-mins.and.maxes$closest.to.zero==mins.and.maxes$abs.min.diffs
mins.and.maxes$closest.to.zero.max<-mins.and.maxes$closest.to.zero==mins.and.maxes$abs.max.diffs

#If there is equal change in positive and negative direction, code this as zero, otherwise take the largest difference from baseline as the derived variable

mins.and.maxes$max.dist.from.zero<-ifelse(mins.and.maxes$abs.min.diffs==mins.and.maxes$abs.max.diffs,
0,mins.and.maxes$max.dist.from.zero<-ifelse(mins.and.maxes$closest.to.zero.max,
mins.and.maxes$max.diffs,
mins.and.maxes$min.diffs))

#Plot the largest decrease in UPDRS and perform one-way test for differences between caseness

UPDRS_MaxBenefitPlot<-ggplot(mins.and.maxes, aes(x=min.diffs, color=Signif.Psych)) +
labs(x="Max Decrease in UPDRS", y="density") +
geom_density(alpha = .3) +
theme_bw(base_size = 15)

print(UPDRS_MaxBenefitPlot)
ggsave("UPDRS_MaxDecrease.png", plot = UPDRS_MaxBenefitPlot)

MaxChange<-oneway_test(min.diffs~Signif.Psych, data=mins.and.maxes,
distribution=approximate(B=9999))
print(MaxChange)

#We can also calculate the "effect size" of the change using a within-subject standard deviation

mins.and.maxes$standard.deviation<-apply(mins.and.maxes[,c(2:6)],
1, sd, na.rm=T)

mins.and.maxes$UPDRS_EffectSize<-mins.and.maxes$min.diffs/mins.and.maxes$standard.deviation

UPDRS_EffectSizePlot<-ggplot(mins.and.maxes, aes(x=UPDRS_EffectSize, color=Signif.Psych)) +
labs(x="UPDRS Effect Size", y="density") +
geom_density(alpha = .3) +
theme_bw(base_size = 15)

print(UPDRS_EffectSizePlot)
ggsave("UPDRS_EffectSize.png", plot = UPDRS_EffectSizePlot)

EffectSize<-oneway_test(UPDRS_EffectSize~Signif.Psych, data=mins.and.maxes,
distribution=approximate(B=9999))
print(EffectSize)

#Create a data frame with derived variables

UPDRS_Total_Summary<-data.frame("ID" = mins.and.maxes$ID, "UPDRS_MaxDecrease" = mins.and.maxes$min.diffs, "UPDRS_EffectSize" = mins.and.maxes$UPDRS_EffectSize)



### UPDRS LEFT (RIGHT STN) ###

UPDRS_Left<-Imputed[,c("ID","TimepointNum",
"ClinicalSubtype", "TremorAkinesiaSubtype", "Gender", "Age", "Signif.Psych",
"Signif.Psych.CaseYN", "UPDRS_Left")]

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

diff.matrix<-UPDRS_Leftsc[,2:5]-UPDRS_Leftsc[,1]

#Calculate minimum and maximum differences and absolute values

mins.and.maxes<-data.frame(UPDRS_Left_wide,PreDBS[,c("ID","Age", "ClinicalSubtype", "TremorAkinesiaSubtype")],
anatomical[,c("ID","Signif.Psych")],
min.diffs=apply(diff.matrix,1,min,na.rm=T),
max.diffs=apply(diff.matrix,1,max,na.rm=T))

mins.and.maxes$abs.min.diffs<-abs(mins.and.maxes$min.diffs)
mins.and.maxes$abs.max.diffs<-abs(mins.and.maxes$max.diffs)

#Calculate the measurement furthest from zero

mins.and.maxes$closest.to.zero<-apply(mins.and.maxes[,c("abs.min.diffs","abs.max.diffs")],
1, max)
mins.and.maxes$closest.to.zero.min<-mins.and.maxes$closest.to.zero==mins.and.maxes$abs.min.diffs
mins.and.maxes$closest.to.zero.max<-mins.and.maxes$closest.to.zero==mins.and.maxes$abs.max.diffs

#If there is equal change in positive and negative direction, code this as zero, otherwise take the largest difference from baseline as the derived variable

mins.and.maxes$max.dist.from.zero<-ifelse(mins.and.maxes$abs.min.diffs==mins.and.maxes$abs.max.diffs,
0,mins.and.maxes$max.dist.from.zero<-ifelse(mins.and.maxes$closest.to.zero.max,
mins.and.maxes$max.diffs,
mins.and.maxes$min.diffs))

#Plot the largest decrease in UPDRS_Left and perform one-way test for differences between caseness

UPDRS_Left_MaxBenefitPlot<-ggplot(mins.and.maxes, aes(x=min.diffs, color=Signif.Psych)) +
labs(x="Max Decrease in UPDRS_Left", y="density") +
geom_density(alpha = .3) +
theme_bw(base_size = 15)

print(UPDRS_Left_MaxBenefitPlot)
ggsave("UPDRS_Left_MaxDecrease.png", plot = UPDRS_Left_MaxBenefitPlot)

MaxChange<-oneway_test(min.diffs~Signif.Psych, data=mins.and.maxes,
distribution=approximate(B=9999))
print(MaxChange)

#We can also calculate the "effect size" of the change using a within-subject standard deviation

mins.and.maxes$standard.deviation<-apply(mins.and.maxes[,c(2:6)],
1, sd, na.rm=T)

mins.and.maxes$UPDRS_Left_EffectSize<-mins.and.maxes$min.diffs/mins.and.maxes$standard.deviation

UPDRS_Left_EffectSizePlot<-ggplot(mins.and.maxes, aes(x=UPDRS_Left_EffectSize, color=Signif.Psych)) +
labs(x="UPDRS_Left Effect Size", y="density") +
geom_density(alpha = .3) +
theme_bw(base_size = 15)

print(UPDRS_Left_EffectSizePlot)
ggsave("UPDRS_Left_EffectSize.png", plot = UPDRS_Left_EffectSizePlot)

EffectSize<-oneway_test(UPDRS_Left_EffectSize~Signif.Psych, data=mins.and.maxes,
distribution=approximate(B=9999))
print(EffectSize)

#Create a data frame with derived variables

UPDRS_Left_Summary<-data.frame("ID" = mins.and.maxes$ID, "UPDRS_Left_MaxDecrease" = mins.and.maxes$min.diffs, "UPDRS_Left_EffectSize" = mins.and.maxes$UPDRS_Left_EffectSize)



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

diff.matrix<-UPDRS_Rightsc[,2:5]-UPDRS_Rightsc[,1]

#Calculate minimum and maximum differences and absolute values

mins.and.maxes<-data.frame(UPDRS_Right_wide,PreDBS[,c("ID","Age", "ClinicalSubtype", "TremorAkinesiaSubtype")],
anatomical[,c("ID","Signif.Psych")],
min.diffs=apply(diff.matrix,1,min,na.rm=T),
max.diffs=apply(diff.matrix,1,max,na.rm=T))

mins.and.maxes$abs.min.diffs<-abs(mins.and.maxes$min.diffs)
mins.and.maxes$abs.max.diffs<-abs(mins.and.maxes$max.diffs)

#Calculate the measurement furthest from zero

mins.and.maxes$closest.to.zero<-apply(mins.and.maxes[,c("abs.min.diffs","abs.max.diffs")],
1, max)
mins.and.maxes$closest.to.zero.min<-mins.and.maxes$closest.to.zero==mins.and.maxes$abs.min.diffs
mins.and.maxes$closest.to.zero.max<-mins.and.maxes$closest.to.zero==mins.and.maxes$abs.max.diffs

#If there is equal change in positive and negative direction, code this as zero, otherwise take the largest difference from baseline as the derived variable

mins.and.maxes$max.dist.from.zero<-ifelse(mins.and.maxes$abs.min.diffs==mins.and.maxes$abs.max.diffs,
0,mins.and.maxes$max.dist.from.zero<-ifelse(mins.and.maxes$closest.to.zero.max,
mins.and.maxes$max.diffs,
mins.and.maxes$min.diffs))

#Plot the largest decrease in UPDRS_Right and perform one-way test for differences between caseness

UPDRS_Right_MaxBenefitPlot<-ggplot(mins.and.maxes, aes(x=min.diffs, color=Signif.Psych)) +
labs(x="Max Decrease in UPDRS_Right", y="density") +
geom_density(alpha = .3) +
theme_bw(base_size = 15)

print(UPDRS_Right_MaxBenefitPlot)
ggsave("UPDRS_Right_MaxDecrease.png", plot = UPDRS_Right_MaxBenefitPlot)

MaxChange<-oneway_test(min.diffs~Signif.Psych, data=mins.and.maxes,
distribution=approximate(B=9999))
print(MaxChange)

#We can also calculate the "effect size" of the change using a within-subject standard deviation

mins.and.maxes$standard.deviation<-apply(mins.and.maxes[,c(2:6)],
1, sd, na.rm=T)

mins.and.maxes$UPDRS_Right_EffectSize<-mins.and.maxes$min.diffs/mins.and.maxes$standard.deviation

UPDRS_Right_EffectSizePlot<-ggplot(mins.and.maxes, aes(x=UPDRS_Right_EffectSize, color=Signif.Psych)) +
labs(x="UPDRS_Right Effect Size", y="density") +
geom_density(alpha = .3) +
theme_bw(base_size = 15)

print(UPDRS_Right_EffectSizePlot)
ggsave("UPDRS_Right_EffectSize.png", plot = UPDRS_Right_EffectSizePlot)

EffectSize<-oneway_test(UPDRS_Right_EffectSize~Signif.Psych, data=mins.and.maxes,
distribution=approximate(B=9999))
print(EffectSize)

#Create a data frame with derived variables

UPDRS_Right_Summary<-data.frame("ID" = mins.and.maxes$ID, "UPDRS_Right_MaxDecrease" = mins.and.maxes$min.diffs, "UPDRS_Right_EffectSize" = mins.and.maxes$UPDRS_Right_EffectSize)



### CREATE A SUMMARY CSV ###

Max_Change<-merge(CarerEQ_Summary, EQ_Summary, by="ID")
Max_Change<-merge(Max_Change, CarerBIS_Summary, by="ID")
Max_Change<-merge(Max_Change, BIS_Summary, by="ID")
Max_Change<-merge(Max_Change, ICD_Summary, by="ID")
Max_Change<-merge(Max_Change, QUIP_Summary, by="ID")
Max_Change<-merge(Max_Change, BDI_Summary, by="ID")
Max_Change<-merge(Max_Change, Hayling_Summary, by="ID")
Max_Change<-merge(Max_Change, ELF_Summary, by="ID")
Max_Change<-merge(Max_Change, LEDD_Summary, by="ID")
Max_Change<-merge(Max_Change, UPDRS_Total_Summary, by="ID")
Max_Change<-merge(Max_Change, UPDRS_Left_Summary, by="ID")
Max_Change<-merge(Max_Change, UPDRS_Right_Summary, by="ID")

write.csv(Max_Change, file = "Max_Change.csv", row.names = FALSE)



