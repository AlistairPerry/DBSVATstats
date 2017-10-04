---
title: "Phil Mosley PD subjects: Calculate Max Change Across Longitudinal Variables"
author: "Phil Mosley"
date: "27 August 2017"
output: html_document
---

library(ggplot2)

setwd("/Users/philipm/Dropbox/Philip/Professional/Statistical Analyses Parkinsons Disease and DBS/Data sets/08 2017 DATA SET/")

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

### CAREGIVER EQ ###



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
    max.diffs=apply(diff.matrix,1,max,na.rm=T)

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
labs(x="Max Decrease in Caregiver EQ", y="density") +
geom_density(alpha = .3) +
theme_bw(base_size = 15)

print(EQ_MaxImpairmentPlot)
ggsave("CaregiverEQ_MaxDecrease.png", plot = EQ_MaxImpairmentPlot)

MaxChange<-oneway_test(min.diffs~Signif.Psych, data=mins.and.maxes,
distribution=approximate(B=9999))
print(MaxChange)

#We can also calculate the "effect size" of the change using a within-subject standard deviation

mins.and.maxes$standard.deviation<-apply(mins.and.maxes[,c(2:6)],
    1, sd, na.rm=T)

mins.and.maxes$CaregiverEQ_EffectSize<-mins.and.maxes$min.diffs/mins.and.maxes$standard.deviation

EQ_EffectSizePlot<-ggplot(mins.and.maxes, aes(x=CaregiverEQ_EffectSize, color=Signif.Psych)) +
    labs(x="CaregiverEQ Effect Size", y="density") +
    geom_density(alpha = .3) +
    theme_bw(base_size = 15)

print(EQ_EffectSizePlot)
ggsave("CaregiverEQ_EffectSize.png", plot = EQ_EffectSizePlot)

EffectSize<-oneway_test(CaregiverEQ_EffectSize~Signif.Psych, data=mins.and.maxes,
        distribution=approximate(B=9999))
print(EffectSize)

#Create a data frame with derived variables

Caregiver_EQ<-data.frame("ID" = mins.and.maxes$ID, "CaregiverEQ_MaxDecrease" = mins.and.maxes$min.diffs, "CaregiverEQ_EffectSize" = mins.and.maxes$CaregiverEQ_EffectSize)



### HAYLING ###



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
max.diffs=apply(diff.matrix,1,max,na.rm=T)

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
labs(x="Max Increase in Hayling Error Score", y="density") +
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

Hayling_ABErrorScore<-data.frame("ID" = mins.and.maxes$ID, "Hayling_MaxIncrease" = mins.and.maxes$max.diffs, "Hayling_EffectSize" = mins.and.maxes$Hayling_EffectSize)



### Excluded Letter Fluency ###



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
max.diffs=apply(diff.matrix,1,max,na.rm=T)

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

ELF_RuleViolations<-data.frame("ID" = mins.and.maxes$ID, "ELF_MaxIncrease" = mins.and.maxes$max.diffs, "ELF_EffectSize" = mins.and.maxes$ELF_EffectSize)



### Caregiver BIS ###




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
max.diffs=apply(diff.matrix,1,max,na.rm=T)

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

CarerBIS_MaxImpairmentPlot<-ggplot(mins.and.maxes, aes(x=max.diffs, color=Signif.Psych)) +
labs(x="Max Increase in Caregiver BIS", y="density") +
geom_density(alpha = .3) +
theme_bw(base_size = 15)

print(CarerBIS_MaxImpairmentPlot)
ggsave("CarerBIS_MaxIncrease.png", plot = CarerBIS_MaxImpairmentPlot)

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
ggsave("CarerBIS_EffectSize.png", plot = CarerBIS_EffectSizePlot)

EffectSize<-oneway_test(CarerBIS_EffectSize~Signif.Psych, data=mins.and.maxes,
distribution=approximate(B=9999))
print(EffectSize)

#Create a data frame with derived variables

CaregiverBIS<-data.frame("ID" = mins.and.maxes$ID, "CaregiverBIS_MaxIncrease" = mins.and.maxes$max.diffs, "CaregiverBIS_EffectSize" = mins.and.maxes$CarerBIS_EffectSize)



### UPDRS Total ###



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

#Calculate minimum and maximum differences and absolute values

mins.and.maxes<-data.frame(Motor_wide,PreDBS[,c("ID","Age", "ClinicalSubtype", "TremorAkinesiaSubtype")],
anatomical[,c("ID","Signif.Psych")],
min.diffs=apply(diff.matrix,1,min,na.rm=T),
max.diffs=apply(diff.matrix,1,max,na.rm=T)

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

UPDRS<-data.frame("ID" = mins.and.maxes$ID, "UPDRS_MaxDecrease" = mins.and.maxes$min.diffs, "UPDRS_EffectSize" = mins.and.maxes$UPDRS_EffectSize)



### CREATE A SUMMARY CSV ###




a<-merge(Caregiver_EQ, Hayling_ABErrorScore, by="ID")
b<-merge(a, ELF_RuleViolations, by="ID")
c<-merge(b, CaregiverBIS, by="ID")
Longitudinal_Change<-merge(c, UPDRS, by="ID")

write.csv(Longitudinal_Change, file = "Longitudinal_Change.csv", row.names = FALSE)



