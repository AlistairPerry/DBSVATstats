---
title: "Phil Mosley PD subjects: Calculate Max Change Across Longitudinal Variables"
author: "Phil Mosley"
date: "27 August 2017"
output: html_document
---

library(ggplot2)

setwd("/Users/philipm/Dropbox/Philip/Professional/Statistical Analyses Parkinsons Disease and DBS/Data sets/07 2017 DATA SET UPDATED/")

#read in imputed data set

Imputed<-read.csv("stacked_long_unified_with_cart_imputation.csv")



### CAREGIVER EQ ###



CarerEQ<-Imputed[,c("ID","TimepointNum",
    "ClinicalSubtype", "TremorAkinesiaSubtype", "Gender", "Signif.Psych",
    "Signif.Psych.CaseYN", "CarerEQ_Total")]

CarerEQ_wide<-reshape(CarerEQ, idvar = "ID", timevar = "TimepointNum", direction = "wide",
    drop = c("ClinicalSubtype", "TremorAkinesiaSubtype", "Gender", "Signif.Psych", "Signif.Psych.CaseYN"))

#Create difference matrix

diff.matrix<-CarerEQ_wide[,3:6]-CarerEQ_wide[,2]

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

EQ_MaxImpairmentPlot<-ggplot(mins.and.maxes, aes(x=max.dist.from.zero, color=Signif.Psych)) +
labs(x="Max Change in Caregiver EQ", y="density") +
geom_density(alpha = .3) +
theme_bw(base_size = 15)

print(EQ_MaxImpairmentPlot)
ggsave("CaregiverEQ_MaxChange.png", plot = EQ_MaxImpairmentPlot)

MaxChange<-oneway_test(max.dist.from.zero~Signif.Psych, data=mins.and.maxes,
distribution=approximate(B=9999))
print(MaxChange)

#We can also calculate the "effect size" of the change using a within-subject standard deviation

mins.and.maxes$standard.deviation<-apply(mins.and.maxes[,c(2:6)],
    1, sd, na.rm=T)

mins.and.maxes$CaregiverEQ_EffectSize<-mins.and.maxes$max.dist.from.zero/mins.and.maxes$standard.deviation

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

Caregiver_EQ<-data.frame("ID" = mins.and.maxes$ID, "CaregiverEQ_MaxChange" = mins.and.maxes$max.dist.from.zero, "CaregiverEQ_EffectSize" = mins.and.maxes$CaregiverEQ_EffectSize)



### HAYLING ###



Hayling<-Imputed[,c("ID","TimepointNum",
"ClinicalSubtype", "TremorAkinesiaSubtype", "Gender", "Signif.Psych",
"Signif.Psych.CaseYN", "Hayling_ABErrorScore")]

Hayling_wide<-reshape(Hayling, idvar = "ID", timevar = "TimepointNum", direction = "wide",
drop = c("ClinicalSubtype", "TremorAkinesiaSubtype", "Gender", "Signif.Psych", "Signif.Psych.CaseYN"))

#Create difference matrix

diff.matrix<-Hayling_wide[,3:6]-Hayling_wide[,2]

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

Hayling_MaxImpairmentPlot<-ggplot(mins.and.maxes, aes(x=max.dist.from.zero, color=Signif.Psych)) +
labs(x="Max Change in Hayling Error Score", y="density") +
geom_density(alpha = .3) +
theme_bw(base_size = 15)

print(Hayling_MaxImpairmentPlot)
ggsave("Hayling_MaxChange.png", plot = Hayling_MaxImpairmentPlot)

MaxChange<-oneway_test(max.dist.from.zero~Signif.Psych, data=mins.and.maxes,
distribution=approximate(B=9999))
print(MaxChange)

#We can also calculate the "effect size" of the change using a within-subject standard deviation

mins.and.maxes$standard.deviation<-apply(mins.and.maxes[,c(2:6)],
1, sd, na.rm=T)

mins.and.maxes$Hayling_EffectSize<-mins.and.maxes$max.dist.from.zero/mins.and.maxes$standard.deviation

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

Hayling_ABErrorScore<-data.frame("ID" = mins.and.maxes$ID, "Hayling_MaxChange" = mins.and.maxes$max.dist.from.zero, "Hayling_EffectSize" = mins.and.maxes$Hayling_EffectSize)



### Excluded Letter Fluency ###



ELF<-Imputed[,c("ID","TimepointNum",
"ClinicalSubtype", "TremorAkinesiaSubtype", "Gender", "Signif.Psych",
"Signif.Psych.CaseYN", "ELF_RuleViolations")]

ELF_wide<-reshape(ELF, idvar = "ID", timevar = "TimepointNum", direction = "wide",
drop = c("ClinicalSubtype", "TremorAkinesiaSubtype", "Gender", "Signif.Psych", "Signif.Psych.CaseYN"))

#Create difference matrix

diff.matrix<-ELF_wide[,3:6]-ELF_wide[,2]

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

ELF_MaxImpairmentPlot<-ggplot(mins.and.maxes, aes(x=max.dist.from.zero, color=Signif.Psych)) +
labs(x="Max Change in ELF Rule Violations", y="density") +
geom_density(alpha = .3) +
theme_bw(base_size = 15)

print(ELF_MaxImpairmentPlot)
ggsave("ELF_MaxChange.png", plot = ELF_MaxImpairmentPlot)

MaxChange<-oneway_test(max.dist.from.zero~Signif.Psych, data=mins.and.maxes,
distribution=approximate(B=9999))
print(MaxChange)

#We can also calculate the "effect size" of the change using a within-subject standard deviation

mins.and.maxes$standard.deviation<-apply(mins.and.maxes[,c(2:6)],
1, sd, na.rm=T)

mins.and.maxes$ELF_EffectSize<-mins.and.maxes$max.dist.from.zero/mins.and.maxes$standard.deviation

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

ELF_RuleViolations<-data.frame("ID" = mins.and.maxes$ID, "ELF_MaxChange" = mins.and.maxes$max.dist.from.zero, "ELF_EffectSize" = mins.and.maxes$ELF_EffectSize)



### Caregiver BIS ###




BIS<-Imputed[,c("ID","TimepointNum",
"ClinicalSubtype", "TremorAkinesiaSubtype", "Gender", "Signif.Psych",
"Signif.Psych.CaseYN", "CarerBIS_Total")]

BIS_wide<-reshape(BIS, idvar = "ID", timevar = "TimepointNum", direction = "wide",
drop = c("ClinicalSubtype", "TremorAkinesiaSubtype", "Gender", "Signif.Psych", "Signif.Psych.CaseYN"))

#Create difference matrix

diff.matrix<-BIS_wide[,3:6]-BIS_wide[,2]

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

BIS_MaxImpairmentPlot<-ggplot(mins.and.maxes, aes(x=max.dist.from.zero, color=Signif.Psych)) +
labs(x="Max Change in Caregiver BIS", y="density") +
geom_density(alpha = .3) +
theme_bw(base_size = 15)

print(BIS_MaxImpairmentPlot)
ggsave("BIS_MaxChange.png", plot = BIS_MaxImpairmentPlot)

MaxChange<-oneway_test(max.dist.from.zero~Signif.Psych, data=mins.and.maxes,
distribution=approximate(B=9999))
print(MaxChange)

#We can also calculate the "effect size" of the change using a within-subject standard deviation

mins.and.maxes$standard.deviation<-apply(mins.and.maxes[,c(2:6)],
1, sd, na.rm=T)

mins.and.maxes$BIS_EffectSize<-mins.and.maxes$max.dist.from.zero/mins.and.maxes$standard.deviation

BIS_EffectSizePlot<-ggplot(mins.and.maxes, aes(x=BIS_EffectSize, color=Signif.Psych)) +
labs(x="Caregiver BIS Effect Size", y="density") +
geom_density(alpha = .3) +
theme_bw(base_size = 15)

print(BIS_EffectSizePlot)
ggsave("BIS_EffectSize.png", plot = BIS_EffectSizePlot)

EffectSize<-oneway_test(BIS_EffectSize~Signif.Psych, data=mins.and.maxes,
distribution=approximate(B=9999))
print(EffectSize)

#Create a data frame with derived variables

CaregiverBIS<-data.frame("ID" = mins.and.maxes$ID, "CaregiverBIS_MaxChange" = mins.and.maxes$max.dist.from.zero, "CaregiverBIS_EffectSize" = mins.and.maxes$BIS_EffectSize)



### UPDRS Total ###



Motor<-Imputed[,c("ID","TimepointNum",
"ClinicalSubtype", "TremorAkinesiaSubtype", "Gender", "Signif.Psych",
"Signif.Psych.CaseYN", "UPDRS_Total")]

Motor_wide<-reshape(Motor, idvar = "ID", timevar = "TimepointNum", direction = "wide",
drop = c("ClinicalSubtype", "TremorAkinesiaSubtype", "Gender", "Signif.Psych", "Signif.Psych.CaseYN"))

#Create difference matrix

diff.matrix<-Motor_wide[,3:6]-Motor_wide[,2]

#Calculate minimum and maximum differences and absolute values

mins.and.maxes<-data.frame(Motor_wide,PreDBS[,c("ID","Age", "ClinicalSubtype", "TremorAkinesiaSubtype")],
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

UPDRS_MaxBenefitPlot<-ggplot(mins.and.maxes, aes(x=max.dist.from.zero, color=Signif.Psych)) +
labs(x="Max Change in UPDRS", y="density") +
geom_density(alpha = .3) +
theme_bw(base_size = 15)

print(UPDRS_MaxBenefitPlot)
ggsave("UPDRS_MaxChange.png", plot = UPDRS_MaxBenefitPlot)

MaxChange<-oneway_test(max.dist.from.zero~Signif.Psych, data=mins.and.maxes,
distribution=approximate(B=9999))
print(MaxChange)

#We can also calculate the "effect size" of the change using a within-subject standard deviation

mins.and.maxes$standard.deviation<-apply(mins.and.maxes[,c(2:6)],
1, sd, na.rm=T)

mins.and.maxes$UPDRS_EffectSize<-mins.and.maxes$max.dist.from.zero/mins.and.maxes$standard.deviation

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

UPDRS<-data.frame("ID" = mins.and.maxes$ID, "UPDRS_MaxChange" = mins.and.maxes$max.dist.from.zero, "UPDRS_EffectSize" = mins.and.maxes$UPDRS_EffectSize)



### CREATE A SUMMARY CSV ###




a<-merge(Caregiver_EQ, Hayling_ABErrorScore, by="ID")
b<-merge(a, ELF_RuleViolations, by="ID")
c<-merge(b, CaregiverBIS, by="ID")
Longitudinal_Change<-merge(c, UPDRS, by="ID")

write.csv(Longitudinal_Change, file = "Longitudinal_Change.csv", row.names = FALSE)



