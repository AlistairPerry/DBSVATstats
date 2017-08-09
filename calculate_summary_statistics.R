---
title: "Phil Mosley PD subjects: Calculate Summary Statistics For Publication"
author: "Phil Mosley"
date: "5 August 2017"
output: html_document
---

#Split the stacked long data frame into separate data frames by timepoint

setwd("/Users/philipm/Dropbox/Philip/Professional/Impulsivity & Caregiver Committment Trial/Key Documents/Recruitment and Data/Data Analysis")

Long<-read.csv("/Users/philipm/Dropbox/Philip/Professional/Impulsivity & Caregiver Committment Trial/Key Documents/Recruitment and Data/Data Analysis/stacked_long_unified_with_cart_imputation.csv")

Split.Long<-split(Long, Long$Timepoint)
Frames <- lapply(seq_along(Split.Long), function(x) as.data.frame(Split.Long[[x]]))
Baseline <- Frames[[1]]
FU1 <- Frames[[2]]
FU2 <- Frames[[3]]
FU3 <- Frames[[4]]
FU4 <- Frames[[5]]

#Calculate summary statistics on variables and combinations of variables

#For factors I used dplyr

library(dplyr)

Group.By.Factors<-
Baseline %>%
group_by(TremorAkinesiaSubtype, Gender) %>%
summarise(Frequency = n(), Percentage = (n()/length(Baseline$ID))*100)

print(Group.By.Factors)

#For numerical variables I used aggregate

mean(Baseline$Age)
sd(Baseline$Age)
median(Baseline$Age)
range(Baseline$Age)

aggregate(Age ~ Gender, data=Baseline, mean)
aggregate(Age ~ Gender, data=Baseline, sd)
aggregate(Age ~ Gender, data=Baseline, median)
aggregate(Age ~ Gender, data=Baseline, range)

aggregate(Age ~ TremorAkinesiaSubtype, data=Baseline, mean)
aggregate(Age ~ TremorAkinesiaSubtype, data=Baseline, sd)
aggregate(Age ~ TremorAkinesiaSubtype, data=Baseline, median)
aggregate(Age ~ TremorAkinesiaSubtype, data=Baseline, range)

aggregate(Age ~ TremorAkinesiaSubtype + Gender, data=Baseline, mean)
aggregate(Age ~ TremorAkinesiaSubtype + Gender, data=Baseline, sd)
aggregate(Age ~ TremorAkinesiaSubtype + Gender, data=Baseline, median)
aggregate(Age ~ TremorAkinesiaSubtype + Gender, data=Baseline, range)

#But I can also use dplyr

Group.By.Numeric<-
Baseline %>%
group_by(TremorAkinesiaSubtype) %>%
summarise(Mean.Age = mean(Age), SD.Age = sd(Age),
Median.Age = median(Age), Min.Age = min(Age),
Max.Age = max(Age))

print(Group.By.Numeric)

#Is there a way to do this more elegantly with a for loop such that all summary statistics can be calculated iteratively? Something like "for i in ... for the numeric and factor variables in Baseline.

