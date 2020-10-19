library(GGally)
library(ggplot2)
library(dplyr)
library(ggpubr)
library(lattice)

sourcedir <- "C:/Users/student/Documents/Fifth Semester/SYS 4021/Source"
traindir <- "C:/Users/student/Documents/Fifth Semester/SYS 4021/Data/TrainData"


# Source AccidentInput
setwd(sourcedir)
source("AccidentInput.R")

# you should have two data structures in working memory
# First - a list of data frames for each year of accident data

acts <- file.inputl(traindir)

# Next a data frame with all accidents from all years from 2001 - 2019

# the combined data frame

totacts <- combine.data(acts)

dmgbox <-boxplot(totacts$ACCDMG)

xdmg <- totacts[totacts$ACCDMG > dmgbox$stats[5],]

xdmg <- xdmg[-186,]

xdmg_nd <- xdmg[!(duplicated(xdmg[, c("INCDTNO", "YEAR", "MONTH", "DAY", "TIMEHR", "TIMEMIN")])),]

xdmg_nd$Cause <- rep(NA, nrow(xdmg_nd))
xdmg_nd$Cause[which(substr(xdmg_nd$CAUSE, 1, 1) == "M")] <- "M"
xdmg_nd$Cause[which(substr(xdmg_nd$CAUSE, 1, 1) == "T")] <- "T"
xdmg_nd$Cause[which(substr(xdmg_nd$CAUSE, 1, 1) == "S")] <- "S"
xdmg_nd$Cause[which(substr(xdmg_nd$CAUSE, 1, 1) == "H")] <- "H"
xdmg_nd$Cause[which(substr(xdmg_nd$CAUSE, 1, 1) == "E")] <- "E"

xdmg_nd$Cause = as.factor(xdmg_nd$Cause)

View(xdmg_nd)

#make casualty column
totacts$Casualty = totacts$TOTKLD + totacts$TOTINJ

#casualities
casualties = totacts %>% filter(Casualty>=1)
casualties_nd <- casualties[!(duplicated(casualties[, c("INCDTNO", "YEAR", "MONTH", "DAY", "TIMEHR", "TIMEMIN")])),]

casualties_nd$Cause <- rep(NA, nrow(casualties_nd))
casualties_nd$Cause[which(substr(casualties_nd$CAUSE, 1, 1) == "M")] <- "M"
casualties_nd$Cause[which(substr(casualties_nd$CAUSE, 1, 1) == "T")] <- "T"
casualties_nd$Cause[which(substr(casualties_nd$CAUSE, 1, 1) == "S")] <- "S"
casualties_nd$Cause[which(substr(casualties_nd$CAUSE, 1, 1) == "H")] <- "H"
casualties_nd$Cause[which(substr(casualties_nd$CAUSE, 1, 1) == "E")] <- "E"

casualties_nd$Cause = as.factor(casualties_nd$Cause)

View(casualties_nd)


########################################################


## QUANTITATIVE VARIABLES
ggpairs(xdmg_nd, columns=c("ACCDMG", "TRNSPD", "TEMP", "TOTKLD", "TOTINJ", "LOADF2"))

pca.xdmg.corr = princomp(xdmg_nd[,c("ACCDMG", "TRNSPD", "TEMP", "Casualty", "LOADF2")], cor=T)
barplot(pca.xdmg.corr$loadings[,1])
barplot(pca.xdmg.corr$loadings[,2])

#PICKING TRNSPD bc SECOND HIGHEST correlation, look for qual variables relating to total kld bc that was highest initial interaction


ggpairs(casualties_nd[,c("Casualty", "ALCOHOL", "LOADP1", "TRNSPD", "CARS")])
pca.casualty.corr = princomp(casualties_nd[,c("Casualty", "LOADP1", "TRNSPD", "CARS")], cor=T)
barplot(pca.casualty.corr$loadings[,1])
barplot(pca.casualty.corr$loadings[,2])

#LOADP1 QUANT

######################################################

#QUALITATIVE
#ACCDMG
      #TYPE
bwplot(TYPE~log(ACCDMG), xdmg_nd)
      #TYPEQ - Freight train or not
bwplot(TYPEQ~log(ACCDMG), xdmg_nd)
ggplot(xdmg_nd, aes(x=TYPEQ)) + geom_bar()
ggplot(xdmg_nd, aes(x=TYPEQ, y=ACCDMG)) + geom_bar(stat="identity")
xdmg_nd$Freight = ifelse(xdmg_nd$TYPEQ==1, 1, 0)
      #WEATHER - RAIN or not
bwplot(WEATHER ~ log(ACCDMG), xdmg_nd)
ggplot(xdmg_nd, aes(x=WEATHER)) + geom_bar()
ggplot(xdmg_nd, aes(x=WEATHER, y=ACCDMG)) + geom_bar(stat="identity") + facet_wrap(~VISIBLTY, scales="free_y")
xdmg_nd$Rain = ifelse(xdmg_nd$WEATHER==3, 1, 0)
      #VISIBILITY - no interaction with Rain
bwplot(VISIBLTY ~ log(ACCDMG), xdmg_nd)
ggplot(xdmg_nd, aes(x=VISIBLTY)) + geom_bar()
ggplot(xdmg_nd, aes(x=VISIBLTY, y=ACCDMG)) + geom_bar(stat="identity")
interaction.plot(xdmg_nd$Rain, xdmg_nd$VISIBLTY, xdmg_nd$ACCDMG)
      #TYPTRK - interaction term with Rain
interaction.plot(xdmg_nd$Rain, xdmg_nd$TYPTRK, xdmg_nd$ACCDMG)
      #CAUSE - no interaction with freight or Rain
bwplot(Cause~log(ACCDMG), xdmg_nd)
interaction.plot(xdmg_nd$Cause, xdmg_nd$Freight, xdmg_nd$ACCDMG)
interaction.plot(xdmg_nd$Cause, xdmg_nd$Rain, xdmg_nd$ACCDMG)

#CASUALTY
    #TYPE - highway rail crossing or not
bwplot(TYPE~log(Casualty), casualties_nd)
ggplot(casualties_nd, aes(x=TYPE)) + geom_bar()
ggplot(casualties_nd, aes(x=TYPE, y=Casualty)) + geom_bar(stat="identity")
casualties_nd$HRCrossing = ifelse(casualties_nd$TYPE==7, 1, 0)
    #CAUSE - interaction with HRCrossing
bwplot(Cause~log(Casualty), casualties_nd)
interaction.plot(casualties_nd$HRCrossing, casualties_nd$Cause, casualties_nd$Casualty)
    #PART OF DAY - Night or not (10pm - 6am)
casualties_nd = casualties_nd %>% mutate(partOfDay = case_when(
  TIMEHR >= 7 & AMPM =="AM" ~ "morning",
  TIMEHR <= 9 & TIMEHR >5 & AMPM =="PM" ~ "evening",
  TIMEHR <=5 & AMPM =="PM" ~ "afternoon",
  TIMEHR > 9 & AMPM == "PM" ~ "night",
  TRUE ~ "night"
))
casualties_nd$partOfDay = as.factor(casualties_nd$partOfDay)
bwplot(partOfDay ~ log(Casualty), casualties_nd)
ggplot(casualties_nd, aes(x=partOfDay)) + geom_bar()
ggplot(casualties_nd, aes(x=partOfDay, y=Casualty)) + geom_bar(stat="identity")
casualties_nd$Night = ifelse(casualties_nd$partOfDay == "night", 1, 0)
    #interaction term Night and WEATHER
interaction.plot(casualties_nd$WEATHER, casualties_nd$Night, casualties_nd$Casualty)
interaction.plot(casualties_nd$Cause, casualties_nd$Night, casualties_nd$Casualty)


###########################################################
#ACCDMG MODEL 1
xdmg.lm1 = lm(ACCDMG~(Freight + TRNSPD + WEATHER + Cause)^2, data=xdmg_nd)
xdmg.lm1.step = step(xdmg.lm1, trace=F)
anova(xdmg.lm1, xdmg.lm1.step)
#ACCDMG MODEL 2
xdmg.lm2 = lm(ACCDMG~(Rain + TRNSPD + TYPTRK)^2, data=xdmg_nd)
xdmg.lm2.step = step(xdmg.lm2, trace=F)
anova(xdmg.lm2, xdmg.lm2.step)
#CASUALTY MODEL 1
casualty.lm1 = lm(Casualty~(HRCrossing + LOADP1 + Cause)^2, casualties_nd)
casualty.lm1.step = step(casualty.lm1, trace=F)
anova(casualty.lm1, casualty.lm1.step)
#CASUALTY MODEL 2
casualty.lm2 = lm(Casualty~(Night + LOADP1 + WEATHER)^2, casualties_nd)
casualty.lm2.step = step(casualty.lm2, trace=F)
anova(casualty.lm2, casualty.lm2.step)
