library(GGally)
library(ggplot2)
library(dplyr)
library(ggpubr)
library(lattice)
library(MASS)

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

xdmg_nd = xdmg_nd %>% mutate(WeatherType = case_when(
  WEATHER == 1 ~ "clear",
  WEATHER %in% c(2, 4) ~ "obscure",
  WEATHER %in% c(3, 5, 6) ~ "precipitation"))
xdmg_nd$WeatherType = as.factor(xdmg_nd$WeatherType)


      #VISIBILITY - no interaction with Rain
bwplot(VISIBLTY ~ log(ACCDMG), xdmg_nd)
ggplot(xdmg_nd, aes(x=VISIBLTY)) + geom_bar()
ggplot(xdmg_nd, aes(x=VISIBLTY, y=ACCDMG)) + geom_bar(stat="identity")
interaction.plot(xdmg_nd$Rain, xdmg_nd$VISIBLTY, xdmg_nd$ACCDMG)
      #TYPTRK - interaction term with Rain
interaction.plot(xdmg_nd$Rain, xdmg_nd$TYPTRK, xdmg_nd$ACCDMG)
      #CAUSE - no interaction with freight or Rain
bwplot(Cause~log(ACCDMG), xdmg_nd)
ggplot(xdmg_nd, aes(x=Cause)) + geom_bar()
ggplot(xdmg_nd, aes(x=Cause, y=ACCDMG)) + geom_bar(stat="identity")
xdmg_nd$Rack = ifelse(xdmg_nd$Cause == "T", 1, 0)
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

casualties_nd %>% group_by(Cause) %>% mutate(Prop_Cas = Casualty/sum(casualties_nd$Casualty)) %>%
  ggplot(aes(x=Cause, y=Prop_Cas)) + geom_bar(stat="identity")

casualties_nd$Human = ifelse(casualties_nd$Cause == "H", 1,0)

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
#ACCDMG Freight, TRNSPD, WeatherType, Rack
interaction.plot(xdmg_nd$WeatherType, xdmg_nd$Freight, xdmg_nd$ACCDMG)
#ACCDMG MODEL 1
xdmg.lm1 = lm(ACCDMG~(Freight + TRNSPD + WeatherType + Rack)^2, data=xdmg_nd)
xdmg.lm2 = lm(ACCDMG~(Freight + TRNSPD + WeatherType + Rack + WeatherType*Freight), data=xdmg_nd)
xdmg.lm1.step = step(xdmg.lm1, trace=F)
xdmg.lm2.step = step(xdmg.lm2, trace=F)

#lm1.step is better than the lm1
anova(xdmg.lm1, xdmg.lm1.step)

#lm2.step is better than the lm2
anova(xdmg.lm2, xdmg.lm2.step)

#lm1.step is better than lm2.step
anova(xdmg.lm1.step, xdmg.lm2.step)

summary(xdmg.lm1.step)

AIC(xdmg.lm1.step)

#DIAGNOSTICS
#QQPLOT
plot(xdmg.lm1.step, which=2)
#RESIDUALS
plot(xdmg.lm1.step, which=1)
library(olsrr)
ols_test_breusch_pagan(xdmg.lm1.step)
#COOKS DISTANCE
plot(xdmg.lm1.step, which=4)
#RESIDUALS AND LEVERAGES
plot(xdmg.lm1.step, which=5)

#BOXCOX
boxcox(xdmg.lm1.step, plotit=T)
#lambda
boxcox(xdmg.lm1.step, plotit = F)$x[which.max(boxcox(xdmg.lm1.step, plotit = F)$y)] 
#max y value
max(boxcox(xdmg.lm1.step, plotit = F)$y)
#store best lambda
L<-boxcox(xdmg.lm1.step, plotit = F)$x[which.max(boxcox(xdmg.lm1.step, plotit = F)$y)] 
#model with best lambda
xdmg.lm1.boxcox<-lm(ACCDMG^L ~ Freight + TRNSPD + WeatherType + Rack + 
                      Freight*TRNSPD + Freight*WeatherType + TRNSPD*WeatherType + 
                      TRNSPD*Rack + WeatherType*Rack,data=xdmg_nd)
plot(xdmg.lm1.boxcox, which=4)
plot(xdmg.lm1.boxcox, which=1)
ols_test_breusch_pagan(xdmg.lm1.boxcox)
plot(xdmg.lm1.boxcox, which=2)
summary(xdmg.lm1.boxcox)
AIC(xdmg.lm1.boxcox)

#remove leverage points 5900, 5337
xdmg.lm1_2 = lm(ACCDMG~(Freight + TRNSPD + WeatherType + Rack)^2, data=xdmg_nd[-c(5900,5337),])
xdmg.lm1_2.step = step(xdmg.lm1_2, trace=F)
#lm1_2.step is better than the lm1
anova(xdmg.lm1_2, xdmg.lm1_2.step)
summary(xdmg.lm1_2.step)
AIC(xdmg.lm1_2.step)

plot(xdmg.lm1_2.step, which=1)
ols_test_breusch_pagan(xdmg.lm1_2.step)
plot(xdmg.lm1_2.step, which=2)

#REMOVED DATA BOXCOX
boxcox(xdmg.lm1_2.step, plotit=T)
#lambda
boxcox(xdmg.lm1_2.step, plotit = F)$x[which.max(boxcox(xdmg.lm1_2.step, plotit = F)$y)] 
#max y value
max(boxcox(xdmg.lm1_2.step, plotit = F)$y)
#store best lambda
L<-boxcox(xdmg.lm1_2.step, plotit = F)$x[which.max(boxcox(xdmg.lm1_2.step, plotit = F)$y)] 
#model with best lambda
xdmg.lm1_2.boxcox<-lm(ACCDMG^L ~ Freight + TRNSPD + WeatherType + Rack + 
                        Freight*Rack + TRNSPD*Rack + WeatherType*Rack, data = xdmg_nd[-c(5900, 
                                                                                         5337), ])
plot(xdmg.lm1_2.boxcox, which=4)
plot(xdmg.lm1_2.boxcox, which=1)
ols_test_breusch_pagan(xdmg.lm1_2.boxcox)
plot(xdmg.lm1_2.boxcox, which=2)
summary(xdmg.lm1_2.boxcox)
AIC(xdmg.lm1_2.boxcox)


#CASUALTY HRCrossing, LOADP1, Human
#CASUALTY MODEL 1
casualty.lm1 = lm(Casualty~(HRCrossing + LOADP1 + Human + Night)^2, casualties_nd)
casualty.lm1.step = step(casualty.lm1, trace=F)
#lm1.step better
anova(casualty.lm1, casualty.lm1.step)
summary(casualty.lm1.step)
AIC(casualty.lm1.step)

#DIAGNOSTICS
plot(casualty.lm1.step, which=1)
ols_test_breusch_pagan(casualty.lm1.step)
plot(casualty.lm1.step, which=2)
plot(casualty.lm1.step, which=4)

#REMOVE 205 NEW MODEL
casualty.lm1_2 = lm(Casualty~(HRCrossing + LOADP1 + Human + Night)^2, casualties_nd[-205,])
casualty.lm1_2.step = step(casualty.lm1_2, trace=F)
#lm1_2.step better
anova(casualty.lm1_2, casualty.lm1_2.step)
summary(casualty.lm1_2.step)
AIC(casualty.lm1_2.step)

#DIAGNOSTICS NEW MODEL
plot(casualty.lm1_2.step, which=1)
ols_test_breusch_pagan(casualty.lm1_2.step)
plot(casualty.lm1_2.step, which=2)
plot(casualty.lm1_2.step, which=4)

#REMOVE 205, 372, 2652 NEW MODEL
casualty.lm1_3 = lm(Casualty~(HRCrossing + LOADP1 + Human + Night)^2, casualties_nd[-c(205,372,2652),])
casualty.lm1_3.step = step(casualty.lm1_3, trace=F)
#lm1_3.step better
anova(casualty.lm1_3, casualty.lm1_3.step)
summary(casualty.lm1_3.step)
AIC(casualty.lm1_3.step)

#DIAGNOSTICS NEW MODEL
plot(casualty.lm1_3.step, which=1)
ols_test_breusch_pagan(casualty.lm1_2.step)
plot(casualty.lm1_3.step, which=2)
plot(casualty.lm1_3.step, which=4)

#REMOVED DATA BOXCOX
boxcox(casualty.lm1_3.step, plotit=T)
#lambda
boxcox(casualty.lm1_3.step, plotit = F)$x[which.max(boxcox(casualty.lm1_3.step, plotit = F)$y)] 
#max y value
max(boxcox(casualty.lm1_3.step, plotit = F)$y)
#store best lambda
L<-boxcox(casualty.lm1_3.step, plotit = F)$x[which.max(boxcox(casualty.lm1_3.step, plotit = F)$y)] 
#model with best lambda
casualty.lm1_3.boxcox<-lm(Casualty^L ~ HRCrossing + LOADP1 + Human + Night + 
                            LOADP1*Human + LOADP1*Night, data = casualties_nd[-c(205, 
                                                                                 372, 2652), ])
plot(casualty.lm1_3.boxcox, which=4)
plot(casualty.lm1_3.boxcox, which=1)
ols_test_breusch_pagan(casualty.lm1_3.boxcox)
plot(casualty.lm1_3.boxcox, which=2)
summary(casualty.lm1_3.boxcox)
AIC(casualty.lm1_3.boxcox)
