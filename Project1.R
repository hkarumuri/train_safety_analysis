library(GGally)

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

###################################################################

#remove duplicates
totacts_nd = totacts[!(duplicated(totacts[, c("INCDTNO", "YEAR", "MONTH", "DAY", "TIMEHR", "TIMEMIN")])),]

#make new time variable
#totacts_nd = totacts_nd %>% mutate(militaryTime = case_when(
#                          AMPM == "PM" & TIMEHR == 12 ~12,
 #                         AMPM == "AM" & TIMEHR == 12 ~0,
  #                        AMPM == "PM" ~ TIMEHR + 12,
   #                       TRUE ~ TIMEHR+0))

#make cause column
totacts_nd$Cause <- rep(NA, nrow(totacts_nd))
totacts_nd$Cause[which(substr(totacts_nd$CAUSE, 1, 1) == "M")] <- "M"
totacts_nd$Cause[which(substr(totacts_nd$CAUSE, 1, 1) == "T")] <- "T"
totacts_nd$Cause[which(substr(totacts_nd$CAUSE, 1, 1) == "S")] <- "S"
totacts_nd$Cause[which(substr(totacts_nd$CAUSE, 1, 1) == "H")] <- "H"
totacts_nd$Cause[which(substr(totacts_nd$CAUSE, 1, 1) == "E")] <- "E"

totacts_nd$Cause = as.factor(totacts_nd$Cause)

#make casualty column
totacts_nd$Casualty = totacts_nd$TOTKLD + totacts_nd$TOTINJ

#make dummy variable for 'dark' vs 'light'
#totacts_nd = totacts_nd %>% mutate(dark_light = case_when(
#                                  militaryTime<=5 | militaryTime>=9 ~ "dark",
#                                  TRUE ~ "light")) %>%
#  mutate(season = case_when(
#    MONTH %in% c(12, 1, 2) ~ "winter",
#    MONTH %in% c(3, 4, 5) ~ "spring",
#    MONTH %in% c(6, 7, 8) ~ "summer",
#    MONTH %in% c(9, 10, 11) ~ "fall"))

#maybe looking into normalizing this actually...
#ggplot(totacts_nd, aes(x=dark_light, y=ACCDMG)) + geom_bar(stat="identity")
#ggplot(totacts_nd, aes(x=season, y=ACCDMG)) + geom_bar(stat="identity")

#NEW VARIABLES MADE
#if speed is greater than or equal to 13.82048 (mean trainspeed), 1, else 0
#mean of trainspeed doesnt include 0s (means trainspeed is unknown)
totacts_nd$DummySpeed <- ifelse(totacts_nd$TRNSPD >= mean(totacts_nd[totacts_nd$TRNSPD != 0,]$TRNSPD), 1, 0) 

totacts_nd$totalCars <- totacts_nd$LOADF1 + totacts_nd$LOADP1 + totacts_nd$EMPTYF1 + totacts_nd$EMPTYP1 + totacts_nd$CABOOSE1

#if no loaded passanger cars, just equal 0, else find proportion
totacts_nd$propDerailedCars <- ifelse(totacts_nd$LOADP1 == 0, 0, totacts_nd$LOADP2/totacts_nd$LOADP1)

#https://www.networkrail.co.uk/running-the-railway/looking-after-the-railway/delays-explained/buckled-rail-and-summer-heat/
# Stated temperature can be handled up to 86 degrees
totacts_nd$isHot <- ifelse(totacts_nd$TEMP > 86, 1, 0)
# https://trn.trains.com/railroads/abcs-of-railroading/2018/01/inconveniently-frozen-truth
# Article stated issues specifically with sub 0 temperatures
totacts_nd$isCold <- ifelse(totacts_nd$TEMP < 32, 1, 0)
#neutral is 0, hot is 1, cold is -1
totacts_nd = totacts_nd %>% mutate(dummyTemp = case_when(
                      isHot == 1 ~ 1,
                      isCold == 1 ~ -1, 
                      TRUE ~ 0))

#GGPAIRS TO SEE RELATIONSHIPS
#ACCDMG
  #Type, totalCars, visibility, weather, DummySpeed, typeq, cause, month, engrs, dummyTemp

totacts_nd = totacts_nd %>% mutate(TYPE = as.factor(TYPE),
                                   VISIBLTY = as.factor(VISIBLTY),
                                   WEATHER = as.factor(WEATHER),
                                   DummySpeed = as.factor(DummySpeed))
ggpairs(totacts_nd, columns=c("ACCDMG", "TYPE","totalCars", "VISIBLTY", "WEATHER", "DummySpeed"))

ggplot(totacts_nd, aes(x=TYPE)) + geom_histogram(stat="count")
ggplot(totacts_nd, aes(x=totalCars, y=ACCDMG)) + geom_point() + geom_smooth(method="lm", se=FALSE) + 
  xlim(0, 200)
ggplot(totacts_nd, aes(x=VISIBLTY)) + geom_histogram(stat="count")
ggplot(totacts_nd, aes(x=WEATHER)) + geom_histogram(stat="count")
ggplot(totacts_nd, aes(x=DummySpeed)) + geom_histogram(stat="count")

totacts_nd = totacts_nd %>% mutate(TYPEQ=as.factor(TYPEQ),
                                   Cause = as.factor(Cause),
                                   MONTH = as.factor(MONTH),
                                   dummyTemp = as.factor(dummyTemp))
ggpairs(totacts_nd, columns=c("ACCDMG", "TYPEQ", "Cause", "MONTH", "ENGRS", "dummyTemp"))

ggplot(totacts_nd, aes(x=TYPEQ)) + geom_histogram(stat="count")
ggplot(totacts_nd, aes(x=Cause)) + geom_histogram(stat="count")
ggplot(totacts_nd, aes(x=MONTH)) + geom_histogram(stat="count")
ggplot(totacts_nd, aes(x=ENGRS)) + geom_histogram(stat="count")
ggplot(totacts_nd, aes(x=dummyTemp)) + geom_histogram(stat="count") + facet_wrap(~MONTH)

#CASUALITIES
  #Type, propDerailedCars, visibility, weather, DummySpeed, typeq, cause, month, engrs, dummyTemp
ggplot(totacts_nd, aes(x=TYPE, y=Casualty)) + geom_histogram(stat="identity")
ggplot(totacts_nd, aes(x=propDerailedCars, y=Casualty)) + geom_point() + geom_smooth(se=FALSE) +
  ylim(0, 500)
ggplot(totacts_nd, aes(x=propDerailedCars)) + geom_density() + xlim(0,.15)
ggplot(totacts_nd, aes(x=VISIBLTY, y=Casualty)) + geom_histogram(stat="identity")
ggplot(totacts_nd, aes(x=WEATHER, y=Casualty)) + geom_histogram(stat="identity")
ggplot(totacts_nd, aes(x=DummySpeed)) + geom_histogram(stat="count")

ggpairs(totacts_nd, columns=c("ACCDMG", "TYPEQ", "Cause", "MONTH", "ENGRS", "dummyTemp"))

ggplot(totacts_nd, aes(x=TYPEQ)) + geom_histogram(stat="count")
ggplot(totacts_nd, aes(x=Cause)) + geom_histogram(stat="count")
ggplot(totacts_nd, aes(x=MONTH, y=Casualty)) + geom_histogram(stat="identity")
ggplot(totacts_nd, aes(x=ENGRS, y=Casualty)) + geom_histogram(stat="identity")
ggplot(totacts_nd, aes(x=dummyTemp, y=Casualty)) + geom_histogram(stat="identity")
