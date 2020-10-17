library(GGally)
library(ggplot2)
library(dplyr)
library(ggpubr)

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

#remove duplicates
totacts_nd = totacts[!(duplicated(totacts[, c("INCDTNO", "YEAR", "MONTH", "DAY", "TIMEHR", "TIMEMIN")])),]

############################################################################

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


#dummy speed
mean(totacts_nd[totacts_nd$TRNSPD != 0,]$TRNSPD)
totacts_nd$DummySpeed <- ifelse(totacts_nd$TRNSPD >= 14.48643, 1, 0) 
totacts_nd$DummySpeed = as.factor(totacts_nd$DummySpeed)


#total cars
totacts_nd$totalCars <- totacts_nd$LOADF1 + totacts_nd$LOADP1 + totacts_nd$EMPTYF1 + totacts_nd$EMPTYP1 + totacts_nd$CABOOSE1


#if no loaded passenger cars, just equal 0, else find proportion of loaded pass cars
totacts_nd$propDerailedCars <- ifelse(totacts_nd$LOADP1 == 0, 0, totacts_nd$LOADP2/totacts_nd$LOADP1)


#factor TYPE
totacts_nd$TYPE = as.factor(totacts_nd$TYPE)
totacts_nd %>% group_by(TYPE) %>% 
  summarise(totcas = sum(Casualty)) %>% 
  arrange(desc(totcas)) %>% top_n(5, totcas)

totacts_nd %>% group_by(TYPE) %>% 
  summarise(totdamage = sum(ACCDMG)) %>% 
  arrange(desc(totdamage)) %>% top_n(5, totdamage)

totacts_nd = totacts_nd %>% mutate(accidentType = case_when(
  TYPE == 1 ~ "derail",
  TYPE == 7 ~ "highway crossing",
  TRUE ~ "other"))


#factor TYPEQ
totacts_nd$TYPEQ = as.factor(totacts_nd$TYPEQ)
#which train types are the most damaging
totacts_nd %>% group_by(TYPEQ) %>% 
  summarise(totdamage = sum(ACCDMG)) %>% 
  arrange(desc(totdamage)) %>% top_n(6, totdamage)

totacts_nd %>% group_by(TYPEQ) %>% 
  summarise(totcas = sum(Casualty)) %>% 
  arrange(desc(totcas)) %>% top_n(6, totcas)

#freight=1, 2=passenger, 3=commuter, 6=cut of cars, 7=yard/switching 
#top 3 were 1, 7, and 2
totacts_nd = totacts_nd %>% mutate(trainType = case_when(
                            TYPEQ == 1 ~ "freight",
                            TYPEQ == 7 ~ "yard/switch",
                            TYPEQ == 2 ~ "passenger",
                            TRUE ~ "other"))


#dummyWeather
totacts_nd = totacts_nd %>% mutate(dummyWeather = case_when(
                            WEATHER == 1 ~ "clear",
                            WEATHER %in% c(2, 4) ~ "obscure",
                            WEATHER %in% c(3, 5, 6) ~ "precipitation"))
totacts_nd$dummyWeather = as.factor(totacts_nd$dummyWeather)
    
                        
#dummyVis
totacts_nd = totacts_nd %>% mutate(dummyVis = case_when(
                            VISIBLTY %in% c(1, 2) ~ "light",
                            VISIBLTY %in% c(3, 4) ~ "dark"))
totacts_nd$dummyVis = as.factor(totacts_nd$dummyVis)


#dummyTemp
totacts_nd = totacts_nd %>% mutate(dummyTemp = case_when(
              TEMP >= 86 ~ 1,
              TEMP <= 32 ~ 1, 
              TRUE ~ 0))


#INITIAL GRAPHICAL DATA VISUALIZATION
#ACCDMG
ggpairs(totacts_nd, columns=c("ACCDMG", "TYPE","totalCars", "VISIBLTY", "WEATHER", "DummySpeed"))

ggplot(totacts_nd, aes(x=TYPE)) + geom_histogram(stat="count")
ggplot(totacts_nd, aes(x=totalCars, y=ACCDMG)) + geom_point() + geom_smooth(method="lm", se=FALSE) + 
  xlim(0, 200)
ggplot(totacts_nd, aes(x=VISIBLTY)) + geom_histogram(stat="count")
ggplot(totacts_nd, aes(x=WEATHER)) + geom_histogram(stat="count")
ggplot(totacts_nd, aes(x=DummySpeed)) + geom_histogram(stat="count")

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

ggpairs(totacts_nd, columns=c("Casualty", "TYPEQ", "Cause", "MONTH", "ENGRS", "dummyTemp"))

ggplot(totacts_nd, aes(x=TYPEQ)) + geom_histogram(stat="count")
ggplot(totacts_nd, aes(x=Cause)) + geom_histogram(stat="count")
ggplot(totacts_nd, aes(x=MONTH, y=Casualty)) + geom_histogram(stat="identity")
ggplot(totacts_nd, aes(x=ENGRS, y=Casualty)) + geom_histogram(stat="identity")
ggplot(totacts_nd, aes(x=dummyTemp, y=Casualty)) + geom_histogram(stat="identity")

###########################################################################

#INTERACTION TERMS
#accdmg
interaction.plot(totacts_nd$dummyVis, totacts_nd$dummyWeather, totacts_nd$ACCDMG)
interaction.plot(totacts_nd$TYPE, totacts_nd$trainType, totacts_nd$ACCDMG)
#casualties
interaction.plot(totacts_nd$DummySpeed, totacts_nd$Cause, totacts_nd$Casualty)
interaction.plot(totacts_nd$dummyVis, totacts_nd$dummyWeather, totacts_nd$Casualty)

###################################################################
#LINEAR MODELING
#ACCDMG
#smallest
accdmg.lm0 = lm(ACCDMG ~ accidentType + dummyWeather + trainType + dummyVis*dummyWeather, data=totacts_nd)
#smaller
accdmg.lm1 = lm(ACCDMG ~ accidentType + dummyVis + dummyWeather + DummySpeed + trainType + Cause + dummyVis*dummyWeather + dummyTemp, data=totacts_nd)
summary(accdmg.lm1)
#larger
accdmg.lm2 = step(accdmg.lm1, trace=F)
summary(accdmg.lm2)
#smaller > smallest
anova(accdmg.lm0, accdmg.lm1)
#smaller > larger
anova(accdmg.lm1, accdmg.lm2)

#compare PSME with test sets
source("TestSet.R")
##set test sets size:
test.size<-1/3
##generate training sets and test sets from original data:
totacts_nd.data<-test.set(totacts_nd,test.size)
##Check distribution of ACCDMG of test set, training set:
#2 different plotting methods
##method 1
par(mfrow=c(2,2))
hist(totacts_nd.data$train$ACCDMG)
hist(totacts_nd.data$test$ACCDMG)
hist(totacts_nd$ACCDMG)
par(mfrow=c(1,1))
##Build models with training set:
accdmg.lm1.train<-lm(ACCDMG ~ accidentType + dummyVis + dummyWeather + DummySpeed + trainType + Cause + dummyVis*dummyWeather + dummyTemp, data=totacts_nd.data$train)
accdmg.lm0.train<-lm(ACCDMG ~ accidentType + dummyWeather + trainType + dummyVis*dummyWeather,data=totacts_nd.data$train)
##Recall that we need to measure predicted MSE. 
##First, how to predict with lm models:
accdmg.lm1.pred<-predict(accdmg.lm1.train,newdata=totacts_nd.data$test) 
accdmg.lm0.pred<-predict(accdmg.lm2.train,newdata=totacts_nd.data$test)
##Next, compute PMSE:
pmse.accdmg.lm1<-mse(accdmg.lm1.pred,totacts_nd.data$test$ACCDMG)
pmse.accdmg.lm1
pmse.accdmg.lm0<-mse(accdmg.lm0.pred,totacts_nd.data$test$ACCDMG)
pmse.accdmg.lm0
#smaller model better that smallest based on test sets and PSME too



#CASUALITIES
#smallest
casualty.lm0 = lm(Casualty ~ accidentType + dummyWeather + DummySpeed + Cause + dummyVis*dummyWeather, data=totacts_nd)
summary(casualty.lm0)
#smaller
casualty.lm1 = lm(Casualty ~ accidentType + dummyVis + dummyWeather + DummySpeed + trainType + Cause + dummyVis*dummyWeather, data=totacts_nd)
summary(casualty.lm1)
#step didn't change anything
casualty.lm2 = step(casualty.lm1, trace=F)
summary(casualty.lm2)
#smaller = step
anova(casualty.lm1, casualty.lm2)
#smaller > smallest
anova(casualty.lm0, casualty.lm1)

#compare with test sets
par(mfrow=c(2,2))
hist(totacts_nd.data$train$Casualty)
hist(totacts_nd.data$test$Casualty)
hist(totacts_nd$Casualty)
par(mfrow=c(1,1))
##Build models with training set:
casualty.lm0.train<-lm(Casualty ~ accidentType + dummyWeather + DummySpeed + Cause + dummyVis*dummyWeather, data=totacts_nd.data$train)
casualty.lm1.train<-lm(Casualty ~ accidentType + dummyVis + dummyWeather + DummySpeed + trainType + Cause + dummyVis*dummyWeather,data=totacts_nd.data$train)

##Recall that we need to measure predicted MSE. 
##First, how to predict with lm models:

casualty.lm0.pred<-predict(casualty.lm0.train,newdata=totacts_nd.data$test) 
casualty.lm1.pred<-predict(casualty.lm1.train,newdata=totacts_nd.data$test)


##Next, compute PMSE:
pmse.casualty.lm0<-mse(casualty.lm0.pred,totacts_nd.data$test$Casualty)
pmse.casualty.lm0

pmse.casualty.lm1<-mse(casualty.lm1.pred,totacts_nd.data$test$Casualty)
pmse.casualty.lm1
#smaller model better than smallest based on test sets and PSME too
