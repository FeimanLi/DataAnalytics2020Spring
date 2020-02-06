rm(list=ls())
EPI_data <- read.csv(file.choose(),header = TRUE)
attach(EPI_data)
head(EPI_data)
#Lab2a
#Central Tendency for EPI and DALY

summary(EPI_data$EPI)
mean(EPI_data$EPI,na.rm = TRUE)
median(EPI_data$EPI, na.rm = TRUE)
names(table(EPI_data$EPI))[table(EPI_data$EPI) == max(table(EPI_data$EPI))]


summary(EPI_data$DALY)
mean(EPI_data$DALY,na.rm = TRUE)
median(EPI_data$DALY, na.rm = TRUE)
names(table(EPI_data$DALY))[table(EPI_data$DALY) == max(table(EPI_data$DALY))]
#histogram for EPI and DALY
hist(EPI_data$EPI)
hist(EPI_data$DALY)

library("dplyr")

sample_n(EPI_data[!is.na(EPI_data$EPI),],5)$EPI
sample_n(EPI_data[!is.na(EPI_data$DALY),],5)$DALY

sample_frac(EPI_data[!is.na(EPI_data$EPI),],0.1)$EPI
sample_frac(EPI_data[!is.na(EPI_data$EPI),],0.1)$DALY

new_decs_EPI <- arrange(EPI_data,desc(EPI))$EPI
new_decs_DALY <- arrange(EPI_data,desc(DALY))$DALY

double_EPI <- mutate(EPI_data,MyNewColmn = EPI*2)
double_DALY <- mutate(EPI_data,MyNewColmn = DALY*2)


summarise(EPI_data,avg_EPI = mean(EPI, na.rm = TRUE))
summarise(EPI_data,avg_DALY = mean(DALY, na.rm = TRUE))
boxplot(ENVHEALTH,ECOSYSTEM)
qqplot(ENVHEALTH,ECOSYSTEM)


#Lab2b
#ENVH
boxplot(ENVHEALTH,DALY,AIR_H,WATER_H)
lmENVH <-lm(ENVHEALTH~DALY+AIR_H+WATER_H)
lmENVH
summary(lmENVH)
cENVH<-coef(lmENVH)
DALYNEW<-c(seq(5,95,length.out = 231))
AIR_HNEW<-c(seq(5,95,length.out = 231))
WATER_HNEW<-c(seq(5,95,length.out = 231))
predENVH<-data.frame(DALYNEW,AIR_HNEW,WATER_HNEW)
names(predENVH) <- c("DALY","AIR_H","WATER_H")
pENV<- predict(lmENVH,predENVH,interval='prediction')
cENV<- predict(lmENVH,predENVH,interval='confidence')
pENV
cENV

#AIR_E
lmAIR_E <- lm(AIR_E~AIR_H+WATER_E+CLIMATE)
lmAIR_E
summary(lmAIR_E)
coefAIR_E<-coef(lmAIR_E)

AIR_HNEW <- c(seq(5,95,length.out = length(AIR_E)))
WATER_ENEW <- c(seq(5,95,length.out = length(AIR_E)))
CLIMATENEW <- c(seq(5,95,length.out = length(AIR_E)))
predAIR_E <- data.frame(AIR_HNEW,WATER_ENEW,CLIMATENEW)
names(predAIR_E) <- c("AIR_HNEW","WATER_ENEW","CLIMATENEW")
pAIR_E <- predict(lmAIR_E,predAIR_E,interval='prediction')
cAIR_E<- predict(lmAIR_E,predAIR_E,interval='confidence')
pAIR_E
cAIR_E


#CLIMATE
lmCLIMATE <- lm(CLIMATE~AIR_H+WATER_E+FORESTRY)
lmCLIMATE
summary(lmCLIMATE)
coeCLIMATE<-coef(lmCLIMATE)

AIR_HNEW <- c(seq(5,95,length.out = length(CLIMATE)))
WATER_ENEW <- c(seq(5,95,length.out = length(CLIMATE)))
CLIMATENEW <- c(seq(5,95,length.out = length(CLIMATE)))
predCLIMATE <- data.frame(AIR_HNEW,WATER_ENEW,FORESTRY)
pCLIMATE <- predict(lmAIR_E,predCLIMATE,interval='prediction')
cCLIMATE<- predict(lmAIR_E,predCLIMATE,interval='confidence')
pCLIMATE
cCLIMATE

