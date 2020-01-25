#screenshot
rm(list=ls())
days <- c('Mon','Tue','Wed','Thur','Fri','Sat','Sun')
temp <- c(28,30.5,32,31.2,29.3,27.9,26.4)
snowed <- c('T','T','F','F','T','T','F')
RPI_Weather_Week <- data.frame(days,temp,snowed)
RPI_Weather_Week
head(RPI_Weather_Week)
str(RPI_Weather_Week)
summary(RPI_Weather_Week)
RPI_Weather_Week[1,]
RPI_Weather_Week[,1]
RPI_Weather_Week[,'snowed']
RPI_Weather_Week[,'days']
RPI_Weather_Week[,'temp']
RPI_Weather_Week[1:5,c('days','temp')]
RPI_Weather_Week$temp
subset(RPI_Weather_Week,subset=snowed==TRUE)
sorted.snowed <- order(RPI_Weather_Week['snowed'])
sorted.snowed
RPI_Weather_Week[sorted.snowed,]

dec.snow <- order(-RPI_Weather_Week$temp)
dec.snow

empty.DataFrame <- data.frame()
v1 <- 1:10
v1
letters
v2 <- letters[1:10]
df <- data.frame(col.name.1=v1,col.name.2=v2)
df
write.csv(df,file = 'saved_df1.csv')
df2 <- read.csv('saved_df1.csv')
df2

#Exercise
GPW3 <- read.csv(file.choose(),header = TRUE)
GPW3     

EPI_data <- read.csv(file.choose(),header = TRUE,skip = 1)
dim(EPI_data)
attach(EPI_data)
View(EPI_data)
fix(EPI_data)
tf <- is.na(EPI)
E <- EPI[!tf]
summary(EPI)
fivenum(EPI,na.rm=TRUE)
stem(EPI)
hist(EPI)
hist(EPI,seq(30.,95.,1.0),prob=TRUE)
lines(density(EPI,na.rm=TRUE,bw=1.))
rug(EPI)
plot(ecdf(EPI),do.points = FALSE,verticals = TRUE)
par(pty='s') #pty defines area. s is square, m is max
qqnorm(EPI)
qqline(EPI)
x<-seq(30,95,1)
qqplot(qt(ppoints(250),df=5),x,xlab = 'Q-Q plot for tdsn')
qqline(x)

#Exercise 1
rf <- is.na(DALY)
E <- DALY[!rf]
summary(DALY)
fivenum(EPI,na.rm = TRUE)
stem(DALY)
hist(DALY)
hist(DALY,seq(0,92,1),pro=TRUE)
lines(density(DALY,na.rm=TRUE,bw=1))
rug(DALY)
plot(ecdf(DALY),do.points=FALSE,vertical=TRUE)
par(pty='s')
qqnorm(DALY)
qqline(DALY)
x<-seq(0,92,1)
qqplot(qt(ppoints(250),df=5),x,xlab='Q-Q plot for tdsn')
qqline(x)
boxplot(EPI,DALY)


#Exercise 2
EPILand <- EPI[!Landlock]
Eland <- EPILand[!is.na(EPILand)]
hist(Eland)
hist(Eland,seq(30,95,1),prob=TRUE)
rug(Eland)
plot(ecdf(DALY),do.points=FALSE,vertical=TRUE)
par(pty='s')
qqnorm(Eland)
qqline(Eland)
x<-seq(30,95,1)
qqplot(qt(ppoints(250),df=5),x,xlab='Q-Q plot for tdsn')
qqline(x)
boxplot(Eland)

#filering
EPI_South_Asia <-EPI[which(EPI_regions == 'South Asia')]
