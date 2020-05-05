rm(list=ls())
train <- read.csv("C:/RPI/Data Analytics/Data/train.csv",stringsAsFactors = F)
test <- read.csv("C:/RPI/Data Analytics/Data/test.csv",stringsAsFactors = F)
dim(train)
str(train[,c(1:10,81)])
test_labels <- test$Id
test$Id <- NULL
train$Id <- NULL
test$SalePrice <- NA
df <- rbind(train,test)
HighestPrice <- df[which.max(df$SalePrice),'SalePrice']
HighestPrice
ggplot(data=df[!is.na(df$SalePrice),], aes(x=SalePrice)) +
  geom_histogram(fill="blue", binwidth = 10000) +
  scale_x_continuous(breaks= seq(0, 800000, by=100000))
summary(df$SalePrice)

#sort on decreasing correlations with SalePrice
cor_sorted <- as.matrix(sort(cor_numVar[,'SalePrice'], decreasing = TRUE))
#select only high corelations
CorHigh <- names(which(apply(cor_sorted, 1, function(x) abs(x)>0.5)))
cor_numVar <- cor_numVar[CorHigh, CorHigh]
corrplot.mixed(cor_numVar, tl.col="black", tl.pos = "lt")
ggplot(data=df[!is.na(df$SalePrice),],aes(x=factor(OverallQual),y=SalePrice))+
  geom_boxplot(col='blue')+labs(x='Overall Quality')+
  scale_y_continuous(breaks=seq(0,800000,by=100000))
ggplot(data=all[!is.na(df$SalePrice),], aes(x=GrLivArea, y=SalePrice))+
  geom_point(col='blue') + geom_smooth(method = "lm", se=FALSE, color="black", aes(group=1)) +
  scale_y_continuous(breaks= seq(0, 800000, by=100000)) +
  geom_text_repel(aes(label = ifelse(df$GrLivArea[!is.na(df$SalePrice)]>4500, rownames(all), '')))

#deal with missing value 
library('ggplot2')
library('dplyr')
library('tidyr')
missing.values <- df %>%
  gather(key = "key", value = "val") %>%
  mutate(isna = is.na(val)) %>%
  group_by(key) %>%
  mutate(total = n()) %>%
  group_by(key, total, isna) %>%
  summarise(num.isna = n()) %>%
  mutate(pct = num.isna / total * 100)
levels <-
  (missing.values  %>% filter(isna == T) %>% 
     arrange(desc(pct)))$key

percentage.plot <- missing.values %>%
  ggplot() +
  geom_bar(aes(x = reorder(key, desc(pct)), 
               y = pct, fill=isna), 
           stat = 'identity', alpha=0.8) +
  scale_x_discrete(limits = levels) +
  scale_fill_manual(name = "", 
                    values = c('steelblue', 'tomato3'), 
                    labels = c("Present", "Missing")) +
  coord_flip() +
  labs(title = "Percentage of missing values", x =
         'Variable', y = "% of missing values")

percentage.plot


summary(df.numVar)
df1<-df
attach(df1)
hist(GarageYrBlt)
library(dplyr)
mean(as.numeric(df1$GarageYrBlt),na.rm=TRUE)
df1<-df1 %>% replace_na(replace = list(
  GarageYrBlt = mean(as.numeric(df1$GarageYrBlt),na.rm=TRUE)))
df1[,c("Fence","PoolQC","MiscFeature",'Alley','FireplaceQu')]<-NULL
df1<-df1 %>%replace_na(replace=list(LotFrontage=0))
a <- c('OT')
df1[is.na(df1$GarageCond),]$GarageCond<- as.character(a)
df1[is.na(df1$GarageFinish),]$GarageFinish<- as.character(a)
df1[is.na(df1$GarageQual),]$GarageQual<- as.character(a)
df1[is.na(df1$GarageType),]$GarageType<- as.character(a)
df1[is.na(df1$BsmtCond),]$BsmtCond<- as.character(a)
df1$GarageYrBlt[is.na(df1$GarageYrBlt)]<-mean(df1$GarageYrBlt)
df1$MasVnrType <- NULL
df1$MasVnrArea <- NULL
df1$BsmtFinType1 <- NULL
df1$BsmtFinType2 <- NULL
df1$BsmtQual <- NULL
df1$BsmtExposure <- NULL
par(mfrow=c(1,1))

#deal with outliers

outlier_values <- boxplot.stats(df1$GrLivArea)$out 
boxplot(df1$GrLivArea, main="GrLivArea")

QL <- quantile(df1$GrLivArea, probs = 0.25,na.rm = TRUE)
QU <- quantile(df1$GrLivArea, probs = 0.75,na.rm = TRUE)
QU_QL <- QU-QL
out<- df1$GrLivArea[which(GrLivArea > QU + 4*QU_QL)]
out

df1$GarageYrBlt[which(df$GrLivArea > 4000),] <- 4000


# PCA
df2 <- df1
it <- c(2,5,6,7,8,9,10,11,12,13,14,15,18,19,20,21,22,23,24,25,26,27,32,33,34,35,36,38,40,42,44,47,46,50,52,55,56,57,67,68)
for (i in it) {
  df2[,i]<- as.integer(as.factor(df1[,i]))
  
}
str(df2)

df3<-df2[,-1]
for (i in 1:68){
 df3[,i] <- as.numeric(df3[,i]) 
}

require(factoextra)
df3<-df3[,which(apply(df3, 2, var) != 0)]
#PCA for train dataset
train <-df3[c(1:1460),]
test <- df3[c(1461:2919),]
pca_train <- prcomp(train, center = TRUE,scale. = TRUE)
pca_train$rotation
pca_train$x
res.var <- as.data.frame(c(get_pca_var(pca_train)))
cons<-data.frame(get_pca_var(pca_train)$contrib) 
#compute standard deviation of each principal component
std_dev <-pca_train$sdev
#compute variance
pr_var <- std_dev^2
#check variance of first 10 components
pr_var[1:10]
#proportion of variance explained
prop_varex <- pr_var/sum(pr_var)
prop_varex[1:20]
get_pca_var(pca_train)$contrib 
get_pca_var(pca_train)$cos2    
library(ggfortify)
autoplot(pca_train,data = pca_train)
biplot(pca_train, scale = 0)
pcaVarNew <- pcaVar[, 1:10]
#scree plot
plot(prop_varex, xlab = "Principal Component",
       ylab = "Proportion of Variance Explained",
       type = "b")

#cumulative scree plot
plot(cumsum(prop_varex), xlab = "Principal Component",
       ylab = "Cumulative Proportion of Variance Explained",
       type = "b")
fviz_eig(pca_train, addlabels = TRUE, ylim = c(0, 50), xlab = "Principal Component",
         ylab = "Percentage of explained variance For training dataset",)
#pca for test
pca_test <- prcomp(test, center = TRUE,scale. = TRUE)
fviz_eig(pca_test, addlabels = TRUE, ylim = c(0, 50), xlab = "Principal Component",
         ylab = "Percentage of explained variance For test dataset",)

library("corrplot")
corrplot(get_pca_var(pca_train)$cos2, is.corr=FALSE)
# Total cos2 of variables on Dim.1 and Dim.2
fviz_cos2(pca_train, choice = "var", axes = 1:10)
#Kmeans
traindf <-df1[c(1:1460),]
testdf <- df1[c(1461:2919),]
set.seed(300)
k.max <- 20
# tot.withinss = Total within-cluster sum of square 
# iter.max = the maximum number of iterations allowed
# nstart = if centers is a number, how many random sets should be chosen.
wss<- sapply(1:k.max,function(k){kmeans(pca_train$x[,1:3],k,nstart = 20,iter.max = 20)$tot.withinss})
wss # within sum of squares.
plot(1:k.max,wss, type= "b", xlab = "Number of clusters(k)", ylab = "Within cluster sum of squares")
icluster <- kmeans(pca_train$x[,c(1:3)],15,nstart = 20)

kmeans1<-kmeans(pca_train$x[,1:3],15)
traindf$cluster<-as.factor(kmeans1$cluster)
table(kmeans1$cluster)
library(rgl)
plot3d(pca_train$x[,1:3], col=train$cluster, main="k-means clusters for training data")

#K-Means clustering
kmeans2<-kmeans(pca_test$x[,1:3],15)
testdf$cluster<-as.factor(kmeans2$cluster)
plot3d(pca_test$x[,1:3], col=test$cluster, main="k-means clusters for test data")
table(kmeans2$cluster)


#Hierarchical
distmat <- dist(pca_train$x[,1:3], method = 'euclidean')
hclustavg <- hclust(distmat, method = 'average')
plot(hclustavg)
cutavg <- cutree(hclustavg, k = 8)
rect.hclust(hclustavg , k = 4,border = 2:6)
abline(h = 8, col = 'red')

suppressPackageStartupMessages(library(dendextend))
avg_dendobj <- as.dendrogram(hclustavg)
avg_coldend <- color_branches(avg_dendobj, h = 8)
plot(avg_coldend)
#-----------------
dist_mat <- dist(pca_test$x[,1:3], method = 'euclidean')
hclust_avg <- hclust(dist_mat, method = 'average')
plot(hclust_avg)
cut_avg <- cutree(hclust_avg, k = 8)
rect.hclust(hclust_avg , k = 8)
abline(h = 8, col = 'red')

suppressPackageStartupMessages(library(dendextend))
avg_dend_obj <- as.dendrogram(hclust_avg)
avg_col_dend <- color_branches(avg_dend_obj, h = 8)
plot(avg_col_dend)

#Interpretation

#for kmeans
library(plyr)
mu <- ddply(traindf, "cluster", summarise, grp.mean=mean(OverallQual))
mu
mu <- ddply(traindf, "cluster", summarise, grp.mean=mean(OverallQual))
mu
mu <- ddply(traindf, "cluster", summarise, grp.mean=mean(SalePrice))
mu
mu <- ddply(traindf, "cluster", summarise, grp.mean=mean(GrLivArea))
mu
mu <- ddply(traindf, "cluster", summarise, grp.mean=mean(GarageCars))
mu
mu <- ddply(traindf, "cluster", summarise, grp.mean=mean(GarageArea))
mu
mu <- ddply(traindf, "cluster", summarise, grp.mean=mean(TotRmsAbvGrd))
mu
mu <- ddply(traindf, "cluster", summarise, grp.mean=mean(YearBuilt))
mu
mu <- ddply(traindf, "cluster", summarise, grp.mean=mean(TotalBsmtSF))
mu
mu <- ddply(traindf, "cluster", summarise, grp.mean=mean(X2ndFlrSF))
mu
mu <- ddply(traindf, "cluster", summarise, grp.mean=mean(FullBath))
mu
