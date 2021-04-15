list.of.packages <- c("boot", "car","QuantPsyc","lmtest","sandwich","vars","nortest","MASS","caTools")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos="http://cran.rstudio.com/")

library(MASS)
library(car)
library(boot) 
library(car)
library(QuantPsyc)
library(lmtest)
library(sandwich)
library(vars)
library(nortest)
library(MASS)
library(caTools)
library(dplyr)

#to load the data
df<-read.csv(file.choose())

#look at the data
head(df)

#look at the datatypes - are they appropriate or not?
str(df)

#to check mean value, quartiles , max and min value of each column in the data
summary(df)


# Qualitative variables are - title, mediatype, description, studios, tags, contentwarn
# Quantitative variables are - eps, duration, watched, watching, wantwatch, dropped, rating, votes
# Categorical variables are - ongoing, sznofRelease, new_tags

df$mediaType[df$mediaType==""]=NA
df$sznOfRelease[df$sznOfRelease==""]=NA
df$description[df$description==""]=NA
df$studios[df$studios=="[]"]=NA
df$tags[df$tags=="[]"]=NA
df$contentWarn[df$contentWarn=="[]"]=NA

#to check all the null values
as.data.frame(colSums(is.na(df)))

#to check unique values in each column
sapply(df, function(x) length(unique(x)))

#to drop the variables which have missing values more than 30% and a lots of unique values so we have to drop this variables because we couldn't crate dummy variables.
df1=df[, !(colnames(df) %in% c("description","tags","contentWarn","studios","sznOfRelease","title"))]
dim(df1)

#to check all the null values
as.data.frame(colSums(is.na(df1)))

#to replace the null values of watched column by median value
df1$watched[is.na(df1$watched)]<-round(median(df1$watched,na.rm=TRUE), digits = 0)

df1$duration[is.na(df1$duration)]<-round(median(df1$duration,na.rm=TRUE), digits = 0)


#by visualizing the mediatype column, it is shown that TV has highest frequency
df1 <- within(df1,
               mediaType <- factor(mediaType,
                                   levels=names(sort(table(mediaType),
                                                     decreasing=TRUE))))
library(ggplot2)
ggplot(df1, aes(x = mediaType)) + geom_bar()


#to replace the null values of the mediatype column by mode value i.e. TV because the frequency of TV is higher
df1$mediaType[is.na(df1$mediaType)] = 'TV'

#to count the categorical variable
table(df1$mediaType)
table(df1$ongoing)

#to check the distribution of the below mentioned column
ggplot(df1, aes(x=eps)) + 
  geom_histogram(color="black", fill="white", binwidth = 20)

ggplot(df1, aes(x=watched)) + 
  geom_histogram(color="black", fill="white", binwidth = 50)


#to check outlier
quantile(df1$eps, seq(0,1,.05))
boxplot(df1$eps)
quantile(df1$watched, seq(0,1,.05))
boxplot(df1$watched)
quantile(df1$wantWatch, seq(0,1,.05))
boxplot(df1$wantWatch)
quantile(df1$dropped, seq(0,1,.05))
boxplot(df1$dropped)
quantile(df1$votes, seq(0,1,.05))
boxplot(df1$votes)
hist(df1$duration)
quantile(df1$duration, seq(0,1,.05))
quantile(df1$duration, probs = c(.25,.99), na.rm = TRUE)


df2 <- df1[df1$eps <104,]
boxplot(df2$eps)
quantile(df2$eps, seq(0,1,.05))
quantile(df2$eps, probs = c(.25,.995), na.rm = TRUE)

boxplot(df2$watched)
quantile(df2$watched, seq(0,1,.05))
quantile(df2$watched, probs = c(.25,.99), na.rm = TRUE)
df3 <- df2[df2$watched <40553, ]
boxplot(df3$watched)

boxplot(df3$watching)
quantile(df3$watching, seq(0,1,.05))
quantile(df3$watching, probs = c(.25,.99), na.rm = TRUE)
df4 <- df3[df3$watching<3378, ]
boxplot(df4$watching)

boxplot(df4$wantWatch)
quantile(df4$wantWatch, seq(0,1,.05))
quantile(df4$wantWatch, probs = c(.25,.99), na.rm = TRUE)

df5 <- df4[df4$wantWatch<9402, ]
boxplot(df5$wantWatch)

boxplot(df5$votes)
quantile(df5$votes, seq(0,1,.05))
quantile(df5$votes, probs = c(.25,.99), na.rm = TRUE)
df6 <- df5[df5$votes<17169, ]
boxplot(df6$votes)

boxplot(df6$dropped)
quantile(df6$dropped, seq(0,1,.05))
quantile(df6$dropped, probs = c(.25,.995), na.rm = TRUE)
df7 <- df6[df6$dropped<1330, ]
boxplot(df7$dropped)

quantile(df7$duration, probs = c(.25,.995), na.rm = TRUE)
df8 <- df7[df7$duration < 120,]
boxplot(df8$duration)


#statistical test
cor(df8$eps,df8$rating)# 0.15
cor(df8$watched,df8$rating)#0.50
cor(df8$wantWatch,df8$rating)#0.59
cor(df8$dropped,df8$rating)#0.33
cor(df8$votes,df8$rating)#0.50
cor(df8$new_tags,df8$rating)#0.42

#Visualization
#to check the distribution of target variable i.e. rating
densityPlot(df11$rating, xlab = "rating")

#to check the relation
scatter.smooth(df7$watched, df7$rating, xlab = "watched", ylab = "rating")
scatter.smooth(df7$watching, df7$rating, xlab = "watching", ylab = "rating")
scatter.smooth(df7$wantWatch, df7$rating, xlab = "wantWatch", ylab = "rating")
scatter.smooth(df7$dropped, df7$rating, xlab = "dropped", ylab = "rating")
scatter.smooth(df7$votes, df7$rating, xlab = "votes", ylab = "rating")


ggplot(data=df7, aes(x=ongoing)) +
  geom_bar(stat="count", width=0.5)


#logarithmic transformation
#to replace 0's from all the column with nearest values
df8$watched[df8$watched == 0] = 5
df8$watching[df8$watching == 0] = 1
df8$wantWatch[df8$wantWatch == 0] = 3
df8$dropped[df8$dropped == 0] = 1


df8$watched = log(df8$watched)
df8$watching = log(df8$watching)
df8$wantWatch = log(df8$wantWatch)
df8$dropped = log(df8$dropped)


#train test split
sample = sample.split(df8$rating,SplitRatio = 0.70)
train1 =subset(df8,sample ==TRUE)
str(train1)
dim(train1)

test1=subset(df8, sample==FALSE)
str(test1)
dim(test1)

#Model building
L0 <- lm(rating~. ,data = train1)
summary(L0)

#first I drop DVD SPECIAL
L1 <- lm(rating~I(mediaType == "Movie") +I(mediaType == "Music Video") +I(mediaType == "Other") +I(mediaType == "OVA") +I(mediaType == "TV Special") +I(mediaType == "Web") +I(ongoing == "Yes") +new_tags +watched +watching +wantWatch +dropped +votes +eps +duration, data = train1)
summary(L1)

#under mediatype removing TV Special
L2 <- lm(rating~I(mediaType == "Movie") +I(mediaType == "Music Video") +I(mediaType == "Other") +I(mediaType == "OVA") +I(mediaType == "Web") +I(ongoing == "Yes") +new_tags +watched +watching +wantWatch +dropped +votes +eps +duration, data = train1)
summary(L2)


#under mediatype OVA is dropped
L3 <- lm(rating~I(mediaType == "Movie") +I(mediaType == "Music Video") +I(mediaType == "Other") +I(mediaType == "Web") +I(ongoing == "Yes") +new_tags +watched +watching +wantWatch +dropped +votes +eps +duration, data = train1)
summary(L3)

#removing other under mediatype
L4 <- lm(rating~I(mediaType == "Movie") +I(mediaType == "Music Video") +I(mediaType == "Web") +I(ongoing == "Yes") +new_tags +watched +watching +wantWatch +dropped +votes +eps +duration, data = train1)
summary(L4)

#removing 'ongoing'
L5 <- lm(rating~I(mediaType == "Movie") +I(mediaType == "Music Video") +I(mediaType == "Web") +new_tags +watched +watching +wantWatch +dropped +votes +eps +duration, data = train1)
summary(L5)


#to check multicolinearity
vif(L5)

#By checking vif it is shown that some variables have highest vif. so first drop watched column
L6 <- lm(rating~I(mediaType == "Movie") +I(mediaType == "Music Video") +I(mediaType == "Web") +new_tags +watching +wantWatch +dropped +votes +eps +duration, data = train1)
summary(L6)
vif(L6)


#watching has vif so it is dropped from the model
L7 <- lm(rating~I(mediaType == "Movie") +I(mediaType == "Music Video") +I(mediaType == "Web") +new_tags +wantWatch +dropped +votes +eps +duration, data = train1)
summary(L7)
vif(L7)

#after dropping 'watching' it is shown that under mediatype web is insignificant so I drop it
L8 <- lm(rating~I(mediaType == "Movie") +I(mediaType == "Music Video") +new_tags +wantWatch +dropped +votes +eps +duration, data = train1)
summary(L8)


#after dropping 'watching' it is shown that under mediatype Movie is insignificant so I drop it
L9 <- lm(rating~I(mediaType == "Music Video") +new_tags +wantWatch +dropped +votes +eps +duration, data = train1)
summary(L9)


vif(L9)

#fitted model
t1 <- lm(rating~I(mediaType == "Music Video") +new_tags +wantWatch +dropped +votes +eps +duration, data = test1)
summary(t1)


par(mfrow=c(2,2))
plot(t1)

#andersion-darling test
resids1 <- t1$residuals
qqnorm(resids1)
ad.test(resids1)

bptest(t1)#we reject null hypothesis that variance are homoschedastic. So it is actually heteroschedastic

durbinWatsonTest(t1)# No autocorrelation in the dataset

vif(t1)#to check multicolinearity

test1$pred <- fitted(t1)

#MAPE
attach(test1)
(sum((abs(rating-pred))/rating))/nrow(test1)

#To calculate RMSE
library(Metrics)
predictions <- predict(t1, test1)
rmse(test1$rating, predictions)


#Mean and median accuracy
test1$Pred_LM=predict(t1,test1)
test1$LM_APE= 100*(abs(test1$rating-test1$Pred_LM)/test1$rating)
head(test1)
MeanAPE=mean(test1$LM_APE)
MeanAPE #16.53
MedianAPE=median(test1$LM_APE)
MedianAPE #11.61







