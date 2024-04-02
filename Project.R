library(dplyr)
library(ggplot2)
library(repr)
library(lubridate)
library(psych)
library(dunn.test)

## Read data from file
sleep <-read.csv("Sleep_Efficiency.csv")
str(sleep)

## Data Cleaning
sleep[4] = substring(sleep$Bedtime, 11) # remove date from Bedtime
sleep[5] =  substring(sleep$Wakeup.time, 11) # remove date from Wakeup.time
clean_sleep <- sleep %>% filter(Age >= 18) %>% select(-c(ID, REM.sleep.percentage, Deep.sleep.percentage, Light.sleep.percentage)) %>% na.omit() # select adults' data; omit 'ID' column & NAs 
head(clean_sleep)
str(clean_sleep)

# convert Bedtime to numeric (in hours)
format(clean_sleep[,3], format = "%H:%M:%S", tz = "", usetz = FALSE, digits = getOption("digits.secs"))
clean_sleep[,3] <- sapply(strsplit(clean_sleep[,3], ":"), function(x) {
  x <- as.numeric(x)
  x[1] + x[2]/60 + x[3]/3600 })

# convert Wakeup.time to numeric (in hours)
format(clean_sleep[ ,4], format = "%H:%M:%S", tz = "", usetz = FALSE, digits = getOption("digits.secs"))
clean_sleep[ , 4] <- sapply(strsplit(clean_sleep[ , 4], ":"), function(x) {
  x <- as.numeric(x)
  x[1] + x[2]/60 + x[3]/3600 })
str(clean_sleep)
summary(clean_sleep)

# Set plotting size
options(repr.plot.height = 4, repr.plot.width = 6)

## 3.1 MAIN VARIABLE: SLEEP EFFICIENCY
par(mfrow=c(1,3))

efficiency <- clean_sleep[,'Sleep.efficiency']; 
# x1 <- seq(-10, 100, by=0.1); n1 <- dnorm(x1, mean(efficiency), sd(efficiency))
hist(efficiency, main='Histogram of Sleep Efficiency', xlab='Sleep Efficiency', ylab='Proportion of Time', col='white')
box("figure", col="black")  

# x1 <- seq(-10, 100, by=0.1); n1 <- dnorm(x1, mean(log_efficiency), sd(log_efficiency))
hist((efficiency)^2, main='Histogram of (Sleep Efficiency)^2', xlab='Sleep Efficiency', ylab='Proportion of Time', col='white')
box("figure", col="black")  

boxplot(efficiency, main='Boxplot of Sleep Efficiency', lwd=2, col='white')
box("figure", col="black")  

summary(efficiency)

## 3.2.1 AGE
par(mfrow=c(1,1))
hist(clean_sleep$Age, col='white', main='Histogram of Age',  ylim=c(10,70), xlab='Age', ylab='Number of subjects', cex=4)
boxplot(clean_sleep$Age, col='white', main='Boxplot of Age', ylim=c(10,70))
summary(clean_sleep$Age)
# box("figure", col="black",  lwd = 1)  

## 3.2.2 GENDER
barplot(table(clean_sleep$Gender), ylab = 'Number of subjects', xlab="Gender", ylim=c(0,200), col='white', main='Barplot of Gender')

## 3.2.3 BEDTIME
hist(clean_sleep$Bedtime, col='white', main='Histogram of Bedtime', xlab='Bedtime', ylab='Number of subjects', ylim=c(0,200))
boxplot(clean_sleep$Bedtime, col='white', main='Boxplot of Bedtime', ylim=c(0,25))
summary(clean_sleep$Bedtime)

## 3.2.4 WAKEUP TIME
hist1 = hist(clean_sleep$Wakeup.time, xlim=c(2,12), ylim=c(0,100), col="white", xlab="Wakeup Time", main="Histogram of Wakeup Time")
boxplot(clean_sleep$Wakeup.time, main="Boxplot of Wakeup Time")

## 3.2.5 SLEEP DURATION
hist2 = hist(clean_sleep$Sleep.duration, ylim=c(0,200), col= "white", xlab="Sleep Duration", main="Histogram of Sleep Duration")
boxplot(clean_sleep$Sleep.duration, main="Boxplot of Sleep Duration")

## 3.2.6 AWAKENINGS
hist3 = hist(clean_sleep$Awakenings, ylim=c(0,200), col="white", xlab="Awakening Count", main="Histogram of Awakenings")
boxplot(clean_sleep$Awakenings, main="Boxplot of Awakenings")

## 3.2.7 CAFFEINE CONSUMPTION
hist(clean_sleep$Caffeine.consumption, main="Histogram of Caffeine Consumption", col='white', xlab="Caffeine consumed in the past 24hrs (in mg)", ylab="Number of subjects")
boxplot(clean_sleep$Caffeine.consumption, main="Boxplot of Caffeine Consumption", col='white')

## 3.2.8 ALCOHOL CONSUMPTION
hist(clean_sleep$Alcohol.consumption, main="Histogram of Alcohol Consumption", col="white", xlab="Alcohol consumed in the past 24hrs (in oz)", ylab="Number of subjects")
boxplot(clean_sleep$Alcohol.consumption, main="Boxplot of Alcohol Consumption", col='white')

## 3.2.9 SMOKING STATUS
barplot(table(clean_sleep$Smoking.status), col="white", main="Barplot of Smoking Status", ylab="Number of subjects", xlab="Smoking Status")
nrow(clean_sleep %>% select(Smoking.status) %>% filter(Smoking.status=="No"))
nrow(clean_sleep %>% select(Smoking.status) %>% filter(Smoking.status=="Yes"))

## 3.2.10 EXERCISE FREQUENCY
hist(clean_sleep$Exercise.frequency, col="white", main="Historgram of Exercise Frequency", xlab="Exercise Frequency", ylab="Number of subjects")
boxplot(clean_sleep$Exercise.frequency, col="white", main="Boxplot of Exercise Frequency")

## 4.1 Correlation Plot
# convert Smoking into numeric data 
clean_sleep$Smoking.status <- as.factor(clean_sleep$Smoking.status) 
clean_sleep$Smoking.status <- ifelse(clean_sleep$Smoking.status=="Yes",1,0)

num_var <- clean_sleep %>% select(-Gender)
pairs(num_var, main = "Sleep Efficiency and Other Variables")
pairs.panels(num_var, method = "pearson", hist.col = "steelblue", pch = 21, density = TRUE, ellipses = FALSE)             

## 4.2 
## 4.2.1 Relationship between Sleep Efficiency and Gender
# boxplot of Sleep Efficiency by Gender
ggplot(clean_sleep, aes(x=Gender, y=Sleep.efficiency)) +geom_boxplot(fill="white", alpha=0.2) + xlab("Gender") + ylab("Sleep Efficiency") + ggtitle("Box Plot: Sleep Efficiency by Gender") + theme_bw()

# compare variance of males' and females' sleep efficiency: H0: var(M)=var(F); H1: var(M)!=var(F)
var.test(Sleep.efficiency ~ Gender, data = clean_sleep)

# test for normality in the sleep efficiency data for males and females separately
# For males
male_data <- clean_sleep[clean_sleep$Gender == "Male",]
shapiro.test(male_data$Sleep.efficiency)
# For females
female_data <- clean_sleep[clean_sleep$Gender == "Female",]
shapiro.test(female_data$Sleep.efficiency)

# t.test for differences in mean of sleep efficency of Males & Females at 95% of confidence interval 
t.test(Sleep.efficiency ~ Gender, data = clean_sleep)

## 4.2.2 Relationship between Sleep Efficiency and Age
# linear regression between Sleep Efficiency and Age
plot(clean_sleep$Age, clean_sleep$Sleep.efficiency, xlab = "Age", ylab = "Sleep Efficiency")
age_efficiency_model <- lm(Sleep.efficiency ~ Age, data = clean_sleep)
abline(age_efficiency_model, col = "red")
summary(age_efficiency_model)

# correlation between Sleep Efficiency and Age
cor(clean_sleep$Sleep.efficiency, clean_sleep$Age)

## 4.2.3 Relationship between Sleep Efficiency and Sleep Duration
# linear regression between Sleep Efficiency and Sleep Duration
plot(clean_sleep$Sleep.duration, clean_sleep$Sleep.efficiency, xlab = "Sleep Duration", ylab = "Sleep Efficiency")
sd_efficiency_model <- lm(Sleep.efficiency ~ Sleep.duration, data = clean_sleep)
abline(sd_efficiency_model, col = "red")
summary(sd_efficiency_model)

# correlation between Sleep Efficiency and Sleep Duration
cor(clean_sleep$Sleep.efficiency, clean_sleep$Sleep.duration)

## 4.2.4 Relationship between Sleep Efficiency and Awakenings
# linear regression between Sleep Efficiency and Awakenings
plot(clean_sleep$Awakenings, clean_sleep$Sleep.efficiency, xlab = "Awakenings", ylab = "Sleep Efficiency", main = "Awakenings vs Sleep Efficiency")
awakenings_model <- lm(Sleep.efficiency ~ Awakenings, data = clean_sleep)
abline(awakenings_model, col = "red")
summary(awakenings_model)

## 4.2.5 Relationship between Sleep Efficiency and Alcohol Consumption
# linear regression between Sleep Efficiency and Alcohol Consumption
plot(clean_sleep$Alcohol.consumption, clean_sleep$Sleep.efficiency, xlab = "Alcohol Consumption", ylab = "Sleep Efficiency", main = "Alcohol Consumption vs Sleep Efficiency")
sleep_efficiency_model <- lm(clean_sleep$Sleep.efficiency ~ clean_sleep$Alcohol.consumption)
abline(sleep_efficiency_model, col = "blue")
summary(sleep_efficiency_model)

## 4.2.6 Relationship between Sleep Efficiency and caffeine consumption
# scatterplot of Sleep Efficiency and Caffeine Consumption
ggplot(clean_sleep, aes(x=Caffeine.consumption, y=Sleep.efficiency)) + geom_point() + geom_smooth(method=lm) +
  labs(title="Scatter Plot: Sleep Efficiency vs Caffeine Consumption", x="Caffeine.consumption", y="Sleep Efficiency") + theme_bw() 
caff_model <- lm(Sleep.efficiency ~ Caffeine.consumption, data = clean_sleep)
summary(caff_model)

## 4.2.7 Relationship between Sleep Efficiency and Smoking Status
clean_sleep$Smoking.status = as.factor(clean_sleep$Smoking.status)

# boxplot of Sleep Efficiency by Smoking Status
ggplot(clean_sleep, aes(x=Smoking.status, y=Sleep.efficiency)) +geom_boxplot(fill="blue", alpha=0.2) + xlab("Smoking Status") + ylab("Sleep Efficiency") + ggtitle("Boxplot of Sleep Efficiency by Smoking Status")

# wilcoxon rank sum test
sleepesmoke = clean_sleep$Sleep.efficiency[clean_sleep$Smoking.status == "1"]
sleepenosmoke = clean_sleep$Sleep.efficiency[clean_sleep$Smoking.status == "0"]
wilcox.test(sleepesmoke, sleepenosmoke)

## 4.2.8 Relationship between Sleep Efficiency and Exercise Frequency
# scatterplot of Sleep Efficiency vs Exercise Frequency
ggplot(clean_sleep, aes(x = Exercise.frequency, y = Sleep.efficiency)) +
  geom_point() +
  geom_smooth(method = "loess", span = 0.75, se = FALSE, color = "blue") +
  xlab("Exercise Frequency") +
  ylab("Sleep Efficiency") +
  theme_minimal()

# Kruskal-Wallis test
kruskal.test(Sleep.efficiency ~ Exercise.frequency, data = clean_sleep)

# Dunn test
dunn.test(clean_sleep$Sleep.efficiency, clean_sleep$Exercise.frequency, method = "bonferroni")

# Categorise exercise frequency into 3 groups
clean_sleep$ExFreqGroup[clean_sleep$Exercise.frequency==0|clean_sleep$Exercise.frequency==1]= "0-1"
clean_sleep$ExFreqGroup[clean_sleep$Exercise.frequency==2|clean_sleep$Exercise.frequency==3]= "2-3"
clean_sleep$ExFreqGroup[clean_sleep$Exercise.frequency==4|clean_sleep$Exercise.frequency==5]= "4-5"
clean_sleep$ExFreqGroup = ordered(clean_sleep$ExFreqGroup, c("0-1","2-3","4-5"))
#179 0-1 entries
length(clean_sleep$ExFreqGroup[clean_sleep$ExFreqGroup == "0-1"])
#158 2-3 entries
length(clean_sleep$ExFreqGroup[clean_sleep$ExFreqGroup == "2-3"])
#42 4-5 entries
length(clean_sleep$ExFreqGroup[clean_sleep$ExFreqGroup == "4-5"])
# boxplot of Sleep Efficiency by Exercise Frequency
ggplot(clean_sleep, aes(x=ExFreqGroup, y=Sleep.efficiency)) +geom_boxplot(fill="green", alpha=0.2) + xlab("Exercise Frequency") + ylab("Sleep Efficiency")

# Kruskal- Wallis rank sum test
kruskal.test(clean_sleep$Sleep.efficiency, clean_sleep$ExFreqGroup)

# pairwise comparison between the sleep efficiency of the 3 groups of exercise frequency
pairwise.wilcox.test(clean_sleep$Sleep.efficiency, clean_sleep$ExFreqGroup, p.adjust.method = "none")

## 4.2.9 Relationship between Sleep Efficiency and Bedtime
# categorise sleep timings into 2 groups
clean_sleep$BedtimeGroup[clean_sleep$Bedtime==21|clean_sleep$Bedtime==21.5|clean_sleep$Bedtime==22|clean_sleep$Bedtime==22.5] = "Before 10.30pm"
clean_sleep$BedtimeGroup[clean_sleep$Bedtime==23|clean_sleep$Bedtime==23.5|clean_sleep$Bedtime==0|clean_sleep$Bedtime==0.5|clean_sleep$Bedtime==1|clean_sleep$Bedtime==1.5|clean_sleep$Bedtime==2|clean_sleep$Bedtime==2.5] = "After 10.30pm"
clean_sleep$BedtimeGroup = as.factor(clean_sleep$BedtimeGroup)
clean_sleep$BedtimeGroup = ordered(clean_sleep$BedtimeGroup, c("Before 10.30pm","After 10.30pm"))
# boxplot of Sleep Efficiency and Bedtime
ggplot(clean_sleep, aes(x=as.factor(BedtimeGroup), y=Sleep.efficiency)) +geom_boxplot(fill="red", alpha=0.2) + xlab("Sleep Timing") + ylab("Sleep Efficiency")
# Wilcoxon rank sum test of Sleep Efficiency and Bedtime
sleepbefore2230 = clean_sleep$Sleep.efficiency[clean_sleep$BedtimeGroup == "Before 10.30pm"]
sleepafter2230 = clean_sleep$Sleep.efficiency[clean_sleep$BedtimeGroup == "After 10.30pm"]
wilcox.test(sleepbefore2230, sleepafter2230)

# 4.2.10 Most important factor affecting Sleep Efficiency
set.seed(100)
training.idx=sample(1:nrow(clean_sleep),nrow(clean_sleep)*0.8)
train.data=clean_sleep[training.idx, ]
test.data=clean_sleep[-training.idx, ]
lmodel=lm(Sleep.efficiency~.,data=train.data)
summary(lmodel)

modela = lm(Sleep.efficiency~ Awakenings, data = train.data)
summary(modela)
res = resid(modela)
plot(fitted(modela), res)
abline(0,0)
qqnorm(res)
qqline(res)
modelb = lm(Sleep.efficiency~ Bedtime, data = train.data)
summary(modelb)
res = resid(modelb)
plot(fitted(modelb), res)
abline(0,0)
qqnorm(res)
qqline(res)
modelc = lm(Sleep.efficiency~ Sleep.duration, data = train.data)
summary(modelc)
res = resid(modelc)
plot(fitted(modelc), res)
abline(0,0)
qqnorm(res)
qqline(res)

## 4.3 Multiple Linear Regression
model2 =lm(Sleep.efficiency~ Age + Wakeup.time + Caffeine.consumption + Alcohol.consumption + Awakenings + Sleep.duration + Bedtime + Gender, data = train.data)
step(model2, direction="backward")
