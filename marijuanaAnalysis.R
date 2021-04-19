## The data will be analyzed by Quantity, Location, Usage
library(ggplot2)
library(dplyr)
library(plyr)
library(lubridate)

##Exploratory Analysis
marijuana=read.csv("cannabis file for 472.csv", header = TRUE, stringsAsFactors = FALSE)
attach(marijuana)
head(marijuana, n=10)

marijuana$Submission.Date <- as.Date(marijuana$Submission.Date)


##Overall data before and after prices inlcuding
prior <- filter(marijuana, marijuana$Submission.Date<"2018-10-17" & marijuana$Submission.Date>="2018-07-01")
post <- filter(marijuana, marijuana$Submission.Date>="2018-10-17")

median(prior$Price)
mean(prior$Price)

median(post$Price)
mean(post$Price)


## Analyzing Data By Quantities 1, 3.5, 7, 14, and 28 since there are relatively large number of
## observations for these values
q1 <- filter(marijuana, marijuana$Quantity==1)
q1to5 <- filter(marijuana,marijuana$Quantity>1 & marijuana$Quantity<=5)
q5to10 <- filter(marijuana, marijuana$Quantity>5 & marijuana$Quantity<=10)
q10plus<- filter(marijuana, marijuana$Quantity>10)

#Overall Median and Mean Calculations
median(q1$Price)
median(q1to5$Price)
median(q5to10$Price)
median(q10plus$Price)

mean(q1$Price)
mean(q1to5$Price)
mean(q5to10$Price)
mean(q10plus$Price)

## Period 1 = p1 => 2018-01-01 to 2018-03-30
## Period 2 = p2 => 2018-04-01 to 2018-06-30
## Period 3 - p3 => 2018-07-01 to 2018-10-16
## Period 4 = p4 => 2018-10-17 to 2019-01-31

## Q1 Median and Mean Prices over Periods Pi i=3 and i=4 representing 4 months prior and post-legalization
q1p3 <- filter(q1, q1$Submission.Date>="2018-07-01" & q1$Submission.Date<"2018-10-17")
q1p4 <- filter(q1, q1$Submission.Date>="2018-10-17" & q1$Submission.Date<"2019-04-01")

median(q1p3$Price, na.rm = TRUE)
median(q1p4$Price, na.rm = TRUE)

mean(q1p3$Price, na.rm = TRUE)
mean(q1p4$Price, na.rm = TRUE)

## Ontario
q1p3on <- filter(q1p3, q1p3$Province=="Ontario")
q1p4on <- filter(q1p4, q1p4$Province=="Ontario")
median(q1p3on$Price)
median(q1p4on$Price)
mean(q1p3on$Price)
median(q1p4on$Price)

## Non-ontario
q1p3noton <- filter(q1p3, q1p3$Province!="Ontario")
q1p4noton <- filter(q1p4, q1p4$Province!="Ontario")
median(q1p3noton$Price)
median(q1p4noton$Price)
mean(q1p3noton$Price)
median(q1p4noton$Price)

## Q1to5 Median and Mean Prices over Periods Pi i=3 and i=4 representing 4 months prior and post-legalization
q1to5p3 <- filter(q1to5, q1to5$Submission.Date>="2018-07-01" & q1to5$Submission.Date<"2018-10-17")
q1to5p4 <- filter(q1to5, q1to5$Submission.Date>="2018-10-17" & q1to5$Submission.Date<"2019-04-01")
median(q1to5p3$Price, na.rm = TRUE)
median(q1to5p4$Price, na.rm = TRUE)

mean(q1to5p3$Price, na.rm = TRUE)
mean(q1to5p4$Price, na.rm = TRUE)

## Q5to10 Median and Mean Prices over Periods Pi i= 3 and i=4
q5to10p3 <- filter(q5to10, q5to10$Submission.Date>="2018-07-01" & q5to10$Submission.Date<"2018-10-17")
q5to10p4 <- filter(q5to10, q5to10$Submission.Date>="2018-10-17" & q5to10$Submission.Date<"2019-04-01")

median(q5to10p3$Price, na.rm = TRUE)
median(q1to5p4$Price, na.rm = TRUE)

mean(q5to10p3$Price, na.rm = TRUE)
mean(q5to10p4$Price, na.rm = TRUE)

## Q10plus Median and Mean Prices over Periods Pi i=3 and i=4
q10plusp3 <- filter(q10plus, q10plus$Submission.Date>="2018-07-01" & q10plus$Submission.Date<"2018-10-17")
q10plusp4 <- filter(q10plus, q10plus$Submission.Date>="2018-10-17" & q10plus$Submission.Date<"2019-04-01")

median(q10plusp3$Price, na.rm = TRUE)
median(q10plusp4$Price, na.rm = TRUE)

mean(q10plusp3$Price, na.rm = TRUE)
mean(q10plusp4$Price, na.rm = TRUE)

##Regression looking at legalization and Price
legalization <- as.numeric(marijuana$Submission.Date>="2018-10-17")
usage <- as.numeric(marijuana$Purpose=="Use recreationally")
location <- as.numeric(marijuana$Province == "Ontario")
marijuana.lm <- lm(marijuana$Price~legalization+marijuana$Quantity+usage+location)
summary(marijuana.lm)

##Regression of Prices ~ Residual
plot_summs(marijuana.lm, scale=TRUE, plot.distributions = TRUE)
pairs(~marijuana$Price+marijuana$Quantity+marijuana$Submission.Date)
