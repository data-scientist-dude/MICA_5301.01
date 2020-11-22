##Week Five : Inference and Estimation

## Load what you need
library(tidyverse)
library(lsr)
library(psych)
library(ggplot2)
library(gapminder)
library(stats)
library(GGally)
library(car)

## Check your working directory

getwd()

## Change your working directory if necessary

setwd()

## Warm up with some basic functions
warm_up <- c(8,6,7,5,3,0,9)
var(warm_up)
sd(warm_up)
range(warm_up)
IQR(warm_up)
mean(warm_up)
median(warm_up)
table(warm_up)

boxplot(warm_up)

setosa <- filter(iris, Species == "setosa")
dim(setosa)
ggplot(setosa)  + 
  geom_point(aes(x = Sepal.Length, y = Sepal.Width))

##Do some initial exploration of 'gapminder' if you are unfamiliar

names(gapminder)
dim(gapminder)
str(gapminder)
summary(gapminder)

## Lesson 4 we examined a sample (often a variable) to describe the sample (descriptive).
## Lesson 5 we're drawing inferences ABOUT the population from one sample or more.  What are the population parameters?
## Lesson 6 we'll ask a research question, "Is there...?" and conduct hypothesis testing and tests of difference.

# Review of sampling techniques
# Simple Random Sampling (SRS) - select elements from the full data set randomly.  All have the 
# the same probabilty of being selected.

# Stratified - 1) divide population into groups, 2) use SRS on each group,
# 3) Collect data from each group
## For example 13 Baltimore neighborhoods are the strata 
## Stratified sampling: 30 residents from each neighborhood are selected using SRS

# Cluster - 1) divide population into clusters, 2) use SRS on all possible clusters,
# 3) Collect data from a sampling unit
## For example 13 Baltimore neighborhoods are the clusters
## All members of the selected clusters form a sample
## Cluster sampling: All residents in 5 of the 30 neighborhoods are selected using SRS

#random sampling

set.seed(123)
var1 <- rnorm(100, mean = 2, sd = 1)
var2 <- rnorm(100, mean = 3, sd = 1)
var3 <- rnorm(100, mean = 3, sd = 2)

data1 <- tibble(var1, var2, var3)
sample(data1$var1, 5, replace = TRUE)

# Stratified sampling with DPLYR

data("iris")
summary(iris)

iris_sample <- iris %>%
  group_by(Species) %>%
  sample_n(13)

iris_sample

plot(iris_sample)

## Review of cor(x) and cov(x) functions

# Correlation finds how close two variables are, but not tell you the "how" or "why."
# You can get a number between -1 and 1 that describes the relationship.
# If it is zero, there is not a meaningful relationship.
cor(data1$var1, data1$var2)

plot(data1$var1, data1$var2, col = "orange3", pch = 22)

# Covariance is a measure of variability between two variables.  
# The greater the value of one variable and the greater of another variable
# means the covariance will be positive. It shows a linear relationship 
# but the magnitude can be difficult to interpret.
# Covariance has no range.

cov(iris$Petal.Length, iris$Sepal.Length)

plot(iris$Petal.Length, iris$Sepal.Length, col = "black", alpha = 1.3, pch = 13)

## Inferences about a population based on a sample
# brief powerpoint slides

## CONFIDENCE INTERVALS AND STANDARD ERROR
# "How well does a sample statistic provide an estimate of a population parameter?"

# standard error -- how precisely you can know the true number

# confidence interval (CI)-- a range of values that could represent the population parameter

# confidence level -- how likely the true number lies within the confidence interval
#(often 90%, 95% or 99%)

# sampling distribution -- the entire collection of possible samples i could have acquired

# How could you get the CI when you only the mean and standard deviations
# of 'n' observations of data?

#### ONE MEAN

## CI is the estimate plus or minus a margin of error.
## The estimate is the average value computed from the sample.
## The margin of error is the critical value times the standard error.
## The standard error depends if you know the variance of the population.
## If you do know the population variance, use it!
## Critical value t or z times the quotient of the variance divided by the square root of 'n'##

#R-code to find critical t

qt(.90, df = 29)
qt(.95, df = 29)
qt(.99, df = 29)

# You can compute the CI at any confidence level with this function...
## the easy way to get it is to pull it from GitHub
source("https://raw.githubusercontent.com/NicoleRadziwill/R-Functions/master/cimean.R")

## quick test of the function...

ci.mean(30,10, s=12, vname= "probably the mean")

## or the source code if you can't pull from GitHub.
## Read it and interpret the code.

ci.mean <- function(n,ybar,s=NULL,sigma=NULL,conf.level=0.95,
                    vname="[difference between the quantitative variables]") {
  # Example: ci.mean(50,27.3,sigma=12.1)
  z.star <- NULL
  t.star <- NULL
  ME <- NULL
  cint <- NULL
  if(!is.null(sigma)) {
    # we know the population variance so look up z*
    z.star <- qnorm(conf.level+((1-conf.level)/2))
    ME <- z.star*(sigma/sqrt(n))
  }
  if(!is.null(s)) {
    # We only know the sample sd so look up t*
    t.star <- qt(conf.level+((1-conf.level)/2),df=(n-1))
    ME <- t.star*(s/sqrt(n))
  }
  cint <- ybar + c(-ME,ME)
  short <- sprintf("%s%% CI: %.3f+/-%.3f or (%.3f, %.3f)", 
                   (conf.level*100), ybar, ME, cint[1], cint[2])
  verbose <- sprintf("We are %s%% confident that the true %s is between %.3f and %.3f.",
                     (conf.level*100), vname, cint[1], cint[2])
  return(list(short=short,verbose=verbose,cint=cint))
}

## EXAMPLE ONE: Drug Bust
# The DEA thinks a farmer is growing poppies on their 3000 acre property.
# They random sample by satellite photo 30 1-acre segments.
# There is an average of 16 plants on each one with sample standard deviation of 11.7
# The threshold for a bust is 50,000 plants.  Do they conduct the raid?

ci.mean(30,16, s=11.7, vname= "number of plants per acre") #default confidence level is .95

## EXAMPLE TWO: Donut Hole Factory
# Baltimore donuts manufactures 1000 bags of donut holes daily.
# They random sample 30 bags.
# There is an average of 10 donut holes in each one with a sample standard deviation of 4.
# There is supposed to be a dozen donut holes in each.  Is there a potential problem in production?

ci.mean(30,10, s=4, vname= "number of donut holes per bag", conf.level = .95)

## Can you think of an example from your professional domain?  How do you set a standard for the mean?
## How do you know what confidence level to select?

# TWO MEANS
# How well does the difference between TWO sample means approximate
# the difference between TWO population means?
## If you do know the population variances, use them!
## If the two population variances are different it will affect what formula is used.

# You can compute the CI at any confidence level with this function...
## the easy way to get it is to pull it from GitHub
source("https://raw.githubusercontent.com/NicoleRadziwill/R-Functions/master/citwomeans.R")

## quick test of the function...

ci.twomeans(30,10, s=12, vname= "probably the mean")

## or the source code if you can't pull from GitHub.
## Read it and interpret the code.

ci.twomeans <- function(ybar1,ybar2,n1,n2,sd1=NULL,sd2=NULL,
                        sigma1=NULL,sigma2=NULL,eq.var=FALSE,conf.level=0.95,
                        vname="[difference between the quantitative variables]") {
  # Example: ci.twomeans(18.27,16.78,20,19,sqrt(8.74),sqrt(6.58),eq.var=FALSE)
  # We assume a conf.level of 0.95 and eq.var=FALSE just so
  # you have to change it to TRUE if you REALLY mean it
  z.star <- NULL
  t.star <- NULL
  ME <- NULL
  cint <- NULL
  my.df <- NULL # we only need this if we DO know the population variance
  diff.ybars <- ybar1-ybar2
  if(!is.null(sigma1)) {
    # we've been given two population SD's so we
    # DO know the population variance
    z.star <- qnorm(conf.level+((1-conf.level)/2))
    ME <- z.star*sigma1*(sqrt((1/n1)+(1/n2)))
  }
  if(!is.null(sd1)) {
    # we've been given two sample SD's so we
    # DON'T know the population variance
    if(eq.var==FALSE) {
      my.df <- (((sd1^2/n1)+(sd2^2/n2))^2)/((((sd1^2/n1)^2)/(n1-1))+(((sd2^2/n2)^2)/(n2-1)))
      t.star <- qt(conf.level+((1-conf.level)/2),df=my.df)
      ME <- t.star*sqrt((sd1^2/n1)+(sd2^2/n2))
    } else {
      my.df <- n1+n2-1
      s.pooled <- (((n1-1)*(sd1^2))+((n2-1)*(sd2^2)))/my.df
      t.star <- qt(conf.level+((1-conf.level)/2),df=my.df)
      ME <- t.star*s.pooled*sqrt((1/n1)+(1/n2))
    }
  }
  cint <- diff.ybars + c(-ME,ME)
  short <- sprintf("%s%% CI: %.3f+/-%.3f or (%.3f, %.3f)", 
                   (conf.level*100), diff.ybars, ME, cint[1], cint[2])
  verbose <- sprintf("We are %s%% confident that the true %s is between %.3f and %.3f.",
                     (conf.level*100), vname, cint[1], cint[2])
  return(list(short=short,verbose=verbose,cint=cint))
}

## EXAMPLE ONE: Certification Test Standards
# A quality improvement specialist needs to look at a difference in certification exam scores.
# Two classes had a 1000 students each. The second had an updated training manual to use.
# There is an average of 86.71 for the first group, and an average of 79.22 for the second group.
# How do you put a 99 % CI around the mean difference between the two?

ci.twomeans(86.71,78.22,100,100,sd1 = 6.45,sd2 = 7.65, conf.level = .99, vname= "mean difference between test scores")

## EXAMPLE TWO: Dorm Infractions
# University of Baltimore has two dorms with 500 students each.
# A random sample of 50 in each dorm yielded 4 infractions per student in dorm one versus 3 in dorm two.
# How do you put a 95 % CI around the mean difference between the two?

ci.twomeans(4,8,50,50,sd1 = 2, sd2 = 2,conf.level = .95, vname= "mean difference between infractions")

## MORE EXAMPLES: a manufacturer who wishes to estimate the difference in mean daily output 
## from two machines; a medical researcher who wishes to estimate 
## the difference in mean response by patients who are receiving two different drugs; etc.
## Week Six we discuss using these tests with hypotheses.

# PAIRED MEANS
# Sometimes an observed variable in one group is paired with an observed variable in a second group
# They are DEPENDENT on one another.  For example...
# The same person before and after an event.
# A data set has cholesterol levels in 1952 and cholesterol levels in 1962 for each subject.

# You can compute the CI at any confidence level with this function...
## the easy way to get it is to pull it from GitHub
source("https://raw.githubusercontent.com/NicoleRadziwill/R-Functions/master/cipaired.R")

ci.paired <- function(n,dbar,s.d,conf.level=0.95,
                      vname="[difference between the quantitative variables]") {
  # Example: ci.paired(8,3.625,2.066,vname="difference between number of recalled words")
  t.star <- NULL
  ME <- NULL
  cint <- NULL
  if(!is.null(s.d)) {
    t.star <- qt(conf.level+((1-conf.level)/2),df=(n-1))
    ME <- t.star*(s.d/sqrt(n))
  }
  cint <- dbar + c(-ME,ME)
  short <- sprintf("%s%% CI: %.3f+/-%.3f or (%.3f, %.3f)", 
                   (conf.level*100), dbar, ME, cint[1], cint[2])
  verbose <- sprintf("We are %s%% confident that the true %s is between %.3f and %.3f.",
                     (conf.level*100), vname, cint[1], cint[2])
  return(list(short=short,verbose=verbose,cint=cint))
}

## EXAMPLE ONE: Weight Loss
# You drink one cup of pomegranate juice every morning.  After six weeks you have lost 15 pounds.
# 12 friends start to do the same and you record their weight loss.
# If you can get a mean weight loss within a reasonable confidence level you will
# market the pomegranate juice diet and possibly become very wealthy.
# Your friends have an average weight loss of 8.6 with sample standard deviation of 6.5
# Do you market the weight loss product or conduct follow-up studies? (hint: causation)

ci.paired(12,8.6,s.d = 6.5, vname= "number of pounds lost") #default confidence level is .95
# You can be 95 percent confident that the true mean number of pounds
# ' all people' will lose will be....   What kind of follow-up study could you conduct?

## EXAMPLE TWO: General Assemb.ly Course Effectiveness
# 100 students take a pre-test before a data science course, then the same test afterwards.
# The mean of all differences of the pre-test and after test is 8.
# What kind of difference could you reasonably market to potential corporate clients?

ci.paired(100,8,s.d = 4, conf.level = .99, vname= "score difference") #default confidence level is .99
# You can be 99 percent confident that the true mean score difference of
# ' all people' will be....   What kind of follow-up study could you conduct?

# ONE PROPORTION
# How well does a sample proportion approximate a population proportion?
# Unlike other CIs, this is not necessarily symmetrical.
# There are many variations for one proportion but due to constraints
# this is just about the Wilson score.
# Working with a Wilson score by hand has been described as "suffocating."
# However it is very accurate and R makes it easier to use.

prop.test(39, 120, p = .25, conf.level = .95) #Wilson is default used in the prop.test(x) function

?prop.test()

#or else install the PropCIs package for a simplified output
install.packages("PropCIs")
library(PropCIs)

scoreci(39,120, conf.level = .90) # probability by default is .5

#EXAMPLE ONE: A population of mice containing half male and have female (p = 0.5 = 50%). 
## Some of these mice (n = 160) have developed a spontaneous cancer, including 95 male and 65 female.
# Does the cancer affect more males than females?

binom.test(95, 160, p = 0.5, alternative = "two.sided") ## you could actually use binom.test(x)
prop.test(95, 160, p = NULL, alternative = "two.sided",
          correct = TRUE)

##The p-value of the test is 0.01771, which is less than 
## the significance level alpha = 0.05. We can conclude 
## that the proportion of male with cancer is significantly different from 0.5 with a p-value = 0.01771.

#EXAMPLE TWO: A population of consumers half boomers and half gen-Xers (p = 0.5)
## Some consumers (n = 222) don't understand how to use a smart phone app, including 
## 109 boomers and 113 gen-Xers and no other categories (factors!)
# Is noncomprehension more a trait of boomers than gen-Xers?
# Are we going to have a problem marketing this app to boomers?

prop.test(109, 222, p = NULL, alternative = "two.sided",
          correct = TRUE)

# TWO PROPORTIONS
# How well does the difference between two sample proportions approximate 
# the difference between two population proportions?
# Unlike other CIs, this is not necessarily symmetrical.
# There are many variations for two proportion tests but due to constraints
# this is just about the Newcombe Hybrid score.
# It is recommended by researchers for its balanced approach but
# like many methods it is not ideal for small sample sizes.
# It is very accurate and R makes it easier to use.
# The basic principles in terms of "CI is the estimate plus or minus error"
# apply to all these methods, but the mathematical models are different
# and worth reading about.

install.packages("pairwiseCI")
library(pairwiseCI)

## I did feel the need to comment this out and define some arguments.

##pairwiseCI(cbind(successes, failures)  ~group, data=my.props,
## method = "Prop.diff", CImethod = "NHS")

## EXAMPLE ONE:  A/B testing is often used by interface designers.
## Let's see if users prefer Design "A" over Design "B."
## If zero does not appear in the CI you have some evidence that 
## there is a preference for one over the other.
# Two samples have 200 people each.  In one group 92 out of 200 prefer Design "A."
# In the second group 64 out of 200 prefer Design "B."

successes <- c(92,64)
failures <- c(108,136)
group <- c(1,2)
my.props <- data.frame(cbind(successes, failures, group))

pairwiseCI(cbind(successes, failures)~group, data = my.props, 
method="Prop.diff", CImethod = "NHS")

# EXAMPLE TWO: Let's see if people prefer dogs or cats.
# Group A and Group B are both n = 500
# The preferences in each group is as follows:
# Group A those who prefer dogs: 99
# Group B those who prefer cats: 78

successes2 <- c(99,78)
failures2 <- c(401,422)
group2 <- c(1,2)
my.props2 <- data.frame(cbind(successes2, failures2, group2))

pairwiseCI(cbind(successes2, failures2)~group2, data = my.props2, 
           method="Prop.diff", CImethod = "NHS")
# causation? Other factors?  Further study?

# ONE VARIANCE
# How well does the sample variance provide an estimate of the population variance?
# This is also no symmetric.
# It can be expressed as "greater than or equal to, and less than or equal to."
# Upper and lower bounds are expressed as Chi-Square.
# The numbers look off for each CI because part of the mathematical
# model is finding numbers NOT included in the CI.
# Straight to an example....
# EXAMPLE: Smartphone Thickness
# Apple takes a sample of 32 smart phones and finds a mean thickness of .82cm
# and a sample standard deviation of .08cm.  What is a 99 % CI of 
# the variance of the thickness of the sample?

qchisq(.995, df = 31) # Finding Chi-Square UPPER
qchisq(.005, df = 31) # Finding Chi-Square LOWER

#There may be a function somewhere out there, but you would plug them into this formula
UPPER = (31 * (.08^2)) / 55.0027
LOWER = (31 * (.08^2)) / 14.45777

print("We are 99% confident that the true variance of thickness of our smartphones is between these two values.")
UPPER
LOWER

#Google to find the other chi-square codes, but you get the idea.

# TWO VARIANCES
# How well does the ratio of two sample varances provide an estimate of the true
# ratio between population variances?
# If the CI does not contain 1, there is evidence that the variance is different
# between the groups.
# This statistic is used to improve a given real-world process, 
# making it more robust or reliable.
# The result is not symmetric and works off of an F distribution.

#EXAMPLE: You prefer to live somewhere that temperatures are less variable.
# Take a random sample of low winter temperatures for 24 days.
# Where you live now has an average low of 29 degrees fahrenheit.
# The variance is 2.6 degrees fahrenheit.
# A candidate location has an average low of 25 degrees fahrenheit.
# The variance is 3.2 degrees fahrenheit.
# Let's find a 95 % CI around the ratio of variances from the sample.

qf(.975,df1 = 19, df2 = 23) ## Find F - UPPER
qf(.025,df1 = 19, df2 = 23) ## Find F - LOWER

UPPER =  (2.6/3/2) * 2.374466 
LOWER =  (2.6/3/2) * .4057143 

print("We are 95% confident that the true ratio of variances is between these two values.")
UPPER
LOWER
##Since the value of 1 DOES appear, there is no evidence that 
##the variability in temperatures for the two locations is different.

##Plotting CIs on Box Plots

ToothGrowth$dose <- as.factor(ToothGrowth$dose)
head(ToothGrowth)

library(ggplot2)
# Basic box plot
p <- ggplot(ToothGrowth, aes(x=dose, y=len)) + 
  geom_boxplot()
p
# Rotate the box plot
p + coord_flip()

# NOTCHED box plot
ggplot(ToothGrowth, aes(x=dose, y=len)) + 
  geom_boxplot(notch=TRUE)

# Change outlier, color, shape and size
ggplot(ToothGrowth, aes(x=dose, y=len)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=4)

### STATISTICAL INFERENCE ###

# One Sample t-test (an introduction with more detail next week)
# What if we random sample 17 cars during rush hour on Smith Road?
# Here is our data:
speeds <- c(29,25,22,34,38,40,27,29,30,30,23,34,42,36,35,27,37,28)
length(speeds)
summary(speeds)
sd(speeds)
mean(speeds)
# EXAMPLE ONE: Rush Hour
# What is a 95 % CI around the speeds of all the cars in rush hour on Smith Road?

t.test(speeds, mu=31) #I'm taking a guess at the population mean.
# We are 95 % confident that the true mean of cars driving down Smith Road
# at rush hour is between these numbers.

# EXAMPLE TWO: Restaurant Tips
# On average wait staff working at a restaurant makes $180- in tips on the weekend.
# Do you earn an average of exactly $180- in tips on weekends?
tips <- c(181,79,200,166,114,195,183,199,169,201)

t.test(tips, mu=180, conf.level = .90)

# One Sample Proportion z-test (a function or else us the exact binomial test)

# Is there a difference between counts of observations that
# form your sample expressed as a proportion of the total observations
# and a standard or recommended proportion.
## This is frequently used for comparison against a goal or a target.

source("https://raw.githubusercontent.com/NicoleRadziwill/R-Functions/master/ztest.R")

#..or else there is the source code.

z.test <- function(x,n,p=NULL,conf.level=0.95,alternative="less") {
  ts.z <- NULL
  cint <- NULL
  p.val <- NULL
  phat <- x/n
  qhat <- 1 - phat
  # If you have p0 from the population or H0, use it.
  # Otherwise, use phat and qhat to find SE.phat:
  if(length(p) > 0) {
    q <- 1-p
    SE.phat <- sqrt((p*q)/n)
    ts.z <- (phat - p)/SE.phat
    p.val <- pnorm(ts.z)
    if(alternative=="two.sided") {
      if(p.val > 0.5) {
        p.val <- 1 - p.val
        p.val <- p.val * 2
      }
    }
    if(alternative=="greater") {
      p.val <- 1 - p.val
    }
  } else {
    # If all you have is your sample, use phat to find
    # SE.phat, and don't run the hypothesis test:
    SE.phat <- sqrt((phat*qhat)/n)
  }
  cint <- phat + c(
    -1*((qnorm(((1 - conf.level)/2) + conf.level))*SE.phat),
    ((qnorm(((1 - conf.level)/2) + conf.level))*SE.phat) )
  return(list(estimate=phat,ts.z=ts.z,p.val=p.val,cint=cint))
}

## EXAMPLE ONE: Consultants used cluster sampling to survey 360 homebuyers
# in 8 areas used by real estate agents.  They asked prospective buyers whether
# they would invest at least 2 - 3 % of the home's value in an energy efficient home.
# 312 said yes.  Is the proportion of ALL members of the population who
# would make this investment less than 90 % ?

z.test(312,360)
# We are 95 % confidient that the true proportion of homebuyers from our city
# who would invest 2 to 3 % of the homes value is between....
# but is this CI enough?  What else could we do?

z.test(312,360,p = .9)


## Simple Linear Regression (Hui, location 2396)
# Objective: develop a linear model to describe and asses the relationship
# between two continuous, quantitative variables.
## One is independent and predicts the other one.
## The other is dependent and predicted by the other one.
## The simplest alternative is plotting all the points and using the 
## mean as a predictor.

tips
mean(tips)

plot(tips, col = "red", pch = 4)

##EXAMPLE ONE:

## Let's use data from the daily weather reports of the Shenandoah Valley Airport
# near Harrisonburg, VA.  Load directly from GitHub.

library(tidyverse)

shd_wx <- read.delim("https://raw.githubusercontent.com/NicoleRadziwill/Data-for-R-Examples/master/kshd-2013.txt")
str(shd_wx)
shd_wx <- tibble(shd_wx)
str(shd_wx)

names(shd_wx)
# These are kind of hard to read so install a package to help...

install.packages("janitor")
library(janitor)

# Now read the data in again...

shd_wx <- janitor::clean_names(read.delim("https://raw.githubusercontent.com/NicoleRadziwill/Data-for-R-Examples/master/kshd-2013.txt"))
shd_wx <- tibble(shd_wx)

# What would a scatterplot of dewpoint versus temperature look like?
# 1) plot the dewpoint versus temperature
# 2) find a line that is the best fit for the data
# 3) plot the residuals
# 4) plot the equation for the best fit line

slope <- cor(shd_wx$dewp, shd_wx$temp) * ( sd(shd_wx$dewp)) / sd(shd_wx$temp) 
slope

intercept <- mean(shd_wx$dewp) - (slope* mean(shd_wx$temp))
intercept

## if you put these values into a linear model you can see the line of best fit...

fit <- lm(shd_wx$dewp ~ shd_wx$temp)
shd_wx$residuals <- resid(fit) #store the residuals for later

summary (fit)

#plotting all of this out...

left <- shd_wx %>%
    ggplot(aes(x=temp, y=dewp)) + geom_point() + 
    geom_smooth(aes(color="red"), se=FALSE, method="lm") +
    ggtitle("Dewpoint vs. Temperature") +
    theme(legend.position = "none")

right <- shd_wx %>%
  ggplot(aes(x=temp, y=residuals)) + geom_point() +
  geom_hline(yintercept = 0) + ggtitle("Residuals")

cowplot::plot_grid(left,right)

#double check the relationship of temperature and dewpoint with a correlation

pirate <- cor(shd_wx$dewp, shd_wx$temp)
pirate^2

summary(fit)$r.squared

par(mfrow=c(2,2))
plot(fit)

#EXAMPLE TWO:
#  x is a woman's age, y is the total number of kids she had at that particular age
# Using lm we can predict the number of kids by age.

df <- data.frame(age=c(21,24,25,32), number_of_kids=c(1,2,3,5))
new_fit <- lm(df$number_of_kids ~ df$age)
df$residuals <- resid(new_fit)

left <- df %>% ggplot(aes(x=age, y=number_of_kids)) +
  geom_point(size=5) +
  geom_smooth(aes(color="red"), se=FALSE, method="lm") +
  ggtitle("Number of Kids by Mother's Age at Birth") +
  theme(legend.position = "none")

right <- df %>% ggplot(aes(x=age, y=residuals)) + geom_point() +
  geom_hline(yintercept = 0) + ggtitle("Residuals")

cowplot::plot_grid(left,right)

summary(new_fit)

# But! according to the model, How many children should a woman have by age 50?
# y = 0.3538x - 6.0231
# y = 0.3538(50) - 6.0231 = 11.6669

## Multiple Linear Regression

##EXAMPLE ONE:
# Using the same weather data...

fit1 <- lm(shd_wx$dewp ~ shd_wx$temp + shd_wx$visib + shd_wx$wdsp + shd_wx$max +
             shd_wx$prcp, data = shd_wx)
summary(fit1)

#Try to eliminate some predictors to see if you can simplify the model
# without losing much explanatory power.  Drop the maximum daily temperature and try again.

fit2 <- lm(shd_wx$dewp ~ shd_wx$temp + shd_wx$visib + shd_wx$wdsp + shd_wx$max +
             shd_wx$prcp, data = shd_wx)
summary(fit2)

## One More Linear Regression on a chart 
# data on Old Faithful geyser eruptions is built into R

fit <- lm(waiting~eruptions, data=faithful)

plot(faithful)
lines(faithful$eruptions, fitted(fit), col="blue")
abline(v=3, col="purple")
abline(h=mean(faithful$waiting))
abline(a=coef(fit)[1], b=coef(fit)[2])
abline(fit, col = 'orchid2')


## https://rstudio.com/resources/cheatsheets/