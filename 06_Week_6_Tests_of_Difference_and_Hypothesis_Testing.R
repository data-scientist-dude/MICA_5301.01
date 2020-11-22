##Week Six : Hypothesis Testing

## Load what you need
library(tidyverse)
library(lsr)
library(psych)
library(ggplot2)
library(gapminder)
library(stats)

## Check your working directory

getwd()

## Change your working directory if necessary

setwd("C:/Users/John/Documents")

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

## Hypothesis Testing 
# "Is there a significant difference in something?" 
# It is time to formalize our language so we can present our results.

# Null hypothesis: there is no difference
# Alternate hypothesis: a difference exists

# Test Statistic: calculated from the sample value, which has a known 
# distribution under the null hypothesis.

##Steps to conduct a hypothesis test.
# 0) Check your assumptions.  Some are different depending on the test.
# 1) Set null and alternative hypotheses
# 2) Choose alpha, level of significance
# 3) Calculate test statistic
# 4) Draw a picture
# 5) Find a p-value
# 6) Draw conclusion - is the p-value greater than alpha? If so, reject the null
# 7) Compute CI and double check your work

## p-value
#  a small p-value <= alpha, usually 0.05, indicates that the observed data
# is sufficiently inconsistent is with the null hypothesis, so the null 
# hypothesis may be rejected.  A larger p-value means that you 
# 'failed to reject the null' but....
#"IF THE P IS LOW, THE NULL MUST GO"

# t-tests are one of the more important tests in statistics.
# Used to determine whether the mean between two data points or samples are
# equal to each other.

# One sample t-test....
# https://cran.r-project.org/web/packages/distributions3/vignettes/one-sample-t-test.html
# Is the mean of a population equal to a specified mean?
# EXAMPLE ONE

set.seed(123)
var1 <- rnorm(100, mean=2, sd=1)
var2 <- rnorm(100, mean=3, sd=1)
var3 <- rnorm(100, mean=3, sd=2)
var4 <- rnorm(100, mean=2, sd=3)
data <- tibble(var1,var2,var3,var4)

# You are actually using a calculated t-statistic and degrees of freedom
# to estimate the p-value "using a t-table," (ie. cross reference a table from a book)
# If you want to find the t-statistic for this test by hand:
# step 1) first subtract the specified mean from the mean of the sample.  
# step 2) second divide the sample standard deviation by the square root of the sample size
# step 3) third divide step 1) by step 2)
# We won't describe every t-statistic in detail but you get the idea
# "mu" is the specified mean

t.test(data$var1, mu=0.6)

# EXAMPLE TWO
# You can actually calculate a p-value using a cdf command in R
# Check out the commented out link at CRAN to do so, you can check your work (step 7)
# however...here is the easy way
# A student wants to estimate how much time people spend showering daily
# step (0)

# read in the data
x <- c(3, 7, 11, 0, 7, 0, 4, 5, 6, 2)

# make the qqplot
qqnorm(x)
qqline(x)

# this looks pretty normal because the data is close to the lines

# step(1) the null hypothesis is that the mean is equal to three.
# the alternative hypothesis is that the mean is not equal to three.

# step(2) we can choose a significance level of 0.05
# step(3) comes from t.test(x)
# step(4) relates to drawing an actual curve if doing it by hand
# step(5) comes from t.test(x)
# step(6) is the p-value greater than the chosen alpha?
# step(7) a CI comes from t.test(x) but you can double check in R as well

t.test(x, mu = 3)

# Two Sample Independent t-test
# Compare two means of two independent samples

# this isn't a great format to show the formula for the relevant formulas for all
# the test statistic and the degrees of freedom but that is why we learn R Markdown.

# EXAMPLE ONE
# If the variance is equal...
set.seed(123)
var5 <- rnorm(100, mean=2, sd=1)
var6 <- rnorm(100, mean=3, sd=1)
var7 <- rnorm(100, mean=3, sd=2)
var8 <- rnorm(100, mean=2, sd=3)
data1 <- tibble(var5,var6,var7,var8)

qqnorm(data1)
qqline(data1)

t.test(data1$var5, data1$var6, var.equal = TRUE, paired = FALSE)

# the null hypothesis may be rejected

# EXAMPLE TWo
# If the variance is not equal...

t.test(data1$var5, data1$var6, var.equal = FALSE, paired = TRUE)

# the null hypothesis may be rejected
# the alternate hypothesis of one mean minus another mean does NOT equal zero
# is true at the 95% CI

# Two Sample Dependent (paired) t-test
# test the mean of two samples that depend on each other
# Is water quality different at the same location on 1 February 2019 and 1 February 2020
# Do Data Camp students improve on a standardized test before and after a class?

set.seed(123)
var9 <- rnorm(100, mean=2, sd=1)
var10 <- rnorm(100, mean=3, sd=2)
var11 <- rnorm(100, mean=3, sd=1)
data2 <- tibble(var9,var10,var11)

qqline(data2)

t.test(data2$var9, data2$var10, paired = TRUE)

# One Proportion z-test

# Is there a difference between counts of observations that form your
# sample expressed as a proportion of the total observations, and a 
# standard or recommended proportion?
# 'Approximately 16 percent  of the country population has an allergy,
# is the incidence of the allergy higher in Baltimore?'

# a good function exists that will let you do a one proportion z-test
# and find the same measures as you would by hand.
# Specify the number of successes, total number of observations, 
# the proportion you'd like to compare your sample to, the CI,
# and the form of alternative hypothesis.

source("https://raw.githubusercontent.com/NicoleRadziwill/R-Functions/master/ztest.R")

## or else here is the source code.  Note that you can put this address
## in your web browser and a text version appears

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

# 360 consumers were asked if they favor paying more for environmentally
# safe products.  312 of the respondents said yes.  Is the proportion of 
# ALL members of the population who would say yes less than 90% ?

z.test(312,360, p = 0.9)

# This relies on 'the Wald test' which can be somewhat inaccurate.  
# Better but more complex alternatives exist.  You can do a step (7) and
# check your CI using prop.test(x) or binom.test(x)<- (two tailed)
# each has their own adjustments and assumptions.  Be sure to read on each
# before making business decisions based on them.

prop.test(312,360, p = 0.9)

binom.test(312,360, p = 0.9)


# Two Proportion z-test

# Checks the difference between counts of observations that you collect 
# from your sample expressed as a proportion of the total observations in 
# one group, and copares it to the same thing, but measured in ANOTHER group.

# "Based on observation, coworkers are convinced they don't drink as heavily
#  as employees in the accounting division.  Is there a greater proportion of
#  heavy drinkers in your division or another division?"

# stratified sampling yields 206 coworkers and 170 accountants.
# 71 accountants drank more than six drinks or the equivalent four nights or
# more a week.  46 coworkers reported the same pattern.
# Is the proportion of heavy drinkers in accounting greater than the 
# proportion of heavy drinkers among our co-workers?

# What are some assumptions?  What are the hypotheses?  What should alpha be?

#if you follow the steps and calculate the test statistic by hand you could "draw"
# the curve first.

ggplot(data.frame(x=c(-4,4)), aes(x=x)) +
  stat_function(fun=dnorm) +
  stat_function(fun=dnorm, xlim=c(1.56,4), geom = "area")

# Source the function z2.test(x)
source("https://raw.githubusercontent.com/NicoleRadziwill/R-Functions/master/z2test.R")

# or use the code....  Note the author's comments within the function.
z2.test <- function(x1,n1,x2,n2,conf.level=0.95,
                    alternative="two.sided") {
  ts.z <- NULL
  cint <- NULL
  p.val <- NULL
  phat1 <- x1/n1
  qhat1 <- 1 - phat1
  phat2 <- x2/n2
  qhat2 <- 1 - phat1
  diff.phats <- phat1 - phat2
  pooled.p <- (x1 + x2)/(n1 + n2)
  pooled.q <- 1 - pooled.p
  SE.diffs <- sqrt( ((phat1*qhat1)/n1) + ((phat2*qhat2)/n2) )
  SE.pooled <- sqrt(pooled.p*pooled.q*((1/n1)+(1/n2)))
  # Why two SE's? SE.pooled is used in the calculation of
  # the test statistic z. We can pool because we are making
  # the assumption in the null hypothesis that there is no
  # difference between the two proportions.
  ts.z <- diff.phats/SE.pooled
  p.val <- pnorm(ts.z) # defaults to alternative="less"
  if(alternative=="two.sided") {
    if(p.val > 0.5) {
      p.val <- 1 - p.val  
      p.val <- p.val * 2
    }
    if(p.val > 1) { p.val = 1 }
  }
  if(alternative=="greater") {
    p.val <- 1 - p.val
  }
  cint <- diff.phats + c(
    -1*((qnorm(((1 - conf.level)/2) + conf.level))*SE.diffs),
    ((qnorm(((1 - conf.level)/2) + conf.level))*SE.diffs) )
  return(list(estimate=diff.phats,ts.z=ts.z,p.val=p.val,cint=cint));
  
#Running it on the drinking example looks like this...
  
z2.test(x1=71,x2=46,n1=206,n2=170,alternative = "greater")  

# There is more than one way to do this, the most common being prop.test(x)

prop.test(x=c(71,46),n=c(206,170),alternative = "greater")


# Chi-Square Test for Goodness of Fit
# When you have only one CATEGORICAL variable from a population and you want
# to compare whether the sample is consistent with a hypothesized distribution.

#EXAMPLE ONE:
data3 <- c(B=200,c=300,D=400)
chisq.test(data3)

#EXAMPLE TWO:

## roll dice six times
fair_rolls<- c(12,8,11,9,7,5)
## does it come out to a uniform distribution?
chisq.test(fair_rolls, p=c(1/6,1/6,1/6,1/6,1/6,1/6))

#EXAMPLE THREE:

## roll an unfair set of dice
unfair_rolls<- c(17,3,13,7,5,15)
chisq.test(unfair_rolls, p=c(1/6,1/6,1/6,1/6,1/6,1/6))

# Chi-Square Test for Independence
# Are two categorical random variables independent, or alternatively,
# does a relationship exist between them?

## Suppose you want to use a chi-square test of association to determine 
## whether sex and eye color are associated, using the people2 dataset

people2 <- read.csv("https://raw.githubusercontent.com/Apress/using-r-for-statistics/master/Individual%20Datasets/people2.csv")

people3 <- tibble(people2, rm.na = FALSE)

glimpse(people3)

summary(table(people3$Sex, people3$Eye.Color))

## Some cells have expected counts less than five. 
## This means that the results may be unreliable.  Go back, wrangle data,
## find those cells with a value of less than five and decide what to do with them.

# Fisher's Exact Test (association)
## we can see the counts of handedness by gender
table(people3$Sex, people3$Handedness)

## Perform a test has the null hypothesis that sex and
## handedness are independent, and the alternative hypothesis that they are associated. 
## A significance level of 0.05 is used.

fisher.test(people2$Sex, people2$Handedness)

##we cannot reject the null hypothesis that sex and handedness are independent. 
## This means that there is no evidence of a relationship between sex and handedness.

## Proportion Test (association)
## This allows you to compare the proportion of observations
## with a a given outcome or attribute across two or more groups of
## observations to determine if they are significantly different.
#
## You must have two columns.  You can also 'transpose' using R if it is instead 2xn

prop.test(c(15,8), c(26,25))

# A very abbreviated example but the first list has successes and the second list has total
# number of observations.
##Null hypothesis is that NO difference exists. We cannot reject the 
## null that the proportion of 'successful outcomes' in the is the 
## same in both groups.
## How could this apply to an experimental group and a control group?

## ANOVA
## Allows you to compare the means of three or more independent samples.
head(PlantGrowth)

boxplot(PlantGrowth$weight ~ as.factor(PlantGrowth$group), xlab = 'group', ylab = 'weight')

aov_test_one <- aov(weight ~ group, PlantGrowth)

anova(aov_test_one)

coef(aov_test_one)

## NON-PARAMETRIC TESTS
# The variable and sample do not have to be normally distributed.
# What can you do if normally distributed data does not exist and the 
# sample data is big?

## Wilcoxon-Mann-Whitney Test
# test hypotheses about one or two sample means (the bizarro t-test)

head(sleep)
wilcox.test(extra ~ group, sleep, paired = T, alternative = 'two.sided')


## Kruskal Walls Test
# an alternative to ANOVA that relaxes the dependency on an 
## assumption of normality

library(MASS)
head(survey)

boxplot(survey$Age ~ as.factor(survey$Smoke), xlab = 'Smoke', ylab = 'Age')

kruskal.test(Age ~ Smoke, data = survey)
# What does this tell us?

# Exercises with 'mtcars'

# 1) make a boxplot for mtcars comparing mpg(y) and gear(x) variables.

#2) apply the ANOVA to examine if the mean of mpg changes with different
# numbers of forward gears.  What is the p-value?

#3) make a boxplot for mtcars comparing the mpg(y) and transmission type(x)

#4) use the Wilcox-Mann-Whitney test to determine if the distribution of 
# automatic transmissions is identical to manual.

#5) first make a table comparing the am and gear vectors.  Perform a chi-square
# test on this table to see if the number of gears in automatic and manual 
# transmission cars are the same.


## Instructor's note: these are not comprehensive, there are many
## good tests as well as many nuances and details that are not mentioned in
## this document.  To learn more, read Radziwill's "Statistics (the easier way) with R"
## It contains a decision tree type of glossary to find, write and interpret the 
## correct test for your situation.