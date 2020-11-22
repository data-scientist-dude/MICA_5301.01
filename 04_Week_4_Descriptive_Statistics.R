##Week Four : Descriptive Statistics

## Load what you need
library(tidyverse)
library(lsr)
library(psych)
library(ggplot2)
library(gapminder)
library(statsr)

## Check your working directory

getwd()

## Set your working directory if necessary

setwd("C:/Users/John/Documents")

## Warm up with some basic functions
warm_up <- c(0,2,3,2,1,0,0,2,1,5,8,3)
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

## DESCRIPTIVE STATISTICS part one, at our own pace tutorial

## Statistics is about decision making.
## You crunch numbers to come to conclusions.  Fortunately computers are
## very good at crunching numbers.  You mainly have to be good at what computers cannot do.
#
# If you are feeling pretty good with basic R at this point
# try doing some of the EDA with only Tidyverse commands

## Create your own vector.  

x <- c(0,2,3,2,1,0,0,2,0)

## Define the vector into a variable called x
x

## Determine the mean of x using the mean function

mean(x)

## Determine the median of x using the median function

median(x)

## Before we dive into variance and standard deviation
# try to build a histogram of x 

barplot(table(x))

## Determine the variance of x 

sum((x - mean(x))^2)/(length(x)-1)

var(x)

## Determine the standard deviation of x

sd(x)

## Here is a new data set.  Look at a histogram and identify outliers

y <- c(1,4,4,5,8,2,3,5,6,7,4,4,8)

## copy and paste your last histogram function here
# practice changing the x and y labels along with the color

barplot(table(y))

mean(y)

## let's talk about covariance and correlation 
# using the mtcars data set.  What are some of the ways
# you can take a quick look at it?  Remember there is 
# usually more than one way to skin a cat in R.

tibble(mtcars)

head(mtcars[1:3])

## select five rows and assign them to a variable

more_cars <- head(mtcars[1:3])

more_cars %>%
  filter( == "Hornet ")

glimpse(more_cars)

# covariance can be hard to explain and interpret but
# here is a start.  The directional relationship between two things.


## now use the cov() function on our chosen mtcars data

cov(mtcars[1:3])

## Correlation just requires you to divide the co-variance 
# by the standard deviations of the variables
# use the cor() function on our data while I explain more.

cor(more_cars)

##EXERCISES

## 1) what is the average and median sepal length for the
# Iris data set?  The average and median sepal length for only
# the setosa species?

mean(iris$Sepal.Length)

subset1 <- iris[iris$Species == 'setosa',]

mean(subset1$Sepal.Length)

## 2) what is the standard deviation and variance for the sepal
# width of the entire data set? What is it for only 
# the virginica species?

sd(iris$Sepal.Width)

## 3) create a histogram for the petal length of the entire data set.
## What is the most common petal length?

hist(iris$Petal.Length)

## 4) calculate the covariance and correlation for the data set.
## Do the relationships change if you only look at one species?

cov(iris[1:3])

cor(setosa[1:3])

str(setosa)


## RECAP

## The simplest form of analysis is describing a single variable.
## "Univariate" ##

## Try pulling up the mtcars data set again but with a 
# different function than you used earler

data(mtcars)

dim(mtcars)

## Now find the range of one vector or column of mtcars

range(mtcars$mpg)

## Compute the length of one variable

length(mtcars$mpg)

## Obtain the mean of mpg




## Obtain the median of mpg


## Obtain the standard deviation of mpg

## Obtain the variance of mpg

## Aside from var(mtcars$mpg), how else could you compute the variance?

sd(mtcars$mpg)^2

## Obtain the IQR of mpg

gas <- mtcars$mpg

IQR(gas)

## How could you find the quantile of a specific percentage?
## ex. I want to know what value MPG has 67 percent of the other
# observations below it

quantile(gas, 0.67)

## Obtain the maximum mpg

max(gas)
## Obtain the minimum mpg


## Obtain the cumulative maxima of mpg. 
## What is happening? The computer goes through all values and replaces the maximum
# value according to a conditional statement

cummax(gas)

## Obtain the cumulative minima of mpg.  How could this be useful?


## Use the summary(x) to look at mtcars

## How could you get a frequency count of mtcars$cyl ?
# Note that this is non-numeric data

table(mtcars$cyl)



## But maybe I would like a frequency count of numerical data
# such as mtcars$mpg.  How could that be done?  
# HINT : stem and leaf plots

stem(mtcars$mpg)


## How could you use ggplot2 ?  You could also use hist(mtcars$mpg)

hist(gas, col = "bisque")

# Congratulations! You just conducted univariate analysis
# a key part of descriptive statistics.  
##You have already done some bivariate analysis by way of cov(x) and cor(x)
# Univariate descriptive stats generate a frequency distribution
# It can identify obvious patterns, and promotes an understanding of the data
# It can identify central tendency and skewness descriptors
# It is used at the beginning of the data exploration process

cov(mtcars$mpg, mtcars$cyl)

## More DESCRIPTIVE STATISTICS with a more complicated data set (almost; thanks subset)
## 'univariate analysis' is somewhat of a misnomer
## it just presents what the data looks like before a thorough analysis

# load data from the NOAA
# note how you could download directly from GitHub
# set up an account first just to be certain or else 
# try it and report what happens

NOAA <- read_csv("https://raw.githubusercontent.com/NicoleRadziwill/Data-for-R-Examples/master/tvs-201407.csv")

## otherwise download from the 'Modules' Week 4 on Canvas
## Now we have a tibble.  Take a glimpse and what do you see?

glimpse(NOAA)

## We really only want to use DEPTH, TOP, MAX_SHEAR.  Can you build
# a subset just using these columns?

sub_tvs <- NOAA %>% select(DEPTH, TOP, MAX_SHEAR)


## Now use the summary(x) and explain the descriptive statistics displayed

summary(sub_tvs)

## Now you have a quick indication of some characteristics of the distribution
# If the mean is far greater than the median (to the right on the probability distribution)
# is it skewed right or left?

## If the mean is far less than the median (to the left on the probability distribution)
# is it skewed right or left?

# Now for some more criticality, using the variance, standard deviation and mode...
# How could you find the standard deviation in Base R?  In DPLYR?

## But what about mode?  Base R does have an indirect solution through combining formula into a function

mode <- function (x) {
        uniq.vals <- unique(x) 
        uniq.vals [which.max(tabulate(match(x, uniq.vals)))]
}

## Alternatively, "source" it from GitHub directly

source("https://raw.githubusercontent.com/NicoleRadziwill/R-Functions/master/mode.R")

mode(c(2,3,4,4,4,5,6)) # checking to see that it works

##ensure it works on the NOAA data
mode(NOAA$DEPTH)

mode(NOAA$TOP)

##but wait, there is always another package and maybe a better way

install.packages("pastecs")
library(pastecs)

#try these commands

NOAA2 <- NOAA %>% select(DEPTH, TOP, MAX_SHEAR)

options(scipen = 100)
options(digits = 3)
stat.desc(NOAA2)

## Now try another package...

library(devtools)
install_github("ujjwalkarn/xda") ##This was a workaround to get XDA on my version of "R"
library(xda)

numSummary(NOAA2) ##this works ok, other functions not as much.  
## devtools is overall probably a better package to learn about...

## Univariate Analysis part II 
?fivenum() #min, lower-hinge, median, upper-hinge, max
fivenum(NOAA2$TOP)

## Review question, how could we remove missing data values in each function?
## mean(dataset$variable, na.rm = TRUE)

## STATISTICS BY GROUP "Iterate a function over an object."
# apply(x)
# lapply(x)
# sapply(x)

m1 <- tibble(C<-(1:10),nrow=5, ncol=6)
m1
a_m1 <- apply(m1, 2, sum)
a_m1

library(gapminder)
(mini_gap <- gapminder %>%
    filter(country %in% c("Canada", "Germany"), year > 2000) %>% 
    droplevels())

aggregate(lifeExp ~ country, mini_gap, mean)

##tidyverse equivalents

a_m2 <- m1 %>%
  mutate_at(2, sum)

a_m1 == a_m2

mini_gap %>% 
  group_by(country) %>% 
  summarize(lifeExp = mean(lifeExp))

## more details at https://jennybc.github.io/purrr-tutorial/bk01_base-functions.html#why_not_base

## we've discussed frequency tables and these sorts of commands 
## in Lesson 3, so we will leave it there

sepalmeans<-aggregate(cbind(Sepal.Width, Sepal.Length)~Species,iris, mean)
sepalmeans

## MEASURES OF ASSOCIATION
# Covariance is a measure of linear association between two continuous variables
# However it is scale dependent.  This makes it difficult to interpret, as discussed

cov(NOAA2$DEPTH, NOAA2$TOP)

cov(trees$Height, trees$Volume)

##Pearson's correlation coefficient 
# One of the most important concepts for analysis 
# Only measures linear relationships so use plot(x) for a quick look beforehand.

plot(trees$Girth, trees$Volume)
pairs(trees)
cor(trees$Girth, trees$Volume)

cor(trees)
##The Pearson correlation evaluates the linear relationship between two continuous variables. 
##The Spearman correlation coefficient is based on the ranked values for each variable rather 
##than the raw data. 
#Spearman correlation is often used to evaluate relationships involving ordinal variables.
#Data at the ordinal level of measurement are quantitative or qualitative. 
#"Rate this dress from one to five, one being the worst, five being the best."

##Spearman's rank correlation coefficient
## The cor function is also used for this, just change the argument.

cor(trees$Girth, trees$Volume, method="spearman")

## or literally pulled straight from Kaggle and a quick test of the function finds...
reviews <- read_csv("C:/Users/John/Documents/reviews.csv")
names(reviews)

str(reviews)

cor(reviews$Rating, reviews$'Positive Feedback Count', method = "spearman")
## This data set needs some wrangling but it is a good illustration of 
# 'solving a business problem.'
# https://www.kaggle.com/nicapotato/womens-ecommerce-clothing-reviews

## you can also set 'Spearman' to show the whole dataset

cor(trees, method="spearman")

## If any of the variables have missing values, set the USE argument to "pairwise"

## Hypothesis Test of Correlation 
# Is a correlation statistically significant?
# Perform a hypothesis test to help determine whether the correlation between
## tree girth and tree volume is statistically significant.

cor.test(trees$Girth, trees$Volume, conf.level = .95)

# so what is a p-value and how do we form hypotheses?
# Hypothesis testing is one of the most useful applications of statistics.

#### Comparison of a Sample with a Specified Distribution

## QQ-plots are very useful

library(tidyverse)

distro <- c(22,23,8,3,5,7,11,18,43,13,8,4,2,0,12,8,9,11,34,32)
supports_distro <- c(13,31,8,0,7,6,14,15,11,8,11,0,4,9,13,5,8,12,2,23)

length(distro)
length(supports_distro)

data1 <- tibble(distro, supports_distro)

str(data1)

## check out a histogram
data1 %>%
  ggplot() + geom_histogram(aes(x = x), binwidth = 3, col = "orchid")

## creating a qq-plot
data1 %>%
  ggplot(aes(sample = x)) + stat_qq() + geom_qq_line()

trees %>%
  ggplot(aes(sample = Girth)) + stat_qq() + geom_qq_line(col = "tomato")

##How is the diamond carat data skewed?
diamonds %>%
  ggplot(aes(sample = carat)) + stat_qq() + geom_qq_line(col = "gold")

##What kind of distribution is the diamond price data?

diamonds %>%
  ggplot(aes(sample = price )) + stat_qq() + geom_qq_line(col = "violetred4")

# Shapiro-Wilk Test

shapiro.test(trees$Height)

# Kolmogorov-Smirnov Test

bottles <- read_csv("https://raw.githubusercontent.com/data-scientist-dad/using-r-for-statistics/master/Individual%20Datasets/bottles.csv")

ks.test(bottles$Volume, "pnorm", 500, 25)


## Confidence Intervals and Prediction Intervals

t.test(trees$Height)

t.test(trees$Height, conf.level=0.99)

predict(lm(trees$Height~1), interval="prediction")[1,]

predict(lm(trees$Height~1), interval="prediction", level=0.99)[1,]

### TABULAR DATA

# Chi-squared Goodness of Fit (review)
##Let's do a simple example starting with a few .ppt slides as necessary
## then running the example below

## roll dice six times
fair_rolls<- c(12,8,11,9,7,5)
## does it come out to a uniform distribution?
chisq.test(fair_rolls, p=c(1/6,1/6,1/6,1/6,1/6,1/6))

## roll an unfair set of dice
unfair_rolls<- c(17,3,13,7,5,15)
chisq.test(unfair_rolls, p=c(1/6,1/6,1/6,1/6,1/6,1/6))

# The function goodnessOfFitTest(x) uses factors and requires a lot of numbers for best results


# Chi-squared Association between Two Variables
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


##PROBABILITY BASICS with an emphasis on 'R'
# We often compare samples with distributions
# However where do these distributions come from?

# Think of coin flips.  If you have a fair coin, how often could you expect 
# to get heads or tails?  What kind of distribution is this?

## For an event X, the probability of that event P(x) is a number that lies between 0 and 1. 
# The bigger the value of P(x), the more likely the event is to occur.
# All P(x) of events add up to one.  
## These are the two requirements of a distribution.

# Normal, exponential, bionomial and poisson and others can all be described using R
# If we know an approximate distribution of a data set we can classify, predict, label and simulate

#we can easily calculate probabilities and quantiles in R for all commonly used distributions
## PDF  and PMF are two ways to specify the probability distribution of a random variable

#Probability density function (PDF), calculate a probability 
## for CONTINUOUS distributions, "What is the probability that the outcome will be equal to x?"
## Relative likelihood that the value of the random variable will be equal to x

dnorm(x)

##Find the value of the PDF at x = 2.5 for a normal distribution
# with a mean of 5 and a standard deviation of 2

dnorm(2.5, mean=5, sd=2)

#Probability mass function (PMF), calculate a probability 
## for DISCRETE distributions, "What is the probability that the outcome will be equal to x?"
## Relative likelihood that the value of the random variable will be equal to x

mean(warm_up)
sd(warm_up)
dnorm(warm_up, 1.22, 1.09)

##roll a dice 10 times.  What is the probability of throwing two sixes?
# possibilities are distinct and non-overlapping
dbinom(2, 10, 1/6)

##the time between customers in Starbucks is known to follow a Poisson distribution
# with a mean of 4 minutes.  What is the probability that it will be exactly 8 minutes?

dpois(4, 8)

#Cumulative distribution function, calculate a probability "area under a curve equal to PDF"
## "If we were to randomly select a member of a given population, what is the probability
## that it will have a value less than x, or a value between x and y?"
## Relative likelihood that value of a random variable will be less than or equal to x

# Will a randomly selected value be less than or equal to 2.5?
pnorm(2.5)
# change the mean and sd arguments for a "non-standard" distribution
# in this case the observation of '3' in the vector of warm_up
# Will a value be less than or equal to 3?  What are the chances?

pnorm(3, mean = 1.22, sd = 1.09)

## changing arguments you can find the complementary probabilities
## as well as changing the prefix gives other distributions

# quantiles are useful if we want to answer: 
## "A known percent of the population falls below x?" 

qnorm(.95, mean = 5, sd = 2)

## value above a specified percent of the population falls

qnorm(.95, mean = 5, sd = 2, lower.tail = FALSE)

#Random sample from a given distribution.
## Use a random number generator to simulate a random sample from a given distribution.
## Sometimes you don't have data or want to understand your sample better.
## Example : Hand span is a population is known to be normally distributed
## with a mean of 195 mm and a standard deviation of 17 mm
## Simulate the hand spans of three randomly selected people

rnorm(3, mean = 195, sd = 17)

##Post-script

# what is a p-value and why is it important? (preview)



