##Week Two : Importing Datasets and Visualizing Data

## Load what you need
library(tidyverse)
library(lsr)
library(psych)
library(ggplot2)
library(stats)

## Check your working directory

getwd()

## Change your working directory if necessary

cwd()

## Warm up with some basic functions
warm_up <- as.numeric(c(8,6,7,5,3,0,9))
warm_down <- as.numeric(c(4,5,6,7,8,7,9))
var(warm_up)
sqrt(var(warm_up))
sd(warm_up)
range(warm_up)
IQR(warm_up)
mean(warm_up)
median(warm_up)
table(warm_up)

plot(warm_down, warm_up)

## Fundamentals ## 
## basic arithmetic operators are what you would expect
2+2
2-2
2*2
2/2
2^2
(2^2) + 4

## You'll find yourself using keyboard commands more and more
# ctrl + return to execute, but also other short cuts such as ctrl + X
# One advantage of the console is that the up and down arrows show 
# you the last command you typed

## Functions require arguments and they perform a certain task with the input
date()
round(3.141593, digits = 2)
?round()
ceiling(3.14)
## note you can change the behavior of a function by changing its arguments

## You can create your own functions, and as you finish this course you will discover
# it is actually not that difficult.  Know your if/else, for/while loops etc.
# More useful mathematics functions...

abs(-20)
exp(20)
log(485165195)
log10(20)
sqrt(20)
cos(20)
sin(20)
tan(20)
acos(20) #Arc cosine
asin(20)
atan(20) #Arc tangent
factorial(20)

## NaN NA NULL 

#Objects

height <- 68.5

heightcm <- round(height*2.54)

string1 <- "Hello World"

string2 <- "I said \"Hello!\""  ##to include the quotation marks

string1
string2

##vectors and dataframes are the most commonly used objects

temperatures <- c(2, 3.75, -.35, 1.2, -5)
temperatures ## to view an object just type the name.  No 'print' necessary

##length function can tell you the number of values in a vector
length(temperatures)

##each value has a positon and you can refer to it with 
#square brackets, and do any number of actions that way
temperatures[1:3]

getwd()

##there are several built in data frames to work with
Puromycin

str(Puromycin)

##different variables are refered to in a data frame with ($)
# you will wear your ($) out, this is how important this point is

test <- Puromycin$rate

# You can find individual values with the row and column number
## row is always first, just as 'x' is always first in any action

Puromycin[3,2]
Puromycin[3, ] ##select for the whole row, works for column too [,x]
Puromycin[2] ## select entire column

## Take out selected values, like removing the first column

Puromycin[-1]

## we've seen how you use colons to select a range

Puromycin[6:10,]
Puromycin[,c(1,3)] #select non-consective columns

## object names can also be used once assigned

rownumb <- c(6,8,14)

Puromycin[rownumb,]

Puromycin[sqrt(25),] ##even functions, which is very useful, think mean(x)

Puromycin$conc[10] ##specific values in a column

##Data editor helps you view and edit data frames

fix(Puromycin) ##this probably works best with minor quick fixes

##other useful functions
objects()
rm(height, string1, string2)

## remember to save your work and know how to set your file path
  
## Entering Data ## 

# You can enter data directly, and this might be done when
# trying something new

chain <- c("Costco", "Piggly Wiggly", "Giant", "Kroger", "WinCo")
stores <- c(439, NA, 203, 433, 612)
sales_area <- c(12261, NA, 36722, 19109, 26300)
market_share <- c(22, 21.5, 30, 16, 10.5)

##vectors have the same length, each vector has one kind of value

supermarkets <- data.frame(chain, stores, sales_area, market_share)

str(supermarkets)
## avoiding confusion, you can 'remove' the individual vectors
rm(chain, stores, sales_area, market_share)

##more often you import a plain text file

## dataset1 <- read.csv("C:/folder/filename.csv")
dataset1 <- read.csv("C:/Users/John/Documents/yyz.csv")
## easy to use the 'files' tab on the 'help' window to review what you have
getwd()

##importing tab-delimited files use read.delim(x)
# Each of these R assumes the first line will be the dataset variable names
# There are arguments to stop this, as well as supply column names
# Cleaning data note! Use na.strings="." to tell R how to treat missing data
# Importing a Excel file as a .csv is very common
# To ensure a smooth Excel load, prepare it first
# Other supported software is Stata, Minitab, etc.

# Practice downloading data for this week for R from Git Hub
## https://github.com/data-scientist-dad/MICA_5301.01/tree/main/Week_2_MICA_dataset_examples_credit_Stowall/Individual%20Datasets

# Exporting data sets is done with write.csv(x) and it will use your working directory
# or another place if you specify it

## Preparation and Manipulation ## 
# name, rename and arrange

newdata <- subset(supermarkets, sales_area >= 20000, select=c(chain, stores))
newdata

Puromycin2 <-Puromycin[-c(1,3)] #remove first and third variables
Puromycin2

names(newdata)
names(newdata)[2] <- "total_area"
names(newdata)

# change data type
# Each variable is numeric, integer, factor, character or date&POSISlt

sapply(Puromycin, class) #applies function simultaneously to all variables

Puromycin$state <- as.character(Puromycin$state)
class(Puromycin$state)

Puromycin$state <- as.factor(Puromycin$state)

# calculate new variables using old ones

Puromycin$conc2 <- Puromycin$conc ##creates new duplicate variable
names(Puromycin)
## You can set conditions for entry into a new variable

Puromycin$conc[Puromycin$conc > .05]<- NA ##set these as NA

view(Puromycin)



# divide numeric variables into categories
## review: if I needed to upload a .csv file named 'people' 
# what would I type?

people <- read.csv("C:/Users/John/Documents/people.csv")

getwd()

##type your answer here##
##if you want to code along, then import the data sets ##

people$Height.Cat<-cut(people$Height, c(150, 160, 180, 200),
                       c("Short", "Medium", "Tall"))

str(people)
summary(people)

# modify category names

levels(people$Sex)<-c("Male", "Female")
people$Eye.Color <- as.factor(people$Eye.Color) #make sure its a factor
levels(people$Eye.Color)
levels(people$Eye.Color)[2]<-"Brown"
levels(people$Eye.Color)

people$Eye.color<-relevel(people$Eye.Colour, "Brown")

# manipulate strings
## paste(x), substring(x) and grep(x) are the most common string functions

fruit <- read.csv("C:/Users/John/Documents/fruit.csv")
flights <- read.csv("C:/Users/John/Documents/flights.csv")
customers <- read.csv("C:/Users/John/Documents/customers.csv")
getwd()

fruit$Label<-paste(fruit$Product, ": £", format(fruit$Price,
                                                trim=T, digits=2), " ", fruit$Unit, sep="")

flights$Airline<-substring(flights$Flight.Number, 1, 2)

grep("reading", customers$Address)

# work with dates and times
## dates are not automatically recognized.  Convert them
## and you will find them easy to work with

coffeeshop <- read.csv("C:/Users/John/Documents/coffeeshop.csv")

coffeeshop$Date<-as.Date(coffeeshop$Date, "%d/%b/%Y") ##convert a variable to date class
coffeeshop$Day<-weekdays(coffeeshop$Date)
coffeeshop

flights$DateTime<-paste(flights$Date, flights$Time)
flights$DateTime<-strptime(flights$DateTime, "%d/%m/%Y %H:%M")
round(flights$DateTime, units="hours")

# remove observations

flights <-unique(flights)  ## remove duplicates
dups<-flights[duplicated(flights),]  ## save the duplicates

# select a subset

subset(people, Eye.Color %in% c("Brown", "Green"))
subset(people, Eye.Color!="Blue")
subset(people, Height==169)

# sort the data
## this uses the order(x) function

people2<-people[order(people$Hand.Span),]

## Combinations and Restructuring  ## 


## Append datasets vertically
CIAdata1 <- read.csv("C:/Users/John/Documents/CIAdata1.csv")
CIAdata2 <- read.csv("C:/Users/John/Documents/CIAdata2.csv")

CIAdata<-rbind(CIAdata1, CIAdata2)

## Append datasets horizontally
# to add more than two data sets just keep adding them
WHOdata <- read.csv("C:/Users/John/Documents/WHOdata.csv")
CIAWHOdata<-cbind(CIAdata1, WHOdata)

## Merge datasets
CPIdata <- read.csv("C:/Users/John/Documents/CPIdata.csv")
CIACPIdata<-merge(CIAdata1, CPIdata)

## Stack dataset
grades1 <- read.csv("C:/Users/John/Documents/grades1.csv")
grades2<-stack(grades1)

## Unstack dataset

grades1<-unstack(grades2)

## Reshape (wide to long) 
resistance <- read.csv("C:/Users/John/Documents/resistance.csv")
resistance2<-reshape(resistance, direction="long",
                     varying=list(c("Day3", "Day7", "Day14")), times=c(3, 7, 14),
                     idvar="Formula", v.names="Resistance", timevar="Day")

## Reshape (long to wide) 
vitalsigns <- read.csv("C:/Users/John/Documents/vitalsigns.csv")
vitalsigns2<-reshape(vitalsigns, direction="wide",
                     v.names="result", timevar="test", idvar="subject",
                     varying=list(c("SysBP", "DiaBP", "Pulse")))


## There are many arguments and nunances in these functions
## Use the noted data sets and built in ones to practice daily
## other useful functions that we will use in Week 4
## when we discuss descriptive statistics
## split()
## apply()
## sapply()
## lapply()
## tapply()
## aggregate()

## GGPLOT2 ##
## Plus some 'exploring visualizatons' from R4DS (Wickham) 
## Data
## Aesthetic attributes
## Geometric objects
## Statistical transformations
## Coordinate system
## Facets

dataset_gg <- ("midwest")
glimpse(midwest)
head(midwest)

## First ggplot ever....

library(ggplot2)

ggplot(midwest, aes(x = area, y = poptotal))
# pretty underwhelming but the basis of everything

# Scatterplot
ggplot(midwest, aes(x = area, y = poptotal)) + geom_point()

g <- ggplot(midwest, aes(x = area, y = poptotal)) + geom_point() +
  geom_smooth(method = "lm") #you can turn off confidence bands with se = FALSE
  plot(g)

## Let's delete some points out of a certain range

g1 <- g + xlim(c(0, 0.1)) + ylim(c(0,1000000)) 

g1

## ..and add a title and axis labels, in this case together
##with labs(x) but you can also do it separately with xlab(x) and ylab(x)

g2 <- g1 + labs(title = "Area vs. Population", subtitle = "from midwest dataset", y = "Population", x = "Area", caption = "Midwest Demographics")
g2

## Changes to the color and size of points is easy

ggplot(midwest, aes(x = area, y = poptotal)) + geom_point(col = "orchid2", size = 5)+ 
  geom_smooth(method = "lm", col = "firebrick")+
  xlim(c(0, 0.1)) + ylim(c(0,1000000))+ 
  labs(title = "Area vs. Population", subtitle = "from midwest dataset", y = "Population", x = "Area", caption = "Midwest Demographics")


?colors

colors()


## However using color for a feature requires mapping through the aesthetic layer
## put a color on each state by placing it inside aes(x)

g3 <- ggplot(midwest, aes(x = area, y = poptotal, col = state)) + 
  geom_point( size = 3, alpha = 0.5)+ 
  geom_smooth(method = "lm", col = "firebrick", size = 2)+
  xlim(c(0, 0.1)) + ylim(c(0,1000000))+ 
  labs(title = "Area vs. Population", subtitle = "from midwest dataset", y = "Population", x = "Area", caption = "Midwest Demographics")

g3

g3 + theme(legend.position = "None") #Remove legend

## control the color scales

g3 + scale_color_brewer(type = 'qual', palette = 1)
library(RColorBrewer)
head(brewer.pal.info, 10) ##first ten color palattes

## Changing axis text and tick location
# step 1: set the breaks

g3 + scale_x_continuous(breaks = seq(0,0.1, 0.01))

# step 2: change the labels

g3 + scale_x_continuous(breaks = seq(0,0.1, 0.01), labels = letters[1:11])

## Try setting the breaks for y axis then reformatting both labels

g3 + scale_x_continuous(breaks = seq(0,0.1, 0.01), + 
      labels = sprintf("%1.2f%%", seq(0,0.1, 0.01))) +
      scale_y_continuous(breaks = seq(0, 1000000, 200000), labels = function(x){paste0 (x/1000, 'K')})
      
## Finally themes can be changed...

theme_set(theme_dark())
g3

g3 + theme_dark() + labs(subtitle = "Dark Theme")

library(ggthemes)
g3 + theme_tufte() + labs(subtitle = "Tufte Theme")
g3 + theme_fivethirtyeight() + labs(subtitle = "538 Theme")

##time dependent but...
data(mpg)
glimpse(mpg)

plot()
## make a scatterplot of hwy vs. city
## try editing the labels
## add another variable such as 'class.' How would you do it?

## Tidyverse Introduction ##
## What makes data tidy?
## Why would we want tidy data?

# A standard way of mapping data to its structure
# It makes it easy to manipulate data and gain insights

##Three Key Points
##Each variable forms a column
##Each observation forms a row
##Each type of observational unit forms a table

##Tidyverse covers readr, tidyr, dplyr, ggplot2, purr and tibble
# 'Easier to work with than comparable base R functions'

# import -> tidy -> transform -> visualize -> model
#                        ^-------------------------]

# READR
## read_csv(x)
## ...for LARGE data sets try fread(x) or the vroom package
## the RIO package also offers helpful loading features

## TIDYR
## you can pivot and reshape much like basic R but with tibbles

table4a
#we can make this longer with pivot_longer(x)
table4a %>%
  pivot_longer(cols = c("1999","2000"), names_to = "year", values_to = "cases")

table3
#we can separate one variable into two with separate(x)
separate_table <-table3 %>%
  separate(rate, c("count","population"), sep = "/")
separate_table

#we can unite two variables into one with unite(x)
unite_table <-separate_table %>% unite(rate, count, population, sep = "/")
unite_table  

#finally we can pivot wider using, you guessed it, pivot_wider(x)
unite_table2 <-unite_table %>% pivot_wider(names_from = "year", values_from = "rate")
unite_table2

## DPLYR (preview) basic verbs:
# select(x)  -- COLUMNS aka VARIABLES
# filter(x)  -- ROWS aka OBSERVATIONS
# arrange(x) -- re-order ROWS
# mutate(x)  -- create new COLUMNS
# summarize(x) --values summary
# group_by(x)  --see your results in different distinct groups

install.packages("gapminder")
library(gapminder)

str(gapminder)
class(gapminder)

head(gapminder)

## but what does this look like?
library(cowplot) ##puts plots side by side
p <- ggplot(gapminder, aes(year, lifeExp)) + geom_point()
q <- ggplot(gapminder, aes(gdpPercap, lifeExp)) + geom_point()
r <- ggplot(gapminder, aes(log(gdpPercap), lifeExp)) + geom_point()
## why would we use log(x)?
theme_set(theme_clean())
plot_grid(p,q,r)
  
## Let's discuss design points, when to use certain charts, and common coding mistakes....