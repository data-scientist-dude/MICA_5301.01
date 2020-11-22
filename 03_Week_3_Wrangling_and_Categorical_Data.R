##Week Three : Wrangling Datasets and Categorical Data

## https://www.youtube.com/watch?v=uLcd6tRTUEY 
## "Working with Categorical Data in R without Losing your Mind" 

## Load what you need
library(tidyverse)
library(lsr)
library(psych)
library(ggplot2)
library(gapminder)

tidyverse_packages()

## Check your working directory

getwd()

## Change your working directory if necessary

cwd()

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

datasets::iris

setosa <- filter(iris, Species == "setosa")
dim(setosa)

ggplot(setosa)  + 
  geom_point(aes(x = Sepal.Length, y = Sepal.Width))


##Do some initial exploration of 'gapminder' if you are unfamiliar

names(gapminder)
dim(gapminder)

head(gapminder)
glimpse(gapminder)

str(gapminder)
summary(gapminder)
view(gapminder)
mean(gapminder$lifeExp)

plot(gapminder$lifeExp, gapminder$gdpPercap, main="Scatterplot Example", 
  xlab= "Life Expectancy", ylab= "GDP per Capita", pch=6, col="orchid")

?plot()

## add fit lines to your plot
abline(lm(gapminder$lifeExp~gapminder$gdpPercap), col="red") # regression line (y~x)
lines(lowess(gapminder$lifeExp~gapminder$gdpPercap), col="blue") # lowess line (x,y)

?pch
?col

## DPLYR PART TWO ## 
## Use the glimpse(x) function from tidyverse for tibbles instead of a head()

glimpse(gapminder)

##SELECT a few columns aka variables, the order you type them is the order you will get them

gapminder %>%
  select(country, continent, year, pop) %>%
  glimpse()

##If you want a big chunk of variables use the start_col:end_col syntax

gapminder %>%
  select(country:pop) %>%
  glimpse()

##An alternative is to deselect by using a minus in front of the columns name

gapminder %>%
  select(-continent, -lifeExp) %>%
  glimpse()

##Deselect an entire chunk by using a minus in front of the columns name but re-add something

gapminder %>%
  select(-(lifeExp:gdpPercap), pop) %>%
  glimpse()

##Try a partial select with starts_with(x), ends_with(x) or contains(x)

gapminder %>%
  select(starts_with("co")) %>%
  glimpse()

gapminder %>%
  select(contains("co"), ends_with("y")) %>%
  glimpse()

## What if you don't have an exact pattern match?  Use any regular expression with matches(x)

gapminder %>%
  select(matches("c.+y")) %>%
  glimpse()

## Utilze select( one_of(x)) functions or !! to avoid re-typing column names

classification <- c("country", "year", "continent")

gapminder %>%
  select(one_of(classification))

gapminder %>%
  select(!!classification)

##Selecting columns by data type means using logical operators.
## select_if(x), select_if(is.character), is.double(x), is.factor(x), is.integer(x)
## is.numeric(x), is.logical(x)
## the lubridate package can add is.Date(x)

gapminder %>%
  select_if(is.numeric) %>%
  glimpse()

## you can add negation but that requires a tilde to map the function

gapminder %>%
  select_if(~! is_double(.)) %>%
  glimpse()

##Selecting columns by data type means using logical operators also means >, <, etc.

gapminder %>%
  select_if(is.numeric) %>%
  select_if(~max(., na.rm = TRUE) < 50)

##Selecting only for distinct values is always useful, we always need counts

gapminder %>%
  select_if(~n_distinct(.)<20)

### Re-order your rows also using select(x).  Remember you can assign them to a new variable name

gapminder %>%
  select(continent, country, pop) %>%
  glimpse()

## Moving a few columns to the front is easy with everything(x)

gapminder %>%
  select(lifeExp, everything()) %>%
  glimpse()

### Renaming columns is possible with select(x) or rename(x)

gapminder %>%
  select(life_expectancy = lifeExp, population = pop) %>%
  glimpse()

gapminder %>%
  rename(lifeExpect = lifeExp, poptn = pop) %>%
  glimpse()

## If you want to reformat all columns use select_all(toupper), select_all(tolower)
# or other argument

gapminder %>%
  select_all(toupper) %>%
  glimpse()

### Sometimes row names should be columns so DPLYR has rownames_to_column(x) for this purpose.

install.packages("nycflights13")
library(nycflights13)
str(flights)

flights %>%
  rownames_to_column(var = "2013")

## It is very useful to transform columns and make new ones.
### This is done using MUTATE
## One of the most common things to do is to create a column from other columns

gapminder_years <- gapminder %>%
  select(country, year, lifeExp) %>%
  mutate(time_elapsed_in_years = 2020 - year) %>%
  rename (lifeExp_years = lifeExp)
glimpse(gapminder_years)

## use MUTATE to create rows about measures of central tendency and variance

gapminder_years <- gapminder %>%
  select(country, pop, year, lifeExp) %>%
  mutate(pop_min = min(pop), pop_vs_MIN = pop - min(pop))
glimpse(gapminder_years)

## ifelse(x) is wonderful to only MUTATE elements that meet certain conditions
# this is an example of using logical arguments, TRUE or FALSE

gapminder %>%
  select(country, lifeExp) %>%
  mutate(lifeExp_category = ifelse(lifeExp > 55, 'normal', 'low')) %>%
arrange (desc(lifeExp_category))

### Fun ggplot break ###

# Data Prep

data("mtcars")  # load data
mtcars$`car name` <- rownames(mtcars)  # create new column for car names
mtcars$mpg_z <- round((mtcars$mpg - mean(mtcars$mpg))/sd(mtcars$mpg), 2)  # compute normalized mpg
mtcars$mpg_type <- ifelse(mtcars$mpg_z < 0, "below", "above")  # above / below avg flag
mtcars <- mtcars[order(mtcars$mpg_z), ]  # sort
mtcars$`car name` <- factor(mtcars$`car name`, levels = mtcars$`car name`)  # convert to factor to retain sorted order in plot.

# Lollipop Chart
theme_set(theme_bw())

ggplot(mtcars, aes(x=`car name`, y=mpg_z, label=mpg_z)) + 
  geom_point(stat='identity', fill="black", size=6)  +
  geom_segment(aes(y = 0, 
                   x = `car name`, 
                   yend = mpg_z, 
                   xend = `car name`), 
               color = "black") +
  geom_text(color="white", size=2) +
  labs(title="Diverging Lollipop Chart", 
       subtitle="Normalized mileage from 'mtcars': Lollipop") + 
  ylim(-2.5, 2.5) +
  coord_flip()


### STRNGR preview: mutate string columns with str_extract(x)

tango <- gapminder %>%
  select(country) %>%
  mutate(continent_first_ltr = tolower(str_extract(country, pattern = "\\D"))) %>%
  group_by(continent_first_ltr) %>%
  tally() %>%
  arrange (desc(n))


## but what if you want to mutate several columns... use mutate_all(x), mutate_if(x)
# and mutate_at()

gapminder %>%
  mutate_all(tolower)

## mutate_at(x) can also be for specfic columns but add two arguments inside the pipe..

names(gapminder_years)

gapminder_years %>%
  select(country, year, lifeExp) %>%
  mutate_at(vars(contains("lifeExp")), ~(.*365)) %>%
  rename(lifeExp_days = lifeExp)

## Changing the column names after mutation is by using rename_at(x)

?msleep
glimpse(msleep)

msleep %>%
  select(name, sleep_total:awake) %>%
  mutate_at(vars(contains("sleep")), ~(.*60)) %>%
  rename_at(vars(contains("sleep")), ~paste0(.,"_min"))

## some may tell you to assign a 'tag' inside funs(x), but try it and read the warning

msleep %>%
  select(name, sleep_total:awake) %>%
  mutate_at(vars(contains("sleep")), funs(min = .*60))

## Working with discrete columns: Recoding certain columns requires this syntax...

mx <- msleep %>%
  mutate(conservation2 = recode(conservation,
                                "en" = "Endangered",
                                "lc" = "Least_Concern",
                                "domesticated" = "Least_Concern",
                                .default = "other")) %>%
  count(conservation2)

str(mx)

## A special version exists to return a factor.  conservation2 was a factor
# in the previous example 
## In order to return a factor set the .ordered argument to TRUE


msleep %>%
  mutate(conservation2 = recode_factor(conservation,
                                       "en" = "Endangered",
                                       "lc" = "Least_Concern",
                                       "domesticated" = "Least_Concern",
                                       .default = "other",
                                       .missing = "no data",
                                       .ordered = TRUE)) %>%
  count(conservation2)

## The ifelse(x) statement can be used to turn a numeric column to a discrete one
# change sleep into either long or short

msleep %>%
  select(name, sleep_total) %>%
  mutate(sleep_time = ifelse(sleep_total > 10, "long", "short")) 



## Creating multiple discrete columns is also easy.  Use case_when(x)

msleep %>%
  select(name, sleep_total) %>%
  mutate(sleep_total_discr = case_when(sleep_total > 13 ~ "very long", sleep_total > 10 ~ "long", sleep_total > 7 ~ "limited", TRUE ~ "short")) %>%
  mutate(sleep_total_discr = factor(sleep_total_discr, 
                                    levels = c("short", "limited", 
                                               "long", "very long")))

## you can also use case_when(x) across columns

msleep %>%
  mutate(silly_groups = case_when(brainwt < 0.001 ~ "light_headed", sleep_total > 10 ~ "lazy_sleeper",is.na(sleep_rem) ~ "absent_rem", TRUE ~ "other")) %>%
  count(silly_groups)

## Merging columns, or bringing in columns from other data tables,
## is possible with left_join(x) which is covered later in this document

## Finally, what if you need to turn data in NA?  In most cases
#this will be an empty string na_if(" ")

gapminder %>%
  select (country:continent) %>%
  na_if("Asia")

## What about rows/observations/records?  How do we manipulate ROWS using DPLYR?
## This is the job of filter(x)

##basic operators provide a clear example

gapminder %>%
  select (country, lifeExp) %>%
  filter(lifeExp > 82)

## a range of values can use operators or you can insert between(x)

gapminder %>%
  select (country, lifeExp) %>%
  filter(lifeExp > 80 | lifeExp < 40 ) 
  

## also handy is the near(x) function

gapminder %>%
  select (country, lifeExp) %>%
  filter(near(lifeExp, 80, tol = sd(lifeExp)))

## specific comparisons require an ==

gapminder %>%
  select (country, continent, year, pop) %>%
  filter(continent == "Africa")

## remember how to check your factor options
levels(gapminder$continent)

## a non-obvious option might be filter(country > "t")
## filtering rows for countries with a name in the
## alphabet after the letter 't'
## filtering for two continents with %in%...

gapminder %>%
  select (country:continent) %>%
  filter(continent %in% c("Asia", "Africa"))

## or deselecting...

removal <- c("Africa","Asia")
gapminder %>%
  select (country, continent, lifeExp) %>%
  filter(!continent %in% removal)

## Filtering based on partial matches is done
## in the Tidyverse using a STRINGR function
## although its possible in base R with grepl(x)

gapminder %>%
  select (country, lifeExp) %>%
  filter(str_detect(tolower(country), pattern = "stan"))

## Multiple condtions require an AND / OR / NOT

gapminder %>%
  select (country, continent, pop, lifeExp) %>%
  filter(pop > 10000000, (lifeExp > 45 & continent != "Europe"))

## an example of xOR(x)

gapminder %>%
  select (country, lifeExp:pop) %>%
  filter(xor(lifeExp > 80, pop > 100000000))

## ! is very flexible as well

gapminder %>%
  select (country, continent, year, lifeExp, gdpPercap) %>%
  filter(lifeExp > 75, !gdpPercap > 10000)

## Filtering empty rows uses is.na(x), and the drop.na(x) could also be used
# these datasets are particularly 'clean' so you have to try this on your own

## Fun ggplot break ##

# Diverging Barcharts
ggplot(mtcars, aes(x=`car name`, y=mpg_z, label=mpg_z)) + 
  geom_bar(stat='identity', aes(fill=mpg_type), width=.5)  +
  scale_fill_manual(name="Mileage", 
                    labels = c("Above Average", "Below Average"), 
                    values = c("above"="#00ba38", "below"="#f8766d")) + 
  labs(subtitle="Normalised mileage from 'mtcars'", 
       title= "Diverging Bars") + 
  coord_flip()


## Filtering across multiple columns can be a filter_all(x), filter_if(x), filter_at(x)

result <- gapminder %>%
  select(country:continent, lifeExp) %>%
  filter_all(any_vars(str_detect(., pattern = "Aus") & (continent != "Europe")))
tail(result)

gapminder %>%
  select (country, lifeExp) %>%
  filter_all(any_vars(lifeExp < 50 ))

## pokemon is kind of a fun dataset to play with
## available on GitHub at https://gist.github.com/armgilles/194bcff35001e7eb53a2a8b441e8b2c6

poke <- read_csv("pokemon.csv")

poke %>%
  filter_if(is.numeric, any_vars(between(., 150, 400)))

poke %>%
  select(Name, Attack, Defense) %>%
  filter_at(vars(Attack, Defense), all_vars( . > 50)) %>%
  arrange(desc(Name))

## Counts are extremely useful, or have we already mentioned that?

poke %>%
  count(`Type 1`, sort = TRUE)

## Tallies act like nrow(x)

poke %>%
  tally()

poke %>%
  select(1:3) %>%
  add_tally()

## add_count(x) saves the trouble of grouping, mutating and ugrouping

gapminder %>%
  select(continent:country) %>%
  add_count(country) %>%
  filter(continent == 'Africa')

## SUMMARIZE and GROUP_BY almost always go together and are two of the most
# useful functions for handling rows

gapminder %>%
  summarize(n = n(), average_gdp = mean(gdpPercap), maximum = max(lifeExp))
  
gapminder %>%
  group_by(continent) %>%
  summarize(n = n(), average_gdp = mean(gdpPercap), maximum = max(pop))

## SUMMARIZE works with aggregate functions.  Try them out
## n() --gives the number of observations
## n_distinct(var) --functions like unique(x)
## sum(var), max(var), min(var), mean(var), median(var), sd(var), IQR(var)
## can you group_by continent and summarize the measures of variance?  
## how could you handle range?

## SUMMARIZE works like filter, with summarize_all(x), summarize_if(x) and summariz_at(x)

gapminder %>%
  group_by(continent) %>%
  select(continent,gdpPercap, pop) %>%
  summarize_all(mean)

poke %>%
  group_by(`Type 1`) %>%
  select(Attack, Defense, Generation) %>%
  summarize_all(~mean(., na.rm = TRUE ) + 5)

gapminder %>%
  group_by(continent) %>%
  select(-year)%>%
  summarize_if(is.numeric, mean, na.rm = TRUE)
  
gapminder %>%
    group_by(continent) %>%
    select(-year) %>%
    summarize_if(is.numeric, mean, na.rm = TRUE) %>%
    rename_if(is.numeric, ~paste0("avg_", .)) ##rename the new columns

gapminder %>%
  group_by(continent) %>%
  summarize_at(vars(contains("life")), mean, na.rm = TRUE) %>%
  rename_at(vars(contains("life")), ~paste0("avg_", .))

## ARRANGE sorts your summary tables into the form of your choice

gapminder %>%
  group_by(continent) %>%
  summarize(avg_pop = mean(pop)) %>%
  arrange(desc(avg_pop))

gapminder %>%
  select(continent, country, lifeExp) %>%
  group_by(continent) %>%
  arrange(desc(lifeExp), .by_group = TRUE)

##What about only showing part of your data?  There are a lot of good ways

gapminder %>%
  group_by(country) %>%
  summarize(average = mean(pop)) %>%
  top_n(5) ##notice how this works like head(x) and you can call a certain number of rows
##...and the bottom five values
gapminder %>%
  group_by(country) %>%
  summarize(average = mean(pop)) %>%
  top_n(-5)

gapminder %>%
  group_by(country) %>%
  summarize(average_pop = mean(pop), max_gdpPercao = max(gdpPercap)) %>%
  top_n(5, average_pop)

gapminder %>%
  group_by(continent) %>%
  sample_frac(.08)

gapminder %>%
  slice(50:53)

###...and now for some Exploratory Data Analysis 
## Star Wars is a dataset available in the tidyverse

class(starwars)
dim(starwars)
colnames(starwars)
starwars ##Because it is a tibble it displays nicer than a normal R dataframe
view(starwars)


## lets trim down some list columns just to simplify the discussion
starwars_small <- starwars %>%
  select(-(films:starships))

summary(starwars_small)

##Bivariate Relationships and Outliers
## a nice quick plot is pairs(x) with the numeric data
starwars_small %>%
  select_if(is.numeric) %>%
  pairs()

## Outliers

starwars_small%>%
  filter(mass > 600 | birth_year > 400)

## Linear relationships

##remove outliers:
starwars_filtered <- starwars_small %>%
  filter(mass < 600 & birth_year < 400)

#scatter plot with a regression line
starwars_filtered %>%
  ggplot(aes(mass, height)) +
  geom_point() +
  geom_smooth(method = "lm")

#compute correlations:
starwars_filtered %>%
  select(mass, height) %>%
  cor()

##Exercise...install and load the 'GGally' package and try this code
## do you like the results better?

library(GGally)

starwars_filtered %>%
  select_if(is.numeric) %>%
  ggpairs()

## FACTORS and FORCATS highlights

##Let's try and add Gender to the pairs plot and see what happens
## if you tried the commented out code below, it wouldn't work
## you have to convert all character fields to factors

##starwars_small %>%
  #select(height, mass, birth_year, gender) %>%
  #pairs()

## last fun ggplot break ##

install.packages("ggcorrplot")
library("ggcorrplot")
# Compute a correlation matrix
my_data <- mtcars[, c(1,3,4,5,6,7)]
corr <- round(cor(my_data), 1)
# Visualize
(ggcorrplot(corr, p.mat = cor_pmat(my_data),
           hc.order = TRUE, type = "lower",
           color = c("#FC4E07", "white", "#00AFBB"),
           outline.col = "white", lab = TRUE))


# Convert all star wars character fields to factors
library(magrittr)
sf <-starwars_small %>%
  mutate_if(is.character, as.factor)

table(sf$hair_color) ##frequency tables are a nice quick way to "see" the data

table(sf$hair_color, sf$gender)

# Pairs plot:
sf %>% 
  select(height, mass, birth_year, gender) %>%
  pairs()
##Is this another job for ggpairs?  Try it out!
## also check out all the genders

sf %>%
  select(gender) %>%
  unique()

## what is the summary of the factors like?
summary(sf)

sf %>%
  group_by(species) %>%
  summarize(mean_height = round(mean(height), 1), n = n()) %>%
  arrange(desc(mean_height))

## You may want to convert characters to factors to make them easy to work with
## using is.factor  --- once they are converted you can count them as well as
## look at them on a bar chart

sf %>%
  count(sex)

sf %>%
  ggplot(aes(sex)) + geom_bar()
  
## you can change the order to any you like
## including increasing frequency

sf %>%
  mutate(sex = sex %>% fct_infreq() %>% fct_rev()) %>%
  ggplot(aes(sex)) +
  geom_bar()

sf %>%
  mutate(species = species %>% fct_infreq() %>% fct_rev()) %>%
  ggplot(aes(species)) +
  geom_bar()


##The most general and powerful tools to organize
## your factors are fct_recode(x), fct_collapse(x) and fct_lump(x)
##It allows you to recode, or change, the value of each level

levels(sf$species) ##probably too many to graph well

sfx <- sf %>%
  mutate( species = fct_lump(species)) %>%
  count(species)
sfx

table(starwars_small$sex)

## STRINGR highlights
## Functions from STRINGR tend to be more consistent than base R
## so that may be worth tidying your data alone if you have many 
##in your dataset

str_length(c("a", "R for data science", NA))


### SELECTED FUNCTIONS FROM CH. 7, LSR ###
## If some of this seems easy after what you just reviewed
## then that is a good thing

## Loading Data review
load("nightgarden.Rdata")

batman<-read_csv("C:/Users/John/Documents/batman.csv")


## Tabulating Data
## 3 useful functions are table(x), xtabs(x) and tabulate(x) 
library(psych)
library(lsr)

who()

print(speaker)

print(utterance)

table(speaker)

table(speaker, utterance)

itng <- data.frame( speaker, utterance ) ##make a table into an object
itng

table( itng )

table(batman)

xtabs( formula = ~ speaker + utterance, data = itng )##select certain variables

## How would you display proportions?
itng_table <- table( itng ) # create the table, and assign it to a variable
itng_table
prop.table( x = itng_table, margin = 1) ## 1 is divide by row total, 2 is by column

##tabulate(x) is actually doing most of the work
some_data <- c(1,2,3,1,1,3,1,1,2,8,3,1,2,4,2,3,5,2)
tabulate( some_data )


## Transforming or recoding a variable
load( "likert.Rdata" )
likert.raw
##One reason why it might be useful to have the data 
##in this format is that there are a lot of situations
##where you might prefer to analyze the strength of the opinion 
##separately from the direction of the opinion.

df <- data.frame( likert.raw ) # create data frame
df$likert.centred <- df$likert.raw - 4 # create centered data
df$opinion.strength <- abs( df$likert.centred ) # create strength variable
df$opinion.dir <- sign( df$likert.centred ) # create direction variable
df # print the final data frame

##..or cut numerics into levels

age <- c( 60,58,24,26,34,42,31,30,33,2,9 )
age_breaks <- seq( from = 0, to = 60, by = 20 )
age_labels <- c( "young", "adult", "older" )

age_group <- cut( x = age, breaks = age_breaks, labels = age_labels )
data.frame( age, age_group )
## now use the table(x) function and see the result

## Two more mathematical functions ...
floor(1.42)
ceiling(1.42)

## Extracting a subset of a vector
## What the heck is %in% ?  It is a lot like ==

utterance %in% c("pip","oo")
speaker[ utterance %in% c("pip","oo") ]


## Extracting a subset of a data frame

df2 <- subset( x = itng, subset = speaker == "makka-pakka", select = utterance ) # keep only the utterance variable
print( df2 )
##aside from the subset function you can use [ ] but 
## it is not always easy since you need the index number of the value
## the LSR book goes into good detail on this subject

## Sorting data sets
# Works for numeric or character

numbers <- c(2,4,3)
sort( x = numbers )

print( fruit )
sortFrame( fruit, Product, Unit, Price, Label) ##sorts your whole data frame

## Reshaping a data frame
rbind(speaker, utterance) ##more like contstructing a data frame from vectors

cbind(speaker, utterance)

## widetoLong(x) and longtoWide(x) are non-tidyverse functions from basic R

### Manipulating text
animals <- c( "cat", "dog", "kangaroo", "whale" )
strtrim( x = animals, width = 3 )
substr( x = animals, start = 2, stop = 3 )
paste( "hello", "world" )

coder <- "I can't believe I understand this. I must be a coder."
coder_1 <- strsplit( x = coder, split = " ", fixed = TRUE )
coder_1

##be sure to Google 'escape sequence' and if you don't use
# tidy data read about grp(x) for matching text

##### SELECTED FUNCTIONS FROM CH. 12, LSR
### chi^2 distributions
# What are chi^2 distributions?

##The chi^2 goodness-of-fit test is one of the oldest hypothesis tests around

##Let's do a simple example starting with a few .ppt slides
## then running the example below

## roll dice six times
fair_rolls<- c(12,8,11,9,7,5)
## does it come out to a uniform distribution?
chisq.test(fair_rolls, p=c(1/6,1/6,1/6,1/6,1/6,1/6))

## roll an unfair set of dice
unfair_rolls<- c(17,3,13,7,5,15)
chisq.test(unfair_rolls, p=c(1/6,1/6,1/6,1/6,1/6,1/6))

# goodnessOfFitTest(x) uses factors and requires a lot of numbers for best results

##what is the result of these two experiments?  
# How do you know to reject the null hypothesis?

##### WRANGLE R4DS  ### ch.9 - 16

#BUILD SKILLS in FOUR key TIDYVERSE areas

##1) RELATIONAL data ....working with multiple interrelated datasets.

# If you have already worked with SQL these functions will seem very familiar
# left_join(x)

##2) STRINGS data : STRINGR will introduce regular expressions, a powerful tool for manipulating strings.

# Match patterns, determine lengths, combine and split strings
# str_len(x)

##3) FACTORS... how R stores categorical data. They are used when a variable has a fixed set of possible values, or when you want to use a non-alphabetical ordering of a string.
# functions such as fct_lump(x)

## https://www.youtube.com/watch?v=uLcd6tRTUEY "Working with categorical data in R without losing your mind" 

## 4) DATES and times: LUBRIDATE is one of key tools for working with dates and date-times.
##



