# Andrew Wang
#
# clean up and setup
rm(list=ls()) # clean up any old stuff in R
setwd("~/GitHub/county-case-study") # go to this folder

#load up myfunctions.R
source("~/GitHub/county-case-study/myfunctions.R")

#library import
library(tidyverse)

#https://www2.census.gov/programs-surveys/popest/technical-documentation/file-layouts/2010-2019/cc-est2019-alldata.pdf
data2010 <- read.csv("data/cc-est2019-alldata.csv")

#goal: various racial groups over time, as a percentage 
#minimum viable product: merging the two datasets together 

#1: consolidate racial data 
data2010 <- mutate(
  data2010, 
  white = WA_MALE + WA_FEMALE,
  black = BA_MALE + BA_FEMALE,
  native = IA_MALE + IA_FEMALE,
  asian = AA_MALE + AA_FEMALE,
  pacific = NA_MALE + NA_FEMALE,
  hispanic = H_MALE + H_FEMALE)
data2010 <- data2010 %>% select(-(TOT_MALE:HNAC_FEMALE))
data2010$SUMLEV <- NULL

#2: group by year and age group (sum together all values into one year)
#group_by for state, county, and year?, that would sum everything else (I hope)
data2010 <- data2010 %>% group_by(STNAME, CTYNAME, YEAR) %>% summarize(
  total = sum(TOT_POP),
  white = sum(white),
  black = sum(black),
  native = sum(native),
  asian = sum(asian),
  pacific = sum(pacific),
  hispanic = sum(hispanic)
)

#3: casting year
data2010$YEAR[data2010$YEAR==1] <- 2010
data2010$YEAR[data2010$YEAR==2] <- NA
data2010$YEAR[data2010$YEAR==3] <- NA
data2010$YEAR[data2010$YEAR==4] <- 2011
data2010$YEAR[data2010$YEAR==5] <- 2012
data2010$YEAR[data2010$YEAR==6] <- 2013
data2010$YEAR[data2010$YEAR==7] <- 2014
data2010$YEAR[data2010$YEAR==8] <- 2015
data2010$YEAR[data2010$YEAR==9] <- 2016
data2010$YEAR[data2010$YEAR==10] <- 2017
data2010$YEAR[data2010$YEAR==11] <- 2018
data2010$YEAR[data2010$YEAR==12] <- 2019

#4: remove NULL'ed out years
data2010 <- na.omit(data2010)

#5: done? probably worth changing up the position of year and race but I can't figure out how to 

#https://www2.census.gov/programs-surveys/popest/technical-documentation/file-layouts/2000-2010/intercensal/county/co-est00int-sexracehisp.pdf
data2000 <- read.csv("data/co-est00int-sexracehisp.csv")

#1a: Remove some of rows and save hispanics
data2000 <- data2000 %>% mutate(
  keep = ifelse((ORIGIN == 2 & RACE == 0) | (ORIGIN == 0 & RACE != 0), 1, 0)
)
data2000$keep[data2000$keep == 0] <- NA

#1b: Remove some of the useless columns
data2000$SUMLEV <- NULL
data2000$STATE <- NULL
data2000$COUNTY <- NULL
data2000 <- na.omit(data2000)
data2000$SEX <- NULL
data2000$ORIGIN <- NULL
data2000$ESTIMATESBASE2000 <- NULL
data2000$keep <- NULL

#2: Replace numbers with races
data2000$RACE[data2000$RACE==0] <- "hispanic"
data2000$RACE[data2000$RACE==1] <- "white"
data2000$RACE[data2000$RACE==2] <- "black"
data2000$RACE[data2000$RACE==3] <- "native"
data2000$RACE[data2000$RACE==4] <- "asian"
data2000$RACE[data2000$RACE==5] <- "pacific"
#you can get hispanic off of origin column but otherwise it does not exist 