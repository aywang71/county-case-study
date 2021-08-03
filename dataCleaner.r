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

#1: consolidate racial data 
race2010 <- mutate(
  data2010, 
  white = WA_MALE + WA_FEMALE,
  black = BA_MALE + BA_FEMALE,
  native = IA_MALE + IA_FEMALE,
  asian = AA_MALE + AA_FEMALE,
  pacific = NA_MALE + NA_FEMALE,
  hispanic = H_MALE + H_FEMALE)
race2010 <- race2010 %>% select(-(TOT_MALE:HNAC_FEMALE))
race2010$SUMLEV <- NULL

#2: group by year and age group (sum together all values into one year)
#group_by for state, county, and year?, that would sum everything else (I hope)
race2010 <- race2010 %>% group_by(STNAME, CTYNAME, YEAR) %>% summarize(
  total = sum(TOT_POP),
  white = sum(white),
  black = sum(black),
  native = sum(native),
  asian = sum(asian),
  pacific = sum(pacific),
  hispanic = sum(hispanic)
)

#3: casting year
race2010$YEAR[race2010$YEAR==1] <- 2010
race2010$YEAR[race2010$YEAR==2] <- NA
race2010$YEAR[race2010$YEAR==3] <- NA
race2010$YEAR[race2010$YEAR==4] <- 2011
race2010$YEAR[race2010$YEAR==5] <- 2012
race2010$YEAR[race2010$YEAR==6] <- 2013
race2010$YEAR[race2010$YEAR==7] <- 2014
race2010$YEAR[race2010$YEAR==8] <- 2015
race2010$YEAR[race2010$YEAR==9] <- 2016
race2010$YEAR[race2010$YEAR==10] <- 2017
race2010$YEAR[race2010$YEAR==11] <- 2018
race2010$YEAR[race2010$YEAR==12] <- 2019

#4: remove NULL'ed out years
race2010 <- na.omit(race2010)

#5: cast to new file 
setwd("~/GitHub/county-case-study/data")
write.csv(race2010, file = "2010race.csv")
#6: done? probably worth changing up the position of year and race but I can't figure out how to (maybe a transpose)

#1: need to also find a way to make the age data - I want the single "year" column to be distributed across them 

#https://www2.census.gov/programs-surveys/popest/technical-documentation/file-layouts/2000-2010/intercensal/county/co-est00int-sexracehisp.pdf
setwd("~/GitHub/county-case-study")
data2000 <- read.csv("data/co-est00int-sexracehisp.csv")

#1a: Remove some of rows and save hispanics
race2000 <- data2000 %>% mutate(
  keep = ifelse((ORIGIN == 2 & RACE == 0) | (ORIGIN == 0 & RACE != 0), 1, 0)
)
race2000$keep[race2000$keep == 0] <- NA

#1b: Remove some of the useless columns
race2000$SUMLEV <- NULL
race2000$STATE <- NULL
race2000$COUNTY <- NULL
race2000$RACE[race2000$RACE==6] <- NA
race2000$SEX[race2000$SEX==1] <- NA
race2000$SEX[race2000$SEX==2] <- NA
race2000 <- na.omit(race2000)
race2000$SEX <- NULL
race2000$ORIGIN <- NULL
race2000$ESTIMATESBASE2000 <- NULL
race2000$keep <- NULL
race2000$POPESTIMATE2010 <- NULL 

#2: Replace numbers with races
race2000$RACE[race2000$RACE==0] <- "hispanic"
race2000$RACE[race2000$RACE==1] <- "white"
race2000$RACE[race2000$RACE==2] <- "black"
race2000$RACE[race2000$RACE==3] <- "native"
race2000$RACE[race2000$RACE==4] <- "asian"
race2000$RACE[race2000$RACE==5] <- "pacific"

#3: Updating column names
colnames(race2000) <- c("STNAME", "CTYNAME", "RACE", 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010)

#4: cast to new file
setwd("~/GitHub/county-case-study/data")
write.csv(race2000, file = "2000race.csv")

#https://www2.census.gov/programs-surveys/popest/technical-documentation/file-layouts/2000-2010/intercensal/county/co-est00int-agesex-5yr.pdf
setwd("~/GitHub/county-case-study")
data2000 <- read.csv("data/co-est00int-agesex-5yr.csv")

#1: Remove useless data 
data2000$SUMLEV <- NULL
data2000$STATE <- NULL
data2000$COUNTY <- NULL
data2000$SEX[data2000$SEX==1] <- NA
data2000$SEX[data2000$SEX==2] <- NA
data2000$AGEGRP[data2000$AGEGRP==0] <- NA
data2000 <- na.omit(data2000)

#2: Updating row references
data2000$AGEGRP[data2000$AGEGRP==0] <- "0-4"
data2000$AGEGRP[data2000$AGEGRP==1] <- "5-9"
data2000$AGEGRP[data2000$AGEGRP==2] <- "10-14"
data2000$AGEGRP[data2000$AGEGRP==3] <- "15-19"
data2000$AGEGRP[data2000$AGEGRP==4] <- "20-24"
data2000$AGEGRP[data2000$AGEGRP==5] <- "25-29"
data2000$AGEGRP[data2000$AGEGRP==6] <- "30-34"
data2000$AGEGRP[data2000$AGEGRP==7] <- "35-39"
data2000$AGEGRP[data2000$AGEGRP==8] <- "40-44"
data2000$AGEGRP[data2000$AGEGRP==9] <- "45-49"
data2000$AGEGRP[data2000$AGEGRP==10] <- "50-54"
data2000$AGEGRP[data2000$AGEGRP==11] <- "55-59"
data2000$AGEGRP[data2000$AGEGRP==12] <- "60-64"
data2000$AGEGRP[data2000$AGEGRP==13] <- "65-69"
data2000$AGEGRP[data2000$AGEGRP==14] <- "70-74"
data2000$AGEGRP[data2000$AGEGRP==15] <- "75-79"
data2000$AGEGRP[data2000$AGEGRP==16] <- "80-84"
data2000$AGEGRP[data2000$AGEGRP==17] <- "85-89"
data2000$AGEGRP[data2000$AGEGRP==18] <- "90-94"

#3: Updating column names
colnames(data2000) <- c("STNAME", "CTYNAME", "AGEGRP", 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009, 2010)

#4: cast to new file
setwd("~/GitHub/county-case-study/data")
write.csv(data2000, file = "2000age.csv")


setwd("~/GitHub/county-case-study")
election <- read.csv("data/countypres_2000-2020.csv")

#1: get rid of all rows reading non R/D data
election$party[election$party=="GREEN"] <- NA
election$party[election$party=="LIBERTARIAN"] <- NA
election$party[election$party=="OTHER"] <- NA
#purging these two states because they have no county names 
election$state[election$state=="DISTRICT OF COLUMBIA"] <- NA
election$state[election$state=="ALASKA"] <- NA
election <- na.omit(election)

#2: null useless columns
election$office <- NULL
election$mode <- NULL
election$version <- NULL
election$candidate <- NULL
election$state_po <- NULL

#3: combine the counties - does not work 
data <- election %>% group_by(year, county_fips) %>% mutate(
  democratic = ifelse(party=="DEMOCRATIC", candidatevotes, 0), 
  republican = ifelse(party=="REPUBLICAN", candidatevotes, 0)
)