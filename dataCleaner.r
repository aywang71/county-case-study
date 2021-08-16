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
  white = sum(white)/sum(TOT_POP)*100,
  black = sum(black)/sum(TOT_POP)*100,
  native = sum(native)/sum(TOT_POP)*100,
  asian = sum(asian)/sum(TOT_POP)*100,
  pacific = sum(pacific)/sum(TOT_POP)*100,
  hispanic = sum(hispanic)/sum(TOT_POP)*100
)
race2010$total <- NULL

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

#5: apply pivot long to push race into its own column 
data <- race2010 %>%
  tidyr::pivot_longer( cols = white:hispanic,
  names_to = "RACE",
  values_to = "temp")

#6: apply pivot wide to pull out year column
race2010 <- data %>%
  group_by(YEAR) %>%
  mutate(row = row_number()) %>%
  tidyr::pivot_wider(names_from = YEAR, values_from = temp) %>%
  select(-row)

#6: cast to new file 
setwd("~/GitHub/county-case-study/data")
write.csv(race2010, file = "2010race.csv")

#1: consolidate age data
age2010 <- data2010 %>% select(-(TOT_MALE:HNAC_FEMALE))
age2010$SUMLEV <- NULL
age2010$STATE <- NULL
age2010$COUNTY <- NULL

#2: casting year
age2010$YEAR[age2010$YEAR==1] <- 2010
age2010$YEAR[age2010$YEAR==2] <- NA
age2010$YEAR[age2010$YEAR==3] <- NA
age2010$YEAR[age2010$YEAR==4] <- 2011
age2010$YEAR[age2010$YEAR==5] <- 2012
age2010$YEAR[age2010$YEAR==6] <- 2013
age2010$YEAR[age2010$YEAR==7] <- 2014
age2010$YEAR[age2010$YEAR==8] <- 2015
age2010$YEAR[age2010$YEAR==9] <- 2016
age2010$YEAR[age2010$YEAR==10] <- 2017
age2010$YEAR[age2010$YEAR==11] <- 2018
age2010$YEAR[age2010$YEAR==12] <- 2019
age2010 <- na.omit(age2010)

#3: Updating row references
age2010$AGEGRP[age2010$AGEGRP==0] <- "total"
age2010$AGEGRP[age2010$AGEGRP==1] <- NA
age2010$AGEGRP[age2010$AGEGRP==2] <- NA
age2010$AGEGRP[age2010$AGEGRP==3] <- NA
age2010$AGEGRP[age2010$AGEGRP==4] <- "15-19"
age2010$AGEGRP[age2010$AGEGRP==5] <- "20-24"
age2010$AGEGRP[age2010$AGEGRP==6] <- "25-29"
age2010$AGEGRP[age2010$AGEGRP==7] <- "30-34"
age2010$AGEGRP[age2010$AGEGRP==8] <- "35-39"
age2010$AGEGRP[age2010$AGEGRP==9] <- "40-44"
age2010$AGEGRP[age2010$AGEGRP==10] <- "45-49"
age2010$AGEGRP[age2010$AGEGRP==11] <- "50-54"
age2010$AGEGRP[age2010$AGEGRP==12] <- "55-59"
age2010$AGEGRP[age2010$AGEGRP==13] <- "60-64"
age2010$AGEGRP[age2010$AGEGRP==14] <- "65-69"
age2010$AGEGRP[age2010$AGEGRP==15] <- "70-74"
age2010$AGEGRP[age2010$AGEGRP==16] <- "75-79"
age2010$AGEGRP[age2010$AGEGRP==17] <- "80-84"
age2010$AGEGRP[age2010$AGEGRP==18] <- "85-89"
age2010 <- na.omit(age2010)

#3b: pivot widen on the age groups 
data <- age2010 %>%
  group_by(AGEGRP) %>%
  mutate(row = row_number()) %>%
  tidyr::pivot_wider(names_from = AGEGRP, values_from = TOT_POP) %>%
  select(-row)

#3c: mutate to percentage instead of value
colnames(data) <- c("STNAME", "CTYNAME", "YEAR", 'total', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o')
data <- data %>%  mutate(
  ab = (a + b)/total * 100,
  cd = (c + b)/total * 100,
  ef = (e + f)/total * 100,
  gh = (g + h)/total * 100,
  ij = (i + j)/total * 100,
  kl = (k + l)/total * 100,
  mno = (m + n + o)/total * 100,
) %>%
  select(-(total:o))
colnames(data) <- c("STNAME", "CTYNAME", "YEAR", "15-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75+")

#3d: fold the age section back into a column 
age2010 <- data %>%
  tidyr::pivot_longer( cols = "15-24":"75+",
                       names_to = "AGEGRP",
                       values_to = "TOT_POP")

#4: pivot widening the year column 
data <- age2010 %>%
  group_by(YEAR) %>%
  mutate(row = row_number()) %>%
  tidyr::pivot_wider(names_from = YEAR, values_from = TOT_POP) %>%
  select(-row)

age2010 <- data

#5: write to file
setwd("~/GitHub/county-case-study/data")
write.csv(data, file = "2010age.csv")

#https://www2.census.gov/programs-surveys/popest/technical-documentation/file-layouts/2000-2010/intercensal/county/co-est00int-sexracehisp.pdf
setwd("~/GitHub/county-case-study")
data2000 <- read.csv("data/co-est00int-sexracehisp.csv")

#1a: Remove some of rows and save hispanics
race2000 <- data2000 %>% mutate(
  keep = ifelse((ORIGIN == 2 & RACE == 0) | (ORIGIN == 0), 1, 0)
)

#1b: Remove some of the useless columns
race2000$SUMLEV <- NULL
race2000$STATE <- NULL
race2000$COUNTY <- NULL
race2000$RACE[race2000$RACE==6] <- NA
race2000$SEX[race2000$SEX==1] <- NA
race2000$SEX[race2000$SEX==2] <- NA
race2000 <- na.omit(race2000)
race2000$SEX <- NULL
race2000$ORIGIN[race2000$ORIGIN==1] <- NA
race2000 <- na.omit(race2000)
race2000$ESTIMATESBASE2000 <- NULL
race2000$POPESTIMATE2010 <- NULL 
race2000$CENSUS2010POP <- NULL

#2: Replace numbers with races
race2000$RACE[race2000$RACE==1] <- "white"
race2000$RACE[race2000$RACE==2] <- "black"
race2000$RACE[race2000$RACE==3] <- "native"
race2000$RACE[race2000$RACE==4] <- "asian"
race2000$RACE[race2000$RACE==5] <- "pacific"

#2b: getting the total in
race2000 <- race2000 %>% mutate(
  RACE = ifelse((ORIGIN == 0 & RACE == 0), "total", RACE))
race2000$RACE[race2000$RACE==0] <- "hispanic"
race2000$ORIGIN <- NULL
race2000$keep[race2000$keep==0] <- NA
race2000 <- na.omit(race2000)
race2000$keep <- NULL

#3: Updating column names
colnames(race2000) <- c("STNAME", "CTYNAME", "RACE", 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009)

#3a: update to percentage values instead of counts
#3aa: pivot in the years 
data2 <- race2000 %>%
  tidyr::pivot_longer( cols = "2000":"2009",
                       names_to = "years",
                       values_to = "temp")

#3ab: pivot out the race
data2 <- data2 %>%
  tidyr::pivot_wider(names_from = RACE, values_from = temp)

#3ac: conduct mutate
data2 <- data2 %>% group_by(STNAME, CTYNAME, years) %>% summarize(
  TOT_POP = sum(total),
  white = sum(white)/sum(total)*100,
  black = sum(black)/sum(total)*100,
  native = sum(native)/sum(total)*100,
  asian = sum(asian)/sum(total)*100,
  pacific = sum(pacific)/sum(total)*100,
  hispanic = sum(hispanic)/sum(total)*100
)
data2$TOT_POP <- NULL

#3ad: pivot in the race
data2 <- data2 %>%
  tidyr::pivot_longer( cols = white:hispanic,
                       names_to = "RACE",
                       values_to = "temp")

#3ae: pivot out the years
data2 <- data2 %>%
  tidyr::pivot_wider(names_from = years, values_from = temp)

race2000 <- data2

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
data2000 <- na.omit(data2000)
data2000$CTYNAME[data2000$CTYNAME=="Yakutat City and Borough"] <- NA
data2000$CTYNAME[data2000$CTYNAME=="Kalawao County"] <- NA
data2000$CTYNAME[data2000$CTYNAME=="Loving County"] <- NA

#2: Updating row references
data2000$AGEGRP[data2000$AGEGRP==0] <- "total"
data2000$AGEGRP[data2000$AGEGRP==1] <- NA
data2000$AGEGRP[data2000$AGEGRP==2] <- NA
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
data2000$AGEGRP[data2000$AGEGRP==18] <- NA
data2000 <- na.omit(data2000)
data2000$SEX <- NULL
data2000$CENSUS2010POP <- NULL
data2000$ESTIMATESBASE2000 <- NULL
data2000$POPESTIMATE2010 <- NULL

#3: Updating column names
colnames(data2000) <- c("STNAME", "CTYNAME", "AGEGRP", 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008, 2009)

#3a: bring in the year column 
data2000 <- data2000 %>%
  tidyr::pivot_longer( cols = "2000":"2009",
                       names_to = "YEAR",
                       values_to = "TOT_POP")

#3b: pivot widen on the age groups 
data <- data2000 %>%
  group_by(AGEGRP) %>%
  mutate(row = row_number()) %>%
  tidyr::pivot_wider(names_from = AGEGRP, values_from = TOT_POP) %>%
  select(-row)

#3c: mutate to percentage instead of value
colnames(data) <- c("STNAME", "CTYNAME", "YEAR", 'total', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o')
data <- data %>%  mutate(
  ab = (a + b)/total * 100,
  cd = (c + b)/total * 100,
  ef = (e + f)/total * 100,
  gh = (g + h)/total * 100,
  ij = (i + j)/total * 100,
  kl = (k + l)/total * 100,
  mno = (m + n + o)/total * 100,
) %>%
  select(-(total:o))
colnames(data) <- c("STNAME", "CTYNAME", "YEAR", "15-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75+")

#3d: fold the age section back into a column 
data2000 <- data %>%
  tidyr::pivot_longer( cols = "15-24":"75+",
                       names_to = "AGEGRP",
                       values_to = "TOT_POP")

#4: pivot widening the year column 
data <- data2000 %>%
  group_by(YEAR) %>%
  mutate(row = row_number()) %>%
  tidyr::pivot_wider(names_from = YEAR, values_from = TOT_POP) %>%
  select(-row)

data2000 <- data

#5: cast to new file
setwd("~/GitHub/county-case-study/data")
write.csv(data2000, file = "2000age.csv")

#joining the race data: race2000 and race2010
raceData <- inner_join(race2000, race2010)
#cast to new file
setwd("~/GitHub/county-case-study/data")
write.csv(raceData, file = "final/race.csv")

#joining the age data: data2000 and age2010
ageData <- inner_join(data2000, age2010)
#cast to new file
setwd("~/GitHub/county-case-study/data")
write.csv(ageData, file = "final/age.csv")

setwd("~/GitHub/county-case-study")
election <- read.csv("data/countypres_2000-2020.csv")

#1: get rid of all rows reading non R/D data
election$party[election$party=="GREEN"] <- NA
election$party[election$party=="LIBERTARIAN"] <- NA
election$party[election$party=="OTHER"] <- NA
election <- na.omit(election)
#purging these two states because they have no county names 
election$state[election$state=="DISTRICT OF COLUMBIA"] <- NA
election$state[election$state=="ALASKA"] <- NA
election <- na.omit(election)

#0a: summing up the votes
data <- election %>% 
  group_by(county_fips, year, party) %>% summarize(
    votes = ifelse(((year == 2020) & (mode != "total")), sum(candidatevotes), candidatevotes)
  )
election2 <- election
election3 <- inner_join(election2, data)
election3 <- unique(election3)
election <- election3


#purging this for duplicate checking
election$mode[election$mode=="ELECTION DAY"] <- "TOTAL"
election$mode[election$mode!="TOTAL"] <- NA
#purging counties with missing data
election$county_fips[election$county_fips=="8014"] <- NA
election$county_fips[election$county_fips=="36000"] <- NA
election$county_fips[election$county_fips=="51515"] <- NA
election$county_fips[election$county_fips=="6077"] <- NA

election <- na.omit(election)

#2: null useless columns
election$office <- NULL
election$mode <- NULL
election$version <- NULL
election$candidate <- NULL
election$state_po <- NULL

#2a: update names
colnames(election) <- c("year", "state", "county_name", "county_fips", "party", "rm", "totalvotes", "candidatevotes")
election$rm <- NULL 

election$county_name <- gsub("CITY","",election$county_name)

#3: split into columns and format more
election <- election  %>% 
  group_by(party) %>% 
  mutate(row = row_number()) %>% 
  tidyr::pivot_wider(names_from = party, values_from = candidatevotes) %>% 
  select(-row)

election <- election %>% group_by(county_fips) %>% mutate(
  mov = ((DEMOCRAT) - (REPUBLICAN))/totalvotes)

#4: split election years into columns
election$DEMOCRAT <- NULL
election$REPUBLICAN <- NULL
election$totalvotes <- NULL

election2 <- election %>%
  group_by(year) %>%
  mutate(row = row_number()) %>%
  tidyr::pivot_wider(names_from = year, values_from = mov) %>% select(-row)

data <- election2 %>% select(-("2000":"2016"))
election3 <- election2 %>% select(-"2020")

election4 <- inner_join(data, election3)

data <- na.omit(election4)
dim(data)
dim(election4)

#5: cast to new file
setwd("~/GitHub/county-case-study/data")
write.csv(data, file = "final/election.csv")

#6: compressed data through Tableau
setwd("~/GitHub/county-case-study")
election2 <- read.csv("data/final/election_3.csv")
