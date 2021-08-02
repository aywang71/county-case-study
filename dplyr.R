# Robert Gotwals
# May 8, 2019
# dplyr.R
# this file evaluates presidential election data
# select, filter, mutate, arrange, and summarize
#
# clean up and set working directory
rm(list=ls())
setwd("/Users/gotwals/desktop/RFolder")
#
# load libraries
library(dplyr)
library(pscl)
#
# take a look at the data
head(presidentialElections)
#
# select
votes <- select(presidentialElections, year, demVote)
head(votes)
stateyear <- select(presidentialElections, state:year)
head(stateyear)
nosouth <- select(presidentialElections, -south)
head(nosouth)
#
# filter
votes_2008 <- filter(presidentialElections, year == 2008)
head(votes_2008)
votes_CO_2008 <- filter(
  presidentialElections,
  year == 2008,
  state == "Colorado"
)
head(votes_CO_2008)
#
# mutate
presidentialElections <- mutate(
  presidentialElections,
  other_parties_vote = 100 - demVote,
  abs_vote_difference =abs(demVote - other_parties_vote
  )
)
#3
# arrange
presidentialElections <- arrange(presidentialElections, -year,demVote)
#
# summarize
average_votes <- summarize(
  presidentialElections,
  mean_dem_vote = mean(demVote),
  mean_other_parties = mean(other_parties_vote),
  na.rm = TRUE
)
average_votes
#
# function
furtheset_from_50 <- function(vec) {
  adjusted_values <- vec - 50
  vec[abs(adjusted_values) == max(abs(adjusted_values))]
}
summarize(
  presidentialElections,
  biggest_landslide = furtheset_from_50(demVote)
)
#
# group_by
grouped <- group_by(presidentialElections, state)
head(grouped)
state_voting_summary <- presidentialElections %>%
  group_by(state) %>%
  summarize(
    mean_dem_votge = mean(demVote),
    mean_other_parties = mean(other_parties_vote)
  )
head(state_voting_summary)
