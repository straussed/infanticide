##### Read and tidy data ####
rm(list = ls())
library(dplyr)
library(here)
options(stringsAsFactors = FALSE)
#setwd('L:\\CurrentGradStudents/StraussEli/Infanticide/Final_analysis/')
here()
#setwd('/Volumes/Holekamp/CurrentGradStudents/StraussEli/Infanticide/Final_analysis/')

### Table of all hyenas with known mortality data
known_mortality <- read.csv('Data/known_mortality.csv')
unique(known_mortality$mortality)
unique(known_mortality$clan)
known_mortality[known_mortality$mom == '3','mom'] <- '03'
known_mortality$disappeared <- as.Date(known_mortality$disappeared, format = '%m/%d/%Y')
known_mortality$birthdate <- as.Date(known_mortality$birthdate, format = '%m/%d/%Y')

##what proportion in main study clans? 
nrow(known_mortality)
nrow(filter(known_mortality, clan %in% c('talek', 'pond', 'kcm', 'happy.zebra', 'serena.n', 'serena.s')))

## Table of all dead juvenile hyenas with known mortality data
unknown<- read.csv('Data/unknown_mortality.csv')
unique(unknown$mortality)
unique(unknown$clan)
unknown[unknown$mom == '3','mom'] <- '03'
unknown[unknown$mom == '2','mom'] <- '02'
unknown$disappeared <- as.Date(unknown$disappeared, format = '%m/%d/%Y')
unknown$birthdate <- as.Date(unknown$birthdate, format = '%m/%d/%Y')

##what proportion in main study clans? 
nrow(unknown)
nrow(filter(unknown, clan %in% c('talek', 'pond', 'kcm', 'happy.zebra', 'serena.n', 'serena.s')))

### Check for duplicates or shared entries
any(duplicated(unknown$id))
inner_join(unknown, known_mortality, by = 'id')

### Table of infanticide invents and associated notes
infanticide_notes <- read.csv("Data/infanticide_notes.csv")
infanticide_notes$Date <- as.Date(infanticide_notes$Date, format = '%m/%d/%y')

### Total number of observed mortality
nrow(unknown) + nrow(known_mortality)

## Rank data created using Informed MatReorder described in Strauss & Holekamp 2019
load('Data/tblFemaleRanks.RData')

## Load data from all cubs from primary study groups, even those that survive >1yr
load('Data/all_cubs.RData')

## Determine rank of mother in cubs year of birth.
cubs$birth_year <- as.numeric(format(cubs$dob, '%Y'))
cubs$mom_rank <- left_join(cubs, tblFemaleRanks, by = c('mom' = 'id', 'birth_year' = 'year'))$stan_rank

## If mother has no rank assigned for birth year, check one year later (young mothers)
cubs$birth_year_next <- cubs$birth_year + 1
cat('Number of cubs whose rank was determined from subsequent year: ', 
    sum(!is.na(left_join(cubs[is.na(cubs$mom_rank),], tblFemaleRanks, by = c('mom' = 'id', 'birth_year_next' = 'year'))$stan_rank)))  ## 37
cubs[is.na(cubs$mom_rank),]$mom_rank <- left_join(cubs[is.na(cubs$mom_rank),], tblFemaleRanks, by = c('mom' = 'id', 'birth_year_next' = 'year'))$stan_rank

## If mother has no rank assigned for birth year, check one year prior (end of rank data)
cubs$birth_year_prev <- cubs$birth_year - 1
cat('Number of cubs whose rank was determined from previous year: ', 
    sum(!is.na(left_join(cubs[is.na(cubs$mom_rank),], tblFemaleRanks, by = c('mom' = 'id', 'birth_year_prev' = 'year'))$stan_rank)))  ## 76
cubs[is.na(cubs$mom_rank),]$mom_rank <- left_join(cubs[is.na(cubs$mom_rank),], tblFemaleRanks, by = c('mom' = 'id', 'birth_year_prev' = 'year'))$stan_rank

## Assign cub mortality from known_mortality
cubs[is.na(cubs$mortality),]$mortality <- left_join(cubs[is.na(cubs$mortality),], known_mortality, by = c('id'))$mortality.y

## Assign cub mortality from unknown
cubs[is.na(cubs$mortality),]$mortality <- left_join(cubs[is.na(cubs$mortality),], unknown, by = c('id'))$mortality.y

table(cubs$mortality)

cubs <- cubs[c('id', 'mom', 'dob', 'mortality', 'mom_rank')]

save(list = ls(), 
     file = '01.tidied_data.RData')
