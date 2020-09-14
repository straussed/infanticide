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

cubs$birth_year <- as.numeric(format(cubs$dob, '%Y'))

left_join(cubs, tblFemaleRanks, by = c('mom', ))

save(list = ls(), 
     file = '01.tidied_data.RData')
