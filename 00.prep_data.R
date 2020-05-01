##### Read and tidy data ####
rm(list = ls())
library(tidyverse)
options(stringsAsFactors = FALSE)
#setwd('L:\\CurrentGradStudents/StraussEli/Infanticide/Final_analysis/')
setwd('/Volumes/Holekamp/CurrentGradStudents/StraussEli/Infanticide/Final_analysis/')

### Table of all hyenas with known mortality data
known_mortality <- read.csv('Data/known_mortality.csv')
unique(known_mortality$mortality)
unique(known_mortality$clan)
known_mortality[known_mortality$mom == '3','mom'] <- '03'
known_mortality$disappeared <- as.Date(known_mortality$disappeared, format = '%m/%d/%y')
known_mortality$birthdate <- as.Date(known_mortality$birthdate, format = '%m/%d/%y')

##what proportion in main study clans? 
nrow(known_mortality)
nrow(filter(known_mortality, clan %in% c('talek', 'pond', 'kcm', 'happy.zebra', 'serena.n', 'serena.s')))

## Table of all hyenas with known mortality data
unknown<- read.csv('Data/unknown_mortality.csv')
unique(unknown$mortality)
unique(unknown$clan)
unknown[unknown$mom == '3','mom'] <- '03'
unknown[unknown$mom == '2','mom'] <- '02'
unknown$disappeared <- as.Date(unknown$disappeared)
unknown$birthdate <- as.Date(unknown$birthdate)

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

load('Data/tblFemaleRanks.RData')

save(list = ls(), 
     file = '01.tidied_data.RData')
