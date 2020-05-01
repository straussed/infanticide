###### Timing of infanticide relative to other mortality sources ######

###Load libaries and set global functions
rm(list = ls())
library(tidyverse)
library(survminer)
options(stringsAsFactors = FALSE)
#setwd('L:\\CurrentGradStudents/StraussEli/Infanticide/Final_analysis/')
setwd('/Volumes/Holekamp/CurrentGradStudents/StraussEli/Infanticide/Final_analysis/')
load('01.tidied_data.RData')
set.seed(1989)


mort.timing <- filter(known_mortality, clan %in% c('serena.s', 'serena.n', 'happy.zebra', 'talek', 'kcm','pond'))
mort.timing$full.mortality <- mort.timing$mortality
mort.timing$mortality <- ifelse(mort.timing$mortality == 'infanticide', 'infanticide', 'not.infanticide')
mort.timing$month <- as.numeric(format(mort.timing$disappeared, '%m'))

#### Migration (July - November)
mort.timing$migration <- ifelse(mort.timing$month >= 7 & mort.timing$month <= 11, 1, 0) 

#### Hardship (i.e., end of dry season) (Dec - June)
mort.timing$hardship <- ifelse(mort.timing$month == 12 | (mort.timing$month >= 1 & mort.timing$month <= 6), 1, 0) 


#### Chisq test - migration
chisq.table <- table(mort.timing[,c('mortality', 'migration')])
chisq.test(chisq.table, simulate.p.value = TRUE, B = 10000)
