library(hyenadata)
library(dplyr)
library(here)

data(tblHyenas)
data(tblFemaleRanks)
data(tblLifeHistory.wide)

tblLifeHistory.wide$mom <- left_join(tblLifeHistory.wide, tblHyenas, by = c('id'))$mom

### Select cubs for whom we have rank data (for the mom)
### Not in talek-east (group that fissioned from study clan and was briefly followed)
### has been seen
### Was born after initiation of project
### Was not born within the last year of study (to evaluate survival to 1)
cubs <- filter(tblLifeHistory.wide, 
               tblLifeHistory.wide$dob_event_data != 'talek.e',
               mom %in% tblFemaleRanks$id,
               !is.na(dfs),
               dob >= '1988-06-01',
               dob <= '2019-05-01')[c('id', 'mom', 'dob', 'disappeared')]

cubs$mortality <- ifelse(cubs$disappeared < (cubs$dob + 365) & !is.na(cubs$disappeared),
                         NA,
                         'survive_to_1yo')

cubs <- cubs[,c('id', 'mom', 'dob', 'mortality')]

save(cubs, file = 'Data/all_cubs.RData')



