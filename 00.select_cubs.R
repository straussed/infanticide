###############################################################################
## Extract mortality and cub info
rm(list = ls())
hyenadata::update_tables('1.2.88')
library(hyenadata)  ### Version 1.2.88
library(dplyr)
library(here)
source('000.functions.R')

data("tblHyenas")
data("tblLifeHistory.wide")
data("tblFemaleRanks")
data('tblSessions')
data('tblHyenasPerSession')
data('tblPreyCensus')

infanticide_notes <- read.csv("Data/infanticide_notes.csv")
infanticide_notes$Date <- as.Date(infanticide_notes$Date, format = '%m/%d/%Y')

### Table of cleaned mortality data
cleaned.mortality <- read.csv(here('Data/known_mortality.csv'))

## Combine rare mortality into an 'other' category
cleaned.mortality[cleaned.mortality$mortality %in% c('illness', 'flooded den'),]$mortality <- 'other'

names(cleaned.mortality)
cleaned.mortality$birthdate <- as.Date(cleaned.mortality$birthdate, format = '%m/%d/%Y')
cleaned.mortality$disappeared <- as.Date(cleaned.mortality$disappeared, format = '%m/%d/%Y')

###############################################################################

### Table of all mortality
all.mortality <- filter(tblLifeHistory.wide, disappeared <= (dob + 365), 
                        disappeared < '2019-01-01')

## Restrict to study clans
all.mortality <- filter(all.mortality, dob_event_data %in% c('talek', 'pond', 'kcm', 'fig.tree', 'mara.river', 'happy.zebra', 'serena.n', 'serena.s', 'oltukai', 'airstrip'))

## How many cubs survived to 1 year old? 
all.cubs <- filter(tblLifeHistory.wide, 
       dob < '2018-01-01', dob >= '1988-01-01',
       dob_event_data %in% c('talek', 'pond', 'kcm', 'fig.tree', 'mara.river', 'happy.zebra', 'serena.n', 'serena.s', 'oltukai', 'airstrip'))

survive.to.1 <- tblHyenasPerSession %>%
  filter(id %in% all.cubs$id) %>%
  group_by(id) %>%
  summarise(last.seen = max(date), .groups = 'drop_last') %>%
  left_join(all.cubs[,c('id', 'dob')], by = 'id') %>%
  filter(last.seen >= (dob+365))

## 938 cubs who survived to 1 year old
num.survivors <- nrow(survive.to.1)

## Number of sessions

## Masai mara
length(unique(filter(tblSessions, clan  %in% c('talek', 'pond', 'kcm', 'fig.tree', 'mara.river', 'happy.zebra', 'serena.n', 'serena.s'),
                     date <= '2018-12-31')$session))
## Amboseli
length(unique(filter(tblSessions, clan  %in% c('amboseli'))$session))

## remove some individuals with incorrect dates or who are unverified
all.mortality <- filter(all.mortality, !id %in% c('grig', 'lb', 'mc2', 'mc3', '44', 'aber', 'mbrk', 'sdc'))

## remove dummy cubs
all.mortality <- filter(all.mortality, !grepl('dc[0-9]', id))

all.mortality <- left_join(all.mortality, tblHyenas[,c('id', 'sex', 'mom')])
all.mortality$age_at_death <- as.numeric((all.mortality$disappeared - all.mortality$dob)/30.4375)
all.mortality$mom_disappeared <- left_join(all.mortality, tblLifeHistory.wide, by = c('mom' = 'id'))$disappeared.y
all.mortality$mom_disappeared <- (all.mortality$mom_disappeared < all.mortality$disappeared) & !is.na(all.mortality$mom_disappeared)

all.mortality <- all.mortality %>%
  select(id, disappeared, dob, dob_event_data, disappeared_event_data, sex, mom, mom_disappeared, age_at_death) %>%
  rename(mortality = disappeared_event_data, clan = dob_event_data, birthdate = dob)


all.mortality <- all.mortality[,c('id', 'sex', 'disappeared', 'mom', 'birthdate',
                                  'mortality', 'clan', 'mom_disappeared', 'age_at_death')]


unknown.mortality <- filter(all.mortality, !id %in% cleaned.mortality$id)


## Remove mc2 and mc3, who are unverifiable hyenas
unknown.mortality <- filter(unknown.mortality, !id %in% c('mc2', 'mc3'))
unique(unknown.mortality$mortality) ## Make sure these are all different versions of unknown.

unknown.mortality$mortality <- 'unknown'


#### Finalize large dataset
all.mortality <- rbind(unknown.mortality, cleaned.mortality)
all.mortality[all.mortality$sex%in% c('u', '') & !is.na(all.mortality$sex),'sex'] <- NA


### Ensure clan names for tblPreyCensus match cub mortality table
tblPreyCensus$clan[tblPreyCensus$clan == 'talek.w'] <- 'talek'
all.mortality$prey_density <- get_prey_density(all.mortality$disappeared, all.mortality$clan, time.period = -(365.25/12))

### Get number of cubs at den in month prior to death
all.mortality$cub_associates <- get_cub_associates(all.mortality$id, all.mortality$disappeared, time.period = -(365.25/12))

save(num.survivors, all.mortality, tblFemaleRanks, infanticide_notes, file = here('Data/cub_data.RData'))
