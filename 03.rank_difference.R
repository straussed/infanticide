#################### Rank discrepency- Ally Brown 7 November, 2018 #####################
###Load libaries and set global functions
rm(list = ls())
library(dplyr)
options(stringsAsFactors = FALSE)
# #setwd('L:\\CurrentGradStudents/StraussEli/Infanticide/Final_analysis/')
# setwd('/Volumes/Holekamp/CurrentGradStudents/StraussEli/Infanticide/Final_analysis/')
#load('01.tidied_data.RData')
load(file = here('Data/cub_data.RData'))
  
### add year to the date
tblFemaleRanks$year <- as.numeric(tblFemaleRanks$year)
tblFemaleRanks$stan_rank <- as.numeric(tblFemaleRanks$stan_rank)
infanticide_notes$Mom_rank <- NA
infanticide_notes$Killer_rank <- NA
infanticide_notes$rank_notes <- ''

###for latest years where no rank info available, use rank from most recent year and make a note
for(i in 1:nrow(infanticide_notes)){
  mom <- infanticide_notes$Mom[i]
  killer <- infanticide_notes$Killers[i]
  date <- infanticide_notes$Date[i]
  ###mom rank
  if(mom %in% filter(tblFemaleRanks, year == infanticide_notes[i,'Year'])$id){
    infanticide_notes$Mom_rank[i] <- filter(tblFemaleRanks, year == infanticide_notes[i,'Year'],
                                            id == mom)$stan_rank
  }else if(mom %in% tblFemaleRanks$id){
    infanticide_notes$Mom_rank[i] <- tblFemaleRanks[tblFemaleRanks$id == mom,'stan_rank'][which.max(tblFemaleRanks[tblFemaleRanks$id == mom,'year'])]
    infanticide_notes$rank_notes[i] <- paste0(infanticide_notes$rank_notes[i], ';', 
                                              'mom rank used from ', tblFemaleRanks[tblFemaleRanks$id == mom,'year'][which.max(tblFemaleRanks[tblFemaleRanks$id == mom,'year'])])
  }
  
  ##killer rank
  if(killer %in% filter(tblFemaleRanks, year == infanticide_notes[i,'Year'])$id){
    infanticide_notes$Killer_rank[i] <- filter(tblFemaleRanks, year == infanticide_notes[i,'Year'],
                                            id == killer)$stan_rank
  }else if(killer %in% tblFemaleRanks$id){
    infanticide_notes$Killer_rank[i] <- tblFemaleRanks[tblFemaleRanks$id == killer,'stan_rank'][which.max(tblFemaleRanks[tblFemaleRanks$id == killer,'year'])]
    infanticide_notes$rank_notes[i] <- paste0(infanticide_notes$rank_notes[i], ';', 
                                              'killer rank used from ', tblFemaleRanks[tblFemaleRanks$id == killer,'year'][which.max(tblFemaleRanks[tblFemaleRanks$id == killer,'year'])])
  }
  
}


mom.killer.rank <- rbind(data.frame(whose = rep('mom'), 
                                    rank = infanticide_notes$Mom_rank),
                         data.frame(whose = rep('killer'),
                                    rank = infanticide_notes$Killer_rank))
mom.killer.rank$whose <-factor(mom.killer.rank$whose, levels = c('killer', 'mom'))


labs = c('Killer', 'Mother of victim')
labs = paste0(labs, '\n(n = ', table(na.omit(mom.killer.rank)$whose), ')')
mom.killer.rank$whose <-factor(mom.killer.rank$whose, levels = c('killer', 'mom'),
                               labels = labs)


png(file = 'Plots/Killer_rank.png',width = 3.5, height = 3.5,
    units = 'in', res = 400)
ggplot(mom.killer.rank, aes(x = whose, y= rank))+
  geom_boxplot(color = 'grey30', size= 1, fill = 'grey85')+
  theme_classic(base_size = 14)+
  xlab("")+
  ylab("Standardized Rank\n(Low)                                 (High)")+
  scale_x_discrete()
dev.off()

t.test(infanticide_notes$Mom_rank[!is.na(infanticide_notes$Mom_rank)], 
       infanticide_notes$Killer_rank[!is.na(infanticide_notes$Killer_rank)])

