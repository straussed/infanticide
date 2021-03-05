#################### Rank discrepancy- Ally Brown 7 November, 2018 #####################
###Load libaries and set global functions
rm(list = ls())
library(dplyr)
library(here)
options(stringsAsFactors = FALSE)

load(file = here('Data/cub_data.RData'))
################################################################################
  
### Examine ranks of killers and victims. Use table of qualitative data on
#   infanticide events

### add year to infanticide data
tblFemaleRanks$year <- as.numeric(tblFemaleRanks$year)
tblFemaleRanks$stan_rank <- as.numeric(tblFemaleRanks$stan_rank)
infanticide_notes$Mom_rank <- NA
infanticide_notes$Killer_rank <- NA


### Add killer and mother ranks
for(i in 1:nrow(infanticide_notes)){
  mom <- infanticide_notes$Mom[i]
  killer <- infanticide_notes$Killers[i]
  date <- infanticide_notes$Date[i]
  ###mom rank
  if(mom %in% filter(tblFemaleRanks, year == infanticide_notes[i,'Year'])$id){
    infanticide_notes$Mom_rank[i] <- filter(tblFemaleRanks, year == infanticide_notes[i,'Year'],
                                            id == mom)$stan_rank
  }
  
  ##killer rank
  if(killer %in% filter(tblFemaleRanks, year == infanticide_notes[i,'Year'])$id){
    infanticide_notes$Killer_rank[i] <- filter(tblFemaleRanks, year == infanticide_notes[i,'Year'],
                                               id == killer)$stan_rank
  }
  
}

### Statistical test comparing ranks of killers and mothers
t.test(infanticide_notes$Mom_rank[!is.na(infanticide_notes$Mom_rank)], 
       infanticide_notes$Killer_rank[!is.na(infanticide_notes$Killer_rank)])

### Prep data for plotting
mom.killer.rank <- rbind(data.frame(whose = rep('mom'), 
                                    rank = infanticide_notes$Mom_rank),
                         data.frame(whose = rep('killer'),
                                    rank = infanticide_notes$Killer_rank))
mom.killer.rank$whose <-factor(mom.killer.rank$whose, levels = c('killer', 'mom'))


labs = c('Killer', 'Mother of victim')
labs = paste0(labs, '\n(n = ', table(na.omit(mom.killer.rank)$whose), ')')
mom.killer.rank$whose <-factor(mom.killer.rank$whose, levels = c('killer', 'mom'),
                               labels = labs)

################################################################################
### Plotting
png(file = 'Plots/Killer_rank.png',width = 3.5, height = 3.5,
    units = 'in', res = 400)
ggplot(mom.killer.rank, aes(x = whose, y= rank))+
  geom_boxplot(color = 'grey30', size= 1, fill = 'grey85')+
  theme_classic(base_size = 14)+
  xlab("")+
  ylab("Standardized Rank\n(Low)                                 (High)")+
  scale_x_discrete()
dev.off()