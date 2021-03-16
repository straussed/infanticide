
library(brms)
library(dplyr)
library(here)
library(ggplot2)
library(patchwork)

set.seed(1989)
options(stringsAsFactors = FALSE)
################################################################################
### Load data for analysis
rm(list = ls())
load(file = here('Data/cub_data.RData'))
load(file = here('Data/known_mortality_cleaned.RData'))


### Set infanticide as intercept for modeling
known.mortality$mortality.model <- factor(known.mortality$mortality, levels = c('infanticide', 'lion', 'other', 'siblicide', 'death of mother', 'starvation', 'human'))

################################################################################
### Does prey density vary by mortality source?
prey_mod <- brm(data = known.mortality[!is.na(known.mortality$prey_density),], prey_density ~ 1 + mortality.model + (1|clan),
                control = list(adapt_delta = 0.99), save_all_pars = TRUE)
prey_null_mod <- brm(data = known.mortality[!is.na(known.mortality$prey_density),], prey_density ~ 1 + (1|clan), 
                     control = list(adapt_delta = 0.99), save_all_pars = TRUE)

## Model comparison
loo(prey_mod, prey_null_mod, moment_match = TRUE)

################################################################################
### Does the number of cubs vary by mortality source?
cub_density_mod <- brm(data = known.mortality[!is.na(known.mortality$cub_associates),], cub_associates ~ 1 + mortality + (1|clan),
                       control = list(adapt_delta = 0.99))
cub_null_mod <- brm(data = known.mortality[!is.na(known.mortality$cub_associates),], cub_associates ~ 1 + (1|clan),
                control = list(adapt_delta = 0.99))

## Model comparison
loo(cub_density_mod, cub_null_mod)

save(prey_mod, prey_null_mod, cub_density_mod, cub_null_mod, file = 'Data/prey_and_cub_models.RData')

################################################################################
### Do the ranks of killers and mothers of victims differ?

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


#### Plotting

known.mortality$mortality.plot <- factor(known.mortality$mortality, levels = c('death of mother', 'infanticide', 'lion', 'starvation', 'siblicide', 'human', 'other'),
                                         labels = c('death of\nmother', 'infanticide', 'lion', 'starvation', 'siblicide', 'human', 'other'))

prey <- ggplot(data = known.mortality, aes(x = mortality.plot, y = prey_density))+
  geom_boxplot(outlier.color = NA, color = 'grey30', size= 1, fill = 'grey85')+
  labs(tags = 'a) ')+
  theme_classic(base_size = 14) +
  coord_cartesian(ylim= c(0,800))+
  ylab('Average prey density')+
  xlab('')


cubs <- ggplot(data = known.mortality, aes(x = mortality.plot, y = cub_associates))+
  geom_boxplot(outlier.color = NA, color = 'grey30', size= 1, fill = 'grey85')+
  labs(tags = 'b) ')+
  theme_classic(base_size = 14)+
  ylab('Average cub density')+
  xlab('Mortality source')


png(file = 'Plots/prey_and_cub_mortality.png', width = 7, height = 6,
    units = 'in', res = 400)
prey + cubs + plot_layout(ncol = 1)
dev.off()
