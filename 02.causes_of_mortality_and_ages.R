
###############Analyze Cause Infant Mortality-Ally Brown 24 October, 2018##############
###Load libraries and set global options
rm(list = ls())
library(tidyverse)
library(survminer)
library(gridExtra)
library(ggridges)
options(stringsAsFactors = FALSE)
#setwd('L:\\CurrentGradStudents/StraussEli/Infanticide/Final_analysis/')
setwd('/Volumes/Holekamp/CurrentGradStudents/StraussEli/Infanticide/Final_analysis/')
load('01.tidied_data.RData')

# 
###Observed frequencies of mortality sources
summarized_mortality <- summarize(select(group_by(known_mortality,mortality),mortality),frequency = length(mortality))
summarized_mortality <- arrange(summarized_mortality,desc(frequency))
summarized_mortality$mortality <- factor(summarized_mortality$mortality, levels = summarized_mortality$mortality)
summarized_mortality$obs_inf <- 'observed'


#### Inferred frequenceis of mortality sources 

### Proportion of mom death associated with starvation
table(filter(known_mortality, mom_disappeared == TRUE)[,c('mortality')])

### Proportion of starvation deaths associated with  mom's death
table(filter(known_mortality, mortality == 'starvation')$mom_disappeared)
nrow(filter(known_mortality, mortality == 'starvation'))

### Number of unknown mortality where mom disappeared
nrow(filter(unknown, mom_disappeared == TRUE))

known_mortality_mom_alive <- filter(known_mortality, mom_disappeared==FALSE)
mortality.sources <- unique(known_mortality_mom_alive$mortality)

summarized_mortality_inferred <- summarized_mortality
summarized_mortality_inferred$obs_inf <- 'inferred'
summarized_mortality_inferred$frequency <- 
  nrow(filter(unknown, !mom_disappeared | is.na(mom_disappeared))) * 
  (table(known_mortality_mom_alive$mortality)[mortality.sources]/
  sum(table(known_mortality_mom_alive$mortality)[mortality.sources]))[as.character(summarized_mortality_inferred$mortality)]

summarized_mortality_inferred$frequency[summarized_mortality_inferred$mortality == 'starvation'] <- 
  summarized_mortality_inferred$frequency[summarized_mortality_inferred$mortality == 'starvation']  +
  nrow(filter(unknown, mom_disappeared))

summarized_mortality <- rbind(summarized_mortality, summarized_mortality_inferred)

### Bootstrap to get confidence interval
mortality.bootstraps <- matrix(data = NA, nrow = length(mortality.sources),
                               ncol = 1001, dimnames = list(mortality.sources, 1:1001))
mortality.bootstraps[,1] <- table(known_mortality_mom_alive$mortality)[mortality.sources]/sum(table(known_mortality_mom_alive$mortality)[mortality.sources])
for(i in 2:1001){
  mortality.samples <- sample(replace = TRUE, known_mortality_mom_alive$mortality)
  mortality.bootstraps[,i] <- table(mortality.samples)[mortality.sources]/sum(table(known_mortality_mom_alive$mortality)[mortality.sources])
}


###Errorbars from bootstraps
mortality.bootstraps <- as.data.frame(t(mortality.bootstraps))
mortality.bootstraps[is.na(mortality.bootstraps)] <- 0

boot.ci <- as.data.frame(t(apply(X = data.frame(t(mortality.bootstraps)),
                                 MARGIN = 1, 
                                 FUN = quantile, c(0.975, 0.025)))) * nrow(filter(unknown, !mom_disappeared))
names(boot.ci) <- c('upper', 'lower')
boot.ci$x <- rownames(boot.ci)
boot.ci[as.character(filter(summarized_mortality, obs_inf == 'observed')$mortality),]$upper <- 
  boot.ci[as.character(filter(summarized_mortality, obs_inf == 'observed')$mortality),]$upper + 
  filter(summarized_mortality, obs_inf == 'observed')$frequency

boot.ci[as.character(filter(summarized_mortality, obs_inf == 'observed')$mortality),]$lower <- 
  boot.ci[as.character(filter(summarized_mortality, obs_inf == 'observed')$mortality),]$lower + 
  filter(summarized_mortality, obs_inf == 'observed')$frequency

boot.ci['starvation',1:2] <- boot.ci['starvation',1:2] + nrow(filter(unknown, mom_disappeared))



### Plotting

lighten <- function(color, factor=1.4){
  col <- col2rgb(color)
  col <- col*factor
  col <- rgb(t(col), maxColorValue=255)
  col
}

desat <- function(cols, sat=0.5) {
  X <- diag(c(1, sat, 1)) %*% rgb2hsv(col2rgb(cols))
  hsv(X[1,], X[2,], X[3,])
}


col1 <- rgb(red = 62, green = 118, blue = 73, maxColorValue = 255)
col2 <- desat(lighten(col1))

summarized_mortality$obs_inf <- factor(summarized_mortality$obs_inf,
                                       labels = c('Inferred mortality source', 'Known mortality source'))


age.by.mortality <- rbind(known_mortality[,c('mortality', 'age_at_death')],
                          unknown[,c('mortality', 'age_at_death')])

age.by.mortality$mortality <- factor(age.by.mortality$mortality, 
                                     levels = c('flooded den', 'illness',
                                                'siblicide', 'human', 'lion',
                                                'infanticide','starvation','unknown'))

levs = c('flooded den', 'illness',
           'siblicide', 'human', 'lion',
           'infanticide','starvation','unknown')
levs.ss <- paste0(levs, '\n(n = ', table(age.by.mortality$mortality), ')')

age.by.mortality$mortality <- factor(age.by.mortality$mortality, 
                                     levels = c('flooded den', 'illness',
                                                'siblicide', 'human', 'lion',
                                                'infanticide','starvation','unknown'),
                                     labels = levs.ss)


ggplot(age.by.mortality, aes(x = age_at_death, y = mortality))+
  geom_density_ridges()+
  theme_survminer()+
  xlab('Age at death (months)')+
  ylab('Mortality source')


### Both together
ages <- ggplot(age.by.mortality, aes(x = age_at_death, y = mortality, fill = mortality))+
  geom_density_ridges(scale = 1.6)+
  theme_survminer()+
  theme(legend.position = 'none')+
  xlab('Age at death (months)')+
  ylab('Mortality source')+
  scale_fill_manual(values = c(rep('grey30', 7), 'grey85'))+
  xlim(-1,12.5)



summarized.mortality.combined.plots <- rbind(summarized_mortality,
                                             data.frame(mortality = rep(factor('unknown'), 2),
                                                        frequency = rep(NA,2),
                                                        obs_inf = c('Known mortality source', 'Inferred mortality source')))
summarized.mortality.combined.plots$mortality <- factor(summarized.mortality.combined.plots$mortality,
                                                        levels = c('flooded den', 'illness',
                                                                   'siblicide', 'human', 'lion',
                                                                   'infanticide','starvation','unknown'))


counts <- ggplot(data=summarized.mortality.combined.plots,aes(x=mortality, y = frequency, width=0.8, color = obs_inf,
                                     fill = obs_inf))+
  geom_bar(stat = 'identity')+
  theme_survminer()+
  xlab("Source of Mortality")+
  ylab("Count")+
  scale_fill_manual(values = c('gray85', 'grey30'))+
  scale_color_manual(values = c('gray85', 'grey30'))+
  theme(legend.position = c(0.5,0.94),
        legend.title = element_blank(),
        axis.text.y = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        plot.margin = unit(c(4,0,6,0), units = 'pt'))+
  geom_errorbar(data = boot.ci, aes(x = x, ymin = lower, ymax = upper), inherit.aes = F, width = 0.2) + 
  coord_flip()

groblist <- list(ggplotGrob(ages), ggplotGrob(counts))

png('Plots/mortality_source_and_age.png', width = 7, height = 5,
    res = 400, units = 'in')
grid.arrange(grobs = groblist,
             layout_matrix = matrix(data = c(1,1,1,2,2,
                                             1,1,1,2,2,
                                             1,1,1,2,2,
                                             1,1,1,2,2,
                                             1,1,1,2,2,
                                             1,1,1,2,2,
                                             1,1,1,2,2,
                                             1,1,1,2,2,
                                             1,1,1,2,2,
                                             1,1,1,2,2),
                                    nrow = 10, ncol = 5, byrow = TRUE))
dev.off()
