library(brms)
library(dplyr)
library(here)
library(ggplot2)

set.seed(1989)
options(stringsAsFactors = FALSE)
################################################################################
### Load data for analysis
rm(list = ls())
load(file = here('Data/known_mortality_cleaned.RData'))


known.mortality$prey_density

known.mortality$mortality.model <- factor(known.mortality$mortality, levels = c('infanticide', 'lion', 'other', 'siblicide', 'death of mother', 'starvation', 'human'))
prey_mod <- brm(data = known.mortality[!is.na(known.mortality$prey_density),], prey_density ~ 1 + mortality.model + (1|clan),
                control = list(adapt_delta = 0.99))
null_mod <- brm(data = known.mortality[!is.na(known.mortality$prey_density),], prey_density ~ 1 + (1|clan), control = list(adapt_delta = 0.99))

## Model comparison
loo(prey_mod, null_mod)


ggplot(data = known.mortality, aes(x = mortality, y = prey_density))+
  geom_boxplot(outlier.color = NA)+
  theme_classic(base_size = 14) +
  coord_cartesian(ylim= c(0,800))
  
