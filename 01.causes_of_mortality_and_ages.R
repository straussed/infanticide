library(brms)
library(dplyr)
library(tidyr)
library(here)
library(grid)
library(ggridges)
library(ggplot2)
library(gridExtra)

set.seed(1989)
options(stringsAsFactors = FALSE)
################################################################################
### Load data for analysis
rm(list = ls())
load(file = here('Data/cub_data.RData'))

##### Split into mortality with known and unknown sources
known.mortality <- filter(all.mortality, mortality != 'unknown')
unknown.mortality <- filter(all.mortality, mortality == 'unknown')

## For estimating frequency of different types of mortality, divide mortality
## based on whether mother preceded offspring in death. Assign 'death of mother' 
## to be it's own type of mortality, even superseding other causes. 

known.mortality[is.na(known.mortality$mom_disappeared),'mom_disappeared'] <- FALSE
known.mortality[known.mortality$mom_disappeared,]$mortality <- 'death of mother'

## unknown mortality when mother precedes offspring in death
um.death.of.mother <- filter(unknown.mortality, mom_disappeared == TRUE)
um.death.of.mother$mortality <- 'death of mother'
unknown.mortality <- filter(unknown.mortality, mom_disappeared == FALSE)

known.mortality$infanticide <- as.numeric(known.mortality$mortality == 'infanticide')
known.mortality$starvation <- as.numeric(known.mortality$mortality == 'starvation')
known.mortality$lion <- as.numeric(known.mortality$mortality == 'lion')
known.mortality$human <- as.numeric(known.mortality$mortality == 'human')
known.mortality$siblicide <- as.numeric(known.mortality$mortality == 'siblicide')
known.mortality$other <- as.numeric(known.mortality$mortality == 'other')

known.mortality$y <- as.matrix(known.mortality[,c('infanticide', 'starvation', 'lion', 'siblicide',
                                                  'human','other')])


known.mortality.mom.alive <- filter(known.mortality, mom_disappeared == FALSE)
nrow(known.mortality.mom.alive)

save(known.mortality, file = 'Data/known_mortality_cleaned.RData')

################################################################################
### Descriptives

## Total juvenile mortality
nrow(unknown.mortality) + nrow(um.death.of.mother) + nrow(known.mortality)
nrow(all.mortality)

## Number of infanticide cases
table(known.mortality$mortality)

## Proportion of known mortality due to different causes
table(known.mortality$mortality)/nrow(known.mortality)

## representations of different clans
table(all.mortality$clan)

## Number of mortality sources recoded as 'death of mother'
table(filter(all.mortality, mom_disappeared == TRUE)$mortality)

################################################################################
### Modeling

## Priors
priors <- get_prior(data = known.mortality.mom.alive, formula = bf(y| trials(1) ~ 0 + age_at_death), family = multinomial())
#priors$prior[grepl('student_t', priors$prior)] <- 'normal(0, 3)'
priors <- c(set_prior('normal(0,3)', class = 'b'), set_prior('normal(0,3)', class = 'Intercept'))

## Model
fit <- brm(data = known.mortality.mom.alive, formula = bf(y|trials(1) ~ 1 + age_at_death), family = multinomial(), 
           prior = priors, chains = 3, iter = 30000, warmup = 15000, seed = 1989, cores = 3, inits = 0)
save(fit, file = 'Data/age_model.RData')




################################################################################
### Predictions

### Predict mortality source for each individual with unknown mortality source
pred.fit <- posterior_predict(fit, newdata = unknown.mortality, nsamples = 200)

## Take mean of predictions to get mean and CI for number of inferred mortality
#  events for each mortality source
posterior.means <- apply(pred.fit, 3, function(x)(sum(x/200)))
probs <- apply(pred.fit, c(2,3), function(x)(sum(x/200)))
posterior.draws <- apply(pred.fit, c(1,3), sum)
posterior.cred.int <- apply(posterior.draws, 2, quantile, c(0.95, 0.05))

post.ci <- data.frame(mortality = rownames(t(posterior.cred.int)),
                      t(posterior.cred.int))
post.ci[,2:3] <- post.ci[,2:3] + table(known.mortality.mom.alive$mortality)[post.ci$mortality]
names(post.ci) <- c('mortality', 'high', 'low')
                      


## Sample posterior for predictions of probability of different mortality
#  sources based on age at death. 
smooth.pred <- posterior_epred(fit, newdata = data.frame(age_at_death = seq(from = 0, to = 12, by = 0.1)),
                               nsamples = 200)
smooth.probs <- data.frame(apply(smooth.pred, c(2,3), mean))
smooth.probs$age <- seq(from = 0, to = 12, by = 0.1)
smooth.probs.high <- data.frame(apply(smooth.pred, c(2,3), quantile, 0.95))
smooth.probs.high$age <- seq(from = 0, to = 12, by = 0.1)
smooth.probs.low <- data.frame(apply(smooth.pred, c(2,3), quantile, 0.05))
smooth.probs.low$age <- seq(from = 0, to = 12, by = 0.1)


################################################################################
### Prepare data for plotting
summarized.mortality <- data.frame(mortality = names(table(known.mortality$mortality)),
                                   frequency = as.numeric(table(known.mortality$mortality)),
                                   obs.inf = 'observed')

summarized.mortality$mortality <- factor(summarized.mortality$mortality,
                                         levels = c( 'other', 'human',
                                                     'siblicide', 'starvation', 'lion','infanticide', 'death of mother',
                                                     'unknown'))

summarized.mortality <- rbind(summarized.mortality,
                              data.frame(mortality = c(names(posterior.means), 'death of mother', 'unknown'),
                                         frequency = c(posterior.means, nrow(um.death.of.mother), NA),
                                         obs.inf = 'inferred'))

age.by.mortality <- rbind(known.mortality[,c('mortality', 'age_at_death')],
                          unknown.mortality[,c('mortality', 'age_at_death')],
                          data.frame(mortality = 'unknown', age_at_death = um.death.of.mother$age_at_death))

age.by.mortality$mortality <- factor(age.by.mortality$mortality, 
                                     levels = c( 'other', 'human',
                                                 'siblicide', 'starvation', 'lion','infanticide', 'death of mother',
                                                 'unknown'))

levs = c( 'other', 'human',
          'siblicide', 'starvation', 'lion', 'infanticide', 'death of mother',  'unknown')
levs.ss <- paste0(levs, '\n(n = ', table(age.by.mortality$mortality), ')')

age.by.mortality$mortality <- factor(age.by.mortality$mortality, 
                                     levels = c( 'other', 'human',
                                                 'siblicide', 'starvation', 'lion','infanticide', 'death of mother',
                                                 'unknown'),
                                     labels = levs.ss)


## CI around infanticide and lion freqeuncy
post.ci["infanticide",2]/nrow(all.mortality)
post.ci["infanticide",3]/nrow(all.mortality)
sum(summarized.mortality[summarized.mortality$mortality == 'infanticide',]$frequency)/nrow(all.mortality)

post.ci["lion",2]/nrow(all.mortality)
post.ci["lion",3]/nrow(all.mortality)
sum(summarized.mortality[summarized.mortality$mortality == 'lion',]$frequency)/nrow(all.mortality)


### Plotting
################################################################################
png('Plots/prob_mortality_source.png', width = 7, height = 5,
    res = 400, units = 'in')
par(mfrow = c(2,3),
    mar = c(0,0,0,0),
    oma = c(4,4,1,1), family = 'sans')

## Infanticide 
plot(x = smooth.probs$age, 
     y = smooth.probs$infanticide, 
     col = 'black', 
     type = 'l', 
     lwd = 2,
     ylim = c(0,1.1),
     xlab = 'Age',
     ylab = '',
     yaxt = 'n',
     xaxt = 'n')

lines(x = smooth.probs.high$age, 
     y = smooth.probs.high$infanticide, 
     col = 'black', 
     type = 'l', 
     lty = 2,
     lwd = 1,
     ylim = c(0,1))

lines(x = smooth.probs.low$age, 
      y = smooth.probs.low$infanticide, 
      col = 'black', 
      type = 'l', 
      lty = 2,
      lwd = 1,
      ylim = c(0,1))
title(main = 'infanticide', line = -2)
axis(side = 2, at = c(0,0.5, 1), labels = TRUE, outer = TRUE)

## Lions 
plot(x = smooth.probs$age, 
     y = smooth.probs$lion, 
     col = 'black', 
     type = 'l', 
     lwd = 2,
     ylim = c(0,1.1),
     xlab = 'Age',
     ylab = '',
     yaxt = 'n',
     xaxt = 'n')
title(main = 'lions', line = -2)

lines(x = smooth.probs.high$age, 
      y = smooth.probs.high$lion, 
      col = 'black', 
      type = 'l', 
      lty = 2,
      lwd = 1,
      ylim = c(0,1))

lines(x = smooth.probs.low$age, 
      y = smooth.probs.low$lion, 
      col = 'black', 
      type = 'l', 
      lty = 2,
      lwd = 1,
      ylim = c(0,1))

## Starvation 
plot(x = smooth.probs$age, 
     y = smooth.probs$starvation, 
     col = 'black', 
     type = 'l', 
     lwd = 2,
     ylim = c(0,1.1),
     xlab = 'Age',
     ylab = '',
     yaxt = 'n',
     xaxt = 'n')

lines(x = smooth.probs.high$age, 
      y = smooth.probs.high$starvation, 
      col = 'black', 
      type = 'l', 
      lty = 2,
      lwd = 1,
      ylim = c(0,1))

lines(x = smooth.probs.low$age, 
      y = smooth.probs.low$starvation, 
      col = 'black', 
      type = 'l', 
      lty = 2,
      lwd = 1,
      ylim = c(0,1))
title(main = 'starvation', line = -2)



## Humans 
plot(x = smooth.probs$age, 
     y = smooth.probs$human, 
     col = 'black', 
     type = 'l', 
     lwd = 2,
     ylim = c(0,1.1),
     xlab = 'Age',
     ylab = '',
     yaxt = 'n',
     xaxt = 'n')

lines(x = smooth.probs.high$age, 
      y = smooth.probs.high$human, 
      col = 'black', 
      type = 'l', 
      lty = 2,
      lwd = 1,
      ylim = c(0,1))

lines(x = smooth.probs.low$age, 
      y = smooth.probs.low$human, 
      col = 'black', 
      type = 'l', 
      lty = 2,
      lwd = 1,
      ylim = c(0,1))
title(main = 'humans', line = -2)
axis(side = 1, at = c(0,6,12), labels = TRUE, outer = TRUE)
mtext('Probability of mortality source', side = 2, line = 2,at = 1)
axis(side = 2, at = c(0,0.5, 1), labels = TRUE, outer = TRUE)

## Siblicide 
plot(x = smooth.probs$age, 
     y = smooth.probs$siblicide, 
     col = 'black', 
     type = 'l', 
     lwd = 2,
     ylim = c(0,1.1),
     ylab = '',
     yaxt = 'n',
     xaxt = 'n')

lines(x = smooth.probs.high$age, 
      y = smooth.probs.high$siblicide, 
      col = 'black', 
      type = 'l', 
      lty = 2,
      lwd = 1,
      ylim = c(0,1))

lines(x = smooth.probs.low$age, 
      y = smooth.probs.low$siblicide, 
      col = 'black', 
      type = 'l', 
      lty = 2,
      lwd = 1,
      ylim = c(0,1))
title(main = 'siblicide', line = -2)
axis(side = 1, at = c(0,6,12), labels = TRUE, outer = TRUE)
mtext('Age at death (months)', side = 1, line = 2)

## Other 
plot(x = smooth.probs$age, 
     y = smooth.probs$other, 
     col = 'black', 
     type = 'l', 
     lwd = 2,
     ylim = c(0,1.1),
     xlab = 'Age',
     ylab = '',
     yaxt = 'n',
     xaxt = 'n')
title(main = 'other', line = -2)

lines(x = smooth.probs.high$age, 
      y = smooth.probs.high$other, 
      col = 'black', 
      type = 'l', 
      lty = 2,
      lwd = 1,
      ylim = c(0,1))

lines(x = smooth.probs.low$age, 
      y = smooth.probs.low$other, 
      col = 'black', 
      type = 'l', 
      lty = 2,
      lwd = 1,
      ylim = c(0,1))
axis(side = 1, at = c(0,6,12), labels = TRUE, outer = TRUE)

dev.off()



ages <- ggplot(age.by.mortality, aes(x = age_at_death, y = mortality, fill = mortality))+
  geom_density_ridges(scale = 1.6, panel_scaling = FALSE)+
  theme_classic(base_size = 14)+
  theme(legend.position = 'none')+
  xlab('Age at death (months)')+
  ylab('Mortality source')+
  scale_fill_manual(values = c(rep('grey30', 7), 'grey85'))+
  xlim(-1,12.5)




counts <- ggplot(data=summarized.mortality,aes(x=mortality, y = frequency, width=0.8, color = obs.inf,
                                                              fill = obs.inf))+
  geom_bar(stat = 'identity')+
  theme_classic(base_size = 14)+
  xlab("Source of Mortality")+
  ylab("Count")+
  scale_fill_manual(values = c('gray85', 'grey30'))+
  scale_color_manual(values = c('gray85', 'grey30'))+
  theme(legend.position = c(0.7,0.95),
        legend.title = element_blank(),
        axis.text.y = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        plot.margin = unit(c(4,0,6,0), units = 'pt'))+
  geom_errorbar(data = post.ci, aes(x = mortality, ymin = low, ymax = high), inherit.aes = F, width = 0.2) + 
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
################################################################################
