## ----echo=FALSE----------------------------------------------------------
if(file.exists('Global_Knitr_Opts.R')){
  source('Global_Knitr_Opts.R')
}
opts_chunk$set(fig.path   = 'figure/TwoSamples/') 
opts_chunk$set(cache.path =  'cache/TwoSamples/')
opts_chunk$set(cache      =  TRUE)

## ----message=FALSE, warning=FALSE----------------------------------------
# Load all the libraries we'll need in this chapter
library(Lock5Data)
library(dplyr)
library(ggplot2)

## ------------------------------------------------------------------------
data(CaffeineTaps)   # load the data from the Lock5Data package
str(CaffeineTaps)

## ----fig.height=2--------------------------------------------------------
ggplot(CaffeineTaps, aes(x=Taps)) +
  geom_dotplot() + 
  facet_grid( Group ~ . )  # two graphs stacked by Group (Caffeine vs non)

## ----fig.height=3--------------------------------------------------------
CaffeineTaps %>% 
  group_by(Group) %>%  # group the summary stats by Treatment group
  summarise(xbar=mean(Taps), s=sd(Taps))

## ----fig.height=3--------------------------------------------------------
# No Caffeine  -  Caffeine
244.8 - 248.3
CaffeineTaps %>% group_by(Group) %>%
  summarise(xbar=mean(Taps)) %>%
  summarise(d = diff(xbar))

## ----cache=TRUE----------------------------------------------------------
# shuffle(): takes an input column and reorders it randomly
CaffeineTaps %>% mutate(ShuffledGroup = shuffle(Group))

## ------------------------------------------------------------------------
CaffeineTaps %>%
  mutate( ShuffledGroup = shuffle(Group) ) %>%
  group_by( ShuffledGroup )  %>%
  summarise(xbar=mean(Taps)) %>%
  summarise(d.star = diff(xbar))  

## ------------------------------------------------------------------------
do(5) * {
  CaffeineTaps %>%
  mutate( ShuffledGroup = shuffle(Group) ) %>%
  group_by( ShuffledGroup )  %>%
  summarise(xbar=mean(Taps)) %>%
  summarise(d.star = diff(xbar))  
}


## ----echo=FALSE----------------------------------------------------------
set.seed(456)

## ----cache=TRUE----------------------------------------------------------
SamplingDist <- do(10000) * {
  CaffeineTaps %>%
  mutate( ShuffledGroup = shuffle(Group) ) %>%
  group_by( ShuffledGroup )  %>%
  summarise(xbar=mean(Taps)) %>%
  summarise(d.star = diff(xbar))  
}
ggplot(SamplingDist, aes(x=d.star)) +
  geom_histogram(binwidth=.2) +
  ggtitle('Est. Sampling dist. of d if H0 is true') +
  xlab('d*')

## ----cache=TRUE----------------------------------------------------------
SamplingDist %>% 
  mutate( MoreExtreme = ifelse( abs(d.star) >= 3.5, 1, 0)) %>%
  summarise( p.value1 = sum(MoreExtreme)/n(),       # these are all the
             p.value2 = mean(MoreExtreme),          # same calculation
             p.value3 = mean( abs(d.star) >= 3.5 )) # but more verbose

## ----cache=TRUE----------------------------------------------------------
SamplingDist %>% 
  summarise( p.value = mean( d.star >= 3.5 ))

## ----fig.height=3.5------------------------------------------------------
# I can't find this dataset on-line so I'll just type it in.
Mosquitoes <- data.frame(
  Number = c(27,19,20,20,23,17,21,24,31,26,28,20,27,
             19,25,31,24,28,24,29,21,21,18,27,20,
             21,19,13,22,15,22,15,22,20,
             12,24,24,21,19,18,16,23,20),
  Group = c( rep('Beer', 25), rep('Water',18) ) )

# Plot the data
dotPlot(~ Number | Group, data=Mosquitoes, layout=c(1,2) )

## ------------------------------------------------------------------------
# Observed difference in number of mosquitoes 
d <- diffmean(Number ~ Group, data=Mosquitoes)
d

## ----cache=TRUE----------------------------------------------------------
SamplingDist <- do(10000) * diffmean(Number ~ shuffle(Group), data=Mosquitoes)
histogram( ~ diffmean, data=SamplingDist, groups=( abs(diffmean) >= abs(d) ),
           main='Sampling distribution of d*', xlab='d*')

## ----cache=TRUE----------------------------------------------------------
p.value <- tally( ~ (abs(diffmean) >= abs(d)), 
                  data=SamplingDist, 
                  format='proportion'        ) 
p.value

## ----cache=TRUE----------------------------------------------------------
# If we wanted to read an excel file...
# library(gdata)
# SleepCaffeine <- read.xls('http://www.lock5stat.com/datasets/SleepCaffeine.xls')

# Just using the standard .csv file...
SleepCaffeine <- read.csv('http://www.lock5stat.com/datasets/SleepCaffeine.csv') 

## ----cache=TRUE----------------------------------------------------------
dotPlot( ~ Words | Group, data=SleepCaffeine, layout=c(1,2))

## ----cache=TRUE----------------------------------------------------------
d <- diffmean(Words ~ Group, data=SleepCaffeine)
d

## ----cache=TRUE----------------------------------------------------------
SamplingDist <- do(10000) * diffmean(Words ~ shuffle(Group), data=SleepCaffeine)
histogram( ~ diffmean, data=SamplingDist, groups=( abs(diffmean) >= abs(d) ),
           main='Sampling distribution of d*', xlab='d*')

## ----cache=TRUE----------------------------------------------------------
p.value <- tally( ~ (abs(diffmean) >= abs(d)), 
                  data=SamplingDist, 
                  format='proportion'         )
p.value

## ----cache=TRUE----------------------------------------------------------
library(mosaicData)  # where the data lives
data(Mites)
str(Mites)

## ----cache=TRUE----------------------------------------------------------
tally(outcome ~ treatment, data=Mites, # table of outcome by treatment
     format='count'                    # give the raw counts, not percentages
)

## ----cache=TRUE----------------------------------------------------------
# function is chisq.test() and we need to tell it not to do the Yates continuity 
# correction and just calculate the test statistic as we've described 
chisq.test(                                                # do a Chi-sq test
  tally(outcome ~ treatment, data=Mites, format='count'),  # on this table
  correct=FALSE                                            # Don't do the correction
)

## ----cache=TRUE, warning=FALSE-------------------------------------------
# extract the X^2 test statistic from the output
Xsq <- chisq.test(                                         # do a Chi-sq test
  tally(outcome ~ treatment, data=Mites, format='count'),  # on this table
  correct=FALSE                                            # Don't do the correction
)$statistic                                                # grab only the test statistic

Xsq

## ----cache=TRUE, warning=FALSE-------------------------------------------
SamplingDist <- do(3)*
  chisq.test( 
    tally(outcome ~ shuffle(treatment), data=Mites, format='count'),
    correct=FALSE 
  )$statistic

SamplingDist

## ----cache=TRUE, warning=FALSE-------------------------------------------
SamplingDist <- do(10000)*
  chisq.test( 
    tally(outcome ~ shuffle(treatment), data=Mites, format='count'),
    correct=FALSE 
  )$statistic

histogram( ~ X.squared, data=SamplingDist, groups=( X.squared >= Xsq ),
           main='Sampling distribution of Xsq*', xlab='Xsq*')

## ----cache=TRUE----------------------------------------------------------
p.value <- tally( ~ (X.squared >= Xsq), data=SamplingDist, format='proportion' )
p.value

## ----cache=TRUE----------------------------------------------------------
Conceived <-  data.frame(
  CoupleID=1:58,
  Treatment=c(rep('Doxycyline',30), rep('Placebo',28)),
  Outcome=c(rep('Conceived',5), rep('Not Conceived',25),
            rep('Conceived',4), rep('Not Conceived',24)))

chisq.test( 
  tally(Outcome ~ Treatment, data=Conceived, format='count'), 
  correct=FALSE )

## ----cache=TRUE, warning=FALSE-------------------------------------------
Xsq <- chisq.test(                    
  tally(Outcome ~ Treatment, data=Conceived, format='count'),
  correct=FALSE 
)$statistic

Xsq

## ----cache=TRUE, warning=FALSE-------------------------------------------
SamplingDist <- do(10000)*
  chisq.test( 
    tally(Outcome ~ shuffle(Treatment), data=Conceived, format='count'),
    correct=FALSE 
  )$statistic

histogram( ~ X.squared, data=SamplingDist, groups=( X.squared >= Xsq ),
           main='Sampling distribution of Xsq*', xlab='Xsq*')

## ----cache=TRUE----------------------------------------------------------
p.value <- tally( ~ (X.squared >= Xsq), data=SamplingDist, format='proportion' )
p.value

