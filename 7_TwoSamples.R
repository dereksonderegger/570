## ----echo=FALSE----------------------------------------------------------
if(file.exists('Global_Knitr_Opts.R')){
  source('Global_Knitr_Opts.R')
}
opts_chunk$set(fig.path   = 'figure/Ch_7_TwoSamples/') 
opts_chunk$set(cache.path =  'cache/Ch_7_TwoSamples/')
opts_chunk$set(cache      =  FALSE)

## ----message=FALSE, warning=FALSE----------------------------------------
# Load all the libraries we'll need in this chapter
library(mosaic)    # for the do{}, shuffle(), resample() functions...
library(Lock5Data)
library(dplyr)
library(tidyr)     # spread() and gather()
library(ggplot2)

## ------------------------------------------------------------------------
data(CaffeineTaps)   # load the data from the Lock5Data package
str(CaffeineTaps)

## ----fig.height=2--------------------------------------------------------
ggplot(CaffeineTaps, aes(x=Taps)) +
  geom_histogram( binwidth=.2) + 
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

## ----Tapping_Permutation, cache=TRUE-------------------------------------
SamplingDist <- do(10000) * {
  CaffeineTaps %>%
  mutate( ShuffledGroup = shuffle(Group) ) %>%
  group_by( ShuffledGroup )  %>%
  summarise(xbar=mean(Taps)) %>%
  summarise(d.star = diff(xbar))  
}

## ----fig.height=2.5------------------------------------------------------
ggplot(SamplingDist, aes(x=d.star)) +
  geom_histogram(binwidth=.2) +
  ggtitle('Permutation dist. of d* assuming H0 is true') +
  xlab('d*')

## ------------------------------------------------------------------------
SamplingDist %>% 
  mutate( MoreExtreme = ifelse( abs(d.star) >= 3.5, 1, 0)) %>%
  summarise( p.value1 = sum(MoreExtreme)/n(),       # these are all the
             p.value2 = mean(MoreExtreme),          # same calculation
             p.value3 = mean( abs(d.star) >= 3.5 )) # but more verbose

## ------------------------------------------------------------------------
SamplingDist %>% 
  summarise( p.value = mean( d.star <= -3.5 ))

## ----Tapping_Bootstrap, cache=TRUE---------------------------------------
BootDist <- do(10000)*{
  resample(CaffeineTaps) %>%
  group_by(Group) %>%
  summarise( xbar=mean(Taps) ) %>%
  summarise( d.star = diff(xbar)  )
}

## ----fig.height=2.5------------------------------------------------------
ggplot(BootDist, aes(x=d.star)) +
  geom_histogram(binwidth=.2) +
  ggtitle('Bootstrap distribution of d*')

## ------------------------------------------------------------------------
CI <- quantile( BootDist$d.star, probs=c(0.025, 0.975) )
CI

## ------------------------------------------------------------------------
pt(-3.39, df=17.89)

## ------------------------------------------------------------------------
Caffeine    <- CaffeineTaps$Taps[ 1:10]  # first 10 are Caffeine
NonCaffeine <- CaffeineTaps$Taps[11:20]  # last 10 are non-Caffeine

# Do the t-test
t.test( NonCaffeine, Caffeine )

## ------------------------------------------------------------------------
CaffeineTaps %>% 
  group_by(Group) %>% 
  summarise(xbar.i = mean(Taps),   # sample mean for each group
            s2.i   = var(Taps),    # sample variances for each group
            s.i    = sd(Taps),     # sample standard deviations for each group
            n.i    = n()      )    # sample sizes for each group
CaffeineTaps %>%
  group_by(Group) %>%
  summarize(  n.i = n(),             
             s2.i = var(Taps) ) %>%     
  summarize( s2.p = sum( (n.i-1)*s2.i ) / ( sum(n.i)-2 ),
             s.p  = sqrt(s2.p) )  

## ------------------------------------------------------------------------
p.value <- 2 * pt(-3.39, df=18)   # 2-sided test, so multiply by 2
p.value      

## ------------------------------------------------------------------------
qt( .975, df=18 )    

## ------------------------------------------------------------------------
Caffeine    <- CaffeineTaps$Taps[ 1:10]  # first 10 are Caffeine
NonCaffeine <- CaffeineTaps$Taps[11:20]  # last 10 are non-Caffeine

# Do the t-test
t.test( NonCaffeine, Caffeine, var.equal=TRUE )

## ------------------------------------------------------------------------
# 99% confidence interval instead
t.test( NonCaffeine, Caffeine, var.equal=TRUE, conf.level=.99 )

## ----fig.height=3.5------------------------------------------------------
# I can't find this dataset on-line so I'll just type it in.
Mosquitoes <- data.frame(
  Number = c(27,19,20,20,23,17,21,24,31,26,28,20,27,
             19,25,31,24,28,24,29,21,21,18,27,20,
             21,19,13,22,15,22,15,22,20,
             12,24,24,21,19,18,16,23,20),
  Treat = c( rep('Beer', 25), rep('Water',18) ) )

# Plot the data
ggplot(Mosquitoes, aes(x=Number)) +
  geom_histogram(binwidth=1) +
  facet_grid( Treat ~ . )

## ------------------------------------------------------------------------
Mosquitoes %>% group_by(Treat) %>%
  summarise(xbar.i = mean(Number),
            s2.i   = var(Number),
            s.i    = sd(Number),
            n.i    = n())

## ----Mosquitos_Permute, cache=TRUE---------------------------------------
SamplingDist <- do(10000) *{
  Mosquitoes %>%
    mutate(ShuffledTreat = shuffle(Treat)) %>%
    group_by( ShuffledTreat )              %>% 
    summarise( xbar.i = mean(Number) )     %>%
    summarise( d.star = diff(xbar.i) )
}

## ----fig.height=2--------------------------------------------------------
ggplot(SamplingDist, aes(x=d.star)) +
  geom_histogram(binwidth=.5) +
  ggtitle('Sampling Dist. of d* assuming H0 is true')

## ----fig.height=2--------------------------------------------------------
p.value <- SamplingDist %>%
  summarise( mean( d.star <= -4.377 ))
p.value

## ----Mosquitos_Boot, cache=TRUE------------------------------------------
BootDist <- do(10000)*{
  resample(Mosquitoes)                  %>%
    group_by(Treat)                     %>%
    summarise( xbar.i = mean(Number) )  %>%
    summarise( d.star = diff(xbar.i) )    
}

## ----fig.height=2.5------------------------------------------------------
ggplot(BootDist, aes(x=d.star)) +
  geom_histogram(binwidth=.5) +
  ggtitle('Bootstrap dist. of d*')

## ------------------------------------------------------------------------
quantile( BootDist$d.star, probs=c(.05, .95))

## ------------------------------------------------------------------------
# the package mosaic augments the t.test() function to 
# input the data in a more convenient fashion.
t.test( Number ~ Treat, data=Mosquitoes, 
        var.equal=TRUE,
        conf.level=0.90)

