## ----echo=FALSE----------------------------------------------------------
if(file.exists('Global_Knitr_Opts.R')){
  source('Global_Knitr_Opts.R')
}
opts_chunk$set(fig.path   = 'figure/Ch_13_ContingencyTables/') 
opts_chunk$set(cache.path =  'cache/Ch_13_ContingencyTables/')

## ----warning=FALSE, message=FALSE----------------------------------------
library(dplyr)
library(tidyr)
library(ggplot2)
library(mosaic)
library(mosaicData)  # where the Mites data lives

## ------------------------------------------------------------------------
data(Mites)
str(Mites)

## ------------------------------------------------------------------------
# Using mosaic's tally function
mosaic::tally(outcome ~ treatment, data=Mites, # table of outcome by treatment
              format='count')                  # give the raw counts, not percentages

# Using dplyr and tidyr
Mites %>% group_by(outcome, treatment) %>%
  summarise(count = n()) %>%
  spread(treatment, count)

# A dataframe summarizing function in base R.
# The select command is to switch the order of the 
# columns so that the result matches the two above
table(Mites %>% select(outcome, treatment))  

## ------------------------------------------------------------------------
# function is chisq.test() and we need to tell it not to do the Yates continuity 
# correction and just calculate the test statistic as we've described 
chisq.test( table(Mites), correct=FALSE )   # do a Chi-sq test  

## ----warning=FALSE-------------------------------------------------------
# extract the X^2 test statistic from the output
X.sq <- chisq.test( table(Mites), correct=FALSE )$statistic # grab only the test statistic
X.sq

## ----warning=FALSE-------------------------------------------------------
Mites.star <- Mites %>% mutate(treatment = shuffle(treatment))
table(Mites.star)
chisq.test( table(Mites.star), correct=FALSE )$statistic # grab only the test statistic

## ----cache=TRUE, warning=FALSE, fig.height=2.5---------------------------
SamplingDist <- do(10000)*{
  Mites.star <- Mites %>% mutate(treatment = shuffle(treatment))
  chisq.test( table(Mites.star), correct=FALSE )$statistic 
}
ggplot( SamplingDist, aes(x=X.squared)) + geom_histogram()

## ------------------------------------------------------------------------
p.value <- SamplingDist %>% summarize( p.value = mean( X.squared >= X.sq ) )
p.value

## ----cache=TRUE----------------------------------------------------------
chisq.test( table(Mites), simulate.p.value=TRUE, B=10000 )

## ----cache=TRUE----------------------------------------------------------
Conceived <-  data.frame(
  Treatment=c(rep('Doxycyline',30), rep('Placebo',28)),
  Outcome=c(rep('Conceived',5), rep('Not Conceived',25),
            rep('Conceived',4), rep('Not Conceived',24)))

chisq.test( table(Conceived), simulate.p.value=TRUE, B=10000 )

