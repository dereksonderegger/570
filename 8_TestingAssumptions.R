## ----echo=FALSE----------------------------------------------------------
if(file.exists('Global_Knitr_Opts.R')){
  source('Global_Knitr_Opts.R')
}
opts_chunk$set(fig.path   ='figure/Ch_8_TestingAssumptions/') 
opts_chunk$set(cache.path = 'cache/Ch_8_TestingAssumptions/')
opts_chunk$set(cache=TRUE)

## ----echo=FALSE, fig.height=3, fig.width=10------------------------------
x <- seq(-3,3,length=1000) 
labels <- NULL 
labels[1] <- expression(paste(z[.05])) 
labels[2] <- expression(paste(z[.15])) 
labels[3] <- expression(paste(z[.25])) 
labels[4] <- expression(paste(z[.35])) 
labels[5] <- expression(paste(z[.45])) 
labels[6] <- expression(paste(z[.55])) 
labels[7] <- expression(paste(z[.65])) 
labels[8] <- expression(paste(z[.75])) 
labels[9] <- expression(paste(z[.85])) 
labels[10]<- expression(paste(z[.95])) 
plot(x, dnorm(x), axes=FALSE, type='l', lwd=2, ylab='', xlab=''); 
box(); axis(1, at=qnorm((1:10-.5)/10), lab=labels); 
lines(c(0,0), c(0,dnorm(0)))

## ----eval=FALSE, fig.height=2.5, fig.width=4-----------------------------
## n <- 10
## data <- data.frame( observed    = sort( rnorm(n, mean=0, sd=1) ),
##                     theoretical = qnorm( (1:n -.5)/n, mean=0, sd=1 ) )
## library(ggplot2)
## ggplot(data) +
##   geom_point( aes(x=theoretical, y=observed) ) +
##   geom_line(  aes(x=theoretical, y=theoretical) ) +
##   labs(x='Theoretical', y='Observed', title="Q-Q Plot: Observed vs Normal Distribution')

## ----echo=FALSE, fig.height=2.5, fig.width=4-----------------------------
set.seed(6276)
n <- 10
data <- data.frame( observed    = sort( rnorm(n, mean=0, sd=1) ),
                    theoretical = qnorm( (1:n -.5)/n, mean=0, sd=1 ) )
library(ggplot2)
ggplot(data) +
  geom_point( aes(x=theoretical, y=observed) ) +
  geom_line(  aes(x=theoretical, y=theoretical) ) +
  labs(x='Theoretical', y='Observed', title='Q-Q Plot: Observed vs Normal Distribution')

## ----echo=TRUE, fig.height=3.5, fig.width=5------------------------------
par(mfrow=c(1,2))
n <- 10
x <- rnorm(n, mean=100, sd=10)
hist(x)
qqnorm(x)
qqline(x)



## ----echo=FALSE, fig.height=12, fig.width=8------------------------------
par(mfrow=c(5,2))
n <- 40
N <- 1000

x <- seq(0, 10, length=N)
plot( x, dexp(x, rate=1/5), ylab='Density', main='Exponential Distribution', type='l', lwd=2)
y <- rexp(n, rate=1/5)
qqnorm(y, ylim=c(-5,25)); qqline(y)

x <- seq(0,60, length=N)
plot(x, dgamma(x, shape=5, rate=1/5), ylab='Density', main='Gamma Distribution', type='l', lwd=2)
y <- rgamma(n, shape=5, rate=1/5)
qqnorm(y, ylim=c(0, 80)); qqline(y);

x <- seq(-0.2,1.2, length=N)
plot(x, dunif(x), ylab='Density', main='Uniform Distribution', type='l', lwd=2)
y <- runif(n)
qqnorm(y, ylim=c(-.4,1.5)); qqline(y);

x <- seq(-3,3, length=N)
plot(x, dt(x, df=2), ylab='Density', main='T(df=2) Distribution', type='l', lwd=2)
y <- rt(n, df=2)
qqnorm(y); qqline(y);

x <- seq(0,40, length=N)
plot(x, dlnorm(x, meanlog=2, sdlog=1), ylab='Density', main='LogNormal Distribution', type='l', lwd=2)
y <- rlnorm(n, meanlog=2, sdlog=1)
qqnorm(y, ylim=c(-10, 60)); qqline(y);

## ------------------------------------------------------------------------
x <- rlnorm(10, meanlog=2, sdlog=2)
shapiro.test(x)

## ------------------------------------------------------------------------
x <- rgamma(10, shape=5, rate=1/5)
shapiro.test(x)

## ----message=FALSE, warning=FALSE----------------------------------------
# libraries I'll need for the rest of the chapter
library(mosaic)
library(dplyr)
library(ggplot2)

## ----fig.height=3--------------------------------------------------------
Alcohol <- data.frame(
  time=c( 0.90, 0.37, 1.63, 0.83, 0.95, 0.78, 0.86, 0.61, 0.38, 1.97,
          1.46, 1.45, 1.76, 1.44, 1.11, 3.07, 0.98, 1.27, 2.56, 1.32 ),
  trt = rep(c("control","alcohol"),each=10)) 
ggplot(Alcohol, aes(x=trt, y=time)) +
  geom_boxplot()

## ----fig.height=4, fig.width=6, echo=FALSE-------------------------------
x <- seq(0, 5, length=1000)
curve1 <- data.frame(x=x, y=df(x,  5,5),  group='F[list(5,5)]')
curve2 <- data.frame(x=x, y=df(x, 20,20), group='F[list(20,20)]')
curves <- rbind(curve1, curve2) 
ggplot(curves, aes(x=x, y=y)) +
  geom_line() +
  facet_grid(group~., labeller=label_parsed) 

## ------------------------------------------------------------------------
qf(0.025, 6, 9)
qf(0.975, 6, 9)

## ------------------------------------------------------------------------
2*pf(0.391, 6, 9)

## ------------------------------------------------------------------------
1 - pf(6/3, 4, 19)
pf(3/6, 19, 4)

## ----echo=FALSE, fig.height=3, fig.width=6-------------------------------
x <- seq(0, 5, length=1000)

curve1 <- data.frame(x=x, y=df(x,  4,19), group='F[list(4,19)]')
curve2 <- data.frame(x=x, y=df(x, 19, 4), group='F[list(19,4)]')
curves <- rbind(curve1, curve2)

x2 <- x[ which(x>6/3) ]
poly1 <- data.frame(x=c(6/3,x2), y=c(0,df(x2, 4,19)), group='F[list(4,19)]')
x2 <- x[ which(x<3/6) ]
poly2 <- data.frame(x=c(x2,3/6), y=c(df(x2, 19,4),0), group='F[list(19,4)]')
polys <- rbind(poly1, poly2) 

text <- data.frame(
     labels = c('P( F[list(4,19)] > 2)', 'P( F[list(19,4)] < 1/2 )'),
     group = c('F[list(4,19)]', 'F[list(19,4)]'),
     x=c(4,4), y=c(0.4, 0.4))
ggplot(curves, aes(x=x, y=y)) +
  geom_line() +
  facet_grid(group~., labeller=label_parsed) +
  geom_polygon(data=polys, fill='red') +
  geom_text(data=text, aes(label=labels), parse=TRUE, size=8) +
  xlab('') + ylab('density')

## ----echo=FALSE----------------------------------------------------------
set.seed(535)

## ------------------------------------------------------------------------
sigma1 <- 1
sigma2 <- 2
n1 <- 10
n2 <- 10
v1 <- var(rnorm(n1, mean=0, sd=sigma1))
v2 <- var(rnorm(n2, mean=0, sd=sigma2))
f <- v1/v2
if( f < 1 ){
  p.value <- 2 *      pf( f, df1 = n1-1, df2 = n2-1 )
}else{
  p.value <- 2 * (1 - pf( f, df1 = n1-1, df2 = n2-1))
}
p.value

## ------------------------------------------------------------------------
sigma1 <- 1
sigma2 <- 2
n1 <- 30
n2 <- 30
v1 <- var(rnorm(n1, mean=0, sd=sigma1))
v2 <- var(rnorm(n2, mean=0, sd=sigma2))
f <- v1/v2
if( f < 1 ){
  p.value <- 2 *      pf( f, df1 = n1-1, df2 = n2-1 )
}else{
  p.value <- 2 * (1 - pf( f, df1 = n1-1, df2 = n2-1))
}
p.value

## ----message=FALSE-------------------------------------------------------
# Calculating everything by hand
F <- Alcohol %>%
  group_by(trt) %>%                # for each trt group,
  summarise( s2 = var(time)) %>%   # calculate variance.
  summarise( F = s2[1] / s2[2] )   # and then take the ratio
F

## ----message=FALSE-------------------------------------------------------
obs.F <- as.numeric( F )           # Convert 1-by-1 data frame to simple number
pvalue <- 2* (1-pf( obs.F, 9,9 ))
pvalue

## ----message=FALSE-------------------------------------------------------
# Using Rs built in function
var.test( time ~ trt, data=Alcohol )

## ----Alcohol_Permute, cache=TRUE, message=FALSE, fig.keep='last'---------
# Permutation distribution of Observed F-statistic assuming H0 is true. 
SampDist <- do(5000) * 
  var.test(time ~ shuffle(trt), data=Alcohol)$statistic

# Figure which parts of the distribution are more extreme than my observed F 
SampDist <- SampDist %>%   
  mutate( extreme = F > obs.F | F < 1/obs.F )

## ----fig.height=3--------------------------------------------------------
# Make a histogram of the permutation distribution along with the theoretical
ggplot(SampDist, aes(x=F, y=..density..)) +
  geom_histogram(binwidth=.25) +
  geom_area( data=data.frame(x=seq(0,10,length=1000)) %>%
                             mutate(y=df(x, 9,9)),
             aes(x=x, y=y), alpha=.3, fill='red')

## ------------------------------------------------------------------------
# p-value... what percent is more extreme than what I observed?
SampDist %>% summarise(p.value = mean(extreme))

## ------------------------------------------------------------------------
Commute <- data.frame(
  time = c(21.0, 22.1, 19.3, 22.4, 19.6, 19.8,
           19.6, 20.4, 21.1, 19.7, 19.9, 20.0,
           25.0, 27.8, 25.2, 25.1, 25.4, 25.9,
           30.3, 29.5, 25.1, 26.4, 24.4, 27.7,
           25.8, 27.1),
  type = c( rep('Morning',12), rep('Evening',14)))

## ----fig.height=3--------------------------------------------------------
ggplot(Commute, aes(x=type, y=time)) + 
  geom_boxplot() +
  labs(title='Commute Times', y='Time (minutes)', x='Time of Day') +
  theme_bw()

## ----message=FALSE-------------------------------------------------------
var.test( time ~ type, data=Commute )

## ----Commute_Permute, cache=TRUE, message=FALSE--------------------------
# obs.F = 3.04
obs.F <- var.test(time ~ type, data=Commute)$statistic

# create the permutation distribution of F-values 
SampDist <- do(5000) * 
  var.test(time ~ shuffle(type), data=Commute)$statistic

# Figure which parts of the distribution are more extreme than my observed F 
SampDist <- SampDist %>%   
  mutate( extreme = F > obs.F | F < 1/obs.F )  # F > 3.04 or F < 1/3.04

## ----message=FALSE, fig.height=3-----------------------------------------
# Make a histogram of the permutation distribution and theoretical
ggplot(SampDist, aes(x=F, y=..density..)) +
  geom_histogram(binwidth=.1) +
  geom_area( data=data.frame(x=seq(0,10,length=1000)) %>%
                             mutate(y=df(x, 13, 11)),
             aes(x=x, y=y), alpha=.3, fill='red')


## ----message=FALSE, fig.keep='last'--------------------------------------
# p-value... what proportion is more extreme than what I observed?
SampDist %>% summarise(p.value = mean(extreme))

## ----echo=TRUE-----------------------------------------------------------
pf(4.2, df1=2, df2=10)

## ----echo=TRUE, eval=FALSE, tidy=FALSE-----------------------------------
## 
## par(mfrow=c(1,2)) # 1 row of 2 graphs, side-by-side
## n <- 5                       # sample size is 5
## x <- rnorm(n, mean=25, sd=5) # draw random sample
## hist(x)                      # histogram
## qqnorm(x)                    # qqplot for normality
## qqline(x)                    # add a line to the above plot
## shapiro.test(x)              # do the test for normality

## ----echo=TRUE, eval=FALSE, tidy=FALSE-----------------------------------
## 
## par(mfrow=c(1,2)) # 1 row of 2 graphs, side-by-side
## n <- 5                          # sample size is 5
## x <- rgamma(n, shape=3, rate=2) # draw random sample
## hist(x)                         # histogram
## qqnorm(x)                       # qqplot for normality
## qqline(x)                       # add a line to the above plot
## shapiro.test(x)                 # do the test for normality

## ----echo=TRUE, eval=FALSE, tidy=FALSE-----------------------------------
## 
## par(mfrow=c(1,1)) # 1 row of 1: Just one graph
## n <- 5
## sigma <- c(2,2)
## my.data <- data.frame(y = c(rnorm( n, mean=0, sd=sigma[1] ),
##                             rnorm( n, mean=0, sd=sigma[2] )),
##                       group = c( rep('g1',n), rep('g2',n)  ))
## boxplot(y ~ group, data=my.data)
## var.test(y ~ group, data=my.data)

