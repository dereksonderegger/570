## ----echo=FALSE----------------------------------------------------------
if(file.exists('Global_Knitr_Opts.R')){
  source('Global_Knitr_Opts.R')
}
opts_chunk$set(fig.path   = 'figure/Ch_3_Bootstrapping/') 
opts_chunk$set(cache.path =  'cache/Ch_3_Bootstrapping/')
opts_chunk$set(cache      = TRUE )

## ----echo=FALSE, message=FALSE, warning=FALSE----------------------------
library(mosaic)

# define the multiplot function
# Multiple plot function 
# 
# ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects) 
# - cols:   Number of columns in layout 
# - layout: A matrix specifying the layout. If present, 'cols' is ignored. 
# 
# If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE), 
# then plot 1 will go in the upper left, 2 will go in the upper right, and 
# 3 will go all the way across the bottom. 
#
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  numPlots = length(plots)
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

  if (numPlots==1) {
    print(plots[[1]])
 } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

# Sample data
data <- data.frame(
  x=rep(1:3, times=3), y=rep(1:3, each=3),
  shape=c('Sq','Sq','Di', 'Cir','Sq','Cir', 'Tri','Cir','Sq') )

data2 <- rbind(
       mutate(data, x=x+0, y=y+0),
       mutate(data, x=x+0, y=y+3),
       mutate(data, x=x+0, y=y+6),
       mutate(data, x=x+3, y=y+0),
       mutate(data, x=x+3, y=y+3),
       mutate(data, x=x+3, y=y+6),
       mutate(data, x=x+6, y=y+0),
       mutate(data, x=x+6, y=y+3),
       mutate(data, x=x+6, y=y+6))

library(ggdendro)
sample.plot <- ggplot(data=data) +
  geom_point(aes(x=x, y=y, shape=shape), size=20, fill='dark grey') +
  scale_shape_manual(values=c(21,23,22,24), guide=FALSE) +
  coord_cartesian(xlim = c(0.5, 3.5), c(0.5,3.5)) +
  theme_dendro() + ggtitle('Sample')

population.plot <- ggplot(data=data2) +
  geom_point(aes(x=x, y=y, shape=shape), size=8, fill='dark grey') +
  scale_shape_manual(values=c(21,23,22,24), guide=FALSE) +
  coord_cartesian(xlim = c(0.5, 10), c(0.5,10)) +
  theme_dendro() + ggtitle('Approximate Population')


## ----echo=FALSE, fig.width=7, fig.height=4-------------------------------
multiplot(sample.plot, population.plot, cols=2)

## ----echo=FALSE----------------------------------------------------------
set.seed(2)

## ------------------------------------------------------------------------
Testing.Data <- data.frame(
  name=c('Alison','Brandon','Chelsea','Derek','Elise'))
Testing.Data

## ------------------------------------------------------------------------
# Sample rows from the Testing Data (with replacement)
resample(Testing.Data)

## ----cache=TRUE----------------------------------------------------------
# as always, our first step is to load the mosaic package
library(mosaic)

# read the Lakes data set
Lakes <- read.csv('http://www.lock5stat.com/datasets/FloridaLakes.csv')

## ----cache=TRUE, fig.width=6, fig.height=3-------------------------------
# make a nice picture... dot plots are very similar to histograms
# but in this case, my y-axis doen't make any sense.
ggplot(Lakes, aes(x=AvgMercury)) +
  geom_dotplot()

## ----cache=TRUE----------------------------------------------------------
Lakes %>% summarise(xbar = mean( AvgMercury ))

## ----LakesMercuryExample, cache=TRUE, fig.height=4-----------------------
# create the sampling distribution of xbar
SamplingDist <- do(10000) * resample(Lakes) %>% summarise(xbar = mean(AvgMercury))

# what columns does the data frame "SamplingDist" have?
str(SamplingDist)

# show a histogram of the sampling distribution of xbar
ggplot(SamplingDist, aes(x=xbar)) +
  geom_histogram() + 
  ggtitle('Estimated Sampling distribution of xbar' )

## ----echo=FALSE, fig.height=3, fig.width=5-------------------------------
x <- seq(-3, 3, length=1000) 
plot(x, dnorm(x), type='l', lwd=2, axes=FALSE, xlab='', ylab='', 
     main=expression(paste('Sampling Distribution of ', bar(x) )) ) 
axis(1, c(0), labels=c(expression(mu)))

## ----echo=FALSE, fig.height=4.5, fig.width=5-----------------------------
set.seed(93457629)
x <- seq(-3, 3, length=1000) 
plot(x, dnorm(x), ylim=c(-.5,.4), type='l', lwd=2, axes=FALSE, xlab='', ylab='',
     main=expression(paste('Sampling Distribution of ', bar(x) )) ) 
axis(1, c(-1.96, 0, 1.96),       
	labels=c('L', expression(mu), 'U' ))
lines(c(-10, 10), c(0, 0)) 
lines( c(-1.96, -1.96), c(dnorm(-1.96), -10)) 
lines( c( 1.96,  1.96), c(dnorm( 1.96), -10))
x.small <- seq(-1.96, 1.96, length=1000) 
polygon( c(-1.96, x.small, 1.96), c(0, dnorm(x.small),0), col='grey')
text(0, .25, '95%') 
text(0, .15, 'of sample means') 
N <- 40
xbar <- rep(0, N)
for(i in 1:N){
  xbar[i] <- rnorm(1)
  if(abs(xbar[i])>1.96){
    points(xbar[i], -i/(2*N), col='red', pch=19)   
  }else{
    points(xbar[i], -i/(2*N), col='black', pch=19)
  } 
}

## ----echo=FALSE, fig.height=6, fig.width=5-------------------------------
x <- seq(-5, 5, length=1000)
plot(x, dnorm(x),       
  ylim=c(-2.5,.5), xlim=c(-5,5),      
  type='l', lwd=2, axes=FALSE, xlab='', ylab='', col='red')
lines(x,rep(0,length(x)), col='red')
text(0,-.08, expression(mu), col='red')
text(0,.5, expression(paste('Sampling distribution of ',bar(x))),col='red')
lines(c(0,0), c(-.2,-10), col='red' )
xbars <- c(.2, -1, 1.2) 
yoffsets <- 1:3 * -.75 
L <- xbars - 1.96 
U <- xbars + 1.96
for(i in 1:3){
 lines(x+xbars[i], yoffsets[i]+dnorm(x), lwd=2)
  lines(x, rep(yoffsets[i], length(x))) 
} 
text(2.2,-1.15, 'Estimated Sampling \n distribution of')
text(3.70, -1.23, expression(bar(x)))
points(xbars, yoffsets, pch=18) 
text(L[1], yoffsets[1]-.07, expression(L[1]))
text(L[2], yoffsets[2]-.07, expression(L[2]))
text(L[3], yoffsets[3]-.07, expression(L[3]))
text(U[1], yoffsets[1]-.07, expression(U[1]))
text(U[2], yoffsets[2]-.07, expression(U[2]))
text(U[3], yoffsets[3]-.07, expression(U[3]))

## ----echo=FALSE, fig.height=6, fig.width=7-------------------------------
x <- seq(-5, 5, length=1000) 
plot(x, dnorm(x), ylim=c(-.5,.4), type='l', lwd=2, axes=FALSE, xlab='', ylab='') 
axis(1, c(-1.96, 0, 1.96),       
	labels=c(expression(mu - a),                
			 expression(mu),               
			 expression(mu + a) ))
lines(c(-10, 10), c(0, 0)) 
lines( c(-1.96, -1.96), c(dnorm(-1.96), -10)) 
lines( c( 1.96,  1.96), c(dnorm( 1.96), -10))
x.small <- seq(-1.96, 1.96, length=1000) 
polygon( c(-1.96, x.small, 1.96), c(0, dnorm(x.small),0), col='grey')
text(0, .25, '95%') 
text(0, .15, 'of sample means') 
N <- 40 
for(i in 1:N){
  if(abs(xbar[i])>1.96){
    points(xbar[i], -i/(2*N), col='red', pch=19)
	lines( c(xbar[i]-1.96, xbar[i]+1.96), c(-i/(2*N), -i/(2*N)), col='red' )  
  }else{
    points(xbar[i], -i/(2*N), col='black', pch=19)
	lines( c(xbar[i]-1.96, xbar[i]+1.96), c(-i/(2*N), -i/(2*N)), col='black' )  
  } 
}

## ----LakesCI, cache=TRUE-------------------------------------------------
# create the sampling distribution of xbar
SamplingDist <- do(10000) * resample(Lakes)%>%summarise(xbar=mean(AvgMercury))

# show a histogram of the sampling distribution of xbar
ggplot(SamplingDist, aes(x=xbar)) +
  geom_histogram() +
  ggtitle('Estimated Sampling distribution of xbar')
# calculate the 95% confidence interval using middle 95% of xbars
quantile( SamplingDist$xbar, probs=c(.025, .975) )

## ------------------------------------------------------------------------
CarMPG <- data.frame( ID=1:5, mpg = c(31.8, 32.1, 32.5, 30.9, 31.3) )
CarMPG %>% summarise( xbar=mean(mpg) )

## ----CarMPG_Ex, cache=TRUE, fig.width=5, fig.height=2.5------------------
SamplingDist <- do(10000) * resample(CarMPG) %>% summarise(xbar=mean(mpg))
# show a histogram of the sampling distribution of xbar
ggplot(SamplingDist, aes(x=xbar)) +
  geom_histogram() +
  ggtitle('Estimated Sampling distribution of xbar')

## ----fig.width=5, fig.height=2.5-----------------------------------------
# calculate the 95% confidence interval using middle 95% of xbars
quantile( SamplingDist$xbar, probs=c(.025, .975) )

## ----fig.height=3--------------------------------------------------------
library(Lock5Data)  # load the package
data(GPAGender)     # from the package, load the dataset

# Now a nice histogram
ggplot(GPAGender, aes(x=Pulse, y=..density..)) +
  geom_histogram(binwidth=2) +
  ggtitle('Sample Data')

## ------------------------------------------------------------------------
# Summary Statistics
GPAGender %>% summarise(xbar = mean(Pulse),
                        StdDev = sd(Pulse))

## ----CollegePulses, cache=TRUE, fig.height=3-----------------------------
# Create the bootstrap replicates
SampDist <- do(10000) * {
  resample(GPAGender) %>% summarise(xbar = mean(Pulse))
}

ggplot(SampDist, aes(x=Pulse, y=..density..)) +
  geom_histogram(binwidth=1) +
  ggtitle('Sampling Distribution of Mean(Pulse)')

## ------------------------------------------------------------------------
quantile( SampDist$xbar )

## ------------------------------------------------------------------------
library(Lock5Data)
data( BodyTemp50 )
?BodyTemp50

