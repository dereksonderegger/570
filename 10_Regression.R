## ----echo=FALSE----------------------------------------------------------
if(file.exists('Global_Knitr_Opts.R')){
  source('Global_Knitr_Opts.R')
}
opts_chunk$set(fig.path   ='figure/Ch_10_regression/') 
opts_chunk$set(cache.path = 'cache/Ch_10_regression/')
opts_chunk$set(cache=TRUE)

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

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
  }else{
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

## ----message=FALSE, warning=FALSE----------------------------------------
library(ggplot2)
library(dplyr)
library(ggfortify)  # for diagnostic plots in ggplot2 via autoplot()

## ----echo=FALSE, fig.height=3, fig.width=5-------------------------------
n <- 20
data <- data.frame( x = runif(n, 0, 1) )
data <- mutate(data, y= 2 + x + rnorm(n, sd=.4))

ggplot(data, aes(x=x, y=y)) + geom_point() + geom_smooth(method='lm')

## ----echo=FALSE, fig.height=3--------------------------------------------
xbar <- mean(data$x ) 
ybar <- mean(data$y )
tile.data <- data.frame(x=c(xbar-1, xbar-1, xbar+1, xbar+1),
                        y=c(ybar-1, ybar+1, ybar+1, ybar-1),
                        sign=c('Positive','Negative','Positive','Negative'))

ggplot(data, aes(x=x, y=y)) +
  geom_tile(data=tile.data, aes(fill=sign), alpha=.5) +
  geom_point() +
  coord_cartesian(xlim=range(data$x),
                  ylim=range(data$y)) +
  scale_x_continuous(breaks=xbar, label=expression(bar(x))) +
  scale_y_continuous(breaks=ybar, label=expression(bar(y))) +
  labs(x=NULL, y=NULL)     

## ----echo=FALSE, fig.height=6, fig.width=8-------------------------------
x <- seq(0,1, length=n) 
lots.data <- rbind(
  data.frame(x=x, y=2 + 2*x + rnorm(n, sd=.5), grp=1),
  data.frame(x=x, y=2 - 2*x + rnorm(n, sd=.5), grp=2),
  data.frame(x=x, y=2 + 2*x + rnorm(n, sd=.1), grp=3),
  data.frame(x=x, y=2 + 2*x + rnorm(n, sd=5), grp=4),
  data.frame(x=x, y=2 - 2*x + rnorm(n, sd=2), grp=5),
  data.frame(x=x, y=2*(2*x-1)^2 + rnorm(n, sd=.2), grp=6))

R.values <-    lots.data %>% dplyr::group_by(grp) %>% dplyr::summarise(R=cor(x,y))
Plots <- list() 
for(i in 1:6){
  Plots[[i]] <- ggplot(lots.data %>% filter(grp==i), aes(x=x, y=y)) +
     geom_point() +
    ggtitle(paste('r = ', round(R.values[i,2], digits=3))) +
    labs(x=NULL, y=NULL) +
    scale_x_continuous(labels=NULL) +
    scale_y_continuous(labels=NULL) 
}  
multiplot(Plots[[1]], Plots[[2]], Plots[[3]], Plots[[4]], Plots[[5]], Plots[[6]],
          cols=3) 

## ----echo=FALSE, fig.height=4, fig.width=8-------------------------------
plot(c(0,1), c(0,1.25), type='n', xlab='x', ylab='y', axes=FALSE);
box(); axis(1);
abline(0,1);
y <- seq(-3,3,length=1000);
x <- dnorm(y, sd=1);
x <- x/4;
y <- y/12;
for(i in c(.3,.7)){
	lines(c(i,i), c(min(y), max(y))+i);
	polygon(x+i, y+i, col='grey');
	lines(c(i,i+max(x)), c(i,i));
	text(i,i+.4, expression(paste("", E(Y)==beta[0]+beta[1], x, sep='')));
}

## ------------------------------------------------------------------------
simple.data <- lm( y ~ 1, data)
simple.data <- fortify(simple)

## ----message=FALSE, fig.height=4, fig.width=8----------------------------
library(ggplot2)
library(dplyr)
setosa <- iris %>% filter( Species == 'setosa' )
ggplot(setosa, aes(x=Sepal.Length, y=Sepal.Width)) +
  geom_point() +
  labs(x="Sepal Length", y="Sepal Width", title='Setosa Irises') 

## ------------------------------------------------------------------------
x <- setosa$Sepal.Length
y <- setosa$Sepal.Width
n <- length(x)
r <- sum( (x-mean(x))/sd(x) * (y-mean(y))/sd(y) ) / (n-1)
b1 <- r*sd(y)/sd(x)
b0 <- mean(y) - b1*mean(x)
cbind(r, b0, b1)
yhat <- b0 + b1*x
resid <- y - yhat
SSE <- sum( resid^2 )
s2 <- SSE/(n-2)
s2
Sxx <- sum( (x-mean(x))^2 )
stderr.b0 <- sqrt(s2) * sqrt( 1/n + mean(x)^2 / Sxx)
stderr.b1 <- sqrt(s2) * sqrt(1 / Sxx )
cbind(stderr.b0, stderr.b1)
t.star <- qt(.975, df=n-2)	
c(b0-t.star*stderr.b0, b0+t.star*stderr.b0)
c(b1-t.star*stderr.b1, b1+t.star*stderr.b1)

## ------------------------------------------------------------------------
cor( setosa$Sepal.Width,  setosa$Sepal.Length )
model <- lm(Sepal.Width ~ Sepal.Length, data=setosa)
coef(model)
confint(model)

## ------------------------------------------------------------------------
model <- lm(Sepal.Width ~ Sepal.Length, data=setosa)
summary(model)

## ------------------------------------------------------------------------
model <- lm(Sepal.Width ~ Sepal.Length, data=setosa)
anova(model)

## ----echo=FALSE, fig.height=4.5, fig.width=8, message=FALSE, warning=FALSE----
library(ggplot2)
set.seed(321)
n <- 10 
x <- seq(0,1,length=n) 
y <- 10 + 3*x + rnorm(n, sd=1) 
data <- data.frame(x=x, y=y, n=paste('n =', n)) 
n <- 100 
x <- seq(0,1,length=n) 
y <- 10 + 3*x + rnorm(n, sd=1) 
data <- rbind( data, data.frame(x=x,y=y,n=paste('n =', n))) 
data$n <- factor(data$n)
model <- lm(y~x*n, data=data) 
data$y.hat <- fitted(model) 
CI <- predict(model, interval='confidence') 
PI <- predict(model, interval='prediction') 
data$conf.lwr <- CI[,2]
data$conf.upr <- CI[,3] 
data$pred.lwr <- PI[,2]
data$pred.upr <- PI[,3] 
ggplot(data, aes(x=x)) +   
  geom_point(aes(y=y)) +
  geom_line(aes(y=y.hat)) +
  geom_ribbon(aes(ymin=conf.lwr, ymax=conf.upr), fill='red', alpha=.2) +
  geom_ribbon(aes(ymin=pred.lwr, ymax=pred.upr), fill='blue', alpha=.2) +   
  facet_grid(. ~ n) + theme_bw()

## ----fig.height=3, fig.width=5-------------------------------------------
# make up some data and graph it
n <- 40
sim.data <- data.frame( x = seq(0,1, length=n) ) %>%
  mutate( y = 2 - 1*x + rnorm(n, sd=.2) )

ggplot(sim.data, aes(x=x, y=y)) + geom_point() + theme_bw()

## ------------------------------------------------------------------------
# fit the regression
model <- lm(y~x, data=sim.data)

# display the first few predictions
head( predict(model, interval="confidence") )

# predict at x = 0.75
predict(model, interval="prediction", newdata=data.frame(x=0.75))

## ----echo=TRUE, fig.height=3, fig.width=5, message=FALSE, warning=FALSE----
# ask for the confidence and prediction intervals
conf.region <- predict(model, interval='confidence')
pred.region <- predict(model, interval='prediction')

# add them to my original data frame
sim.data <- sim.data %>%
  mutate( fit = fitted(model),
          conf.lwr = conf.region[,2],
          conf.upr = conf.region[,3],
          pred.lwr = pred.region[,2],
          pred.upr = pred.region[,3])

## ----echo=TRUE, fig.height=3, fig.width=5, message=FALSE, warning=FALSE----
# make a nice plot
ggplot(sim.data) +
  geom_point( aes(x=x, y=y) ) +
  geom_line(  aes(x=x, y=fit), col='red' ) +
  geom_ribbon( aes(x=x, ymin=conf.lwr, ymax=conf.upr), fill='red',  alpha=.4) +
  geom_ribbon( aes(x=x, ymin=pred.lwr, ymax=pred.upr), fill='blue', alpha=.4) +
  theme_bw()

## ----echo=TRUE, fig.height=4, fig.width=8, warning=FALSE, message=FALSE----
library(HSAUR2)
data(men1500m)
small <- men1500m %>% filter( year != 1896 )  # Remove the 1896 Olympics

# fit the model and get the prediction interval
model <- lm( time ~ year, data=small )
small <- cbind(small, predict(model, interval='prediction') )

ggplot(small, aes(x=year, y=time, ymin=lwr, ymax=upr)) +
  geom_point() +
  geom_line( aes(y=fit), col='red' ) +
  geom_ribbon( fill='red',  alpha=.4) + 
  labs( x='Year', y='Time (s)', title='Winning times of Mens 1500 m' ) + theme_bw()

## ------------------------------------------------------------------------
predict(model, 
        newdata=data.frame(year=c(2008, 2012)), 
        interval="prediction")

## ------------------------------------------------------------------------
predict(model, newdata=data.frame(year=c(3112)), interval="prediction")

## ----echo=FALSE, fig.height=4, fig.width=10------------------------------
set.seed(2233);
par(mfrow=c(1,3));
n <- 20;
x <- seq(0,1,length=n);
data <- data.frame(
  Fitted=c(x,x,x),
  Residual=c(rnorm(n,0,.25), rnorm(n,(2*x-1)^2-.375, .2), rnorm(n,0,x*.45)), 
  Type=factor(rep(1:3, each=n), labels=c('No Trend', 'Non-Linear', 'Non-Constant Variance') ));
for(i in 1:3){
  index <- 1:n + n*(i-1);
  plot(data$Fitted[index], data$Residual[index], 
	   xlab='Fitted', ylab='Residual', main=data$Type[index[1]] );
  abline(0,0, lty=2);
}

## ----fig.height=3.5, fig.width=5-----------------------------------------
data('women')
str(women)

# fit the regression line and add that info to my data frame
model <- lm(weight ~ height, data=women)
women <- women %>%
  mutate( fit   = fitted(model),
          resid = resid(model)  )

## ----fig.height=3.5, fig.width=5-----------------------------------------
ggplot(women) +
  geom_point(aes(x=height, y = weight )) +
  geom_line( aes(x=height, y = fit    )) +
  labs( x='Height', y='Weight', 
        title='Average Weight vs Height of US Women 30-39' ) +
  theme_bw()

## ----fig.height=3, fig.width=8-------------------------------------------
ggplot(women, aes( x=fit, y=resid )) +
  geom_point() +
  labs(title='Residuals vs Fitted', x='Fitted', y='Residual') + theme_bw()

## ----fig.height=3, fig.width=6-------------------------------------------
autoplot(model, which=c(1,2))

## ----echo=FALSE, fig.height=4, fig.width=8-------------------------------
par(mfrow=c(1,2))
x <- c( 9, 10, 11, 12)
y <- c(15, 17, 16, 18)
model <- lm(y ~ x)
model.1 <- lm(c(y,28) ~ c(x,2))
model.2 <- lm(c(y,28) ~ c(x,10.5))
plot(x,y, xlim=c(0,14), ylim=c(15,30))
points(2, 28, pch=2, col='red')
abline(coef(model))
abline(coef(model.1), col='red', lty=2)
plot(x,y, xlim=c(8,13), ylim=c(15,30))
points(10.5, 28, pch=2, col='red')
abline(coef(model))
abline(coef(model.2), col='red', lty=2)

## ----echo=FALSE, fig.height=5, fig.width=9-------------------------------
x <- seq(0, 10, length=100)
plot(c(0,10), c(-2,6), type='n', xlab='x', ylab='y')
lines(x, log(x), lwd=2, col=1, lty=1)
lines(x, sqrt(x), lwd=2, col=2, lty=2)
lines(x, 1/x, lwd=3, col=3, lty=3)
legend(1,6, legend=c('log(x)', 'sqrt(x)', '1/x'), lty=1:3, col=1:3)
abline(h=0)
abline(v=0)

## ----echo=FALSE, fig.height=4, fig.width=9-------------------------------
set.seed(-838)
par(mfrow=c(1,3))
n <- 40
x <- seq(1,30, length=n);
y <- 2 + 30*exp((30-x)/10) + rnorm(n, sd=20)
y <- abs(y)
plot(x,y); abline(coef(lm(y~x)));
plot(x, log(y)); abline(coef(lm(I(log(y))~x)));
plot(x^(1/3), y); abline(coef(lm(y~I(x^(1/3)))));
mydata <- data.frame(x=x, y=y)

## ----echo=FALSE, fig.height=4, fig.width=8-------------------------------
set.seed(-838)
par(mfrow=c(1,2))
n <- 40
x <- seq(0,30, length=n)
y <- 2+ x*exp(rnorm(n))
plot(x,y); abline(coef(lm(y~x)));
plot(x, log(y)); abline(coef(lm(I(log(y))~x)));
mydata <- data.frame(x=x, y=y)

## ----fig.height=5, fig.width=8-------------------------------------------
library(MASS)
str(mydata)
boxcox(y~x, data=mydata, plotit=TRUE)

