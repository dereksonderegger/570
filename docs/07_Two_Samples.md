# Two-Sample Hypothesis Tests and Confidence Intervals




```r
library(ggplot2)
library(dplyr)
library(tidyr)
library(boot)

# Set default behavior of ggplot2 graphs to be black/white theme
theme_set(theme_bw())
```

There are two broad classification types for research, observational studies and designed experiments. These two types of research differ in the way that the researcher interacts with the subjects being observed. In an observational study, the researcher doesn't force a subject into some behavior or treatment, but merely observes the subject (making measurements but not changing behaviors). In contrast, in an experiment, the researcher imposes different treatments onto the subjects and the pairing between the subject and treatment group happens at random.

**Example:** For many years hormone (Estrogen and Progestin) replacement therapy's primary use for post-menopausal woman was to reduce the uncomfortable side-effects of menopause but it was thought to also reduced the rate of breast cancer in post-menopausal women. This belief was the result of many observational studies where women who chose to take hormone replacement therapy also had reduced rates of breast cancer. The lurking variable that the observational studies missed was that hormone therapy is relatively expensive and was taken by predominately women of a high socio- economic status. Those women tended to be more health conscious, lived in areas with less pollution, and were generally at a lower risk for developing breast cancer. Even when researchers realized that socio-economic status was confounded with the therapy, they couldn't be sure which was the cause of the reduced breast cancer rates. Two variables are said to be confounded if the design of a given experiment or study cannot distinguish the effect of one variable from the other. To correctly test this, nearly 17,000 women underwent an experiment in which each women was randomly assigned to take either the treatment (E+P) or a placebo. The Women’s Health Initiative (WHI) Estrogen plus Progestin Study (E+P) was stopped on July 7, 2002 (after an average 5.6 years of follow-up) because of increased risks of cardiovascular disease and breast cancer in women taking active study pills, compared with those on placebo (inactive pills). The study showed that the overall risks exceeded the benefits, with women taking E+P at higher risk for heart disease, blood clots, stroke, and breast cancer, but at lower risk for fracture and colon cancer. Lurking variables such as income levels and education are correlated to overall health behaviors and with an increased use of hormone replacement therapy. By randomly assigning each woman to a treatment, the unidentified lurking variables were evenly spread across treatments and the dangers of hormone replacement therapy were revealed.

In the previous paragraph, we introduced the idea of a **lurking variable** where a lurking variable is a variable the researcher hasn't considered but affects the response variable. In observational studies a researcher will try to measure all the variables that might affect the response but will undoubtedly miss something.


There is a fundamental difference between imposing treatments onto subjects versus taking a random sample from a population and observing relationships between variables. In general, designed experiments allow us to determine cause-and-effect relationships while observational studies can only determine if variables are correlated. This difference in how the data is generated will result in different methods for generating a sampling distribution for a statistic of interest. In this chapter we will focus on experimental designs, though the same analyses are appropriate for observational studies.

## Difference in means between two groups

Often researchers will obtain a group of subjects and divide them into two groups, provide different treatments to each, and observe some response. The goal is to see if the two groups have different mean values, as this is the most common difference to be interested in.

The first thing to consider is that the group of subjects in our sample should be representative of a population of interest. Because we cannot impose an experiment on an entire population, we often are forced to examine a small sample and we hope that the sample statistics (the sample mean $\bar{x}$, and sample standard deviation $s$) are good estimates of the population parameters (the population mean $\mu$, and population standard deviation $\sigma$). First recognize that these are a sample and we generally think of them to be representative of some population.

**Example:** Finger Tapping and Caffeine

The effects of caffeine on the body have been well studied. In one experiment, a group of male college students were trained in a particular tapping movement and to tap at a rapid rate. They were randomly divided into caffeine and non-caffeine groups and given approximately two cups of coffee (with either 200 mg of caffeine or none). After a 2-hour period, the students tapping rate was measured. 

The population that we are trying to learn about is male college-aged students and we the most likely question of interest is if the mean tap rate of the caffeinated group is different than the non-caffeinated group. Notice that we want to take this sample of 20 students to make inference on the population of male college-aged students.  The hypotheses we are interested in are
$$\begin{aligned} 
H_{0}:	\mu_{nc} &=   \mu_{c} \\	
H_{a}:	\mu_{nc} &\ne \mu_{c}
\end{aligned}$$	

where $\mu_{c}$ is the mean tap rate of the caffeinated group and $\mu_{nc}$ is the mean tap rate of the non-caffeinated group. We could equivalently express these hypotheses via
$$\begin{aligned}
H_{0}:	\mu_{nc}-\mu_{c}  &=  0  \\
H_{a}:	\mu_{nc}-\mu_{c} &\ne 0
\end{aligned}$$	

Or we could let $\delta=\mu_{nc}-\mu_{c}$ and write the hypotheses as 
$$\begin{aligned}
H_{0}:\,\delta	&=	  0 \\
H_{a}:\,\delta	&\ne	0
\end{aligned}$$	

The data are available in many different formats at http://www.lock5stat.com/datapage.html


```r
data(CaffeineTaps, package='Lock5Data')   # load the data from the Lock5Data package
str(CaffeineTaps)
```

```
## 'data.frame':	20 obs. of  2 variables:
##  $ Taps : int  246 248 250 252 248 250 246 248 245 250 ...
##  $ Group: Factor w/ 2 levels "Caffeine","NoCaffeine": 1 1 1 1 1 1 1 1 1 1 ...
```

The dataset contains two variables.  Taps are the response of interest.  Group is a factor (or categorical variable) that has 2 levels.  These are the different groupings of Caffeine and NoCaffeine.  The first thing we should do is, as always, graph the data.


```r
ggplot(CaffeineTaps, aes(x=Taps)) +
  geom_dotplot(binwidth=.2) + 
  facet_grid(Group ~ .)  # two graphs stacked by Group (Caffeine vs non)
```

![](07_Two_Samples_files/figure-latex/unnamed-chunk-4-1.pdf)<!-- --> 

From this view, it looks like the caffeine group has a higher tapping rate. It will be helpful to summarize the difference between these two groups with a single statistic by calculating the mean for each group and then calculate the difference between the group means.


```r
CaffeineTaps %>% 
  group_by(Group) %>%  # group the summary stats by Treatment group
  summarise(xbar=mean(Taps), s=sd(Taps))
```

```
## # A tibble: 2 x 3
##   Group       xbar     s
##   <fct>      <dbl> <dbl>
## 1 Caffeine    248.  2.21
## 2 NoCaffeine  245.  2.39
```

We can find then find the difference in the sample means.


```r
# No Caffeine  -  Caffeine
# 244.8 - 248.3
CaffeineTaps %>% group_by(Group) %>%
  summarise(xbar=mean(Taps)) %>%
  summarise(d = diff(xbar))
```

```
## # A tibble: 1 x 1
##       d
##   <dbl>
## 1  -3.5
```

Notationally, lets call this statistic $d=\bar{x}_{nc}-\bar{x}_{c}=-3.5$. We are interested in testing if this observed difference might be due to just random chance and we just happened to assigned more of the fast tappers to the caffeine group. How could we test the null hypothesis that the mean of the caffeinated group is different than the non-caffeinated?

### Inference via resampling 

The key idea is “How could the data have turned out if the null hypothesis is true?” If the null hypothesis is true, then the caffeinated/non-caffeinated group treatment had no effect on the tap rate and it was just random chance that the caffeinated group got a larger percentage of fast tappers. That is to say the group variable has no relationship to tap rate. I could have just as easily assigned the fast tappers to the non-caffeinated group purely by random chance. So our simulation technique is *to shuffle the group labels and then calculate a difference between the group means*!

Below we demonstrate what it would look like to shuffle the groups.  This is the core concept behind the permutation methods, and how we can work to make an inference via resampling.

```r
# shuffle(): takes an input column and reorders it randomly
CaffeineTaps %>% mutate(ShuffledGroup = mosaic::shuffle(Group))
```

```
## Registered S3 method overwritten by 'mosaic':
##   method                           from   
##   fortify.SpatialPolygonsDataFrame ggplot2
```

```
##    Taps      Group ShuffledGroup
## 1   246   Caffeine    NoCaffeine
## 2   248   Caffeine      Caffeine
## 3   250   Caffeine      Caffeine
## 4   252   Caffeine      Caffeine
## 5   248   Caffeine      Caffeine
## 6   250   Caffeine      Caffeine
## 7   246   Caffeine    NoCaffeine
## 8   248   Caffeine    NoCaffeine
## 9   245   Caffeine    NoCaffeine
## 10  250   Caffeine    NoCaffeine
## 11  242 NoCaffeine    NoCaffeine
## 12  245 NoCaffeine    NoCaffeine
## 13  244 NoCaffeine      Caffeine
## 14  248 NoCaffeine    NoCaffeine
## 15  247 NoCaffeine    NoCaffeine
## 16  248 NoCaffeine      Caffeine
## 17  242 NoCaffeine      Caffeine
## 18  244 NoCaffeine      Caffeine
## 19  246 NoCaffeine    NoCaffeine
## 20  242 NoCaffeine      Caffeine
```

We can then calculate the mean difference but this time using the randomly generated groups, and now the non-caffeinated group just happens to have a slightly higher mean tap rate just by the random sorting into two groups.


```r
CaffeineTaps %>%
  mutate( ShuffledGroup = mosaic::shuffle(Group) ) %>%
  group_by( ShuffledGroup )  %>%
  summarise(xbar=mean(Taps)) %>%
  summarise(d.star = diff(xbar)) 
```

```
## # A tibble: 1 x 1
##   d.star
##    <dbl>
## 1   -0.5
```

We could repeat this shuffling several times and see the possible values we might have seen if the null hypothesis is correct and the treatment group doesn't matter at all.


```r
mosaic::do(5) * {
  CaffeineTaps %>%
  mutate( ShuffledGroup = mosaic::shuffle(Group) ) %>%
  group_by( ShuffledGroup )  %>%
  summarise(xbar=mean(Taps)) %>%
  summarise(d.star = diff(xbar))  
}
```

```
##   d.star
## 1   -1.9
## 2    1.9
## 3   -0.7
## 4   -1.7
## 5    2.1
```

Of course, five times isn't sufficient to understand the sampling distribution of the mean difference under the null hypothesis, we should do more.




```r
PermutationDist <- mosaic::do(10000) * {
  CaffeineTaps %>%
  mutate( ShuffledGroup = mosaic::shuffle(Group) ) %>%
  group_by( ShuffledGroup )  %>%
  summarise(xbar=mean(Taps)) %>%
  summarise(d.star = diff(xbar))  
}
```

We can then take the results of our 10000 permutations and view a histogram of the resulting difference in the shuffled group means ($d^*$).


```r
ggplot(PermutationDist, aes(x=d.star)) +
  geom_histogram(binwidth=.2) +
  ggtitle('Permutation dist. of d* assuming H0 is true') +
  xlab('d*') + 
  geom_vline(xintercept = c(-3.5, 3.5), lwd=1.5, col='red')
```

![](07_Two_Samples_files/figure-latex/unnamed-chunk-12-1.pdf)<!-- --> 

We are then interested in how often from our permutations did we observe something more extreme than the mean difference from the original groupings.  Because this is a two-tailed test, we will look for how many observations are either below -3.5 or above +3.5.  The original difference in the means are marked as vertical red lines in the graph above.

We have almost no cases where the randomly assigned groups produced a difference as extreme as the actual observed difference of $d=-3.5$. We can calculate the percentage of the sampling distribution of the difference in means that is farther from zero 


```r
PermutationDist %>% 
  mutate( MoreExtreme = ifelse( abs(d.star) >= 3.5, 1, 0)) %>%
  summarise( p.value = mean(MoreExtreme))
```

```
##   p.value
## 1  0.0055
```

We see that only 58/10,000 simulations of data produced assuming $H_{0}$ is true produced a $d^{*}$ value more extreme than our observed difference in sample means.  This is exactly the definition we have given to a p-value; thus, we can reject the null hypothesis $H_{0}:\mu_{nc}-\mu_{c}=0$ in favor of the alternative $H_{a}:\mu_{nc}-\mu_{c}\ne 0$ at an $\alpha=0.05$ or any other reasonable $\alpha$ level.

#### Using coin

To make the code less cumbersome, we can incorporate the use of the `coin` package.  This package will allow us to perform a variety of permutation tests without having to produce code such as that shown above.  We will only need to ensure that our data is prepared properly. However, for those who are interested more in the R coding that can be done to produce permutation tests, please see Appendix B : Alternative Permutation Test Code.

The data in `CaffeineTaps` has the data separated as `Taps` and `Group`, which is exactly the form we need it in.  We can run the permutation using `coin` simply by using the `oneway_test()` command and asking it to approximate the p-value.  It will then run the permutation test for us.  The same number of reshuffles as above (10000) is used.


```r
library(coin)
oneway_test(Taps~Group, data=CaffeineTaps, alternative="two.sided",
            distribution=approximate(nresample=10^4))
```

```
## 
## 	Approximative Two-Sample Fisher-Pitman Permutation Test
## 
## data:  Taps by Group (Caffeine, NoCaffeine)
## Z = 2.723, p-value = 0.0049
## alternative hypothesis: true mu is not equal to 0
```

We observe excellent agreement to the simulation run above, but with much less involvement on how to handle the code.

#### Different Alternative Hypothesis

Everything we know about the biological effects of ingesting caffeine suggests that we should have expected the caffeinated group to tap faster.  We might want to set up our experiment so only faster tapping represents "extreme" data compared to the null hypothesis.  In this case we want an alternative of $H_{a}:\,\mu_{nc}-\mu_{c}<0$ We can state our null and alternative hypothesis as 
$$\begin{aligned}
H_{0}:\,\mu_{nc}-\mu_{c}	&\ge	0 \\
H_{a}:\,\mu_{nc}-\mu_{c}	&<	0
\end{aligned}$$

The creation of the sampling distribution of the mean difference $d^*$ is identical to our previous technique because if our observed difference $d$ is so negative that it is incompatible with the hypothesis that $\mu_{nc}-\mu_{c}=0$ then it must also be incompatible with any positive difference. We can perform the permutation test and generate the distribution of estimated differences in the same manner as above.  The only difference in the analysis is at the end when we calculate the p-value and don't consider the positive tail. That is, the p-value is the percent of simulations where $d^*<d$. 


```r
PermutationDist %>% 
  summarize( p.value = mean( d.star <= -3.5 ))
```

```
##   p.value
## 1  0.0024
```

We can perform a left-tailed test using `coin`, but need to be sure we call 'NoCaffeine' the first group.  We can do this with `relevel()`.


```r
CaffeineTaps$Group <- relevel(CaffeineTaps$Group, 'NoCaffeine')
oneway_test(Taps~Group, data=CaffeineTaps, alternative="less",
            distribution=approximate(nresample=10^4))
```

```
## 
## 	Approximative Two-Sample Fisher-Pitman Permutation Test
## 
## data:  Taps by Group (NoCaffeine, Caffeine)
## Z = -2.723, p-value = 0.0021
## alternative hypothesis: true mu is less than 0
```

From both methods we see that the p-value is approximately cut in half by ignoring the upper tail, which makes sense considering the observed symmetry in the sampling distribution of $d^*$.

In general, we prefer to use a two-sided test because if the two-sided test leads us to reject the null hypothesis then so would the appropriate one-sided hypothesis (except in the case where the alternative was chosen before the data was collected and the observed data was in the other tail).  Second, by using a two-sample test, it prevents us from from “tricking” ourselves when we don't know the which group should have a higher mean going into the experiment, but after seeing the data, thinking we should have known and using the less stringent test. Some statisticians go so far as to say that using a 1-sided test is outright fraudulent. Generally, we'll concentrate on two-sided tests as they are the most widely acceptable.

#### Inference via Bootstrap Confidence Interval

Just as we could use bootstrapping to evaluate a confidence interval for one-sample, we can do the same for two-samples.  We need only update the function we are give the `boot` function.


```r
diff.mean.function <- function(data, index){
  m1 = mean(subset(data[index, 1], data[index, 2] == levels(data[,2])[1]))
  m2 = mean(subset(data[index, 1], data[index, 2] == levels(data[,2])[2]))
  return(m1 - m2)
}
```

This function works slightly different than the Chapter 3 version.  We must now ensure that we give it a data.frame where the first column are the observations and the second column the factored group labels.  This code will then calculate the difference in the means while bootstrapping the elements observed.  We can run the bootstrap in a nearly identical fashion to Chapter 3.  Notice my `data` is no longer a vector of values, but the data.frame we have been working with.


```r
BootDist <- boot(data = CaffeineTaps, statistic = diff.mean.function, R=10000)
```

We can visualize the results identical to the earlier chapters, but now recognizing this sampling distribution represents the difference in the means of the NoCaffeine and Caffeine groups.


```r
BootDist.graph <- data.frame(dbar=BootDist$t)
ggplot(BootDist.graph, aes(x=dbar)) +
  geom_histogram() + 
  ggtitle('Difference in Mean NoCaffeine Taps and Mean Caffeine Taps')
```

![](07_Two_Samples_files/figure-latex/unnamed-chunk-19-1.pdf)<!-- --> 


```r
CI <- quantile( BootDist$t, probs=c(0.025, 0.975) )
CI
```

```
##  2.5% 97.5% 
##  -5.5  -1.5
```

Thus, we can state that with 95% confidence the difference between the mean NoCaffeine taps and mean Caffeine taps is between -5.4 and -1.5 taps.  Notice that the null hypothesis value, $\delta=0$, is not a value supported by the data because 0 is not in the 95% confidence interval. A subtle point in the above bootstrap code does not re-sampled each group separately. Because the experimental protocol was to have 10 in each group, we might want to use bootstrap code that accounts for the correct design.  For now, we might end up with 12 caffeinated and 8 decaffeinated subjects, which is data that our experimental design couldn't have generated.  This should have minimal consequence and our bootstraps can still be conducted relatively easy.

### Inference via asymptotic results (unequal variance assumption)

Previously we've seen that the Central Limit Theorem gives us a way to estimate the distribution of the sample mean. So it should be reasonable to assume that for our two groups (1=NoCaffeine, 2=Caffeine),
$$\bar{X}_{1}\stackrel{\cdot}{\sim}N\left(\mu_{1},\, \frac{\sigma_{1}^{2}}{n_1}\right)\;\;\;\textrm{and}\;\;\;\bar{X}_{2}\stackrel{\cdot}{\sim}N\left(\mu_{2},\; \frac{\sigma_{2}^{2}}{n_2}\right)$$

It turns out that because $\bar{X}_{C}$ and $\bar{X}_{NC}$ both have approximately normal distributions, then the difference between them also does. This shouldn't be too surprising after looking at the permutation and bootstrap distributions of the $d^*$ values. 

So our hypothesis tests and confidence interval routine will follow a similar pattern as our one-sample tests, but we now need to figure out the correct standardization formula for the difference in means. The only difficulty will be figuring out what the appropriate standard deviation term $\hat{\sigma}_{D}$ should be.

Recall that if two random variables, A and B, are independent then $$Var\left(A-B\right)=Var(A)+Var(B)$$ 
and therefore 
$$\begin{aligned} Var\left(D\right)	
  &=	Var\left(\bar{X}_{1}-\bar{X}_{2}\right) \\
	&=	Var\left(\bar{X}_{1}\right)+Var\left(\bar{X}_{2}\right) \\
	&=	\frac{\sigma_{1}^{2}}{n_{1}}+\frac{\sigma_{2}^{2}}{n_{2}}
	\end{aligned}$$
and finally we have
$$StdErr\left(D\right)=\sqrt{\frac{s_{1}^{2}}{n_{1}}+\frac{s_{2}^{2}}{n_{2}}}$$
and therefore my standardized value for the difference will be
$$\begin{aligned} t_{\Delta}	
  &=	\frac{\textrm{estimate}\,\,\,-\,\,\,\textrm{null hypothesized value}}{StdErr\left(\,\,\textrm{estimate}\,\,\right)} \\
	\end{aligned}$$
	
The test statistic under unequal variance conditions is given by

$$\begin{aligned} t_{\Delta}	
	&=	\frac{\left(\bar{x}_{1}-\bar{x}_{2}\right)}{\sqrt{\frac{s_{1}^{2}}{n_{1}}+\frac{s_{2}^{2}}{n_{2}}}} \\
	\end{aligned}$$
	
For the data evaluated here, we thus have
	
$$\begin{aligned} t_{\Delta}
	&=	\frac{\left(-3.5\right)-0}{\sqrt{\frac{2.39^{2}}{10}+\frac{2.21^{2}}{10}}} \\
	&=	-3.39 
	\end{aligned}$$

This is somewhat painful, but reasonable. The last question is what t-distribution should we compare this to? Previously we've used $df=n-1$ but now we have two samples. So our degrees of freedom ought to be somewhere between $\min\left(n_{1},n_{2}\right)-2=8$ and $\left(n_{1}+n_{2}\right)-1=19$. 

There is no correct answer, but the best approximation to what it should be is called Satterthwaite's Approximation. We will give this degree of freedom a special character, $\Delta$, to keep it clear when we are using it.
$$\Delta=\frac{\left(V_{1}+V_{2}\right)^{2}}{\frac{V_{1}^{2}}{n_{1}-1}+\frac{V_{2}^{2}}{n_{2}-1}}$$
where
$$V_{1}=\frac{s_{1}^{2}}{n_{1}}\;\;\textrm{and }\;\;V_{2}=\frac{s_{2}^{2}}{n_{2}}$$ 

So for our example we have

$$V_{1}=\frac{2.39^{2}}{10}=0.5712\;\;\;\textrm{and}\;\;\;V_{2}=\frac{2.21^{2}}{10}=0.4884$$
and
$$\Delta=\frac{\left(0.5712+0.4884\right)^{2}}{\frac{\left(0.5712\right)^{2}}{9}+\frac{\left(0.4884\right)^{2}}{9}}=17.89$$

So now we can compute our two-tailed p-value as 

$$\textrm{p-value}=2*P\left(T_{17.89}<-3.39\right)$$


```r
2*pt(-3.39, df=17.89, ncp=0)
```

```
## [1] 0.00328554
```

#### Confidence Interval

Similar to the theory discussed earlier, we can calculate the asymptotic confidence interval for the difference in the means.  Recall that in general the confidence interval is given by

$$\begin{aligned}
\textrm{Est}\;\; &\pm\; t_{\Delta}^{1-\alpha/2}\;\textrm{StdErr}\left(\;\textrm{Est}\;\right)	\\	
\end{aligned}$$

For the difference of two means under unequal variance conditions, this can be written

$$\begin{aligned}
\left(\bar{x}_{1}-\bar{x}_{2}\right)  &\pm  t_{\Delta}^{1-\alpha/2}\sqrt{\frac{s_{1}^{2}}{n_{1}}+\frac{s_{2}^{2}}{n_{2}}} \\
\end{aligned}$$

Working this through for the data set evaluated here, we find obtain

$$\begin{aligned}
-3.5  &\pm  2.10\sqrt{\frac{2.39^{2}}{10}+\frac{2.21^{2}}{10}} \\
-3.5  &\pm  2.16 
\end{aligned}$$

Where the critical t-score was found at $\Delta$ degrees of freedom


```r
qt(0.975, 17.89)
```

```
## [1] 2.101848
```

Giving a 95% confidence interval for the difference in mean taps for NoCaffeine and Caffeine groups as

$$\left(-5.66,\;-1.34\right)$$

It is probably fair to say that this is an ugly calculation to do by hand. Fortunately it isn't too hard to make R do these calculations for you. The function `t.test()` will accept two arguments, a vector of values from the first group and a vector from the second group.  We can also give it a formula, which is good to start understanding.  Here we use `Response ~ Predictors`, which will be important for understanding linear models.  We want to test if the response `Taps` differs between the two `Group` levels.


```r
t.test(Taps ~ Group, data=CaffeineTaps)
```

```
## 
## 	Welch Two Sample t-test
## 
## data:  Taps by Group
## t = -3.3942, df = 17.89, p-value = 0.003255
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -5.667384 -1.332616
## sample estimates:
## mean in group NoCaffeine   mean in group Caffeine 
##                    244.8                    248.3
```

### Inference via asymptotic results (equal variance assumption)

In the `CaffeineTaps` example, the standard deviations of each group are quite similar. Instead of thinking of the data as 
$$\bar{X}_{1}\stackrel{\cdot}{\sim}N\left(\mu_{1},\,\frac{\sigma_{1}^{2}}{n_1}\right)\;\;\;\textrm{and}\;\;\;\bar{X}_{2}\stackrel{\cdot}{\sim}N\left(\mu_{2},\;\frac{\sigma_{2}^{2}}{n_2}\right)$$
we could consider the model where we assume that the variance term is the same for each sample.
$$\bar{X}_{1}\stackrel{\cdot}{\sim}N\left(\mu_{1},\,\frac{\sigma^{2}}{n_1}\right)\;\;\;\textrm{and}\;\;\;\bar{X}_{2}\stackrel{\cdot}{\sim}N\left(\mu_{2},\; \frac{\sigma^{2}}{n_2}\right)$$

First, we can estimate $\mu_{1}$ and $\mu_{2}$ with the appropriate sample means $\bar{x}_{1}$ and $\bar{x}_{2}$. Next we need to calculate an estimate of $\sigma$ using all of the data. First recall the formula for the sample variance for one group was
$$s^{2}=\frac{1}{n-1}\left[\sum_{j=1}^{n}\left(x_{j}-\bar{x}\right)^{2}\right]$$

In the case with two samples, we want a similar formula but it should take into account data from both sample groups. Define the notation $x_{1j}$ to be the $j$th observation of group 1, and $x_{2j}$ to be the $j$th observation of group 2 and in general $x_{ij}$ as the $j$th observation from group $i$. We want to subtract each observation from the its appropriate sample mean and then, because we had to estimate two means, we need to subtract two degrees of freedom from the denominator.
$$\begin{aligned} s_{pooled}^{2}	
  &=	\frac{1}{n_{1}+n_{2}-2}\left[\sum_{j=1}^{n_{1}}\left(x_{1j}-\bar{x}_{1}\right)^{2}+\sum_{j=1}^{n_{2}}\left(x_{2j}-\bar{x}_{2}\right)^{2}\right] \\
	&=	\frac{1}{n_{1}+n_{2}-2}\left[\sum_{j=1}^{n_{1}}e_{1j}^{2}+\sum_{j=1}^{n_{2}}e_{2j}^{2}\right]\\
	&=	\frac{1}{n_{1}+n_{2}-2}\left[\sum_{i=1}^{2}\sum_{j=1}^{n_{i}}e_{ij}^{2}\right]
	\end{aligned}$$

where $\bar{x}_{1}$ and $\bar{x}_{2}$ are the sample means and $e_{ij}=x_{ij}-\bar{x}_{i}$ is the residual error of the $i,j$ observation. A computationally convenient formula for this same quantity is
$$s_{pooled}^{2}=\frac{1}{n_{1}+n_{2}-2}\left[\left(n_{1}-1\right)s_{1}^{2}+\left(n_{2}-1\right)s_{2}^{2}\right]$$

Finally we notice that this pooled estimate of the variance term $\sigma^{2}$ has $n_{1}+n_{2}-2$ degrees of freedom. *One benefit of the pooled procedure is that we don't have to mess with the Satterthwaite's approximate degrees of freedom.*

Recall our test statistic in the unequal variance case was
$$t_{\Delta} 
  =\frac{\left(\bar{x}_{1}-\bar{x}_{2}\right)-0}{\sqrt{\frac{s_{1}^{2}}{n_{1}}+\frac{s_{2}^{2}}{n_{2}}}}$$
Now in the equal variance case, we will use the pooled estimate of the variance term $s_{pooled}^{2}$ instead of $s_{1}^{2}$ and $s_{2}^{2}$, and we have known $df = (n_1 + n_2 - 2)$.

$$\begin{aligned} t_{n_{1}+n_{2}-2}	
  &=	\frac{\left(\bar{x}_{1}-\bar{x}_{2}\right)-0}{\sqrt{\frac{s_{pool}^{2}}{n_{1}}+\frac{s_{pool}^{2}}{n_{2}}}} \\
	&=	\frac{\left(\bar{x}_{1}-\bar{x}_{2}\right)-0}{s_{pool}\sqrt{\frac{1}{n_{1}}+\frac{1}{n_{2}}}}
  \end{aligned}$$
  
where we note that 
$$StdErr\left(\bar{X}_{1}-\bar{X}_{2}\right)=s_{pooled}\sqrt{\left(1/n_{1}\right)+\left(1/n_{2}\right)}$$ 

#### Caffeine Example

We can now rework the analysis of the Caffeine data under equal variance assumptions, allowing us to pool our estimate of the variance.

Recall our hypothesis for the `CaffeineTaps` data
$$\begin{aligned}
H_{0}:	&\mu_{nc}-\mu_{c}  =  0	  \\
H_{a}:	&\mu_{nc}-\mu_{c} \ne 0 
\end{aligned}$$


First we have to calculate the summary statistics for each group.


```r
CaffeineTaps %>% 
  group_by(Group) %>% 
  summarise(xbar.i = mean(Taps),   # sample mean for each group
            s2.i   = var(Taps),    # sample variances for each group
            s.i    = sd(Taps),     # sample standard deviations for each group
            n.i    = n()      )    # sample sizes for each group
```

```
## # A tibble: 2 x 5
##   Group      xbar.i  s2.i   s.i   n.i
##   <fct>       <dbl> <dbl> <dbl> <int>
## 1 NoCaffeine   245.  5.73  2.39    10
## 2 Caffeine     248.  4.90  2.21    10
```

We can then use the descriptive statistics to determine the pooled variance estimate $\sigma_{pooled}$). 


```r
CaffeineTaps %>%
  group_by(Group) %>%
  summarize(  n.i = n(),             
             s2.i = var(Taps) ) %>%     
  summarize( s2.p = sum( (n.i-1)*s2.i ) / ( sum(n.i)-2 ),
             s.p  = sqrt(s2.p) ) 
```

```
## # A tibble: 1 x 2
##    s2.p   s.p
##   <dbl> <dbl>
## 1  5.32  2.31
```

Next we can calculate 
$$t_{18}=\frac{\left(244.8-248.3\right)-0}{2.31\sqrt{\frac{1}{10}+\frac{1}{10}}}=-3.39$$

Finally we estimate our p-value


```r
p.value <- 2 * pt(-3.39, df=18)   # 2-sided test, so multiply by 2
p.value
```

```
## [1] 0.003262969
```

The change in the assumption of the variance makes little difference for the Caffeine data set, and we can still conclude that there is a difference in the mean taps between the Caffeine and NoCaffeine groups.

#### Confidence Interval

The associated $95\%$ confidence interval when working under the equal variance assumption is
$$\left(\bar{x}_{1}-\bar{x}_{2}\right)\pm t_{n_{1}+n_{2}-2}^{1-\alpha/2}\;\left(s_{pool}\sqrt{\frac{1}{n_{1}}+\frac{1}{n_{2}}}\right)$$

We now find the critical t-score at the known degree of freedom


```r
qt( .975, df=18 ) 
```

```
## [1] 2.100922
```

Then calculate the confidence interval

$$\begin{aligned}
-3.5 &\pm 2.10\left(\,2.31\sqrt{\frac{1}{10}+\frac{1}{10}}\right) \\
-3.5 &\pm 2.17 
\end{aligned}$$
$$\left(-5.67,\;-1.33\right)$$

This p-value and $95\%$ confidence interval are quite similar to the values we got in the case where we assumed unequal variances.

As usual, these calculations are pretty annoying to do by hand and we wish to instead do them using R. Again the function `t.test()` will do the annoying calculations for us.  We must only state that we want to do the test under equal variance or `var.equal=TRUE`.


```r
# Do the t-test
t.test( Taps ~ Group, data=CaffeineTaps, var.equal=TRUE ) 
```

```
## 
## 	Two Sample t-test
## 
## data:  Taps by Group
## t = -3.3942, df = 18, p-value = 0.003233
## alternative hypothesis: true difference in means is not equal to 0
## 95 percent confidence interval:
##  -5.66643 -1.33357
## sample estimates:
## mean in group NoCaffeine   mean in group Caffeine 
##                    244.8                    248.3
```

Maybe we would like to evaluate a higher confidence level.


```r
# Do the t-test at 99% confidence
t.test( Taps ~ Group, data=CaffeineTaps, var.equal=TRUE, conf.level=.99 ) 
```

```
## 
## 	Two Sample t-test
## 
## data:  Taps by Group
## t = -3.3942, df = 18, p-value = 0.003233
## alternative hypothesis: true difference in means is not equal to 0
## 99 percent confidence interval:
##  -6.4681918 -0.5318082
## sample estimates:
## mean in group NoCaffeine   mean in group Caffeine 
##                    244.8                    248.3
```

### Additional Example

**Example:** Does drinking beer increase your attractiveness to mosquitoes?

In places in the country substantial mosquito populations, the question of whether drinking beer causes the drinker to be more attractive to the mosquitoes than drinking something else has plagued campers. To answer such a question, researchers conducted a study to determine if drinking beer attracts more mosquitoes than drinking water. Of $n=43$ subjects, $n_{b}=25$ drank a liter beer and $n_{w}=18$ drank a liter of water and mosquitoes were caught in traps as they approached the different subjects. The critical part of this study is that the treatment (beer or water) was randomly assigned to each subject.

For this study, we want to test 
$$H_{0}:\:\delta=0\;\;\;\;\;\;\textrm{vs}\;\;\;\;\;\;H_{a}:\,\delta<0$$
where we define $\delta=\mu_{w}-\mu_{b}$ and $\mu_{b}$ is the mean number of mosquitoes attracted to a beer drinker and $\mu_{w}$ is the mean number attracted to a water drinker. As usual we begin our analysis by plotting the data. 


```r
# I can't find this dataset on-line so I'll just type it in.
Mosquitoes <- data.frame(
  Number = c(27,19,20,20,23,17,21,24,31,26,28,20,27,
             19,25,31,24,28,24,29,21,21,18,27,20,
             21,19,13,22,15,22,15,22,20,
             12,24,24,21,19,18,16,23,20),
  Treat = c( rep('Beer', 25), rep('Water',18) ) )

# Set up the Treatment Variable so that Water is the Reference Group
Mosquitoes <- Mosquitoes %>%
  mutate(Treat = forcats::fct_relevel(Treat, 'Water'))

# Plot the data
ggplot(Mosquitoes, aes(x=Number)) +
  geom_histogram(binwidth=1) +
  facet_grid( Treat ~ . )
```

![](07_Two_Samples_files/figure-latex/unnamed-chunk-30-1.pdf)<!-- --> 

For this experiment and the summary statistic that captures the difference we are trying to understand is $d=\bar{x}_{w}-\bar{x}_{b}$ where $\bar{x}_{w}$ is the sample mean number of mosquitoes attracted by the water group and $\bar{x}_{b}$ is the sample mean number of mosquitoes attracted by the beer group. Because of the order we chose for the subtraction, a negative value for d is supportive of the alternative hypothesis that mosquitoes are more attracted to beer drinkers. 


```r
Mosquitoes %>% group_by(Treat) %>%
  summarise(xbar.i = mean(Number),
            s2.i   = var(Number),
            s.i    = sd(Number),
            n.i    = n())
```

```
## # A tibble: 2 x 5
##   Treat xbar.i  s2.i   s.i   n.i
##   <fct>  <dbl> <dbl> <dbl> <int>
## 1 Water   19.2  13.5  3.67    18
## 2 Beer    23.6  17.1  4.13    25
```

Here we see that our statistic of interest is 
$$\begin{aligned} d	
  &=	\bar{x}_{w}-\bar{x}_{b} \\
	&=	19.22-23.6              \\
	&=	-4.37\bar{7}
	\end{aligned}$$

We can use our numerical methods to evaluate statistical significance. First we perform the hypothesis test by creating the sampling distribution of $d^*$ assuming $H_0$ is true by repeatedly shuffling the group labels and calculating differences.  We use `coin` to simplify the work.  


```r
### library(coin) required
coin::oneway_test(Number~Treat, data=Mosquitoes, alternative="less",
            distribution=approximate(nresample=10^4))
```

```
## 
## 	Approximative Two-Sample Fisher-Pitman Permutation Test
## 
## data:  Number by Treat (Water, Beer)
## Z = -3.1673, p-value = 5e-04
## alternative hypothesis: true mu is less than 0
```

From 10000 permutations, we obtain a p-value estimate of $0.0004$.

The associated confidence interval (lets do a $90\%$ confidence level), is created via bootstrapping.  The `diff.mean.func` was defined earlier in the chapter.  Our data is in the form we need it, so we can run the bootstrap with the same setup as earlier.


```r
BootDist <- boot(data = Mosquitoes, statistic = diff.mean.function, R=10000)
```

We can visualize the distribution of the difference in means.


```r
BootDist.graph <- data.frame(dbar=BootDist$t)
ggplot(BootDist.graph, aes(x=dbar)) +
  geom_histogram() + 
  ggtitle('Difference in mean number of Mosquitoes between Water and Beer')
```

![](07_Two_Samples_files/figure-latex/unnamed-chunk-34-1.pdf)<!-- --> 

We can then extract the quantile-based 90% confidence interval.


```r
CI <- quantile( BootDist$t, probs=c(0.05, 0.95) )
CI
```

```
##        5%       95% 
## -6.347715 -2.456786
```

The calculated p-value is extremely small and the associated two-sided 90% confidence interval does not contain 0, so we can conclude at 10% significance that the choice of drink does cause a change in attractiveness to mosquitoes.

If we wanted to perform the same analysis using asymptotic methods we could do the calculations by hand, or just use R.


```r
t.test( Number ~ Treat, data=Mosquitoes, 
        var.equal=TRUE, conf.level=0.90)
```

```
## 
## 	Two Sample t-test
## 
## data:  Number by Treat
## t = -3.587, df = 41, p-value = 0.0008831
## alternative hypothesis: true difference in means is not equal to 0
## 90 percent confidence interval:
##  -6.431666 -2.323889
## sample estimates:
## mean in group Water  mean in group Beer 
##            19.22222            23.60000
```

Because we releveled the groups to make `Water` first, this calculation matches everything demonstrated above.

## Difference in means between two groups: Paired Data

If the context of study is such that we can logically pair an observation from the first population to a particular observation in the second, then we can perform what is called a Paired Test. In a paired test, we will take each set of paired observations, calculate the difference, and then perform a 1-sample regular hypothesis test on the differences.

For example, in the package Lock5Data there is a dataset that examines the age in which men and women get married. The data was obtained by taking a random sample from publicly available marriage licenses in St. Lawrence County, NY. 


```r
data(MarriageAges, package='Lock5Data')
head(MarriageAges)
```

```
##   Husband Wife
## 1      53   50
## 2      38   34
## 3      46   44
## 4      30   36
## 5      31   23
## 6      26   31
```

Unfortunately the format of this dataset is not particularly convenient for making graphs. Instead I want to turn this data into a "long" dataset where I have one row per person, not one row per marriage.  We will also want to retain the groupings, so another column is created with this information (`Marriage`).


```r
# Make a dataset that is more convenient for graphing.
MarriageAges.Long <- MarriageAges %>%
  mutate(Marriage = factor(1:n())) %>%        # Give each row a unique ID 
  gather('Spouse', 'Age', Husband, Wife) %>%  # pivot from Husband/Wife to Spouse/Age
  arrange(Marriage, desc(Spouse))             # Sort by Marriage, then (Wife,Husband)
head(MarriageAges.Long)
```

```
##   Marriage  Spouse Age
## 1        1    Wife  50
## 2        1 Husband  53
## 3        2    Wife  34
## 4        2 Husband  38
## 5        3    Wife  44
## 6        3 Husband  46
```

We can then visualize the ages for each type of `Spouse`.


```r
# Make a graph of ages, by Spouse Type
ggplot(MarriageAges.Long, aes(x=Age)) +
  geom_histogram(binwidth=5) +
  facet_grid(Spouse ~ .) 
```

![](07_Two_Samples_files/figure-latex/unnamed-chunk-39-1.pdf)<!-- --> 

Looking at this view of the data, it doesn't appear that the husbands tend to be older than the wives. A t-test to see if the average age of husbands is greater than the average age of wives gives an insignificant difference.  We will want to test if

$$\begin{aligned}
H_{0}:	&\mu_{h}-\mu_{w}  =  0	  \\
H_{a}:	&\mu_{h}-\mu_{w} > 0 
\end{aligned}$$


```r
t.test( Age ~ Spouse, data=MarriageAges.Long, alternative='greater' )
```

```
## 
## 	Welch Two Sample t-test
## 
## data:  Age by Spouse
## t = 1.8055, df = 203.12, p-value = 0.03624
## alternative hypothesis: true difference in means is greater than 0
## 95 percent confidence interval:
##  0.2398733       Inf
## sample estimates:
## mean in group Husband    mean in group Wife 
##              34.66667              31.83810
```

But critically, we are ignoring that while the average ages might not be different, for a given marriage, the husband tends to be older than the wife. Instead of looking at the difference in the means (i.e $d=\bar{h}-\bar{w}$) we should actually be looking at the mean of the differences $\bar{d}=\frac{1}{n}\sum d_{i}$ where $d_{i}=h_{i}-w_{i}$.


```r
MarriageAges <- MarriageAges  %>%
  mutate( d = Husband - Wife )   

ggplot(MarriageAges, aes(x = d)) +
  geom_histogram(binwidth=2)
```

![](07_Two_Samples_files/figure-latex/unnamed-chunk-41-1.pdf)<!-- --> 

Given this set of differences, we'd like to know if this data is compatible with the null hypothesis that husbands and wives tend to be the same age versus the alternative that husbands tend to be older. (We could chose the two-sided test as well).
$$\begin{aligned}
H_{0}:\;\delta	&=	0  \\
H_{A}:\;\delta	&>	0
\end{aligned}$$

Because we have reduced our problem to a 1-sample test, we can perform the asymptotic t-test easily enough in R.  Notice now we are testing against the null hypothesis that $\delta = 0$.


```r
t.test( MarriageAges$d, mu=0 )
```

```
## 
## 	One Sample t-test
## 
## data:  MarriageAges$d
## t = 5.8025, df = 104, p-value = 7.121e-08
## alternative hypothesis: true mean is not equal to 0
## 95 percent confidence interval:
##  1.861895 3.795248
## sample estimates:
## mean of x 
##  2.828571
```

The result is highly statistically significant, and we see the mean difference in ages for the husband to be 2.8 years older. 

To perform the same analysis using re-sampling methods, we need to be careful to do the re-sampling correctly.  Notice that if we use `coin` how we set it up before, that we get something similar to when we were working under the assumption that the two groups were independent.  The `coin` package requires objects be factors (the data.frame has it contained as a character or `chr`).


```r
oneway_test(Age~factor(Spouse), data=MarriageAges.Long, alternative="greater", distribution=approximate(nresample=10^4-1))
```

```
## 
## 	Approximative Two-Sample Fisher-Pitman Permutation Test
## 
## data:  Age by factor(Spouse) (Husband, Wife)
## Z = 1.7958, p-value = 0.0384
## alternative hypothesis: true mu is greater than 0
```

The issue is that the permutations were done without taking into account that the Spouses are paired.  The permutation test must be updated such that the paired nature of the marriages is taken into account.  We can introduce this pairing using a conditional statement, where we want to ensure that we group the `Spouse` variable given the marriage they are in `Marriage`.


```r
oneway_test(Age~factor(Spouse)|factor(Marriage), data=MarriageAges.Long, alternative="greater", distribution=approximate(nresample=10^4-1))
```

```
## 
## 	Approximative Two-Sample Fisher-Pitman Permutation Test
## 
## data:  Age by
## 	 factor(Spouse) (Husband, Wife) 
## 	 stratified by factor(Marriage)
## Z = 5.0675, p-value < 1e-04
## alternative hypothesis: true mu is greater than 0
```

After 10000 permutations, no mean difference in age was ever observed as extreme as the original and can only state that p-value $< 1e-4$.  This is in agreement with the t.test performed above on the difference in ages for each marriage.

Finally, we can also perform bootstrap analysis.  This now only requires that we use our bootstrap method from Chapter 3, as we only need to bootstrap the differences. We have not introduced `mean.function` so I must provide it now.  I then run the bootstrap on the difference in ages for each marriage.


```r
mean.function <- function(x, index) {
  d <- x[index]
  return(mean(d))  }

BootDist <- boot(data = MarriageAges$d, statistic = mean.function, R=10000)
```

I omit the visualization, but give the resulting 95% confidence interval.


```r
quantile( BootDist$t, probs=c(.025, .975) )
```

```
##     2.5%    97.5% 
## 1.904762 3.800000
```

We observe a similar p-value and confidence interval as we did using the asymptotic test as expected.  We now have a variety of tests and conditions, and can perform the analysis under asymptotic assumptions or through numerical strategies.

### Additional Example

**Example:** Traffic Flow

Engineers in Dresden, Germany were looking at ways to improve traffic flow by enabling traffic lights to communicate information about traffic flow with nearby traffic lights and modify their timing sequence appropriately. The engineers wished to compare new flexible timing system with the standard fixed timing sequence by evaluating the delay at a randomly selected $n=24$ intersections in Dresden. The data show results of one experiment where they simulated buses moving along a street and recorded the delay time for both systems. Because each simulation is extremely intensive, they only simulated $n=24$ intersections instead of simulating the whole city.


```r
data(TrafficFlow, package='Lock5Data')  
head(TrafficFlow)
```

```
##   Timed Flexible Difference
## 1    88       45         43
## 2    90       46         44
## 3    91       45         46
## 4    99       51         48
## 5   101       48         53
## 6   101       48         53
```

We change the shape of the data to make it easier to work with.


```r
# A data set more convenient for Graphing and Permutation Tests.
TrafficFlow.Long <- TrafficFlow           %>%
  mutate(Light = factor(1:n()))           %>% # Give each row a unique ID 
  gather('Seq', 'Delay', Flexible, Timed) %>% # pivot to SequenceType and Delay amount
  arrange(Light, Seq)                         # Sort by Light, then by SequenceType
head(TrafficFlow.Long)
```

```
##   Difference Light      Seq Delay
## 1         43     1 Flexible    45
## 2         43     1    Timed    88
## 3         44     2 Flexible    46
## 4         44     2    Timed    90
## 5         46     3 Flexible    45
## 6         46     3    Timed    91
```

As usual, we'll first examine the data with a graph.


```r
ggplot(TrafficFlow.Long, aes(x=Delay)) +
  geom_histogram(binwidth=2) +              # histograms of Delay time
  facet_grid(Seq ~ .)                       # two plots, stacked by SequenceType
```

![](07_Two_Samples_files/figure-latex/unnamed-chunk-49-1.pdf)<!-- --> 


```r
ggplot(TrafficFlow, aes(x=Difference)) +
  geom_histogram(binwidth=2) +
  ggtitle('Difference (Standard - Flexible)')
```

![](07_Two_Samples_files/figure-latex/unnamed-chunk-50-1.pdf)<!-- --> 

All of the differences were positive, so it is almost ridiculous to do a hypothesis test that there is no decrease in delays with the flexible timing system.  We continue through the analysis.  We begin with the asymptotic results, using the paired differences.


```r
t.test( TrafficFlow$Difference )
```

```
## 
## 	One Sample t-test
## 
## data:  TrafficFlow$Difference
## t = 19.675, df = 23, p-value = 6.909e-16
## alternative hypothesis: true mean is not equal to 0
## 95 percent confidence interval:
##  54.58639 67.41361
## sample estimates:
## mean of x 
##        61
```

As expected, there is significant evidence that mean difference between Standard and Flexible.  We can also run the permutation test under paired conditions.  We are interested in the response `Delay` and how it is influenced by `Seq` the sequence time.  We then also ensure that we properly pair the data, where are groupings are now the variable `Light`.


```r
oneway_test(Delay~factor(Seq)|factor(Light), data=TrafficFlow.Long, 
            alternative="two.sided", distribution=approximate(nresample=10^4-1))
```

```
## 
## 	Approximative Two-Sample Fisher-Pitman Permutation Test
## 
## data:  Delay by
## 	 factor(Seq) (Flexible, Timed) 
## 	 stratified by factor(Light)
## Z = -4.7596, p-value < 1e-04
## alternative hypothesis: true mu is not equal to 0
```

We observe after 10000 iterations that again, no mean difference was ever as extreme as the original data set.  Thus, we have p-value $< 1e^{-4}$.  We finish with the bootstrap, performed on the differences.


```r
BootDist <- boot(data = TrafficFlow$Difference, statistic = mean.function, R=10000)
```

I omit the visualization, but give the resulting 95% confidence interval.


```r
quantile( BootDist$t, probs=c(.025, .975) )
```

```
##     2.5%    97.5% 
## 55.50000 67.41667
```

The confidence interval suggests that these data support that the mean difference between the flexible timing sequence versus the standard fixed timing sequence in Dresden is in the interval $\left(55.5,\,67.5\right)$ seconds.

## Exercises

1. In the 2011 article “Methane contamination of drinking water accompanying gas-well drilling and hydraulic fracturing” in the Proceedings of the National Academy of Sciences, $n_{1}=21$ sites in proximity to a fracking well had a mean methane level of $\bar{x}_{1}=19.2$ mg $CH_{4} L^{-1}$ with a sample standard deviation $s_{1}=30.3$. The $n_{2}=13$ sites in the same region with no fracking wells within 1 kilometer had mean methane levels of $\bar{x}_{2}=1.1$ mg $CH_{4} L^{-1}$ and standard deviation $s_{2}=6.3$. Perform a one-sided, two-sample t-test with unpooled variance and an $\alpha=0.05$ level to investigate if the presence of fracking wells increases the methane level in drinking-water wells in this region. Notice that because I don't give you the data, you can only analyze the data using the asymptotic method and plugging in the give quantities into the formulas presented.
    a) State an appropriate null and alternative hypothesis.
    b) Calculate an appropriate test statistic (making sure to denote the appropriate degrees of freedom, if necessary).
    c) Calculate an appropriate p-value.
    d) At an significance level of $\alpha=0.05$, do you reject or fail to reject the null hypothesis?
    e) Restate your conclusion in terms of the problem.

2. All persons running for public office must report the amount of money raised and spent during their campaign. Political scientists contend that it is more difficult for female candidates to raise money. Suppose that we randomly sample $30$ male and $30$ female candidates for state legislature and observe the male candidates raised, on average, $\bar{y}=\$350,000$ with a standard deviation of $s_{y}=\$61,900$ and the females raised on average $\bar{x}=\$245,000$ with a standard deviation of $s_{x}=\$52,100$. Perform a one-sided, two-sample t-test with pooled variance to test if female candidates generally raise less in their campaigns that male candidates. _Notice that because I don't give you the data, you can only analyze the data using the asymptotic method and plugging in the give quantities into the formulas presented._
    a) State an appropriate null and alternative hypothesis. (Be sure to use correct notation!)
    b) Calculate an appropriate test statistic (making sure to denote the appropriate degrees of freedom, if necessary).
    c) Calculate an appropriate p-value.
    d) At an significance level of $\alpha=0.05$, do you reject or fail to reject the null hypothesis?
    e) Restate your conclusion in terms of the problem.

3. In the Lock5Data package, the dataset `Smiles` gives data “...from a study examining the effect of a smile on the leniency of disciplinary action for wrongdoers. Participants in the experiment took on the role of members of a college disciplinary panel judging students accused of cheating. For each suspect, along with a description of the offense, a picture was provided with either a smile or neutral facial expression. Note, that for each individual only one picture was submitted. A leniency score was calculated based on the disciplinary decisions made by the participants.”
    a) Graph the leniency score for the smiling and non-smiling groups. Comment on if you can visually detect any difference in leniency score.
    b) Calculate the mean and standard deviation of the leniencies for each group. Does it seem reasonable that the standard deviation of each group is the same?
    c) Do a two-sided two-sample t-test using pooled variance using the asymptotic method. Report the test statistic, p-value, and a $95\%$ CI.
    d) Do a two-side two-sample t-test using re-sampling methods. Report the p-value and a $95\%$ CI.
    e) What do you conclude at an $\alpha=0.05$ level? Do you feel we should have used a more stringent $\alpha$ level?

4. In the Lock5Data package, the dataset `StorySpoilers` is data from an experiment where the researchers are testing if a “spoiler” at the beginning of a short story negatively affects the enjoyment of the story. A set of $n=12$ stories were selected and a spoiler introduction was created. Each version of each story was read by at least $30$ people and rated. Reported are the average ratings for the spoiler and non-spoiler versions of each story. The following code creates the “long” version of the data.
    
    ```r
    library(dplyr)
    library(tidyr)
    data(StorySpoilers, package='Lock5Data')
    StorySpoilers.Long <- StorySpoilers %>%
      gather('Type', 'Rating', Spoiler, Original) %>%
      mutate( Story = factor(Story),      # make Story and Type into
              Type  = factor(Type) ) %>%  # categorical variables
      arrange(Story)
    ```
    a) Based on the description, a 1-sided test is appropriate. Explain why.
    b) Graph the ratings for the original stories and the modified spoiler version. Comment on if you detect any difference in ratings between the two.
    c) Graph the difference in ratings for each story. Comment on if the distribution of the differences seems to suggest that a spoiler lowers the rating.
    d) Do a paired one-sided t-test using the asymptotic method. Also calculate a $95\%$ confidence interval. 
    e) Do a paired one-sided t-test using the permutation method. Also calculate a $95\%$ confidence interval using the bootstrap. 
    f) Based on your results in parts (d) and (e), what do you conclude? 

5. In the Lock5Data package, the dataset `Wetsuits` describes an experiment with the goal of quantifying the effect of wearing a wetsuit on the speed of swimming. (It is often debated among triathletes whether or not to wear a wetsuit when it is optional.) A set of $n=12$ swimmers and triathletes did a 1500 m swim in both the wetsuit and again in regular swimwear. The order in which they swam (wetsuit first or non-wetsuit first) was randomized for each participant. Reported is the maximum velocity during each swim.
    
    ```r
    # Code for creating the "long" version of the data
    library(dplyr)
    library(tidyr)
    data('Wetsuits', package='Lock5Data')
    Wetsuits.Long <- Wetsuits %>%
      mutate(Participant = factor(1:12) ) %>%
      gather('Suit', 'MaxVelocity', Wetsuit,NoWetsuit) %>%
      arrange( Participant, Suit) %>%
      mutate(Suit = factor(Suit))
    ```
    a) Why did the researcher randomize which suit was worn first?
    b) Plot the velocities for the wetsuit and non-wetsuit for each participant. Comment on if you detect any difference in the means of these two distributions. 
    c) Ignore the pairing and do a two-sided two-sample t-test using the asymptotic method. What would you conclude doing the t-test this way?
    d) Plot the difference in velocity for each swimmer. Comment on if the observed difference in velocity seems to indicate that which should be preferred (wetsuit or non-wetsuit).
    e) Do a paired two-sided t-test using the asymptotic method. Also calculate the 95% confidence interval. What do you conclude?
    f) Do a paired two-sided t-test using the permutation method. Also calculate the 95% confidence interval using the bootstrap method. What do you conclude?
