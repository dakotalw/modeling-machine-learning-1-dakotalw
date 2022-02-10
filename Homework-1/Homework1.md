Homework 1
================
Dakota Wilson
1/25/2022

Required libraries:

``` r
library('class')
library('dplyr')
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

### Question 1

``` r
## load binary classification example data from author website 
## 'ElemStatLearn' package no longer available
load(url('https://web.stanford.edu/~hastie/ElemStatLearn/datasets/ESL.mixture.rda'))
dat <- ESL.mixture

plot_mix_data <- expression({
  plot(dat$x[,1], dat$x[,2],
       col=ifelse(dat$y==0, 'blue', 'orange'),
       pch=20,
       xlab=expression(x[1]),
       ylab=expression(x[2]))
  ## draw Bayes (True) classification boundary
  prob <- matrix(dat$prob, length(dat$px1), length(dat$px2))
  cont <- contourLines(dat$px1, dat$px2, prob, levels=0.5)
  rslt <- sapply(cont, lines, col='purple')
})

data = data.frame(y = dat$y, x1 = dat$x[,1], x2 = dat$x[,2])
fit_lm = lm(y~x1+x2,data = data)
lm_pred = predict(fit_lm, newdata = data.frame(dat$xnew))
## reshape predictions as a matrix
lm_pred <- matrix(lm_pred, length(dat$px1), length(dat$px2))
contour(lm_pred,
      xlab=expression(x[1]),
      ylab=expression(x[2]))
```

![](Homework1_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
## find the contours in 2D space such that lc_pred == 0.5
lm_cont <- contourLines(dat$px1, dat$px2, lm_pred, levels=0.5)

## plot data and decision surface
eval(plot_mix_data)
sapply(lm_cont, lines)
```

![](Homework1_files/figure-gfm/unnamed-chunk-2-2.png)<!-- -->

    ## [[1]]
    ## NULL

### Question 2

``` r
## load binary classification example data from author website 
## 'ElemStatLearn' package no longer available
load(url('https://web.stanford.edu/~hastie/ElemStatLearn/datasets/ESL.mixture.rda'))
dat <- ESL.mixture

plot_mix_data <- expression({
  plot(dat$x[,1], dat$x[,2],
       col=ifelse(dat$y==0, 'blue', 'orange'),
       pch=20,
       xlab=expression(x[1]),
       ylab=expression(x[2]))
  ## draw Bayes (True) classification boundary
  prob <- matrix(dat$prob, length(dat$px1), length(dat$px2))
  cont <- contourLines(dat$px1, dat$px2, prob, levels=0.5)
  rslt <- sapply(cont, lines, col='purple')
})

data = data.frame(y = dat$y, x1 = dat$x[,1], x2 = dat$x[,2])
fit_lm = lm(y~poly(x1,2)+poly(x2,2),data = data)
lm_pred = predict(fit_lm, newdata = data.frame(dat$xnew))
## reshape predictions as a matrix
lm_pred <- matrix(lm_pred, length(dat$px1), length(dat$px2))
contour(lm_pred,
      xlab=expression(x[1]),
      ylab=expression(x[2]))
```

![](Homework1_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
## find the contours in 2D space such that lc_pred == 0.5
lm_cont <- contourLines(dat$px1, dat$px2, lm_pred, levels=0.5)

## plot data and decision surface
eval(plot_mix_data)
sapply(lm_cont, lines)
```

![](Homework1_files/figure-gfm/unnamed-chunk-3-2.png)<!-- -->

    ## [[1]]
    ## NULL

### Question 3

In the terms of our model, bias refers to the distance of our prediction
from the true value that we are trying to predict. A model with high
bias will oversimplify the model and not pay too much attention to the
training data.

Variance, on the other hand, refers to the variability of the
prediction. This can also be though of as how much the model would
change if different data was used. A model with high variance will very
tightly fit the training data and is not very flexible with new data.

The bias-variance tradeoff is a phenomena in machine learning models
that describes how increasing bias will decrease variance and vice
versa. In order to create a strong model, one must find a good balance
between bias and variance.

Squaring the terms in our model decreased the bias; it now slightly fits
the data better, however has increased the variance of the model.
