Data simulation and example analysis for *Scene Visualization: Transfer
from Text to Visuals*
================
2024-09-02

``` r
library(lme4)
library(lattice)
set.seed(1614)
```

# Description

This is a simulated data set with accompanying data analysis which was
used as basis for a power simulation for an experiment at the
Leibniz-Institut für Wissensmedien (IWM).

Underlying idea of the study: People construct mental representations of
a scene while reading, meaning textual content is, e.g., translated into
visual mental representations. This is tested by switching the
presentation modality between the learning and the test phase. During
the learning phase, subjects are presented with a scene description in
text form. In the test phase, the scenes are presented either in text or
in graphical form.

Transfer of text-based modality to visual modality:

- Are participants able to identify information in a visual
  representation of a scene they read?

- How accurate is it?

Participants read 25 sentences that can be either true (15) or false
(10). Afterwards, they have to judge if a scene is true or false. This
scene is either presented as text or as a picture.

The dependent variable is correct answer: true sentence is classified as
true.

The manipulated conditions are:

- representation: text vs. picture

- veracity of sentence: true vs. false (indicated by color of the
  sentence)

<!--

## Short Description of Project

During reading, people create situation models that are non-linguistic mental
representations (e.g., graphical representations) of what the text is about
instead of a representation of the text itself. The purpose of this study is to
test if this modality transfer is testable. Therefore, we want to investigate
whether people are more effective at recognizing incorrect scene information
they receive while reading in the same modality (text-based presentation) as
compared to a different modality (graphical presentation) in the subsequent test
phase.

-->

# Hypothesis

Participants will be better in identifying *false* information in a
text-based representation than participants who must identify false
information in a graphical representation of the scene. No difference
for true information is expected.

``` r
# Hypothesis: 0.1 probability difference for true vs. false for graphic representation
design <- data.frame(representation = factor(rep(c("text", "graphic"), each = 2),
                                             levels = c("text", "graphic")),
                     veracity = factor(rep(c("true", "false"), 2),
                                       levels = c("true", "false")),
                     prob = c(0.55, 0.55, 0.50, 0.40))

xyplot(prob ~ representation, data = design, groups = veracity,
       type = "b", ylim = 0:1, auto.key = list(space = "top"))
```

<img src="datasim_GLMM_NA_files/figure-gfm/H2-1.png" style="display: block; margin: auto;" />

``` r
knitr::kable(design)
```

| representation | veracity | prob |
|:---------------|:---------|-----:|
| text           | true     | 0.55 |
| text           | false    | 0.55 |
| graphic        | true     | 0.50 |
| graphic        | false    | 0.40 |

# Data simulation

``` r
# Fixed effects
X <- model.matrix( ~ representation * veracity, data = design)
(fix <- as.vector(solve(X) %*% qlogis(design$prob)))
```

    ## [1]  0.2006707 -0.2006707  0.0000000 -0.4054651

``` r
# Random effects (taken from previous study)
ran <- c("id.(Intercept)" = 0.5, "item.(Intercept)" = 0.3)

# Simulate data
n <- 150

dat <- data.frame(
    id              = factor(rep(1:n, each = 25)),
    item            = factor(paste(rep(1:5, each = 25), 1:25, sep = ":")),
    representation  = factor(rep(c("text", "graphic"), each = 25)),
    veracity        = factor(rep(c("true", "false"), c(15, 10)))
)

dat$representation <- relevel(dat$representation, ref = "text")
dat$veracity <- relevel(dat$veracity, ref = "true")

dat$resp <- simulate( ~ representation * veracity + (1|id) + (1|item),
                     newdata   = dat,
                     newparams = list(beta = fix, theta = ran),
                     family    = binomial)$sim_1
```

    ## beta parameter vector not named: assuming same order as internal vector

## Descriptives

``` r
# Frequency table of observations per condition
(tab1 <- xtabs( ~ representation + veracity, dat))
```

    ##               veracity
    ## representation true false
    ##        text    1125   750
    ##        graphic 1125   750

``` r
# Frequency table of correct answers per condition
(tab2 <- xtabs(resp ~ representation + veracity, dat))
```

    ##               veracity
    ## representation true false
    ##        text     623   402
    ##        graphic  584   298

``` r
datm <- as.data.frame(tab2 / tab1)

xyplot(Freq ~ representation, data = datm, groups = veracity,
       type = "b", ylim = 0:1, auto.key = list(space = "top"))
```

<img src="datasim_GLMM_NA_files/figure-gfm/descriptives-1.png" style="display: block; margin: auto;" />

``` r
knitr::kable(datm)
```

| representation | veracity |      Freq |
|:---------------|:---------|----------:|
| text           | true     | 0.5537778 |
| graphic        | true     | 0.5191111 |
| text           | false    | 0.5360000 |
| graphic        | false    | 0.3973333 |

# Analysis with `lme4::glmer()`

``` r
m1 <- glmer(resp ~ representation * veracity + (1|id) + (1|item), dat,
            family = binomial)

summary(m1)
```

    ## Generalized linear mixed model fit by maximum likelihood (Laplace Approximation) [glmerMod]
    ##  Family: binomial  ( logit )
    ## Formula: resp ~ representation * veracity + (1 | id) + (1 | item)
    ##    Data: dat
    ## 
    ##      AIC      BIC   logLik deviance df.resid 
    ##   5076.9   5114.3  -2532.5   5064.9     3744 
    ## 
    ## Scaled residuals: 
    ##     Min      1Q  Median      3Q     Max 
    ## -1.7481 -0.9201  0.5899  0.8899  1.7613 
    ## 
    ## Random effects:
    ##  Groups Name        Variance Std.Dev.
    ##  id     (Intercept) 0.24245  0.4924  
    ##  item   (Intercept) 0.07805  0.2794  
    ## Number of obs: 3750, groups:  id, 150; item, 125
    ## 
    ## Fixed effects:
    ##                                     Estimate Std. Error z value Pr(>|z|)   
    ## (Intercept)                          0.23216    0.09024   2.573  0.01009 * 
    ## representationgraphic               -0.14920    0.11901  -1.254  0.20997   
    ## veracityfalse                       -0.07687    0.11056  -0.695  0.48688   
    ## representationgraphic:veracityfalse -0.45390    0.13949  -3.254  0.00114 **
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Correlation of Fixed Effects:
    ##             (Intr) rprsnt vrctyf
    ## rprsnttngrp -0.661              
    ## veracityfls -0.491  0.294       
    ## rprsnttngr:  0.306 -0.463 -0.624
