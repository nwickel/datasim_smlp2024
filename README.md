# Examples for Eighth Summer School on Statistical Methods for Linguistics and Psychology, 9-13 September 2024

## GLMM

The first example is a generalized linear mixed-effects model (GLMM) with
correct response as dependent variable, two manipulated independent variables
(representation: text vs. picture; veractiy: true vs. false) and two random
intercepts for subjects and items. All subjects saw all conditions.

```r
m1 <- glmer(resp ~ representation * veracity + (1|id) + (1|item), dat, family = binomial)
```

## LMM

The second example is a linear mixed-effects model (LMM) with segmentation
agreement as metric response variable and three manipulated variables (Trend
(metric): -1, -0.5, 0.5, 1; Framing: Invasive vs. Endangered; Spatial Pattern:
Clustered vs. Distributed). All subjects saw all conditions, so random
intercepts and random slopes for all variables are included in the model.

```r
m2 <- lmer(resp ~ framing * trend * spatialdist + (trend + framing + spatialdist | subj_id),
           data = dat)
```

# RvsJulia

After the summer school I started comparing results for power simulations
conducted in R and julia for the same mixed models. The scripts are in this
folder.

