# Examples for Eighth Summer School on Statistical Methods for Linguistics and Psychology, 9-13 September 2024

## GLMM

The first example is a generalized linear mixed-effects model (GLMM) with
correct response as dependent variable, two manipulated independent variables
(representation: text vs. picture; veractiy: true vs. false) and two random
intercepts for subjects and items.

```{r}
m1 <- glmer(resp ~ representation * veracity + (1|id) + (1|item), dat, family = binomial)

```

## LMM

The second example is a linear mixed-effects model (LMM). Files to come...

