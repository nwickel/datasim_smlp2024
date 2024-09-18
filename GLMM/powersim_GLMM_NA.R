# Load packages
library(lme4)
library(lattice)

set.seed(1451)

# Hypothesis: 0.1 probability difference for true vs. false for graphic representation
design <- data.frame(representation = factor(rep(c("text", "graphic"), each = 2),
                                             levels = c("text", "graphic")),
                     veracity = factor(rep(c("true", "false"), 2),
                                       levels = c("true", "false")),
                     prob = c(0.55, 0.55, 0.50, 0.40))

xyplot(prob ~ representation, data = design, groups = veracity,
       type = "b", ylim = 0:1, auto.key=list(space="top"))

# Fixed effects
X <- model.matrix( ~ representation * veracity, data = design)
fix <- as.vector(solve(X) %*% qlogis(design$prob))

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

# Power simulation

get_model <- function(sim) {
  glmer(sim ~ representation * veracity + (1|id) + (1|item), dat,
        family = binomial)
}

models <- xfun::cache_rds({
  sim <- simulate( ~ representation * veracity + (1|id) + (1|item),
                  newdata   = dat,
                  newparams = list(beta = fix, theta = ran),
                  family    = binomial,
                  nsim      = 1000)
  sapply(sim, get_model)  # ATTENTION: Runs for quite some time!
})

# Power
pval <- sapply(models,
  function(x) summary(x)$coef["representationgraphic:veracityfalse", "Pr(>|z|)"])
mean(pval < 0.05)

# Parameter recovery
mean(sapply(models, function(x) fixef(x)["representationgraphic:veracityfalse"]))
# -0.4042824

# Bootstrap SE
sd(sapply(models, fixef)["representationgraphic:veracityfalse", ])
# 0.1432599

# Mean SE of Wald test
mean(sapply(models, function(x) summary(x)$coef["representationgraphic:veracityfalse", 2]))
# 0.1394503

