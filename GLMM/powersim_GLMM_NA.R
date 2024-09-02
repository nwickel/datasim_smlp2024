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
pwrH2 <- function(sim) {
  m1 <- glmer(sim ~ representation * veracity + (1|id) + (1|item), dat,
              family = binomial)
  summary(m1)$coef["representationgraphic:veracityfalse", "Pr(>|z|)"]
}

pval <- xfun::cache_rds({
  sim <- simulate( ~ representation * veracity + (1|id) + (1|item),
                  newdata   = dat,
                  newparams = list(beta = fix, theta = ran),
                  family    = binomial,
                  nsim      = 500)
  sapply(sim, pwrH2)  # ATTENTION: Runs for quite some time!
})

# Power
mean(pval < 0.05) 

