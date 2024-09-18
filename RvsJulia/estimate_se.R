library(lme4)

simdat <- expand.grid(subj = 1:100, item = 1:10, cond = c("cond1", "cond2"))
simdat <- simdat[order(simdat$subj), ]

#--------------- LMM ---------------

beta <- c(500, 10)
sigma <- 200
theta <- c(20, 10) / sigma

sim <- simulate( ~ 1 + cond + (1 | subj) + (1 | item), newdata = simdat,
                newparams = list(beta = beta, sigma = sigma, theta = theta),
                nsim = 1000)

fit_model <- function(sim) {
  lmer(sim ~ 1 + cond + (1 | subj) + (1 | item), simdat)
}

models <- lapply(sim, fit_model)

est <- sapply(models, fixef)

# Parameter recovery
mean(est["condcond2", ])
# 9.984448

# Bootstrap SE
sd(est["condcond2", ])
# 9.296658

# Mean SE of Wald test
mean(sapply(models, function(x) summary(x)$coef[2, 2]))
# 8.94772

#--------------- GLMM ---------------

beta <- c(2, 1.5)
theta <- c(.5, .3)

sim <- simulate( ~ 1 + cond + (1 | subj) + (1 | item), newdata = simdat,
                newparams = list(beta = beta, theta = theta),
                family = binomial, nsim = 1000)

fit_model <- function(sim) {
  glmer(sim ~ 1 + cond + (1 | subj) + (1 | item), simdat, family = binomial)
}

models <- lapply(sim, fit_model)

est <- sapply(models, fixef)

# Parameter recovery
mean(est["condcond2", ])
# 1.516869

# Bootstrap SE
sd(est["condcond2", ])
# 0.2031714

# Mean SE of Wald test
mean(sapply(models, function(x) summary(x)$coef[2, 2]))
# 0.180879

