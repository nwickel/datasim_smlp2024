library("dplyr")
library("ggplot2")
library("lme4")
library("lmerTest")

## For plotting the hypothesis, create small dataframe without including variances
n_subjects <- 10

dat <- expand.grid(
  subj_id = c(paste("subj", stringr::str_pad(1:n_subjects, 2, "left",0), sep = "_")),
  trend = c(-1, -0.5, 0.5, 1  ), #trend = c(-0.25, -0.125, 0.125, 0.25),
  framing = c(-1, 1), # 1 = invasive, -1 = endangered
  spatial_dist = c(0,1),      #c("NonNeighbours","Neighbours"),
  n_regions = c(3,4)
)

##
# Fixed intercept
f_intercept <- 0.597

# Main effects
f_trend        <- 0.0
f_spatial_dist <- 0.0
f_framing      <- 0.0

# Interactions
f_trend_spatial_dist         <- 0.0
f_trend_framing              <- 0.0
f_framing_spatials_dist      <- 0.0
f_trend_spatial_dist_framing <- 0.02

dat$seg_agree <- 
  f_intercept +
  f_trend * dat$trend +
  f_spatial_dist * dat$spatial_dist +
  f_framing * dat$framing +
  f_trend_spatial_dist * dat$trend*dat$spatial_dist +
  f_trend_framing * dat$trend * dat$framing +
  f_trend_spatial_dist_framing * dat$trend * dat$spatial_dist * dat$framing

# Random effect subjects
subject_sd              <- 0.1
subject_trend_sd        <- 0.03
subject_framing_sd      <- 0.01
subject_spatial_dist_sd <- 0.01
residual_sd             <- 0.2

# Power simulation
ns    <- c(150, 175, 200)
pvals <- data.frame(matrix(nrow = 0, ncol = 9))
nsim  <- 2000

### ATTENTION: takes several hours to run
pwrs <- xfun::cache_rds({
 for (n in ns) {
 
   #create data frame
   dat <- expand.grid(
     subj_id      = c(paste("subj", stringr::str_pad(1:n, 2, "left",0), sep = "_")),
     trend        = c(-1, -0.5, 0.5, 1  ),
     framing      = c(-1, 1),  # 1 = invasive, -1 = endangered
     spatial_dist = c(0, 1),   # 0 = NonNeighbours, 1 = Neighbours,
     n_regions    = c(3, 4)
   )
 
 
   for (S in 1:nsim) {
 
     subject_intercepts <- data.frame(
       subj_id     = c(paste("subj", stringr::str_pad(1:n, 2, "left",0), sep = "_")),
       subj_interc = rnorm(n, mean = 0, sd =  subject_sd)
     )
 
     subject_slope_trend <- data.frame(
       subj_id     = c(paste("subj", stringr::str_pad(1:n, 2, "left",0), sep = "_")),
       subj_interc = rnorm(n, mean = 0, sd =  subject_trend_sd)
     )
     
     subject_slope_framing <- data.frame(
       subj_id     = c(paste("subj", stringr::str_pad(1:n, 2, "left",0), sep = "_")),
       subj_interc = rnorm(n, mean = 0, sd = subject_framing_sd)
     )
     
     subject_slope_spatial_dist <- data.frame(
       subj_id     = c(paste("subj", stringr::str_pad(1:n, 2, "left",0), sep = "_")),
       subj_interc = rnorm(n, mean = 0, sd =  subject_spatial_dist_sd)
     )
 
 
     dat <- dat |>
       group_by(subj_id) |>
       mutate("sim_{S}" := f_intercept +
                           # main effects
                           f_trend*trend +
                           f_spatial_dist * spatial_dist +
                           f_framing*framing +
                           # interactions
                           f_trend_spatial_dist * trend * spatial_dist +
                           f_trend_framing * trend * framing +
                           f_framing_spatials_dist * framing * spatial_dist +
                           f_trend_spatial_dist_framing * trend * spatial_dist * framing +
 
                           # random effects
                           subject_intercepts[cur_group_id(),"subj_interc"]+
                           subject_slope_trend[cur_group_id(),"subj_interc"]*trend+
                           subject_slope_framing[cur_group_id(),"subj_interc"]*framing+
                           subject_slope_spatial_dist[cur_group_id(),"subj_interc"]*spatial_dist+
                           rnorm(n(), mean = 0, sd = residual_sd)) |>
 
 
       ungroup()
 
   }
 
   sim <- data.frame(dat |> select(starts_with("sim")))
 
 
   dat$framing      <- as.factor(dat$framing)
   dat$spatial_dist <- as.factor(dat$spatial_dist)
 
 
   for (i in seq_len(nsim)) {
 
     m1 <- lmer(sim[, i] ~ framing + trend + spatial_dist +
                  trend:framing + trend:spatial_dist + framing:spatial_dist +
                  trend:framing:spatial_dist +
                  (trend + framing + spatial_dist | subj_id),
                  dat)
 
     pvals[nrow(pvals)+1,] <- c(n, summary(m1)$coef[,"Pr(>|t|)"])
 
   }
 
   colnames(pvals) <- c("n", rownames(summary(m1)$coef))
 
 
 }
 
 pvals |>
   mutate(across(-n, ~ (. < 0.05))) |>
   group_by(n) |>
   summarize(across(everything(), list(mean = mean)))

})


# save(pwrs, file = "EST_Spatial_pwrs2000_ef02_0412.RData")
# load(file = "EST_Spatial_pwrs2000_ef02_0412.RData")

ggplot(pwrs, aes(x = n)) +
  geom_line(aes(y = `framing1:trend:spatial_dist1_mean`,
                color = "Trend:Framing:Spatial Pattern"),
            linetype = "solid",
            linewidth = 1) +
  labs(x = "n", y = "Proportion of 2000 p-values < 0.05", color = "Parameter") +
  scale_x_continuous(breaks = pwrs$n) +
  scale_y_continuous(labels = scales::percent_format()) +
  theme_minimal()


