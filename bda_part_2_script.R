# Bayesian Data Analysis, Part 2
# Clay Ford
# UVA Library StatLab
# Spring 2024

library(rstanarm)
library(ggeffects)
library(ggplot2)


# multiple regression -----------------------------------------------------

# Example: model patient satisfaction scores as a function of age, anxiety
# level, and illness severity.

ps <- read.csv("https://raw.githubusercontent.com/clayford/BDA/master/data/patient_satisfaction.csv")
# Source: Applied Linear Statistical Models, 5th Edition (page 250)

# ps = patient satisfaction score (dependent variable)
# age = age of patient
# illness = severity of illness
# anxiety = anxiety level


# explore data
summary(ps)
pairs(ps)
hist(ps$ps)

# The traditional approach
lm1 <- lm(ps ~ age + illness + anxiety, data = ps)
summary(lm1)
coef(lm1)
sigma(lm1)
confint(lm1)


# Use model to simulate data and compare to observed data;
# This looks good
sim1 <- simulate(lm1, nsim = 50)
plot(density(ps$ps), ylim = c(0, 0.03))
for(i in 1:50)lines(density(sim1[[i]]), col = "grey80")

# or glm(ps ~ age + illness + anxiety, data = ps, family = gaussian)

# Bayesian approach
# - fit a simple additive model with default priors;
# - Model patient satisfaction as a weighted sum of age, illness and anxiety;
# - "family = gaussian" says we thing the error is normally distributed
mod1 <- stan_glm(ps ~ age + illness + anxiety, 
                 data = ps, 
                 family = gaussian)

# In the console we see output on the sampling procedure. The model is not fit
# with a closed-form math formula or numeric approximation but rather with a
# sophisticated sampling engine. If something goes wrong with the sampling, you
# should get a warning message in red saying not to trust the results.

# which priors were used? "adjusted prior" means the prior distribution was
# rescaled to be on the same range of the outcome variable. These are "weakly
# informative". They rule out extreme values, which helps with the sampling.
prior_summary(mod1)

# How are these adjusted scales obtained?
# http://mc-stan.org/rstanarm/articles/priors.html

# Intercept: 
# location = mean(y)
# scale = 2.5 * sd(y)
2.5 * sd(ps$ps)

# Coefficients: 
# location = 0
# scale = 2.5 * (sd(y)/sd(x_k)) (for kth coefficient)
2.5 * (sd(ps$ps)/sd(ps$age))
2.5 * (sd(ps$ps)/sd(ps$illness))
2.5 * (sd(ps$ps)/sd(ps$anxiety))

# Auxiliary: 
# rate = 1/sd(y)
1/sd(ps$ps)


# The base R curve() function makes it relatively easy to visualize priors.
# dnorm() is the normal density function

# prior for intercept
curve(dnorm(x, mean = 62, sd = 43), 
      from = 62 + -3*43, to = 62 + 3*43, 
      xlab = "prior for intercept")

# prior for age
curve(dnorm(x,mean = 0, sd = 4.83), 
      from = -3*4.83, to = 3*4.83,
      xlab = "prior for age")

# prior for sigma; notice we dexp()
curve(dexp(x, rate = 0.058), 
      from = 0, to = 6/0.058, 
      xlab = "prior for sigma")


# Making the default priors explicit in our model code:
mod1 <- stan_glm(ps ~ age + illness + anxiety,
                 data = ps,
                 family = gaussian,
                 prior_intercept = normal(62,43),
                 prior = normal(c(0,0,0),c(4.83,9.99,143.95)),
                 prior_aux = exponential(0.058))


# model summary; summary statistics of the posterior distributions;
# We want all Rhat < 1.1 (assessment of convergence)
# We want all n_eff > 1000 (n_eff = effective sample size)
summary(mod1)

# Some naive interpretation, judging by means of the distributions:

# - for every one year increase in age, expected patient satisfaction decreases
#   by about 1

# - for every one unit increase in the illness measure, expected patient
#   satisfaction decreases by about 0.4

# - for every one unit increase in the anxiety score, expected patient
#   satisfaction decreases by about 13

# - the intercept is the expected patient satisfaction for someone age 0, with
#   illness = 0 and anxiety = 0. Not useful.

# - sigma is the estimate of the standard deviation of the normal (gaussian)
#   distribution from which we assume the errors are "drawn".


# visualize posterior distributions. This is the objective of Bayesian modeling.
plot(mod1, plotfun = "dens")
plot(mod1, plotfun = "dens", pars = "age")
plot(mod1, plotfun = "dens", pars = c("age", "illness", "anxiety"))

# 90% credibility intervals of coefficients
posterior_interval(mod1)

# 95% credibility intervals of coefficients
posterior_interval(mod1, prob = 0.95)

# The model summary is summarizing 4000 samples; use the as.data.frame()
# function to create an object that contains the samples.
mod1_df <- as.data.frame(mod1)
summary(mod1_df)
dim(mod1_df)

# We can work with this object to make estimates such as...

# what is the probability the effect of anxiety is greater than 0
mean(mod1_df$anxiety > 0)

# what is the probability the effect of age is between -1.0 and -0.5
mean(mod1_df$age > -1.0 & mod1_df$age < -0.5)

# what is the probability the effect of illness is less than 0
mean(mod1_df$illness < 0)


# Is this a good model? Assess model fit with posterior predictive check. The
# dark line is the observed patient satisfaction data represented as a smooth
# distribution. The lighter lines are simulated patient satisfaction scores from
# our model. Our model should generate data that looks similar to our original
# data. They are generated using posterior_predict(). This looks like a good
# model!

# Each of the light blue lines is a prediction generated using a single draw of
# the model parameters from the posterior distribution.
pp_check(mod1)


# fit a model with custom prior distributions
# These are probably not good priors!
mod2 <- stan_glm(ps ~ age + illness + anxiety, 
                 data = ps, 
                 family = gaussian,
                 prior_intercept = normal(location = 100, 
                                          scale = 50), 
                 prior = normal(location = c(0, 10, 10), 
                                scale = c(5, 10, 10)), 
                 prior_aux = NULL) # flat, uniform prior

# evaluate model
prior_summary(mod2)
summary(mod2)
posterior_interval(mod2)
posterior_interval(mod1) # compare to mod1 that used default priors

# mod2 is more uncertain about the effect of anxiety



# CODE ALONG 1 ------------------------------------------------------------

# A commercial real estate company evaluates vacancy rates, square footage,
# rental rates, and operating expenses for commercial properties in a large
# city.

prop <- read.csv("https://raw.githubusercontent.com/clayford/BDA/master/data/properties.csv")
summary(prop)
pairs(prop)

# Can we model the rental rate as a function of the other variables?
# rate = rental rate (in thousands)
# age = age of property
# expenses = operating expenses and taxes
# vacancy = proportion of property vacant
# sqft = total square footage

lm2 <- lm(rate ~ age + expenses + vacancy + sqft, data = prop)
summary(lm2)
confint(lm2) |> round(3)

# (1) Fit a Bayesian model using default priors and review the posterior
# intervals. Name the model "pm1".



# (2) View the posterior distributions



# (3) Assess model fit with a posterior predictive check. 



# Back to presentation


# Multiple regression with interactions -----------------------------------


# fit a model with interactions
# perhaps we hypothesize the effect of anxiety depends on age;
# anxiety:illness means "allow anxiety and age to interact"
mod3 <- stan_glm(ps ~ age + illness + anxiety + anxiety:age, 
                 data = ps, 
                 family = gaussian)


# model summary
summary(mod3)

# visualize posterior distributions
plot(mod3, plotfun = "dens")

# assess model fit with posterior predictive check
pp_check(mod3)

# Effect plots

# visualize the interaction; there does not appear to be any interaction
ggpredict(mod3, terms = c("age", "anxiety")) |> plot()

# change the order to change which variable is on the x-axis
ggpredict(mod3, terms = c("anxiety", "age")) |> plot() 

# Running ggpredict without plot shows the values
ggpredict(mod3, terms = c("anxiety", "age"))

# visualize the interaction at ages = 30, 40, 50
ggpredict(mod3, terms = c("anxiety", "age [30,40,50]")) |> plot() 

# main effect plots
ggpredict(mod3, terms = "illness") |> plot()

# set age to 50 and anxiety to 2.3
ggpredict(mod3, terms = "illness", 
          condition = c("age" = 50, 
                        "anxiety" = 2.3)) |>
  plot()

# The default credibility ribbon is for the mean response; setting ppd = TRUE
# returns the predicted response.
ggpredict(mod3, terms = "illness", ppd = TRUE) |> plot()



# CODE ALONG 2 ------------------------------------------------------------

# Assess the effect of insulation and temp on gas consumption for heating a
# home.
data(whiteside, package = "MASS")

# Insul = A factor, before or after insulation
# Temp = the average outside temperature in degrees Celsius
# Gas = weekly gas consumption in 1000s of cubic feet

summary(whiteside)
ggplot(whiteside) +
  aes(x = Temp, y = Gas, color = Insul) +
  geom_point()


# (1) Fit a Bayesian model that models Gas as a function of Temp, Insul and
# their interaction. Use the default priors. Name the model gm1.



# (2) view the model summary



# (3) view the posterior distributions



# (4) create an effect plot to visualize the interaction.


# back to presentation


# logistic regression -----------------------------------------------------

# Let's analyze data from a double-blind clinical trial investigating a new
# treatment for rheumatoid arthritis. (From the vcd package)
arthritis <- read.csv("https://raw.githubusercontent.com/clayford/BDA/master/data/arthritis.csv")

# Treatment = factor indicating treatment (Placebo, Treated).
# Sex = factor indicating sex (Female, Male).
# Age = age of patient.
# Better = 0/1 integer indicating better (1) or not (0)

xtabs(~ Better + Treatment, data = arthritis)
xtabs(~ Better + Sex, data = arthritis)
stripchart(Age ~ Better, data = arthritis, method = "jitter")


# Model Better as a function of Treatment, Sex and Age
# Need to set family = binomial
# use default priors
arthritis.blm <- stan_glm(Better ~ Treatment + Sex + Age, 
                          data = arthritis,
                          family = binomial) 

# Look at default priors
prior_summary(arthritis.blm)

# summary of posterior distributions
summary(arthritis.blm)

# plot of posterior distributions
plot(arthritis.blm, plotfun = "dens")

# coefficients are on the log-odds scale. One interpretation is to pick a point
# estimate and exponentitate to get an odds ratio
coef(arthritis.blm)
exp(coef(arthritis.blm)[2])

# Odds of getting "better" are about 6 times higher for Treated, versus the odds
# of getting better when on Placebo. (holding other variables constant)

# check model fit; these are density curves for a 0/1 variable. Perhaps not too
# useful for logistic regression models.
pp_check(arthritis.blm)

# Effect plots

# Effect plots can help us visualize the model and get expected probabilities
# instead of log-odds.

# Get effect plots for all three predictors
ggpredict(arthritis.blm) |> plot()

# Take advice: "Consider using `terms="Age [all]"` to get smooth plots."
ggpredict(arthritis.blm, terms = "Age [all]") |> plot()


# CODE ALONG 3 ------------------------------------------------------------


# (1) Re-fit the Bayesian arthritis model with an interaction for Treatment and
# Age. Name the model "arthritis.blm2".


# (2) view the model summary



# (3) view the posterior distributions



# (4) create an effect plot to visualize the interaction. 



# WE'RE DONE!



# Appendix: using model to make predictions -------------------------------

# Using posterior_predict

# Drawing from the posterior predictive distribution at interesting values of
# the predictors lets us see how a manipulation of a predictor affects the
# outcome.

# Find expected patient satisfaction for someone age = 35, illness = 50, and
# anxiety = 2 for mod1. Use the posterior predictive distribution to draw
# samples.
pp <- posterior_predict(mod1, newdata = data.frame(age = 35, 
                                                   illness = 50, 
                                                   anxiety = 2), 
                        draws = 1000)
dim(pp)
mean(pp)
summary(pp)
quantile(pp, probs = c(0.025, 0.975))



# Predicted response (0/1) for Male, age 45, on Treatment
pp.arthritis <- posterior_predict(arthritis.blm, 
                                  newdata = data.frame(Treatment = "Treated", 
                                                       Sex = "Male", 
                                                       Age = 45),
                                  draws = 1000)
head(pp.arthritis)
mean(pp.arthritis)

# predicted probability for Male, age 45, on Treatment
pl.arthritis <- posterior_epred(arthritis.blm,
                                newdata = data.frame(Treatment = "Treated",
                                                     Sex = "Male", 
                                                     Age = 45),
                                draws = 1000)
head(pl.arthritis)
predictive_interval(pl.arthritis)


# Appendix: quickly visualize all normal priors ---------------------------

# fit a model
mod1 <- stan_glm(ps ~ age + illness + anxiety, 
                 data = ps, 
                 family = gaussian)

# save prior summary
priors <- prior_summary(mod1)

# one way to visualize all predictor priors: use the walk2() function in the
# purrr package.
library(purrr)

# The walk2() function allows you to "walk" a function over two inputs and get
# the side effect, in this case a plot. The result should be 3 plots.
walk2(priors$prior$adjusted_scale, 
             attr(mod1$terms, "term.labels"),
             function(y, z)curve(dnorm(x, sd = y), 
                                 from = -3*y, 
                                 to = 3*y, 
                                 xlab = z))



# Appendix: bonus analysis ------------------------------------------------


# prostate cancer data (from Applied Linear Statistical Models, 5th ed)
# A study on 97 men with prostate cancer who were due to receive a 
# radical prostatectomy.

# psa - prostate specific antigen (PSA)
# volume - cancer volume
# weight - prostate weight (in grams)
# age - age of patient
# bph - benign prostatic hyperplasia amount
# svi - seminal vesicle invasion (1 = Yes, 0 = No)
# cap.pen - capsular penetration
# gleason.score - Gleason score (grade of disease)

# can we model PSA as a linear function of other variables? 
# Is there a "best" model?

pros <- read.csv("https://raw.githubusercontent.com/clayford/BDA/master/data/prostate.csv")
str(pros)
summary(pros)
pros$svi <- factor(pros$svi, labels = c("No","Yes"))
summary(pros)

hist(pros$psa)
pros$log.psa <- log(pros$psa)
hist(pros$log.psa)

# pairs plot
pairs(pros[,c("log.psa", "volume", "weight", "age", "bph", "cap.pen")])
subset(pros, weight > 400) # influential observation?
boxplot(log.psa ~ svi, data = pros)
boxplot(log.psa ~ gleason.score, data = pros)

# fit a Bayesian multiple regression model using rstanarm default priors
bm1 <- stan_glm(log.psa ~ volume + weight + age + bph + 
                  svi + cap.pen + gleason.score, 
                data = pros,
                family = gaussian)

# check the default priors
prior_summary(bm1)
priors <- prior_summary(bm1)

# check convergence
plot(bm1, plotfun = "trace")

# look at posterior distributions for model coefficients
plot(bm1, plotfun = "dens")

# check model fit
pp_check(bm1)

# model summary
summary(bm1)


# add interactions
bm2 <- stan_glm(log.psa ~ volume*svi + weight*svi + age + bph + 
                  cap.pen + gleason.score, 
                data = pros,
                family = gaussian)

summary(bm2)
pp_check(bm2)

plot(ggpredict(bm2, terms = c("volume", "svi")))
plot(ggpredict(bm2, terms = c("weight", "svi")))


# check for outliers/influential points
plot(loo(bm1), label_points = TRUE)
plot(loo(bm2), label_points = TRUE)

loo_compare(loo(bm1), loo(bm2))

loo_compare(loo(bm1, k_threshold = 0.7), 
            loo(bm2, k_threshold = 0.7))



# Appendix: pp_check by hand ----------------------------------------------

# pp_check() allows us to assess model fit with a posterior predictive check. In
# this section we show how to do it by hand using base R graphics and ggplot.

# Here's pp_check for mod1. The dark line is the observed patient satisfaction
# data represented as a smooth distribution. The lighter lines are simulated
# patient satisfaction scores from our model. This looks like a good model!
pp_check(mod1)

# The dark line is a density estimate of our observed data. We can get this "by
# hand" as follows:
plot(density(ps$ps))

# or using ggplot
ggplot(ps) +
  aes(x = ps) +
  geom_density() + 
  theme_gray()

# Use posterior_predict() to simulate predictions from your model. The posterior
# predictive distribution is the distribution of the outcome implied by the
# model.
pp.out <- posterior_predict(mod1, draws = 30)

# 30 draws of 46 observations
dim(pp.out)

# create our own posterior predictive check
d <- density(ps$ps)
plot(d, ylim = c(0, 1.5*max(d$y)), lwd = 2)
apply(pp.out, 1, function(x)lines(density(x), col = "grey80"))

# or using ggplot
pp.DF <- tidyr::gather(as.data.frame(t(pp.out)), 
                       key = draw, value = value)
ggplot() + 
  geom_density(aes(x = value, group = draw), pp.DF, color = "lightblue") +
  geom_density(aes(x = ps), ps) + 
  theme_gray()



# Appendix: using loo for model checking ----------------------------------


# check for influential data using loo(); compute approximate leave-one-out
# (loo) cross-validation

# Here we do it for the patient satisfaction model

# p_loo = estimated effective number of parameters (should be similar to
#         specified model); higher than specified model is not good.
mod1.loo <- loo(mod1)
mod1.loo

# For model-checking we want "All Pareto k estimates are good (k < 0.5)"

# associated plot; 
# This looks good.
plot(mod1.loo, label_points = TRUE)

# "Using the label_points argument will label any k values larger than 0.7 with
# the index of the corresponding data point. These high values are often the
# result of model misspecification and frequently correspond to data points that
# would be considered 'outliers' in the data and surprising according to the
# model"

# vignette("loo2-example", package = "loo")


# Appendix: model comparison ----------------------------------------------

## Model comparison

# In traditional statistics, models are often compared using hypothesis tests or
# information criteria, such as AIC.

# In Bayesian statistics, the Pareto Smoothed Importance-Sampling Leave-One-Out
# cross-validation (PSIS-LOO) is often used.

# It is relatively easy to implement with the `loo()` and `loo_compare()`
# functions in the `rstanarm` package.

# Let's fit a few different models for the Arthritis data
# main effects only
arthritis.blm1 <- stan_glm(Better ~ Treatment + Sex + Age,
                           data = arthritis,
                           family = binomial) 

# all two-way interactions
arthritis.blm2 <- stan_glm(Better ~ (Treatment + Sex + Age)^2,
                           data = arthritis,
                           family = binomial) 

# only two-way interactions for Treatment
arthritis.blm3 <- stan_glm(Better ~ Treatment * Sex + Treatment * Age,
                           data = arthritis,
                           family = binomial) 


# compare these models using loo() and loo_compare()

# First we just use loo()
loo(arthritis.blm1)
loo(arthritis.blm2)
loo(arthritis.blm3)

# elpd_loo = expected log predictive density.
# p_loo = effective number of parameters; aka overfitting penalty
# looic = LOO information criterion; -2(elpd_loo)

# Notice the warnings. Some observations are considered "outliers" in the data
# and surprising according to the model. The warning message gives us helpful
# advice: "We recommend calling 'loo' again with argument 'k_threshold = 0.7'"

loo(arthritis.blm2, k_threshold = 0.7)
loo(arthritis.blm3, k_threshold = 0.7)


# Compare all models using loo_compare. The "best" model is listed first. Each
# subsequent comparison is to that model.

# The output reports the difference in expected log predictive density (ELPD)
# along with the standard error of the difference.

# The first model listed has the largest ELPD. Each subsequent model is compared
# to the first model.

# The standard error of the difference, `se_diff`, gives us some idea of how
# certain that difference is. If `se_diff` is bigger than `elpd_diff`, then we
# shouldn't be so sure the first model is necessarily "better".

loo_compare(loo(arthritis.blm1),
            loo(arthritis.blm2, k_threshold = 0.7),
            loo(arthritis.blm3, k_threshold = 0.7))

# It appears model 1 may be no different than model 2. Notice the large standard
# error on the difference. Model 1 appears to be preferable to model 3.


# The expected log predictive density (ELPD) is basically a scoring rule to
# assess how well a model predicts new data.

# When comparing models, models with higher ELPD are closer to the "true" data
# generating process.

# Remember, "expected" is the key word. These are just estimates. Pay attention
# to the standard error of the difference (`se_diff`)


# Example code for comparing multiple models with a similar name.
# - ".blm[0-9]$" is a regular expression that means "ends with .blm and a number"
# - mget gets multiple objects from the memory by name
# - lapply loo to the objects
# - loo_compare accepts a list of loo objects

loo_compare(lapply(mget(ls(pattern = ".blm[0-9]$")), loo))

# Fit the following models using the patient satisfaction data:
ps_mod1 <- stan_glm(ps ~ age + illness + anxiety, data = ps, family = gaussian)
ps_mod2 <- update(ps_mod1, . ~ . - age, data = ps)
ps_mod3 <- update(ps_mod1, . ~ . - anxiety, data = ps)
ps_mod4 <- update(ps_mod1, . ~ . - illness, data = ps)
ps_mod5 <- update(ps_mod1, . ~ . - age - illness, data = ps)
ps_mod6 <- update(ps_mod1, . ~ . - anxiety - illness, data = ps)
ps_mod7 <- update(ps_mod1, . ~ . - age - anxiety, data = ps)

# compare the models using loo.
# Use the lapply/mget code above with the regular expression "^ps_"

loo_compare(lapply(mget(ls(pattern = "^ps_")), loo))

## END OF SCRIPT