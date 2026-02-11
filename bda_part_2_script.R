# Bayesian Data Analysis, Part 2
# Clay Ford
# UVA Library StatLab
# Spring 2026

library(rstanarm)
library(ggeffects)
library(ggplot2)
library(carData)

# multiple regression -----------------------------------------------------

# Example: model patient satisfaction scores as a function of age, anxiety
# level, and illness severity.

ps <- read.csv("https://raw.githubusercontent.com/clayford/BDA/master/data/patient_satisfaction.csv")
# Source: Applied Linear Statistical Models, 5th Edition (Kutner et al, p. 250)

# ps = patient satisfaction score (dependent variable)
# age = age of patient
# illness = severity of illness
# anxiety = anxiety level


# explore data
summary(ps)
pairs(ps)
hist(ps$ps, freq = FALSE)
lines(density(ps$ps))

# The traditional approach
lm1 <- lm(ps ~ age + illness + anxiety, data = ps)
# or glm(ps ~ age + illness + anxiety, data = ps, family = gaussian)

summary(lm1)
coef(lm1)
sigma(lm1)
confint(lm1)

# Bayesian approach

# - fit a simple additive model with default priors;
# - Model patient satisfaction as a weighted sum of age, illness and anxiety;
# - "family = gaussian" says we think the dependent variable is conditionally 
#    normally distributed.
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
      main = "prior for intercept", xlab = "")

# prior for age
curve(dnorm(x,mean = 0, sd = 4.83), 
      from = -3*4.83, to = 3*4.83,
      main = "prior for age", xlab = "")

# prior for sigma; notice we dexp()
curve(dexp(x, rate = 0.058), 
      from = 0, to = 6/0.058, 
      main = "prior for sigma", xlab = "")


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

# The mean_PPD is the sample average posterior predictive distribution of the
# outcome variable. Think of it as the sample average of the curves you see when
# running `pp_check()`. Hopefully the mean_PPD is similar to the mean of the
# response variable. If not, something may be wrong.

# visualize posterior distributions. This is the objective of Bayesian modeling.
plot(mod1, plotfun = "dens")
plot(mod1, plotfun = "dens", pars = c("age", "illness", "anxiety"))


# Is this a good model? Assess model fit with posterior predictive check. 
pp_check(mod1)

# The dark line is the observed patient satisfaction data represented as a
# smooth distribution. The lighter lines are simulated patient satisfaction
# scores from our model. Our model should generate data that looks similar to
# our original data. They are generated using posterior_predict(). This looks
# like a good model!

# Each of the light blue lines is a prediction generated using a single draw of
# the model parameters from the posterior distribution.

# see "Appendix: pp_check() by hand" for how this function works


# Some naive interpretation, judging by medians of the distributions:
coef(mod1)

# AGE
# the average difference in patient satisfaction, comparing two people with
# equal anxiety and illness but one year difference in age, is about -1.

# ILLNESS
# the average difference in patient satisfaction, comparing two people with
# equal anxiety and age but one unit difference in illness, is about -0.4.

# ANXIETY
# the average difference in patient satisfaction, comparing two people with
# equal age and illness but one unit difference in anxiety is about -13.

# INTERCEPT
# the intercept is the expected patient satisfaction for someone age 0, with
# illness = 0 and anxiety = 0. Not useful.

# But remember these coefficients are just single summary measures of entire
# probability distributions!

# 90% credibility intervals of coefficients
# Why is 90% the default; "Computational stability: 90% intervals are more
# stable than 95% intervals"
posterior_interval(mod1)

# 95% credibility intervals of coefficients
posterior_interval(mod1, prob = 0.95)

# The model summary is summarizing 4000 samples; use the as.data.frame()
# function to create an object that contains the samples.
mod1_df <- as.data.frame(mod1)
dim(mod1_df)

# We can work with this object to make estimates such as...

# what is the probability the anxiety coefficient is less than 0
mean(mod1_df$anxiety < 0)

# what is the probability the age coefficient is between -1.0 and -0.5
mean(mod1_df$age > -1.0 & mod1_df$age < -0.5)

# what is the probability the illness coefficient is less than 0
mean(mod1_df$illness < 0)



# CODE ALONG 1 ------------------------------------------------------------

# A commercial real estate company evaluates vacancy rates, square footage,
# rental rates, and operating expenses for commercial properties in a large
# city.

# Can we model the rental rate as a function of the other variables?
# rate = rental rate (in thousands)
# age = age of property
# expenses = operating expenses and taxes
# vacancy = proportion of property vacant
# sqft = total square footage

prop <- read.csv("https://raw.githubusercontent.com/clayford/BDA/master/data/properties.csv")
summary(prop)
pairs(prop)

lm2 <- lm(rate ~ age + expenses + vacancy + sqft, data = prop)
summary(lm2)
confint(lm2) |> round(3)

# (1) Fit a Bayesian model using default priors and review the posterior
# intervals. Name the model "pm1".



# (2) View the posterior distributions



# (3) Assess model fit with a posterior predictive check. 


# Back to presentation


# Multiple regression with interactions -----------------------------------


# fit a model with interactions using default priors;
# perhaps we hypothesize the effect of illness depends on age;
# age:illness means "allow illness and age to interact"
mod3 <- stan_glm(ps ~ age + illness + anxiety + age:illness, 
                 data = ps, 
                 family = gaussian)

prior_summary(mod3)
priors <- prior_summary(mod3)
priors$prior

# model summary
summary(mod3)

# visualize posterior distributions
plot(mod3, plotfun = "dens")

# assess model fit with posterior predictive check
pp_check(mod3)

# Effect plots

# visualize the interaction; there does not appear to be any interaction
ggpredict(mod3, terms = c("age", "illness")) |> plot()

# change the order to change which variable is on the x-axis
ggpredict(mod3, terms = c("illness", "age")) |> plot() 

# visualize the interaction at ages = 30, 40, 50
ggpredict(mod3, terms = c("illness", "age [30,40,50]")) |> plot() 

# main effect plots
ggpredict(mod3, terms = "illness") |> plot()

# set age to 50 and anxiety to 2.3
ggpredict(mod3, terms = "illness", 
          condition = c("age" = 50, 
                        "anxiety" = 2.3)) |>
  plot()

# Another interaction example

# The carData package contains data on the prestige of Canadian occupations from
# the early 1970s. “Prestige” is measured on a scale from 0 - 100, where higher
# values mean higher prestige.

summary(Prestige[,c("prestige", "education", "income", "type")])

# Model prestige as a function of income, type of occupation, and years of
# education, with an interaction between income and type of occupation.

pmod <- stan_glm(prestige ~ income * type + education, 
                 data = Prestige, 
                 family = gaussian, seed = 2026)
pmod
summary(pmod)

# Visualize interactions
ggpredict(pmod, terms = c("income", "type")) |> plot()

# Notice the wide intervals for bc and wc at higher incomes. That's because those professions don't have observations at those incomes
stripchart(income ~ type, data = Prestige)

# Only make predictions for income ranging from 700 to 10000 by 100.
ggpredict(pmod, terms = c("income[700:10000 by=100]", "type")) |> plot()

# Visualize the fixed effect of education
ggpredict(pmod, terms = "education") |> plot()

# How does ggpredict() calculate these values?
# See "Appendix: how ggpredict() calculates values"



# CODE ALONG 2 ------------------------------------------------------------

# Assess the effect of insulation and temp on gas consumption for heating a
# home. "A data set collected in the 1960s by Mr Derek Whiteside of the UK
# Building Research Station."
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
arthritis$Treatment <- factor(arthritis$Treatment)
arthritis$Sex <- factor(arthritis$Sex)


# Treatment = factor indicating treatment (Placebo, Treated).
# Sex = factor indicating sex (Female, Male).
# Age = age of patient.
# Better = 0/1 integer indicating better (1) or not (0)

xtabs(~ Better + Treatment, data = arthritis)
xtabs(~ Better + Treatment + Sex, data = arthritis)

# Traditional model fit using glm()
m <- glm(Better ~ Treatment + Sex + Age,
         data = arthritis,
         family = binomial) 
summary(m)

# Bayesian approach
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
# estimate and exponentiate to get an odds ratio
coef(arthritis.blm) # median of posterior dist'n
exp(coef(arthritis.blm)["TreatmentTreated"])

# Odds of getting "better" are about 6 times higher for Treated, versus the odds
# of getting better when on Placebo. (holding other variables constant)

# get the posterior interval of the odds ratios
posterior_interval(arthritis.blm) |> exp()

# Odds of getting "better" appear to be at least 2.5 times higher for Treated,
# versus the odds of getting better when on Placebo. (holding other variables
# constant)

# check model fit; these are density curves for a 0/1 variable. Perhaps not too
# useful for logistic regression models.
pp_check(arthritis.blm)

# An alternative is to use the "stat" and "bars" plot functions.

# The "stat" plot function allows us to look at Pr(y = 1) by comparing the
# proportion of 1s in the data vs the proportions of 1s in the posterior
# predictive distribution:
pp_check(arthritis.blm, plotfun = "stat")

# The "bars" function allows us to look at the distribution of 0s and 1s by
# comparing the proportion of 0s and 1s in the data vs the proportions of 0s and
# 1s in the posterior predictive distribution:
pp_check(arthritis.blm, plotfun = "bars")

# Both of these plots are encouraging.

# Effect plots

# Effect plots can help us visualize the model and get expected probabilities
# instead of log-odds.

# Get effect plots for all three predictors
ggpredict(arthritis.blm) |> plot()

# Take advice: "Consider using `terms="Age [all]"` to get smooth plots."
ggpredict(arthritis.blm, terms = "Age [all]") |> plot()

# Effect plot for treatment
ggpredict(arthritis.blm, terms = "Treatment") |> plot()

# This plot is for Females, age 57
ggpredict(arthritis.blm, terms = "Treatment")

# Effect plot for Males, age 57
ggpredict(arthritis.blm, terms = "Treatment", 
          condition = c(Sex = "Male")) |> plot()



# CODE ALONG 3 ------------------------------------------------------------


# (1) Re-fit the Bayesian arthritis model with an interaction for Treatment and
# Age. Name the model "arthritis.blm2".



# (2) view the model summary



# (3) view the posterior distributions



# (4) create an effect plot to visualize the interaction. 




# back to presentation


# Model comparison --------------------------------------------------------

# Traditional approach uses test statistics and/or AIC/BIC;
# Fit three progressively more complex models and compare
m1 <- glm(Better ~ Treatment, 
          data = arthritis, 
          family = binomial) 
m2 <- glm(Better ~ Treatment + Sex, 
          data = arthritis, 
          family = binomial) 
m3 <- glm(Better ~ Treatment + Sex + Age, 
          data = arthritis, 
          family = binomial) 

# compare using partial F tests. Two tests: 
# NULL of test 1: model 1 = model 2
# NULL of test 2: model 2 = model 3
anova(m1, m2, m3)

# compare using information criteria
AIC(m1, m2, m3)
BIC(m1, m2, m3)

# Model 3 is selected as "best" of the three

# Can also use Cross Validation to compare models
# Leave one out (LOO)
# common cost function: mean((obs - predicted)^2), aka MSE
# smaller cost means a better performing model

# First get MSE for model tested with same data to fit model
# too optimistic
mean((arthritis$Better - predict(m1, type = "response"))^2)

# Now get MSE using LOO CV

# vector to store difference squared
ds <- numeric(length = nrow(arthritis))

# for loop to run LOO CV
for(i in 1:nrow(arthritis)){
  m <- glm(Better ~ Treatment,
            data = arthritis,
            subset = -i,       # each time leave out the ith obs
            family = binomial) 

    # fitted value for the ith obs left out of model
  fit <- predict(m, newdata = arthritis[i,], type = "response")

    # save difference squared
  ds[i] <- (arthritis$Better[i] - fit)^2
}

# calculate LOO CV
mean(ds)

# A faster way using the cv.glm() function in the boot package
# does LOO CV by default; set K = 10 to do 10-fold CV
library(boot)
cv.m1 <- cv.glm(data = arthritis, glmfit = m1)
cv.m1$delta # second value is "bias corrected"

cv.m2 <- cv.glm(data = arthritis, glmfit = m2)
cv.m3 <- cv.glm(data = arthritis, glmfit = m3)

# Third model also selected by cross validation
rbind(m1 = cv.m1$delta[1], 
      m2 = cv.m2$delta[1], 
      m3 = cv.m3$delta[1])

# We can also use LOO cross validation for Bayesian models 
# Refit Bayesian models with default priors
bm1 <- stan_glm(Better ~ Treatment,
                data = arthritis,
                family = binomial) 
bm2 <- stan_glm(Better ~ Treatment + Sex,
                data = arthritis,
                family = binomial) 
bm3 <- stan_glm(Better ~ Treatment + Sex + Age,
                data = arthritis,
                family = binomial) 

# Using the loo() function to perform LOO CV
loo1 <- loo(bm1)
loo2 <- loo(bm2)
loo3 <- loo(bm3)

# compare all CV results
loo_compare(loo1, loo2, loo3)

# can also name models; need to include in a list() object
loo_compare(list("model 1" = loo1, "model 2" = loo2, "model 3" = loo3))

# Model listed first is the "best" of the three
# elpd_diff is the difference in expected predictive accuracy
# ELPD = expected log pointwise predictive density
# "differences smaller than 4 are hard to distinguish from noise" 
# (Gelman et al 2020)

# printing the loo() result shows additional details
loo3

# elpd_loo, aka "estimated log score" 
# similar to MSE used above with traditional models
# In this case, higher is better

# looic = -2 * elpd_loo (similar to deviance)
# p_loo = estimated "effective number of parameters". 

# If p_loo < N and p_loo < p (p = total number of parameters), then model is
# "well behaved". 

# If p_loo > N or p_loo > p, then model has weak predictive capability and may
# indicate model misspecification.

# Pareto k diagnostic estimates how far an individual leave-one-out distribution
# is from the full distribution. It's a way to check for outliers/influence.

# From the loo documentation:
# "Pareto k is also useful as a measure of influence of an observation. Highly
# influential observations have high k values."

# plot() a loo object can help identify outliers with respect to model 
# PSIS = Pareto smoothed importance-sampling
plot(loo3)

# fit model that has "outliers"
# fit all two way interactions;
# notice the seed argument to make results replicable
bm4 <- stan_glm(Better ~ (Treatment + Sex + Age)^2,
                data = arthritis,
                family = binomial, seed = 555)
bm4

# perform LOO CV
loo4 <- loo(bm4)
loo4
plot(loo4)
plot(loo4, label_points = TRUE)
arthritis[52,]

# Only male on Placebo who reported getting "better"

# following the directions in the warning does literal LOO CV for observation 52 
loo4a <- loo(bm4, k_threshold = 0.7)
loo4a
plot(loo4a) # obs 52 no longer plotted

# No Pareto k value is calculated for obs 52
loo4a$diagnostics$pareto_k[52]

# Compare to model bm3
loo_compare(loo3, loo4a)

# See also the loo package glossary
# ?`loo-glossary`


# YOUR TURN #4 ------------------------------------------------------------

# Fit the following models using the patient satisfaction data:
ps_mod1 <- stan_glm(ps ~ age + illness + anxiety, data = ps, family = gaussian)
ps_mod2 <- update(ps_mod1, . ~ . - age, data = ps)
ps_mod3 <- update(ps_mod1, . ~ . - anxiety, data = ps)
ps_mod4 <- update(ps_mod1, . ~ . - illness, data = ps)

# compare the models using LOO CV




# WE'RE DONE!

# Thanks for coming. Email statlab@virginia.edu if you would like to talk more
# about your research or statistics in general.

# Appendix: pp_check() by hand --------------------------------------------

# Recall the graphical posterior predictive check we can perform with
# pp_check(). If a model is a good fit then we should be able to use the model
# to generate data that looks like the data we observed. The light blue lines
# are the several sets of model-generated data.
pp_check(mod1)

# Here's one way to replicate pp_check() by hand.
mod1_df <- as.data.frame(mod1)  
nreps <- 30  
i <- sample(4000, nreps) 
s <- mod1_df[i,]
mat <- matrix(NA, nrow = nreps, ncol = nrow(ps))
for(i in 1:nreps){
  mat[i,] <- s[i, "(Intercept)"] + 
    s[i,"age"]*ps$age + 
    s[i, "illness"]*ps$illness + 
    s[i,"anxiety"]*ps$anxiety + 
    rnorm(nrow(ps), mean = 0, sd = s[i,"sigma"])
}

# create the plot
plot(density(ps$ps), ylim = c(0, 0.03), lwd = 2)
apply(mat, 1, function(x)lines(density(x), col = "powderblue"))

# the posterior_predict() function makes this easier.
post_pred <- posterior_predict(mod1, draws = 30)
plot(density(ps$ps), ylim = c(0, 0.03))
apply(post_pred, 1, function(x)lines(density(x), col = "powderblue"))

# Can also use ggplot, but need to reshape the data first 
pp.DF <- tidyr::pivot_longer(as.data.frame(t(post_pred)),
                             cols = everything(),
                             names_to = "draw", 
                             values_to = "value")
ggplot() + 
  geom_density(aes(x = value, group = draw), pp.DF, color = "lightblue") +
  geom_density(aes(x = ps), ps) + 
  theme_gray()


# Appendix: using model to make predictions -------------------------------

# Using posterior_predict()

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




# Appendix: comparing multiple models with a similar name -----------------

# Fit the following models using the patient satisfaction data:
ps_mod1 <- stan_glm(ps ~ age + illness + anxiety, data = ps, family = gaussian)
ps_mod2 <- update(ps_mod1, . ~ . - age, data = ps)
ps_mod3 <- update(ps_mod1, . ~ . - anxiety, data = ps)
ps_mod4 <- update(ps_mod1, . ~ . - illness, data = ps)
ps_mod5 <- update(ps_mod1, . ~ . - age - illness, data = ps)
ps_mod6 <- update(ps_mod1, . ~ . - anxiety - illness, data = ps)
ps_mod7 <- update(ps_mod1, . ~ . - age - anxiety, data = ps)

# Do LOO CV on all models using lapply()
loo_compare(lapply(mget(ls(pattern = "^ps_")), loo))

# - "^ps_" is a regular expression that means "begins with ps"
# - ls() lists objects in memory
# - mget() gets multiple objects from memory by name
# - lapply() applis loo() to the objects
# - loo_compare() accepts a list of loo objects


# Appendix: calculate ELPD using same data as used to fit model -----------

# Calculate ELPD for model bm3 with same data used to fit model
bm3 <- stan_glm(Better ~ Treatment + Sex + Age,
                data = arthritis,
                family = binomial) 

# First, get log posterior predictive densities using log_lik()
ll_bm3 <- log_lik(bm3)

# Next exponentiate the columns, take the mean, and take the log again
ppd_all <- log(apply(ll_bm3, 2, function(x)mean(exp(x))))

# First 6
head(ppd_all)

# summing produces the ELPD, a type of cost, similar to MSE
sum(ppd_all)

# Compare to ELPD when calculated using LOO CV
# Notice the LOO version is lower (less optimistic)
loo3 <- loo(bm3)
loo3

# extract the LOO versions of the pointwise predictive densities;
# each observation's pointwise predictive density is calculated assuming that
# observation was left out of the model
ppd_loo <- loo3$pointwise[,"elpd_loo"]

# Notice the LOO ppd is lower (less optimistic) for each obs
head(cbind(ppd_all, ppd_loo))

# Summing ppd_loo yields the elpd_loo
sum(ppd_loo)
loo3

# Again, notice this is much higher than the elpd produced from
sum(ppd_all)


# Appendix: posterior predictive check for traditional models -------------

# We can do something similar to a posterior predictive check for traditional
# models as follows.

# Recall this model:
lm1 <- lm(ps ~ age + illness + anxiety, data = ps)
coef(lm1)

# simulate a single set of patient satisfaction scores using model coefficients.
y <- rnorm(n = nrow(ps), 
           mean = 158.491 + -1.142*ps$age + -0.443*ps$illness + 
             -13.47*ps$anxiety,
           sd = 10.058)

# how does simulated data compare to observed data?
plot(density(ps$ps), ylim = c(0, 0.03))
lines(density(y), col = "blue")

# use the simulate() function to make this easier. Do it 50 times.
# This looks good
sim1 <- simulate(lm1, nsim = 50)
plot(density(ps$ps), ylim = c(0, 0.03))
for(i in 1:50)lines(density(sim1[[i]]), col = "powderblue")

# The difference between this and Bayesian PP checks is that the Bayesian
# approach uses different samples from the posteriors to generate the model
# formula each time, whereas the traditional approach uses the same set of
# coefficients.


# Appendix: how ggpredict() calculates values -----------------------------

# How does ggpredict() calculate these values? What is it actually predicting?


# Recall this model:
pmod <- stan_glm(prestige ~ income * type + education, 
                 data = Prestige, 
                 family = gaussian, seed = 2026)

# calculate one prediction for income = 1000, type = wc, and education = 10.8
ggpredict(pmod, terms = c("income[1000]", "type[wc]"))

# actual education mean is 10.7951;
# calculated only for complete cases
mean(Prestige$education[complete.cases(Prestige)])

# To get that for a Bayesian model, we make 4000 predictions because we have
# 4000 samples. 
parms <- as.matrix(pmod)
head(parms)

# The x matrix below contains the intercept (1), income (1000), prof (0), wc
# (1), education (10.7951) prof*income (0) and wc*income (1 x 1000). The parms
# matrix contains the 4000 posterior samples.

# enter predictor values as a matrix
x <- matrix(c(1, 1000, 0, 1, 10.7951, 0, 1000*1), ncol = 1)
x

# use matrix algebra to make predictions (all but sigma)
est <- parms[,-8] %*% x  
head(est) # 4000 predictions

# The posterior_linpred() function does this for us
posterior_linpred(pmod, newdata = data.frame(income = 1000, 
                                             type = factor("wc"), 
                                             education = 10.7951)) |> 
  head()

# the estimate is the median of the 4000 predictions
median(est)  

# the CI is the posterior interval
posterior_interval(est, prob = 0.95) 

# compare to ggpredict
ggpredict(pmod, terms = c("income[1000]", "type[wc]"))


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

# model 1 appears preferable

## END OF SCRIPT