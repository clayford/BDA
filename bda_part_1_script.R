# Bayesian Data Analysis, Part 1
# Clay Ford
# UVA Library StatLab
# Spring 2024



# load packages -----------------------------------------------------------

# If you do not have, go to Tools...Install Packages...
# or run install.packages("rstanarm")

library(rstanarm)


# backpack survey ---------------------------------------------------------

# Estimate the mean weight of student backpacks at UVA.

# read in data
dat <- read.csv("https://raw.githubusercontent.com/clayford/BDA/master/data/backpacks.csv")
summary(dat)
hist(dat$backpacks)

# How certain is this estimate?
# traditional approach: calculate confidence interval
t.test(dat$backpacks)
save.out <- t.test(dat$backpacks)
save.out$conf.int

# Let's re-do our analysis in the form of a model.

# traditional model-based approach: intercept-only linear model
lm1 <- lm(backpacks ~ 1, data = dat)
summary(lm1)

# The fitted model is a Normal distribution with mean = 16.0500 and standard
# deviation = 4.331

# confidence interval
confint(lm1)

# We can also use glm for this as well
glm1 <- glm(backpacks ~ 1, data = dat, family = gaussian)
summary(glm1)

# confidence interval
confint(glm1)


# Bayesian model-based approach

# Use stan_glm() from rstanarm package
# Use the prior_intercept argument to specify the prior;
# prior based on the following article:
# https://well.blogs.nytimes.com/2009/07/21/weighing-school-backpacks/
# The normal() function is from the rstanarm package

# Again, the proposed "model" here is a Normal distribution with some mean and
# standard deviation. Our prior for the mean is N(18, 5). We let rstanarm
# provide the prior for the standard deviation, which we'll examine below. We're
# basically saying "we think the process that generated this data is a simply a
# Normal distribution."
bmod1 <- stan_glm(backpacks ~ 1, 
                  data = dat, 
                  family = gaussian,
                  prior_intercept = normal(18, 5))

# In the console we see output on the sampling procedure. The model is not fit
# with a closed-form math formula or numeric approximation but rather with a
# sophisticated sampling engine. If something goes wrong with the sampling, you
# should get a warning message in red saying not to trust the results.

# you can turn off sampling messages by adding the argument `refresh = 0`
# Not Recommended!

# Here's a compact summary of the Bayesian model
bmod1

# The medians are the medians of the posterior distributions.

# Notice the auxiliary parameter, MAD_SD. The Bayesian model returns a posterior
# distribution for the standard error estimate

# Instead of a confidence interval we calculate a posterior interval using the
# posterior_interval function. The "(Intercept)" posterior interval refers to
# the mean of our model. The "sigma" posterior interval refers to the standard
# error of our model.
posterior_interval(bmod1, prob = 0.95)

# Use plot method with `plotfun = "dens"` to see the posterior distributions
plot(bmod1, plotfun = "dens")

# And here are the priors that were used
prior_summary(bmod1) 

# In normal(), location = mean, scale = standard deviation

# If we don't specify a prior, rstanarm provides one for us and then adjusts its
# scale. We didn't specify a prior for sigma, so rstanarm used an exponential
# distribution with rate = 1. The adjusted prior for the exponential
# distribution is 1/sd(y) where y is our dependent variable.

1/sd(dat$backpacks)

# Use the argument `prior_aux` to specify a prior for sigma. For example, `prior_aux = exponential(0.5)`

# The base R curve() function makes it relatively easy to visualize priors.
# prior distribution for intercept
# dnorm() is the normal density function
curve(dnorm(x, mean = 18, sd = 5), 
      from = 18 + -3*5,        # mean - 3 SD 
      to = 18 + 3*5)           # mean + 3 SD

# prior distribution for sigma
# dexp() is the exponential density function. It is positive, so it's a
# reasonable choice for sigma, which is always positive.
# 6/rate is a decent choice for the `to` argument
curve(dexp(x, rate = 0.23), 
      from = 0, 
      to = 6/0.23)

# As the rate gets smaller, the distribution gets wider. 


# CODE ALONG 1 ------------------------------------------------------------


# Refit the model above with the following different priors and compare the
# resulting posterior intervals. How different are they?

# Model m1
# prior_intercept = normal(0, 1)
# prior_aux = exponential(1)

# Model m2
# prior_intercept = normal(100, 1000)
# prior_aux = exponential(0.05)

# Model m3
# prior_intercept = NULL 
# prior_aux = NULL 

# The last one, NULL, means a uniform prior giving equal weight to -Inf to Inf



# battery experiment ------------------------------------------------------

# read in data
bat <- read.csv("https://raw.githubusercontent.com/clayford/BDA/master/data/batteries.csv")
aggregate(y ~ grp, data = bat, mean)
stripchart(y ~ grp, data = bat, method = "jitter")

# Traditional approach: 95% confidence interval of the difference in means
# CI is negative because the comparison is grp 0 - grp 1
t.test(y ~ grp, data = bat)

# Traditional model-based approach using lm()
lm.out <- lm(y ~ grp, data = bat)
summary(lm.out)

# Intercept is mean of group 0
# grp is difference between group 0 and group 1
# Residual standard error (0.8371) is pooled standard deviation 

# CI of slope parameter (ie difference in means between group 0 and group 1)
confint(lm.out, parm = "grp")

# equivalent with glm
glm.out <- glm(y ~ grp, data = bat, family = gaussian)
confint(glm.out, parm = "grp")

# Bayesian approach using stan_glm with default priors
bmod3 <- stan_glm(y ~ grp, data = bat, family = gaussian)
bmod3

# Notice the auxiliary parameter. The Bayesian model returns a posterior
# distribution for the residual standard error.

# Use plot method to see the posterior distributions
plot(bmod3, "dens")
plot(bmod3, "dens", pars = "grp")

# use the posterior_interval function to obtain a Bayesian uncertainty interval 
posterior_interval(bmod3, prob = 0.95)
posterior_interval(bmod3, prob = 0.95, pars = "grp")

# And here are the default priors that were used
prior_summary(bmod3, digits = 4)

# "adjusted prior" means the prior distribution was rescaled to be on the same
# range of the outcome variable.

# http://mc-stan.org/rstanarm/articles/priors.html

# Intercept: 
# location = mean(y)
# scale = 2.5 * sd(y)
2.5 * sd(bat$y)

# Coefficients: 
# location = 0
# scale = 2.5 * (sd(y)/sd(x_k)) (for kth coefficient)
2.5 * (sd(bat$y)/sd(bat$grp))

# Auxiliary: 
# rate = 1/sd(y)
1/sd(bat$y)

# We can visualize these as follows:
curve(dnorm(x, 10.6, 2.341), 
      from = 10 - 3*2.341, to = 10 + 3*2.341, 
      main = "Default Prior for mean of group 0 (intercept)")
curve(dnorm(x, 0, 4.635), 
      from = -3*4.635, to = 3*4.635, 
      main = "Default Prior for difference in means")
curve(dexp(x, rate = 1.068), 
      from = 0, to = 6/1.068, 
      main = "Default Prior for pooled standard deviation")

# We can also, and probably should, set our own priors.
# Set intercept to N(10,4) - mean of group 0
# Set grp to N(0,3) - difference between group 0 and group 1
# set sigma to exponential(0.5) - pooled standard deviation (or residual SE);
# setting to 0.5 increases the uncertainty

# visualize prior distributions
curve(dnorm(x, 10, 4), 
      from = 10 - 3*4, to = 10 + 3*4, 
      main = "Prior for mean of group 0 (intercept)")
curve(dnorm(x, 0, 3), 
      from = -3*3, to = 3*3, 
      main = "Prior for difference in means")
curve(dexp(x, rate = 0.5), 
      from = 0, to = 6/0.5, 
      main = "Prior for pooled standard deviation")

# Bayesian approach with our own priors;
bmod4 <- stan_glm(y ~ grp, data = bat, family = gaussian, 
                  prior_intercept = normal(10,4),
                  prior = normal(0,3),
                  prior_aux = exponential(0.5))
bmod4

# since we specified our own priors, they were not rescaled
prior_summary(bmod4)

# use the posterior_interval function to obtain a Bayesian uncertainty interval 
posterior_interval(bmod4, prob = 0.95)
posterior_interval(bmod4, prob = 0.95, pars = "grp")



# CODE ALONG 2 ------------------------------------------------------------

# 15 women were sampled and the lengths of one of their hands and feet were
# measure in inches. What is the relationship between foot and hand size? (From
# Hogg & Tanis, p. 423)

handfoot <- data.frame(foot = c(9, 8.5, 9.25, 9.75, 9, 10, 9.5, 
                           9, 9.25, 9.5, 9.25, 10, 10, 9.75, 9.5),
                       hand = c(6.5, 6.25, 7.25, 7, 6.75, 7, 6.5, 7, 7, 
                           7, 7, 7.5, 7.25, 7.25, 7.25))
plot(hand ~ foot, data = handfoot)

# A traditional modeling approach: linear regression
mod1 <- lm(hand ~ foot, data = handfoot)
summary(mod1)
confint(mod1, parm = "foot")

# with centered foot data, to give intercept an interpretation
handfoot$footC <- handfoot$foot - mean(handfoot$foot)
mod2 <- lm(hand ~ footC, data = handfoot)
summary(mod2)
confint(mod2, parm = "footC")

# Intercept is expected hand size for an "average" foot size (ie, when footC =
# 0)

# For every one-inch increase in foot size, we can expect about a 0.5 inch
# increase in hand size.

# 1) Run the same analysis as a Bayesian model using stan_glm. Call the model
# "bmod5". Use the default priors. Then investigate the posterior interval of
# the footC coefficient.



# 2) Repeat the Bayesian analysis but this time with your own priors. Call the
# model "bmod6". Set the intercept prior to normal(6,1) and coefficients prior
# to normal(0.4,0.2).



# Back to presentation


# convergence, ESS and MCSE -----------------------------------------------


# a trace plot provides a visual assessment of convergence

# the backpack model
bmod1
plot(bmod1, plotfun = "trace")

# The battery model
bmod4
plot(bmod4, plotfun = "trace") # all parameters
plot(bmod4, plotfun = "trace", pars = "grp")

# Both look good.

# Example of non-convergence

# We can force a model to not converge by lowering the warmup and iterations.
# Notice I also set the priors to NULL, which is a flat (improper) uniform
# prior. 
bmod4_x <- stan_glm(y ~ grp, data = bat, family = gaussian, 
                  prior_intercept = NULL,
                  prior = NULL,
                  prior_aux = NULL,
                  iter = 100,    # default = 2000
                  warmup = 10)   # default = 1000 (ie, iter/2)

plot(bmod4_x, plotfun = "trace") # all parameters
plot(bmod4_x, plotfun = "trace", pars = "grp") 

# The plots show we need a longer warm up and more iterations. Hence the
# recommendation to increase iterations and maybe warmup in the event you
# experience non-convergence.


# Rhat statistics provides a numerical assessment of convergence;
# want R-hat < 1.1

# n_eff (effective sample size) estimates number of independent samples after
# correcting for autocorrelation; want n_eff > 1000

# MCSE assess error introduced by MCMC; would like it small relative to
# parameters' posterior standard deviation.

# MCSE, Rhat EFF are available in the summary under MCMC diagnostics
summary(bmod1)
summary(bmod4)

# The log-posterior is the logarithm of the posterior. This value is sometimes used for assessing predictive accuracy and for model comparison. 

# Again these all look good.


# posterior predictive check ----------------------------------------------

# visual assessment of fit of model. The observed data is shown with simulated
# outcomes produced by our model.

# backpack model
pp_check(bmod1)

# The dark line is a density plot (smooth histogram) of the observed data. 
hist(dat$backpacks, freq = FALSE, ylim = c(0,0.15))
lines(density(dat$backpacks))

# What is going on with `pp_check`? It is drawing values from the posterior distributions and using those values in our "model" to generate data.

# We can use the as.data.frame() function to see posterior distribution values.
# Remember, the posterior distributions are comprised of samples. Here are the
# first six of bmod1. Yours will be different.
as.data.frame(bmod1) |> head()

# Take the first row of values and use those to simulate data.
parms <- as.data.frame(bmod1) |> head(n=1)
parms

# recall our "model" is simply a normal distribution. Plug the sample values
# from our posterior distributions into rnorm() to generate a set of data.
d1 <- rnorm(n = 100, 
            mean = parms$`(Intercept)`, 
            sd = parms$sigma)

# and then plot.
plot(density(d1))

# That's what `pp_check()` is doing. Randomly sampling parameter estimates from
# the posterior distributions and then generating data.

# We can do it, too. Apply the "model" to the rows of the posterior samples.

# get first 30 rows
post_samp <- as.data.frame(bmod1)[1:30,]

# apply the rnorm function to those 30 rows.
sim <- apply(post_samp, 1, function(x)rnorm(100, mean = x[1], sd = x[2]))

# plot original data and overlay generate data
plot(density(dat$backpacks))
for(i in 1:30)lines(density(sim[,i]), col = "grey80")

# That's what pp_check() does, but with random rows. 
pp_check(bmod1)


# battery model posterior predictive check
pp_check(bmod4)

# Again the dark line is a density plot of the observed data
plot(density(bat$y))

# We can also produce boxplots and histograms
pp_check(bmod1, plotfun = "hist", nreps = 3)
pp_check(bmod4, plotfun = "boxplot", nreps = 10)
pp_check(bmod4, plotfun = "hist", nreps = 3)

# Check histograms of means by level of grouping variable
pp_check(bmod4, plotfun = "stat_grouped", 
         stat = "mean", 
         group = "grp")

# Scatterplots of y vs. several individual yrep datasets
pp_check(bmod4, plotfun = "scatter", nreps = 3)
pp_check(bmod4, plotfun = "scatter", nreps = 9)


# CODE ALONG 3 ------------------------------------------------------------


# One-way ANOVA

# Food company wishes to test 4 different package designs for a new cereal. 20
# stores selected as experimental units. Each store randomly assigned one of the
# package designs, with each package design assigned to 5 stores. Sales in
# number of cases recorded. (From Kutner, et al, p. 686)

cereal <- data.frame(
  design = gl(n = 4, k = 5),
  sales = c(11, 17, 16, 14, 15,
            12, 10, 15, 19, 11,
            23, 20, 18, 17, 17,
            27, 33, 22, 26, 28)
)

stripchart(sales ~ design, data = cereal)
aggregate(sales ~ design, data = cereal, mean)

# Traditional approach
lm_cereal <- lm(sales ~ design, data = cereal)
anova(lm_cereal)
summary(lm_cereal)


# 1) Do a Bayesian analysis of the cereal data using stan_glm. Feel free to use
# the default priors, or try your own. In this case the defaults are probably
# good since we likely don't have any prior knowledge or hunches about these
# designs. Call the model "blm_cereal"



# 2) Create a trace plot to assess convergence.



# 3) How does MCSE, Rhat and n_eff look?



# 4) Perform two posterior predictive checks: The default and one using 
#    plotfun = "stat_grouped" as we did above



# back to presentation.


# summarize and interpret results -----------------------------------------

summary(bmod1) #  backpacks survey
summary(bmod1, digits = 3) # use digits argument if you want more numbers

summary(bmod4) # battery experiment

# The mean_PPD is the sample average posterior predictive distribution of the outcome variable. Think of it as the sample average of the curves you see when running `pp_check()`. Hopefully the mean_PPD is similar to the mean of the response variable. If not, something may be wrong.

# backpack survey
# create our own posterior summaries
post_samp <- as.data.frame(bmod1)
dim(post_samp)
head(post_samp)

# estimated probability that mean backpack weight is less than 16 lbs
mean(post_samp$`(Intercept)` < 16)

# estimated probability that mean backpack weight is more than 15 but less than
# 16 lbs.
mean(post_samp$`(Intercept)` > 15 & post_samp$`(Intercept)` < 16)


# battery experiment
# extract posterior samples
post_samp2 <- as.data.frame(bmod4)
dim(post_samp2)
head(post_samp2)

# estimated probability that difference is greater than 1 hour
mean(post_samp2$grp > 1)

# estimated probability that difference is greater than 1.5 hours
mean(post_samp2$grp > 1.5)

# We can use posterior samples to create "new" samples;
# (Intercept) is estimated mean of grp == 0
# (Intercept) + grp coefficient is the estimated mean of grp == 1

# Now get credibility intervals for the grp means
quantile(post_samp2$`(Intercept)`, 
         probs = c(0.025, 0.975))
quantile(post_samp2$`(Intercept)` + post_samp2$grp, 
         probs = c(0.025, 0.975))


# CODE ALONG 4 ------------------------------------------------------------


# Earlier we fit a Bayesian model to data on different cereal designs. Let's
# fit it again and save as blm_cereal.

blm_cereal <- stan_glm(sales ~ design, data = cereal, family = gaussian)
coef(blm_cereal) # medians of posterior distributions

# The (Intercept) is the estimated mean of design 1. 
# design2 is the estimated difference between design 2 and design 1.
# design3 is the estimated difference between design 3 and design 1.
# design4 is the estimated difference between design 4 and design 1.

# Create and use a posterior summary to answer the following questions:


# 1) What is the estimated probability the difference between design 3 and
# design 1 is greater than 4?



# 2) Calculate a 95% credibility interval for the mean of design 3.



# 3) Calculate a 95% credibility interval for the difference between design 3
# and design 4.




# shinystan ---------------------------------------------------------------

# Launch the ShinyStan web app to interactively explore your Bayesian model.

# Note you cannot use R while ShinyStan is running.
launch_shinystan(bmod1)
launch_shinystan(bmod4)


# Use launch_shinystan() to explore the Bayesian cereal model.


launch_shinystan(blm_cereal)




# Appendix - Visualizing a simple model -----------------------------------

# We can visualize a simple linear regression model by plotting the raw data and
# adding the fitted model line.

blm2 <- stan_glm(hand ~ footC, data = handfoot, family = gaussian)

# Here's how we can do it with ggplot2. Notice the model coefficients are in the
# model object in the coefficients component.

blm2$coefficients
blm2$coefficients[1]
blm2$coefficients[2]

library(ggplot2)

# raw data with fitted line
ggplot(handfoot, aes(x = footC, y = hand)) + 
  geom_point() +
  geom_abline(intercept = blm2$coefficients[1], 
              slope = blm2$coefficients[2])

# The fitted line intercept and slope are just the medians of the 4000 simulated
# values. We can, and should, add uncertainty to our plot. We can do that by
# simply adding some of the simulated intercepts and slopes.

# Calling as.data.frame on our fitted rstanarm model will return a data frame of
# our posterior sample.
fit.df <- as.data.frame(blm2)
nrow(fit.df) # 4000, number of simulated values
head(fit.df) # first 6

# raw data with fitted line and 100 posterior samples
k <- sample(nrow(fit.df), 100)
ggplot(handfoot, aes(x = footC, y = hand)) + 
  geom_point() +
  geom_abline(mapping = aes(intercept = `(Intercept)`, 
                            slope = footC), 
              data = fit.df[k,], 
              size = 0.2, alpha = 0.2, color = "skyblue") +
  geom_abline(intercept = blm2$coefficients[1], 
              slope = blm2$coefficients[2])
  
  

# Appendix - distribution review ------------------------------------------


# Distributions - how data are "distributed"

# Continuous data often visualized with histograms
hist(iris$Sepal.Length)
hist(rock$area)

# Most histograms give counts by default. It's usually better to think of the
# histogram as a single area with an area of 1, or 100%. This allows us to
# determine, say, what proportion of our data falls between two values.
hist(rock$area, freq = FALSE)

# The areas in the rectangles add to 1, or 100%. Each rectangle represents the
# proportion of data between two values. The proportion between 6000 and 8000
# can be determined by calculating the area of the rectangle.
h.out <- hist(rock$area, freq = FALSE)
2000 * h.out$density[4] # width x height for 4th rectangle in histogram

# all the rectangles sum to 1
sum(h.out$density * 2000)


# Some data in nature can be modeled, or approximated, with a mathematical
# distribution. The area measures in the rock data have a familiar bell-curve
# shape, which can be modeled with a Normal distribution.


# Normal distribution
# Quick way to draw a normal distribution
curve(dnorm(x), from = -3, to = 3)

# dnorm - the formula to draw the distribution

# pnorm - area under the curve
# area less than 0
pnorm(0)
# area between 0 and 1
pnorm(1) - pnorm(0)

# qnorm - the point on the x-axis below which a given area lies
# point below which lies 0.50
qnorm(0.5)

# The Normal distribution has two "parameters" that dictate where the peak of
# curve is located on the x-axis (mean) and how spread out the curve is
# (standard deviation). Examples:
curve(dnorm(x, mean = 15, sd = 1), from = 10, to = 25, ylab = "")
curve(dnorm(x, mean = 14, sd = 2), add = TRUE, col = 2)
curve(dnorm(x, mean = 18, sd = 3), add = TRUE, col = 3)
legend("topright", legend = c("N(15,1)", "N(14,2)","N(18,3)"), col = 1:3, lty = 1)

# The area data in the rock dataframe with Normal curve superimposed 
hist(rock$area, freq = F)
curve(dnorm(x, mean = mean(rock$area), sd = sd(rock$area)), add = TRUE)

# We can also randomly draw data from Normal distributions
rnorm(n = 5, mean = 10, sd = 1)

# R provides functions for many distributions with same d-p-q-r naming
# convention. For example, the Uniform distribution, which is used to model
# uniformly distributed data. It has two parameters: the lowest value and
# highest value.
curve(dunif(x, min = 0, max = 5), from = 0, to = 5)
# proportion less than 3
punif(3, min = 0, max = 5)
# point below which 0.75 lies
qunif(0.75, min = 0, max = 5)
# 5 random values from a Uniform(0,5) dist'n
runif(5, min = 0, max = 5)


# Appendix - stan_lm ------------------------------------------------------


# Bayesian approach using stan_lm
# requires use of the prior argument with R2();
# R2 conveys prior information about all the parameters. The prior hinges on
# prior beliefs about the location of R^2, the proportion of variance in the
# outcome attributable to the predictors.
bmod_lm <- stan_lm(y ~ grp, data = bat, 
                   prior = R2(location = 0.3, what = "mean"))
summary(bmod_lm)
# use the posterior_interval function to obtain a Bayesian uncertainty interval 
posterior_interval(bmod_lm, prob = 0.95, pars = "grp")

# And here are the priors that were used
prior_summary(bmod_lm)




# Appendix - grid approximation -------------------------------------------


# Bayesian approach using approximation and Bayes' theorem:
# posterior is proportional to likelihood * prior

# create vector of possible means
means <- seq(3, 33, by = 0.01)

# define a prior distribution using the normal distribution:
# Say we propose a Normal with mean of 18 and sd of 5

prior <- dnorm(means, mean = 18, sd = 5)
plot(means, prior, type = "l", main = "Prior Distribution")

# prior based on the following article:
# https://well.blogs.nytimes.com/2009/07/21/weighing-school-backpacks/

# Redefine prior on log scale to prevent rounding errors in the coming
# calculations:
prior_log <- dnorm(means, mean = 18, sd = 5, log = TRUE)

# Define the likelihood
# Assume mean is normally distributed

# Examples of likelihood for specific mean
# Again we define on the log scale to prevent rounding error

# Likelihood of data if mean is 2 and sd = 4
sum(dnorm(x = dat$backpacks, mean = 2, sd = 4, log = TRUE))
# Probability of data if mean is 10 and sd = 4
sum(dnorm(x = dat$backpacks, mean = 10, sd = 4, log = TRUE))
# Probability of data if mean is 20 and sd = 4
sum(dnorm(x = dat$backpacks, mean = 20, sd = 4, log = TRUE))

# Notice we set sd = 4. That's actually a second parameter that needs a prior
# distribution! I set it to 4 to keep this demonstration simple. 

# calculate likelihood for all means
likelihood <- sapply(1:length(means), 
                     function(x)sum(dnorm(x = dat$backpacks, 
                                          mean = means[x], 
                                          sd = 4, 
                                          log = TRUE))
)

# calculate posterior using Bayes' theorem
# posterior proportional to likelihood * prior
# On log scale this becomes likelihood + prior
prod <- likelihood + prior_log 
# rescale to prevent too many 0s
post <- exp(prod - max(prod))

plot(means, post, type = "l", main = "Posterior")

# Sample from the posterior to make inferences about the mean
s.rows <- sample(1:length(post), size = 1000, replace = TRUE, prob = post)
sample.mu <- means[s.rows]

# approximate posterior dist'n
plot(density(sample.mu))

# most likely mean
means[which.max(post)]

# posterior probability that mean is less than 16
mean(sample.mu < 16)


# Data references ---------------------------------------------------------

# Hogg and Tanis, Probability and Statistical Inference, 7th ed.

# Kutner, et al., Applied Linear Statistical Models, 5th ed.
