# Bayesian Data Analysis, Part 1
# Clay Ford
# UVA Library StatLab
# Spring 2024

# Code along answers

# CODE ALONG 1 ------------------------------------------------------------


# Refit the model above with the following different priors and compare the
# resulting posterior intervals. How different are they?

# prior_intercept = normal(0, 1)
# prior_aux = exponential(1)

# prior_intercept = normal(100, 1000)
# prior_aux = exponential(0.05)

# prior_intercept = NULL 
# prior_aux = NULL 

# The last one, NULL, means a uniform prior giving equal weight to -Inf to Inf

m1 <- stan_glm(backpacks ~ 1, data = dat,
               prior_intercept = normal(0, 1),
               prior_aux = exponential(1))
m2 <- stan_glm(backpacks ~ 1, data = dat,
               prior_intercept = normal(100, 1000),
               prior_aux = exponential(0.005))
m3 <- stan_glm(backpacks ~ 1, data = dat,
               prior_intercept = NULL,
               prior_aux = NULL)

posterior_interval(m1)
posterior_interval(m2)
posterior_interval(m3)


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
lm1 <- lm(hand ~ foot, data = handfoot)
summary(lm1)
confint(lm1, parm = "foot")

# with centered foot data, to give intercept an interpretation
handfoot$footC <- handfoot$foot - mean(handfoot$foot)
lm2 <- lm(hand ~ footC, data = handfoot)
summary(lm2)
confint(lm2, parm = "footC")

# Intercept is expected hand size for an "average" foot size (ie, when footC =
# 0)

# For every one-inch increase in foot size, we can expect about a 0.5 inch
# increase in hand size.

# 1) Run the same analysis as a Bayesian model using stan_glm. Call the model
# "bmod5". Use the default priors. Then investigate the posterior interval of
# the footC coefficient.

bmod5 <- stan_glm(hand ~ footC, data = handfoot, family = gaussian)

plot(bmod5, plotfun = "dens", pars = "footC")

posterior_interval(bmod5, pars = "footC")

# 2) Repeat the Bayesian analysis but this time with your own priors. Call the
# model "bmod6". Set the intercept prior to normal(6,1) and the slope prior to
# normal(0.4,0.2).

bmod6 <- stan_glm(hand ~ footC, data = handfoot, family = gaussian, 
                  prior_intercept = normal(6, 1), prior = normal(0.4, 0.2))

plot(bmod6, plotfun = "dens", pars = "footC")

posterior_interval(bmod6, pars = "footC")

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

blm_cereal <- stan_glm(sales ~ design, data = cereal)

# 2) Create a trace plot to assess convergence.
plot(blm_cereal, plotfun = "trace")

# 2) How does MCSE, Rhat and n_eff look?
summary(blm_cereal)

# 3) Perform a posterior predictive check
pp_check(blm_cereal)



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

post_cereal <- as.data.frame(blm_cereal)

# 1) What is the estimated probability the difference between design 3 and
# design 1 is greater than 4?

mean(post_cereal$design3 > 4)

# 2) Calculate a 95% credibility interval for the mean of design 3.

quantile(post_cereal$`(Intercept)` + post_cereal$design3, 
         probs = c(0.025, 0.975))


# 3) Calculate a 95% credibility interval for the difference between design 3
# and design 4.

design3 <- post_cereal$`(Intercept)` + post_cereal$design3
design4 <- post_cereal$`(Intercept)` + post_cereal$design4

quantile(design4 - design3, 
         probs = c(0.025, 0.975))
