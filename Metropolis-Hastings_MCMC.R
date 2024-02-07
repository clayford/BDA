# Clay Ford

# A demonstration of MCMC sampling based heavily on the following blog post:
# https://theoreticalecology.wordpress.com/2010/09/17/metropolis-hastings-mcmc-in-r/

# Generate some data from a binomial distribution with size = 1 and prob = 0.3.
# This generates a sequence of zeroes and ones, where ones have a probability of
# 0.3 occurring.
set.seed(1)
y <- rbinom(n = 50, size = 1, prob = 0.3)
y

# Now pretend we came upon this data naturally through an experiment or
# observation and want to use Bayesian statistics to estimate the probability of
# a 1 occurring. Perhaps we randomly surveyed college students and asked them if
# they had an Apple computer, where 1 = yes and 0 = no. This is our parameter of
# interest: the probability of having an Apple computer.

# Bayesian inference does not make a point estimate of the parameter, but
# instead estimates the probability distribution of possible values for the
# parameter. This is the posterior distribution.

# Now we use Markov Chain Monte Carlo to work "backwards" and estimate a
# posterior probability distribution for the parameter of interest.

# First we define the likelihood, which is our model. Notice we're fitting the
# correct model using the dbinom() function. We simulated data from a binomial
# dist with rbinom(), and now we're modeling the probability of ones using a
# binomial distribution. We estimate the probability of getting a 0 or 1 for
# different parameter values, convert to log, and sum up. This is the log
# likelihood. In frequentist statistics we try to maximize the log likelihood.

likelihood <- function(param){
  singlelikelihoods <- dbinom(y, size = 1, prob = param, log = T)
  sumll<- sum(singlelikelihoods)
  sumll
  }

# Below we run parameter values ranging from 0.01 to 0.99 through the function and plot the results versus the parameter values. 
p <- seq(0.01, 0.99, by=.01)
p_likelihoods = sapply(p, likelihood)
plot(p, p_likelihoods , type="l", 
     xlab = "values of parameter p", ylab = "Log likelihood")

# Notice the maximum value is at 0.32.
p[which.max(p_likelihoods)]

# This is simply the proportion of ones
sum(y)/length(y)

# Frequentist statistics would calculate a confidence interval for this point
# estimate. 

# Bayesian inference uses the likelihood to update a prior distribution for the
# parameter. We have to provide the prior distribution. Let's define a prior
# distribution for our parameter of interest. Perhaps prior surveys indicated
# about 0.4 of students use Apple computers. We define our prior as Normal
# distribution with mean 0.4 and standard deviation 0.15. Notice we use plogis()
# to ensure the Normal dist ranges from 0 to 1. plogis() is the inverse logit,
# which transforms values on (-Inf,Inf) to (0,1)

# Here's what the prior distribution looks like:
curve(plogis(dnorm(x, mean = 0.4, sd = 0.15)), from = 0, to = 1)

# We define a prior distribution function which returns the density for a
# proposed parameter using the prior distribution parameters.
prior = function(param){
  dnorm(param, mean = 0.4, sd = 0.15, log = T)
  }

# Now we create a posterior function that is the sum of the likelihood and
# prior. Why sum? Because the sum of logs is equal to the log of products.
# Summing log transformed values is safer numerically as products of many small
# values can result in values with many, many decimals. Our computers can only
# be so precise.
posterior <- function(param){
  likelihood(param) + prior(param)
  }

# Now we're ready to do the MCMC sampling.

# We define a function to execute the Metropolis algorithm that has two
# arguments: startvalue and iterations. The startvalue is the first sample we
# draw from the posterior. It primes the pump. The iterations is the number of
# samples we wish to draw. We want to draw a lot of samples, like in the
# thousands.

run_metropolis_MCMC = function(startvalue, iterations){
  
  # initiate a vector with iterations + 1 elements
  chain <- vector(mode = "numeric", length = iterations + 1)
  
  # set the starting value
  chain[1] <- startvalue
  
  # begin the MCMC sampling
  for (i in 1:iterations){
    
    # propose a new sample value based on current sample
    # plogis() is the inverse logit that converts (-Inf,Inf) to(0,1)
    proposal <- plogis(rnorm(n = 1, mean = chain[i], sd = 2))
    
    # calculate an acceptance ratio called "probab"
    probab <- exp(posterior(proposal) - posterior(chain[i]))
    
    # draw a random value between 0 and 1;
    # if value is less than the acceptance ratio, keep the proposed sample
    if (runif(1) < probab){
      chain[i+1] <- proposal
    
    # otherwise reject the proposed sample and keep previous sample
    }else{
      chain[i+1] <- chain[i]
    }
  }
  chain
}

# Now run the MCMC for 10,000 iterations with startvalue set to 0.5 
startvalue <- 0.5
chain <- run_metropolis_MCMC(startvalue, 10000)

# only keep the last 5000 samples
burnIn = 5000
posterior_sample <- chain[-(1:burnIn)]
hist(posterior_sample)
# calculate the posterior interval
quantile(posterior_sample, probs = c(0.025, 0.975))

# compare to rstanarm
library(rstanarm)
m <- stan_glm(y ~ 1, family = binomial, refresh = 0)
plogis(posterior_interval(m, prob = 0.95))
