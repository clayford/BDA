# BDA, Part 2 code along answers

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

pm1 <- stan_glm(rate ~ age + expenses + vacancy + sqft, data = prop)
posterior_interval(pm1) |> round(3)

# (2) View the posterior distributions
plot(pm1, plotfun = "dens")

# (3) Assess model fit with a posterior predictive check. 
pp_check(pm1)


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

gm1 <- stan_glm(Gas ~ Temp + Insul + Temp:Insul, data = whiteside)

# (2) view the model summary

summary(gm1)

# (3) view the posterior distributions

plot(gm1, plotfun = "dens")

# (4) create an effect plot to visualize the interaction.

ggpredict(gm1, terms = c("Temp", "Insul")) |> plot()


# CODE ALONG 3 ------------------------------------------------------------


# (1) Re-fit the Bayesian arthritis model with an interaction for Treatment and
# Age. Name the model "arthritis.blm2".

arthritis.blm2 <- stan_glm(Better ~ Treatment + Sex + Age + Treatment:Age, 
                           data = arthritis,
                           family = binomial)

# (2) view the model summary

summary(arthritis.blm2, digits = 3)

# (3) view the posterior distributions

plot(arthritis.blm2, plotfun = "dens")

# (4) create an effect plot to visualize the interaction. 

ggpredict(arthritis.blm2, terms = c("Age [all]", "Treatment")) |> plot()


