#Importing ggplot2
library(ggplot2)

# For Reproducibility
set.seed(42)

# 1. Simulate ability
n <- 1000
ability <- rnorm(n, mean=0, sd=1) #Standard Normal

# 2. Simulate treatment: higher ability -> higher chance of treatment
# Use a logistic model to create probability
p_treat <-plogis( -0.5 + 1.2 * ability)
treat <- rbinom(n, size=1, prob = p_treat)

# 3. Simulate outcome: income depends on both ability and treatment
# let's assume treatment increases income by 2 units on average
income <- 3 + 2 * treat + 1.5 * ability + rnorm(n, 0, 1)

# 4. Combine into a data frame
data <- data.frame(ability, treat, income)

head(data)
summary(data)

# 5. Plotting using ggplot2
p1 <- ggplot(data, aes(x = ability, y = treat)) +
  geom_jitter(width = 0, height = 0.05, alpha = 0.5) +
  labs(title = "Treatment vs Ability", x = "Ability", y = "Treatment") +
  theme_minimal()

p1


p2 <- ggplot(data, aes(x = ability, y = income)) +
  geom_point(alpha = 0.5) + 
  labs(title = "Income vs Ability", x = "ability", y = "income") + 
  theme_minimal()

p2


# 5. Save plots as png
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

ggsave("plots/treatment_vs_ability.png", plot = p1, width = 8, height = 6)
ggsave("plots/income_vs_ability.png", plot = p2, width = 8, height = 6)


# 6. Creating vectors for Y(1) and Y(0) (With and Without Treatment) and including in df
Y0 <- 2.5 * ability + rnorm(n, 0, 1)
Y1 <- 2.5 * ability + 3.5 + rnorm(n, 0, 1)

# 7. Create a new observed outcome that depends on treatment and a dataframe
Y_obs <- ifelse(treat == 1, Y1, Y0)

df <- data.frame(
  ability = ability,
  treat = treat,
  Y0 = Y0,
  Y1 = Y1,
  Y_obs = Y_obs
)

head(df)
summary(df)


# 8. Before - After change over time for treated individuals only
ba <- Y1 - Y0
mean(ba[treat == 1]) # average change among the treated.

# 9. With - without (difference between treated and untreated at one time point)
mean(Y_obs[treat == 1]) - mean(Y_obs[treat == 0])

# Overestimates as it doesnt take into account ability and other factors

# 10. True causal effect (Average Treatment Effect)
mean(ba)


# Naive comparisons (with–without) can be misleading when treatment is not randomly assigned.
# Simulated counterfactuals let you see the true causal effect, and the before–after approach can estimate the effect for treated individuals, which is slightly higher here because the treated tend to have higher ability.