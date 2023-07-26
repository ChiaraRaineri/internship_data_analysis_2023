# based on https://statsandr.com/blog/two-way-anova-in-r/#assumptions-of-a-two-way-anova


# First I'll perform a one way anova using treatment as an independent variable
# Then, I'll perform a two way anova using management and restoration as independent variables
# The data are S (stabilization factor) and k (decomposition speed)



library(ggplot2)
library(ggpubr)
library(tidyverse)
library(broom)
library(dplyr)
library(car)


tbi_data <- read.csv("2_tea_indexes/tea_indexes_sites.csv", header = TRUE, colClasses = c("factor", "factor", "factor", "factor", "factor", "factor", "numeric", "numeric"))



########################### ONE WAY ANOVA

# Question: are S and k different between the 4 treatments (RM, RU, NM, NU)?


#### Assumptions validation ####

# ANOVA model
anova1_S <- aov(S ~ treatment, data = tbi_data)
anova1_k <- aov(k ~ treatment, data = tbi_data)


## Checking normality ##

# Visual method
par(mfrow = c(1, 2))

# S
hist(anova1_S$residuals)  # histogram
qqPlot(anova1_S$residuals, id = FALSE)  # QQ-plot

# k
hist(anova1_k$residuals)  # histogram
qqPlot(anova1_k$residuals, id = FALSE)  # QQ-plot


# Shapiro-Wilk test
shapiro.test(anova1_S$residuals)  # p-value = 0.1662
shapiro.test(anova1_k$residuals)  # p-value = 1.813e-05 (!)

# Remember that if the normality assumption was not reached, some transformation(s) 
# would need to be applied on the raw data in the hope that residuals would better 
# fit a normal distribution, or you would need to use the non-parametric version of
# the ANOVA—the Kruskal-Wallis test (https://statsandr.com/blog/kruskal-wallis-test-nonparametric-version-anova/)
# Maybe the problem with k are the outliers


## Checking equality of variances (homogeneity) ##

# Levene's test
leveneTest(S ~ treatment, data = tbi_data)  # p-value = 0.07506 (variances are equal)
leveneTest(k ~ treatment, data = tbi_data)  # p-value = 0.3397 (variances are equal)


# Visual method
par(mfrow = c(1, 2))

# S
plot(anova1_S, which = 3)  # Homogeneity of variances
plot(anova1_S, which = 2)  # Normality

# k
plot(anova1_k, which = 3)  # Homogeneity of variances
plot(anova1_k, which = 2)  # Normality



#### Preliminary analyses ####

# S
aggregate(S ~ treatment,
          data = tbi_data,
          function(x) round(c(mean = mean(x), sd = sd(x)), 5))
# k
aggregate(k ~ treatment,
          data = tbi_data,
          function(x) round(c(mean = mean(x), sd = sd(x)), 8))



#### ANOVA ####




