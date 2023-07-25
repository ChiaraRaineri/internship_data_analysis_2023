# Two way anova for S and k with management and restoration as variables

library(ggplot2)
library(ggpubr)
library(tidyverse)
library(broom)
library(AICcmodavg)
library(lmtest)

setwd("C:/internship/") 


tbi_data <- read.csv("2_tea_indexes/tea_indexes_sites.csv", header = TRUE, colClasses = c("factor", "factor", "factor", "factor", "factor", "factor", "numeric", "numeric"))


## HOMOSCEDASTICITY TEST ##
# H0 = homoscedacity is present (residuals are distributed with equal variance)


### S ###

# One explanatory variable #
mod_S <- lm(S ~ treatment, data = tbi_data, na.action = na.exclude)
summary(mod_S)

# Breusch-Pagan test to assess homoscedasticity (a small p-value indicates that residual variance is non-constant (heteroscedastic))
bptest(mod_S)   # p-value = 0.02732
# I reject the null hypothesis (heteroscedasticity is present) 

# Visual method
par(mfrow = c(2, 2))
plot(mod_S)


# Two explanatory variables #
mod_S2 <- lm(S ~ restoration + management, data = tbi_data, na.action = na.exclude)
summary(mod_S2)

bptest(mod_S2)   # p-value = 0.09099

# Visual method
par(mfrow = c(2, 2))
plot(mod_S2)




### k ###

# One explanatory variable #
mod_k <- lm(k ~ treatment, data = tbi_data, na.action = na.exclude)
summary(mod_k)

# Breusch-Pagan test
bptest(mod_k)   # p-value = 0.1089
# I can't reject the null hypothesis (homoscedasticity is present) 

# Visual method
par(mfrow = c(2, 2))
plot(mod_k)


# Two explanatory variables #
mod_k2 <- lm(k ~ restoration + management, data = tbi_data, na.action = na.exclude)
summary(mod_k2)

bptest(mod_k2)   # p-value = 0.7815

# Visual method
par(mfrow = c(2, 2))
plot(mod_k2)





## TWO WAY ANOVA ##





