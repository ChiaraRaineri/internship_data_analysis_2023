# Two way anova for S and k with management and restoration as variables

library(ggplot2)
library(ggpubr)
library(tidyverse)
library(broom)
library(AICcmodavg)
library(lmtest)
library(dplyr)
library(gridExtra)

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


# To perform anova on S data I have to transform them
# log transformation


#### S ####

# Transformation #
tbi_data <- mutate(tbi_data, logS = log10(S))  # Now the data are log transformed
# Let's see if homoscedasticity is present
# One explanatory variable
mod_S_log <- lm(logS ~ treatment, data = tbi_data, na.action = na.exclude)
summary(mod_S_log)
# Breusch-Pagan test
bptest(mod_S_log)   # p-value = 0.0498
# I reject the null hypothesis (heteroscedasticity is present) 
# Visual method
par(mfrow = c(2, 2))
plot(mod_S_log)
# Two explanatory variables
mod_S_log2 <- lm(logS ~ restoration + management, data = tbi_data, na.action = na.exclude)
summary(mod_S_log2)
# Breusch-Pagan test
bptest(mod_S_log2)   # p-value = 0.02177 (heteroscedasticity is present)
# Visual method
par(mfrow = c(2, 2))
plot(mod_S_log2)

# Two-way anova #
# Even though the data are not normally distributed, I'll do the two-way anova as usual, being extra careful with the results
anova_S <- aov(S ~ restoration + management, data = tbi_data)  
summary(anova_S)  # p-value restoration = 0.00352   # p-value management = 0.52177
anova_S_comb <- aov(S ~ restoration * management, data = tbi_data)
summary(anova_S_comb)  # p-value restoration:management = 0.08246

# Tukey's test
tukey_S <- TukeyHSD(anova_S)
tukey_S

# Table
table_S <- group_by(tbi_data, restoration, management) %>%
  summarise(mean=mean(S), sd=sd(S)) %>%
  arrange(desc(mean))
table_S     # MANCANO GLI UNMANAGED

# Barplot
pmean_S <- ggplot(table_S, aes(x = factor(management), y = mean, fill = restoration, colour = restoration)) + 
geom_bar(stat = "identity", position = "dodge") + theme(axis.title.x = element_blank()) + ggtitle("Stabilization factor (S)")



# k data respect homoscedasticity, so I can perform the two-way anova without further transformations

#### k ####

anova_k <- aov(k ~ restoration + management, data = tbi_data)  
summary(anova_k)  # p-value restoration = 0.0126   # p-value management = 0.6775
anova_k_comb <- aov(k ~ restoration * management, data = tbi_data)
summary(anova_k_comb)  # p-value restoration:management = 0.2311

# Tukey's test
tukey_k <- TukeyHSD(anova_k)
tukey_k

# Table with factors, means and standard deviation using dplyr package
# First I have to remove all the NAs from the data frame
tbi_data <- na.omit(tbi_data)
# Table
table_k <- group_by(tbi_data, restoration, management) %>%
  summarise(mean=mean(k), sd=sd(k)) %>%
  arrange(desc(mean))
table_k     # MANCANO GLI UNMANAGED

# Barplot
pmean_k <- ggplot(table_k, aes(x = factor(management), y = mean, fill = restoration, colour = restoration)) + 
geom_bar(stat = "identity", position = "dodge") + theme(axis.title.x = element_blank()) + ggtitle("Decomposition speed (k)")



# Plotting all the barplots together
pdf("2_tea_indexes/graphs/barplots.pdf", width = 12, height = 8)
pmean_S
pmean_k
grid.arrange(pmean_S, pmean_k, nrow=1)
dev.off()







