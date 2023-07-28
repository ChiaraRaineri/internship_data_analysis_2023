# based on https://statsandr.com/blog/two-way-anova-in-r/#assumptions-of-a-two-way-anova


# First I'll perform a one way anova using treatment as an independent variable
# Then, I'll perform a two way anova using management and restoration as independent variables
# The data are S (stabilization factor) and k (decomposition speed)
# In k data some outliers were removed manually



library(ggplot2)
library(ggpubr)
library(tidyverse)
library(broom)
library(dplyr)
library(car)
library(report)


tbi_data <- read.csv("2_tea_indexes/tea_indexes_sites.csv", header = TRUE, colClasses = c("factor", "factor", "factor", "factor", "factor", "factor", "numeric", "numeric"))



########################### ONE WAY ANOVA ########################### 

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
shapiro.test(anova1_k$residuals)  # p-value = 0.106


## Checking equality of variances (homogeneity) ##

# Levene's test
leveneTest(S ~ treatment, data = tbi_data)  # p-value = 0.07506 (variances are equal)
leveneTest(k ~ treatment, data = tbi_data)  # p-value = 0.4404 (variances are equal)


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

summary(anova1_S)  # p-value = 0.00761
summary(anova1_k)  # p-value = 0.0154 

          
# log transformation of k
# tbi_data <- mutate(tbi_data, logk = log10(k))
# anova1_klog <- aov(logk ~ treatment, data = tbi_data)
# summary(anova1_klog)  # p-value = 0.0279


# Report results
report(anova1_S)  # The effect of treatment is statistically significant and large
report(anova1_k)  # The effect of treatment is statistically significant and medium
# report(anova1_klog)  # The effect of treatment is statistically significant and medium



#### Tukey HSD test ####

TukeyHSD(anova1_S)  # Only RM-NM are significantly different in terms of Stabilization factor (p adj = 0.0051541)
TukeyHSD(anova1_k)  # Only RU-NM are significantly different in terms of Stabilization factor (p adj = 0.0361558)
          

# Plot
plot(TukeyHSD(anova1_S))
plot(TukeyHSD(anova1_k))





########################### TWO WAY ANOVA ########################### 



#### Assumptions validation ####

# ANOVA model
anova2_S <- aov(S ~ restoration * management, data = tbi_data)
anova2_k <- aov(k ~ restoration * management, data = tbi_data)


## Checking normality ##

# Visual method
par(mfrow = c(1, 2))

# S
hist(anova2_S$residuals)  # histogram
qqPlot(anova2_S$residuals, id = FALSE)  # QQ-plot

# k
hist(anova2_k$residuals)  # histogram
qqPlot(anova2_k$residuals, id = FALSE)  # QQ-plot


# Shapiro-Wilk test
shapiro.test(anova2_S$residuals)  # p-value = 0.1662
shapiro.test(anova2_k$residuals)  # p-value = 0.106


## Checking equality of variances (homogeneity) ##

# Levene's test
leveneTest(anova2_S)  # p-value = 0.07506 (variances are equal)
leveneTest(anova2_k)  # p-value = 0.4404 (variances are equal)


# Outliers

# S
ggplot(tbi_data) + aes(x = restoration, y = S) + geom_boxplot()
ggplot(tbi_data) + aes(x = management, y = S) + geom_boxplot()

# k
ggplot(tbi_data) + aes(x = restoration, y = k) + geom_boxplot()
ggplot(tbi_data) + aes(x = management, y = k) + geom_boxplot()



#### Preliminary analyses ####


# S
group_by(tbi_data, restoration, management) %>%
  summarise(
    mean = round(mean(S, na.rm = TRUE), 5),
    sd = round(sd(S, na.rm = TRUE), 5))
# k
group_by(tbi_data, restoration, management) %>%
  summarise(
    mean = round(mean(k, na.rm = TRUE), 8),
    sd = round(sd(k, na.rm = TRUE), 8))


# Plots

# S
tbi_data %>%
  filter(!is.na(restoration)) %>%
  ggplot() +
  aes(x = management, y = S, fill = restoration) +
  geom_boxplot()

tbi_data %>%
  filter(!is.na(management)) %>%
  ggplot() +
  aes(x = restoration, y = S, fill = management) +
  geom_boxplot()

# k
tbi_data %>%
  filter(!is.na(restoration)) %>%
  ggplot() +
  aes(x = management, y = k, fill = restoration) +
  geom_boxplot()

tbi_data %>%
  filter(!is.na(management)) %>%
  ggplot() +
  aes(x = restoration, y = k, fill = management) +
  geom_boxplot()



#### ANOVA ####


## Two-way ANOVA with interaction ##

summary(anova2_S)  # p-value restoration:management = 0.08246
summary(anova2_k)  # p-value restoration:management = 0.73619  

# log transformation of k
# anova2_klog <- aov(logk ~ restoration * management, data = tbi_data)
# summary(anova2_klog)  # p-value = 0.26816


## Two-way ANOVA without interaction ##

anova2_S_noint <- aov(S ~ restoration + management, data = tbi_data)
anova2_k_noint <- aov(k ~ restoration + management, data = tbi_data)
          
summary(anova2_S_noint)  # p-value restoration = 0.00352  # p-value management = 0.52177
summary(anova2_k_noint)  # p-value restoration = 0.00154  # p-value management = 0.57096

# log transformation of k
# anova2_klog_noint <- aov(logk ~ restoration + management, data = tbi_data)
# summary(anova2_klog_noint)  # p-value restoration = 0.00582   # p-value management = 0.58990



#### Tukey HSD test ####

TukeyHSD(anova2_S)  # Only restored:managed-near_natural:managed are significantly different (p adj = 0.0051541)
TukeyHSD(anova2_k)  # Only restored:unmanaged-near_natural:managed are significantly different (p adj = 0.0361558)


# Plot
par(mar = c(4.1, 13.5, 4.1, 2.1))
plot(TukeyHSD(anova2_S, which = "restoration:management"), las = 2)
plot(TukeyHSD(anova2_k, which = "restoration:management"), las = 2)



### Visualization ###

pmean_S <- ggplot(table_S, aes(x = factor(restoration), y = mean, fill = management, colour = management)) + 
  geom_bar(stat = "identity", position = "dodge") + theme(axis.title.x = element_blank()) + ggtitle("Stabilization factor")
pmean_S

pmean_k <- ggplot(table_k, aes(x = factor(restoration), y = mean, fill = management, colour = management)) + 
  geom_bar(stat = "identity", position = "dodge") + theme(axis.title.x = element_blank()) + ggtitle("Decomposition speed")
pmean_k

# Plotting all the barplots together
pdf("2_tea_indexes/graphs/mean_differences.pdf", width = 12, height = 8)
pmean_S
pmean_k
dev.off()


          

