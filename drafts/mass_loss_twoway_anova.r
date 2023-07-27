library(ggplot2)
library(ggpubr)
library(tidyverse)
library(broom)
library(AICcmodavg)
library(lmtest)
library(dplyr)
library(gridExtra)

setwd("C:/internship/")

# I separated the "code" independent variable in different variables:
# name of the river = OM, OD, TR
# management type = managed, unmanaged
# restoration type = near_natural, restored

# This data set contains the mass loss data, the restoration type (restored, near-natural) and the management (managed, unmanaged), as well as the river valley (TR, OM, OD)
mass_data_anova <- read.csv("1_mass_loss/mass_loss_for_anova.csv", header = TRUE, colClasses = c("factor", "factor", "factor", "factor", 
                                                                                            "numeric", "numeric", "numeric", "numeric", 
                                                                                            "numeric", "numeric", "numeric", "numeric"))
str(mass_data_anova)
summary(mass_data_anova)

# This data set contains only the mass loss data and the treatment (RM, RU, NM, NU)
# I need it to check if there is homoscedasticity in the log-transformed data
data_dist <- read.csv("1_mass_loss/mass_loss_for_distribution.csv", header = TRUE, colClasses = c("factor", "factor", "numeric", "numeric", "numeric", "numeric"))
str(data_dist)
summary(data_dist)


# Shapiro-Wilk test
shapiro.test(mass_data_anova$litter_massloss)  # p-value = 0.5066
shapiro.test(mass_data_anova$litter_cg_massloss)  # p-value = 0.0002447   (!)
shapiro.test(mass_data_anova$mean_green)  # p-value = 0.1625
shapiro.test(mass_data_anova$mean_red)  # p-value = 5.2e-06   (!)


## TWO-WAY ANOVA ##

# Litter data and mean red tea data respect homoscedasticity, so I can perform the two-way anova without further transformations


#### Litter ####
anova2_litter <- aov(litter_massloss ~ restoration + management, data = mass_data_anova)   # This model (additive) assumes there is no interaction between the two independent variables
summary(anova2_litter)  # p-value restoration = 0.119  # p-value management = 0.567
anova2_litter_comb <- aov(litter_massloss ~ restoration * management, data = mass_data_anova)  # This model assumes that there is an interaction between the two independent variables
summary(anova2_litter_comb)  # p-value restoration:management = 0.013

# Tukey's test
tukey_litter <- TukeyHSD(anova2_litter)
tukey_litter

# Table with factors, means and standard deviation using dplyr package
# First I have to remove all the NAs from the data frame
mass_data_anova <- na.omit(mass_data_anova)
# Table
table_litter <- group_by(mass_data_anova, restoration, management) %>%
  summarise(mean=mean(litter_massloss), sd=sd(litter_massloss)) %>%
  arrange(desc(mean))
table_litter

# Barplot
# https://statdoe.com/barplot-for-two-factors-in-r/
pmean_litt <-  ggplot(table_litter, aes(x = factor(management), y = mean, fill = restoration, colour = restoration)) + 
geom_bar(stat = "identity", position = "dodge") + theme(axis.title.x = element_blank()) + ggtitle("Litter")


#### Red tea ####
anova2_mean_red <- aov(mean_red ~ restoration + management, data = mass_data_anova)
summary(anova2_mean_red)  # p-value restoration = 0.971  # p-value management = 0.315
anova2_mean_red_comb <- aov(mean_red ~ restoration * management, data = mass_data_anova)
summary(anova2_mean_red_comb)  # p-value restoration:management = 0.747

# Tukey's test
tukey_red <- TukeyHSD(anova2_mean_red)
tukey_red

# Table
table_red <- group_by(mass_data_anova, restoration, management) %>%
  summarise(mean=mean(mean_red), sd=sd(mean_red)) %>%
  arrange(desc(mean))
table_red

# Barplot
pmean_red <- ggplot(table_red, aes(x = factor(management), y = mean, fill = restoration, colour = restoration)) + 
geom_bar(stat = "identity", position = "dodge") + theme(axis.title.x = element_blank()) + ggtitle("Red tea")



# To perform anova on litter common garden data and mean green tea data I have to transform them
# log transformation


#### Litter common garden ####

# Transformation #
mass_data_anova <- mutate(mass_data_anova, loglitter_cg_massloss = log10(litter_cg_massloss))  # Now the data are log transformed
data_dist <- mutate(data_dist, loglitter_cg_massloss = log10(litter_cg_massloss)) 
# Let's see if homoscedasticity is present
# One explanatory variable
mod_litter_cg_log <- lm(loglitter_cg_massloss ~ treatment, data = data_dist, na.action = na.exclude)
summary(mod_litter_cg_log)
# Breusch-Pagan test
bptest(mod_litter_cg_log)   # p-value = 0.4237
# I can't reject the null hypothesis (homoscedasticity is present) 
# Visual method
par(mfrow = c(2, 2))
plot(mod_litter_cg_log)
# Two explanatory variables
mod_litter_cg_log2 <- lm(loglitter_cg_massloss ~ restoration + management, data = mass_data_anova, na.action = na.exclude)
summary(mod_litter_cg_log2)
# Breusch-Pagan test
bptest(mod_litter_cg_log2)   # p-value = 0.221 (homoscedasticity is present)
# Visual method
par(mfrow = c(2, 2))
plot(mod_litter_cg_log2)

# Two-way anova #
anova2_littercg_log <- aov(loglitter_cg_massloss ~ restoration + management, data = mass_data_anova)  
summary(anova2_littercg_log)  # p-value restoration = 0.000771  # p-value management = 0.048263
anova2_littercg_comb_log <- aov(loglitter_cg_massloss ~ restoration * management, data = mass_data_anova)
summary(anova2_littercg_comb_log)  # p-value restoration:management = 0.747

# Tukey's test
tukey_cg <- TukeyHSD(anova2_littercg_log)
tukey_cg

# Table
table_cg <- group_by(mass_data_anova, restoration, management) %>%
  summarise(mean=mean(litter_cg_massloss), sd=sd(litter_cg_massloss)) %>%
  arrange(desc(mean))
table_cg

# Barplot
pmean_cg <- ggplot(table_cg, aes(x = factor(management), y = mean, fill = restoration, colour = restoration)) + 
geom_bar(stat = "identity", position = "dodge") + theme(axis.title.x = element_blank()) + ggtitle("Litter common garden")



#### Green tea ####

# Transformation #
mass_data_anova <- mutate(mass_data_anova, logmean_green = log10(mean_green))  # Now the data are log transformed
data_dist <- mutate(data_dist, logmean_green = log10(mean_green))
# Let's see if homoscedasticity is present
# One explanatory variable
mod_green_log <- lm(logmean_green ~ treatment, data = data_dist, na.action = na.exclude)
summary(mod_green_log)
# Breusch-Pagan test
bptest(mod_green_log)   # p-value = 0.01718
# I reject the null hypothesis (heteroscedasticity is present) 
# Visual method
par(mfrow = c(2, 2))
plot(mod_green_log)
# Two explanatory variables
mod_green_log2 <- lm(logmean_green ~ restoration + management, data = mass_data_anova, na.action = na.exclude)
summary(mod_green_log2)
# Breusch-Pagan test
bptest(mod_green_log2)   # p-value = 0.02067 (heteroscedasticity is present)
# Visual method
par(mfrow = c(2, 2))
plot(mod_green_log2)
# I still can't perform anova with green tea data

# Even though the data are not normally distributed, I'll do the two-way anova as usual, being extra careful with the results
anova2_mean_green <- aov(mean_green ~ restoration + management, data = mass_data_anova)
summary(anova2_mean_green)  # p-value restoration = 0.00189  # p-value management = 0.37930
anova2_mean_green_comb <- aov(mean_green ~ restoration * management, data = mass_data_anova)
summary(anova2_mean_green_comb)  # p-value restoration:management = 0.15350

# Tukey's test
tukey_green <- TukeyHSD(anova2_mean_green)
tukey_green

# Table
table_green <- group_by(mass_data_anova, restoration, management) %>%
  summarise(mean=mean(mean_green), sd=sd(mean_green)) %>%
  arrange(desc(mean))
table_green

# Barplot
pmean_green <- ggplot(table_green, aes(x = factor(management), y = mean, fill = restoration, colour = restoration)) + 
geom_bar(stat = "identity", position = "dodge") + theme(axis.title.x = element_blank()) + ggtitle("Green tea")


# Plotting all the barplots together
pdf("mean_differences.pdf", width = 12, height = 8)
pmean_litt
pmean_red
pmean_cg
pmean_green
grid.arrange(pmean_litt, pmean_red, pmean_cg, pmean_green, nrow=2)
dev.off()

















### FIRST DRAFT, NOT CORRECT ###
# two-way ANOVA  (data are not normally distributed!!)

# litter
anova2_litter <- aov(litter_massloss ~ restoration + management, data = mass_data_anova)
summary(anova2_litter)  # p-value restoration = 0.119  # p-value management = 0.567
anova2_litter_comb <- aov(litter_massloss ~ restoration * management, data = mass_data_anova)
summary(anova2_litter_comb)  # p-value restoration:management = 0.013
anova2_litter_river <- aov(litter_massloss ~ restoration + management + river, data = mass_data_anova)
summary(anova2_litter_river)  # p-value river = 0.174

anova2_litter_cg <- aov(litter_cg_massloss ~ restoration + management, data = mass_data_anova)
summary(anova2_litter_cg)  # p-value restoration = 0.000131  # p-value management = 0.011865
anova2_litter_cg_comb <- aov(litter_cg_massloss ~ restoration * management, data = mass_data_anova)
summary(anova2_litter_cg_comb)  # p-value restoration:management = 0.082941
anova2_litter_cg_river <- aov(litter_cg_massloss ~ restoration + management + river, data = mass_data_anova)
summary(anova2_litter_cg_river)  # p-value river = 0.00742

# green tea
anova2_green_l <- aov(green_l_massloss ~ restoration + management, data = mass_data_anova)
summary(anova2_green_l)  # p-value restoration = 0.00414  # p-value management = 0.22058
anova2_green_l_comb <- aov(green_l_massloss ~ restoration * management, data = mass_data_anova)
summary(anova2_green_l_comb)  # p-value restoration:management = 0.49094
anova2_green_l_river <- aov(green_l_massloss ~ restoration + management + river, data = mass_data_anova)
summary(anova2_green_l_river)  # p-value river = 5.3e-05

anova2_green_t <- aov(green_t_massloss ~ restoration + management, data = mass_data_anova)
summary(anova2_green_t)  # p-value restoration = 0.00481  # p-value management = 0.46532
anova2_green_t_comb <- aov(green_t_massloss ~ restoration * management, data = mass_data_anova)
summary(anova2_green_t_comb)  # p-value restoration:management = 0.10112
anova2_green_t_river <- aov(green_t_massloss ~ restoration + management + river, data = mass_data_anova)
summary(anova2_green_t_river)  # p-value river = 9.59e-06

anova2_mean_green <- aov(mean_green ~ restoration + management, data = mass_data_anova)
summary(anova2_mean_green)  # p-value restoration = 0.00189  # p-value management = 0.37930
anova2_mean_green_comb <- aov(mean_green ~ restoration * management, data = mass_data_anova)
summary(anova2_mean_green_comb)  # p-value restoration:management = 0.15350
anova2_mean_green_river <- aov(mean_green ~ restoration + management + river, data = mass_data_anova)
summary(anova2_mean_green_river)  # p-value river = 3.64e-07

# red tea
anova2_red_l <- aov(red_l_massloss ~ restoration + management, data = mass_data_anova)
summary(anova2_red_l)  # p-value restoration = 0.0921  # p-value management = 0.4383
anova2_red_l_comb <- aov(red_l_massloss ~ restoration * management, data = mass_data_anova)
summary(anova2_red_l_comb)  # p-value restoration:management = 0.9134
anova2_red_l_river <- aov(red_l_massloss ~ restoration + management + river, data = mass_data_anova)
summary(anova2_red_l_river)  # p-value river = 0.00744

anova2_red_t <- aov(red_t_massloss ~ restoration + management, data = mass_data_anova)
summary(anova2_red_t)  # p-value restoration = 0.494  # p-value management = 0.693
anova2_red_t_comb <- aov(red_t_massloss ~ restoration * management, data = mass_data_anova)
summary(anova2_red_t_comb)  # p-value restoration:management = 0.703
anova2_red_t_river <- aov(red_t_massloss ~ restoration + management + river, data = mass_data_anova)
summary(anova2_red_t_river)  # p-value river = 0.341

anova2_mean_red <- aov(mean_red ~ restoration + management, data = mass_data_anova)
summary(anova2_mean_red)  # p-value restoration = 0.971  # p-value management = 0.315
anova2_mean_red_comb <- aov(mean_red ~ restoration * management, data = mass_data_anova)
summary(anova2_mean_red_comb)  # p-value restoration:management = 0.747
anova2_mean_red_river <- aov(mean_red ~ restoration + management + river, data = mass_data_anova)
summary(anova2_mean_red_river)  # p-value river = 0.0141


# Checking which is the best-fit model for my data using the Akaike information criterion (AIC)
model_names <- c("two_way", "combined", "plus_river")

model_litter <- list(anova2_litter, anova2_litter_comb, anova2_litter_river)
aictab(model_litter, modnames = model_names)  # The "combined" model is the best fit

model_litter_cg <- list(anova2_litter_cg, anova2_litter_cg_comb, anova2_litter_cg_river)
aictab(model_litter_cg, modnames = model_names)  # The "plus_river" model is the best fit

model_green_l <- list(anova2_green_l, anova2_green_l_comb, anova2_green_l_river)
aictab(model_green_l, modnames = model_names)  # The "plus_river" model is the best fit

model_green_t <- list(anova2_green_t, anova2_green_t_comb, anova2_green_t_river)
aictab(model_green_t, modnames = model_names)  # The "plus_river" model is the best fit

model_mean_green <- list(anova2_mean_green, anova2_mean_green_comb, anova2_mean_green_river)
aictab(model_mean_green, modnames = model_names)  # The "plus_river" model is the best fit

model_red_l <- list(anova2_red_l, anova2_red_l_comb, anova2_red_l_river)
aictab(model_red_l, modnames = model_names)  # The "plus_river" model is the best fit

model_red_t <- list(anova2_red_t, anova2_red_t_comb, anova2_red_t_river)
aictab(model_red_t, modnames = model_names)  # The "two_way" model is the best fit

model_mean_red <- list(anova2_mean_red, anova2_mean_red_comb, anova2_mean_red_river)
aictab(model_mean_red, modnames = model_names)  # The "plus_river" model is the best fit


# Check for homoscedasticity
par(mfrow=c(2,2))
plot(anova2_litter_comb)
par(mfrow=c(1,1))





