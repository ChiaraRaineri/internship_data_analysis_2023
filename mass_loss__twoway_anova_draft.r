library(ggplot2)
library(ggpubr)
library(tidyverse)
library(broom)
library(AICcmodavg)

setwd("C:/internship/")

# I separated the "code" independent variable in different variables:
# name of the river = OM, OD, TR
# management type = managed, unmanaged
# restoration type = near_natural, restored

mass_data_anova <- read.csv("1_mass_loss/mass_loss_for_anova.csv", header = TRUE, colClasses = c("factor", "factor", "factor", "factor", 
                                                                                            "numeric", "numeric", "numeric", "numeric", 
                                                                                            "numeric", "numeric", "numeric", "numeric"))
str(mass_data_anova)
summary(mass_data_anova)


# Shapiro-Wilk test
shapiro.test(mass_data_anova$litter_massloss)  # p-value = 0.5066
shapiro.test(mass_data_anova$litter_cg_massloss)  # p-value = 0.0002447   (!)
shapiro.test(mass_data_anova$green_l_massloss)  # p-value = 0.03206   (!)
shapiro.test(mass_data_anova$green_t_massloss)  # p-value = 0.2352
shapiro.test(mass_data_anova$mean_green)  # p-value = 0.1625
shapiro.test(mass_data_anova$red_l_massloss)  # p-value = 0.6639
shapiro.test(mass_data_anova$red_t_massloss)  # p-value = 2.877e-11   (!)
shapiro.test(mass_data_anova$mean_red)  # p-value = 5.2e-06   (!)



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





