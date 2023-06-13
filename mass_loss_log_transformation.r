library(ggplot2)
library(ggpubr)
library(tidyverse)
library(broom)
library(AICcmodavg)

mass_data_anova <- read.csv("1_mass_loss/mass_loss_for_anova.csv", header = TRUE, colClasses = c("factor", "factor", "factor", "factor", 
                                                                                                 "numeric", "numeric", "numeric", "numeric", 
                                                                                                 "numeric", "numeric", "numeric", "numeric"))
str(mass_data_anova)

##
mod_litter <- lm(litter_massloss ~ restoration, data = mass_data_anova)
plot(mod_litter, which = 2)

mass_data_anova <- mutate(mass_data_anova, loglitter_massloss = log10(litter_massloss))


# Comparison between normal two_way anova and log data two-way anova
# It doesn't seem too much different (I don't know if log transformation is significant)

anova2_litter_log <- aov(loglitter_massloss ~ restoration + management, data = mass_data_anova)
summary(anova2_litter_log)
anova2_litter <- aov(litter_massloss ~ restoration + management, data = mass_data_anova)
summary(anova2_litter)  


mass_data_anova <- mutate(mass_data_anova, logmean_green = log10(mean_green))
anova2_green_log <- aov(logmean_green ~ restoration + management, data = mass_data_anova)
summary(anova2_green_log)
anova2_mean_green <- aov(mean_green ~ restoration + management, data = mass_data_anova)
summary(anova2_mean_green)
