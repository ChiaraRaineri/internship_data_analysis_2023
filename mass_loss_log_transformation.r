library(ggplot2)
library(ggpubr)
library(tidyverse)
library(broom)
library(AICcmodavg)

# This time all I need is the mass loss percentage (?)
mass_data_anova <- read.csv("1_mass_loss/mass_loss_for_anova.csv", header = TRUE, colClasses = c("factor", "factor", "factor", "factor", 
                                                                                                 "numeric", "numeric", "numeric", "numeric", 
                                                                                                 "numeric", "numeric", "numeric", "numeric"))
str(mass_data_anova)


# I used the steps in this website https://dzchilds.github.io/stats-for-bio/data-transformations.html

# LITTER

# mod_litter <- lm(litter_massloss ~ restoration, data = mass_data_anova)
# plot(mod_litter, which = 2)

mass_data_anova <- mutate(mass_data_anova, loglitter_massloss = log10(litter_massloss))
# Now I have a new column with the transformed values (I transformed a percentage)


# Comparison between normal two_way anova and log data two-way anova
# It doesn't seem too much different (I don't know if log transformation is significant)
anova2_litter_log <- aov(loglitter_massloss ~ restoration + management, data = mass_data_anova)  # New anova
summary(anova2_litter_log)
anova2_litter <- aov(litter_massloss ~ restoration + management, data = mass_data_anova)  # Old anova
summary(anova2_litter)  
# F-value and p-value are not too different in the two models

# Combined anova
anova2_litter_comb_log <- aov(loglitter_massloss ~ restoration * management, data = mass_data_anova)
summary(anova2_litter_comb_log)  # restoration:management is slightly significant (in the normal anova it wasn't)
anova2_litter_comb <- aov(litter_massloss ~ restoration * management, data = mass_data_anova)
summary(anova2_litter_comb)

# Adding "river" to anova
anova2_litter_river_log <- aov(loglitter_massloss ~ restoration + management + river, data = mass_data_anova)
summary(anova2_litter_river_log)
anova2_litter_river <- aov(litter_massloss ~ restoration + management + river, data = mass_data_anova)
summary(anova2_litter_river)
# No difference




# MEAN GREEN
mass_data_anova <- mutate(mass_data_anova, logmean_green = log10(mean_green))
anova2_green_log <- aov(logmean_green ~ restoration + management, data = mass_data_anova)
summary(anova2_green_log)
anova2_mean_green <- aov(mean_green ~ restoration + management, data = mass_data_anova)
summary(anova2_mean_green)
# No difference