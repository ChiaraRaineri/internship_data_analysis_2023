# The first step of my analysis is to calculate the percentage mass loss from the litter in the litterbags and the tea in the teabags (green and rooibos)
# This way I will be able to compare them

# No common garden, only data from the sites
# For the teas, initial and final weight without the bag are used


# install.packages(c("ggplot2", "ggpubr", "tidyverse", "broom", "AICcmodavg"))
library(ggplot2)
library(ggpubr)
library(tidyverse)
library(broom)
library(AICcmodavg)

setwd("C:/internship/")     # Same version is also on RStudio

########

# This data frame contains raw data from the field and the calculation of percentage mass loss
# Mass loss is calculated as (initial_weight -  final_weight) / initial_weight

mass_data <- read.csv("1_mass_loss/mass_loss_sites_percentage.csv", header = TRUE, colClasses = c("factor", "factor", "factor", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))
mass_data

str(data)   

# Converting chr into factor using base R  # Not useful only practice
# mass_data$site <- factor(mass_data$site, levels = c("Ellebaekengen", "Stolpehuse", "Trolle", "Lille_Linde", "Skt_Klemens", "Klem_krat", "Skallebanke", "Brahetrolle", "Lyng", "Lyng_krat", "Ommevej", "Ommekrat"))
# mass_data$code <- factor(mass_data$code, levels = c("TRNM", "TRNU", "TRRM", "TRRU", "ODNM", "ODNU", "ODRM", "ODRU", "OMNM", "OMNU", "OMRM", "OMRU"))



# Summary statistics
summary(mass_data)


# What do I have to do with the NAs? Should I remove the rows that contain them?
# mass_data <- na.omit(mass_data)


# one-way ANOVA
anova_litter <- aov(litter_massloss ~ code, data = mass_data)
summary(anova_litter)
anova_green_l <- aov(green_l_massloss ~ code, data = mass_data)
summary(anova_green_l)
anova_red_l <- aov(red_l_massloss ~ code, data = mass_data)
summary(anova_red_l)
anova_green_t <- aov(green_t_massloss ~ code, data = mass_data)
summary(anova_green_t)
anova_red_t <- aov(red_t_massloss ~ code, data = mass_data)
summary(anova_red_t)


# boxplots
# litter
ggplot(mass_data, aes(code, litter_massloss, fill = code)) + geom_boxplot(show.legend = FALSE) + labs(x="Site", y="Mass loss") + theme_bw() + 
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
# green tea (l)
ggplot(mass_data, aes(code, green_l_massloss, fill = code)) + geom_boxplot(show.legend = FALSE) + labs(x="Site", y="Mass loss") + theme_bw() + 
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
# red tea (l)
ggplot(mass_data, aes(code, red_l_massloss, fill = code)) + geom_boxplot(show.legend = FALSE) + labs(x="Site", y="Mass loss") + theme_bw() + 
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
# green tea (t)
ggplot(mass_data, aes(code, green_t_massloss, fill = code)) + geom_boxplot(show.legend = FALSE) + labs(x="Site", y="Mass loss") + theme_bw() + 
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
# red tea (t)
ggplot(mass_data, aes(code, red_t_massloss, fill = code)) + geom_boxplot(show.legend = FALSE) + labs(x="Site", y="Mass loss") + theme_bw() + 
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())














