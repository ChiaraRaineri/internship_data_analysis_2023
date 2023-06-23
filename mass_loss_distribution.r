library(ggplot2)
library(ggpubr)
library(tidyverse)
library(broom)
library(AICcmodavg)

# install.packages("lmtest")
library(lmtest)


# I need to verify if the data from the different treatments (RM, RU, NM, NU) are normally distributed
# Most importantly, if they respect homoscendasticity

data_dist <- read.csv("1_mass_loss/mass_loss_for_distribution.csv", header = TRUE, colClasses = c("factor", "factor", "numeric", "numeric", "numeric", "numeric"))

str(data_dist)
summary(data_dist)


# Let's start with some boxplots with the treatments for every parameter

# LITTER
plitt2 <- ggplot(data_dist, aes(treatment, litter_massloss, fill = treatment)) + geom_boxplot(show.legend = FALSE) + labs(x="Treatment", y="Mass loss") + theme_bw() +
ggtitle("Litter from the sites") + theme(plot.title = element_text(hjust = 0.5)) + 
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
scale_fill_brewer(palette="Paired") + ylim(0,1)

# LITTER FROM COMMON GARDEN
plittcg2 <- ggplot(data_dist, aes(treatment, litter_cg_massloss, fill = treatment)) + geom_boxplot(show.legend = FALSE) + labs(x="Treatment", y="Mass loss") + theme_bw() +
ggtitle("Litter from the common garden") + theme(plot.title = element_text(hjust = 0.5)) + 
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
scale_fill_brewer(palette="Paired") + ylim(0,1)

# GREEN TEA
pmg2 <- ggplot(data_dist, aes(treatment, mean_green, fill = treatment)) + geom_boxplot(show.legend = FALSE) + labs(x="Treatment", y="Mass loss") + theme_bw() +
ggtitle("Mean values of green tea") + theme(plot.title = element_text(hjust = 0.5)) + 
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
scale_fill_brewer(palette="Paired") + ylim(0,1)

# RED TEA
pmr2 <- ggplot(data_dist, aes(treatment, mean_red, fill = treatment)) + geom_boxplot(show.legend = FALSE) + labs(x="Treatment", y="Mass loss") + theme_bw() +
ggtitle("Mean values of red tea") + theme(plot.title = element_text(hjust = 0.5)) + 
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
scale_fill_brewer(palette="Paired") + ylim(0,1)



pdf("plots_treatment.pdf", width = 12, height = 8)
plitt2
plittcg2
pmg2
pmr2
dev.off()


##########

# Histograms (skewedness)

# pdf("histograms.pdf")
par(mfrow = c(2,2))
hist(data_dist$litter_massloss, xlab = "Percentage of mass loss", main = "Litter from the sites")
hist(data_dist$litter_cg_massloss, xlab = "Percentage of mass loss", main = "Litter from common garden")
hist(data_dist$mean_green, xlab = "Percentage of mass loss", main = "Green tea")
hist(data_dist$mean_red, xlab = "Percentage of mass loss", main = "Red tea")
# dev.off()



##########

# Homoscedasticity test
# H0 = homoscedacity is present (residuals are distributed with equal variance)

### LITTER ###

# One explanatory variable #
mod_litter <- lm(litter_massloss ~ treatment, data = data_dist, na.action = na.exclude)
summary(mod_litter)

# Breusch-Pagan test to assess homoscedasticity (a small p-value indicates that residual variance is non-constant (heteroscedastic))
bptest(mod_litter)   # p-value = 0.1089
# I can't reject the null hypothesis (homoscedasticity is present) 

# Visual method
par(mfrow = c(2, 2))
plot(mod_litter)


# Two explanatory variables #
mass_data_anova <- read.csv("1_mass_loss/mass_loss_for_anova.csv", header = TRUE, colClasses = c("factor", "factor", "factor", "factor", 
                                                                                                 "numeric", "numeric", "numeric", "numeric", 
                                                                                                 "numeric", "numeric", "numeric", "numeric"))
mod_litter2 <- lm(litter_massloss ~ restoration + management, data = mass_data_anova, na.action = na.exclude)
summary(mod_litter2)

bptest(mod_litter2)   # p-value = 0.09099

# Visual method
par(mfrow = c(2, 2))
plot(mod_litter2)





### LITTER FROM COMMON GARDEN ###

# One explanatory variable #
mod_litter_cg <- lm(litter_cg_massloss ~ treatment, data = data_dist, na.action = na.exclude)
summary(mod_litter_cg)

# Breusch-Pagan test
bptest(mod_litter_cg)   # p-value = 0.03739
# I reject the null hypothesis (heteroscedasticity is present) 

# Visual method
par(mfrow = c(2, 2))
plot(mod_litter_cg)


# Two explanatory variables #
mod_litter_cg2 <- lm(litter_cg_massloss ~ restoration + management, data = mass_data_anova, na.action = na.exclude)
summary(mod_litter_cg2)

bptest(mod_litter_cg2)   # p-value = 0.008113

# Visual method
par(mfrow = c(2, 2))
plot(mod_litter_cg2)






### GREEN TEA ###

# One explanatory variable #
mod_green <- lm(mean_green ~ treatment, data = data_dist, na.action = na.exclude)
summary(mod_green)

# Breusch-Pagan test
bptest(mod_green)   # p-value = 0.01214
# I reject the null hypothesis (heteroscedasticity is present) 

# Visual method
par(mfrow = c(2, 2))
plot(mod_green)


# Two explanatory variables #
mod_green2 <- lm(mean_green ~ restoration + management, data = mass_data_anova, na.action = na.exclude)
summary(mod_green2)

bptest(mod_green2)   # p-value = 0.01157

# Visual method
par(mfrow = c(2, 2))
plot(mod_green2)






### RED TEA ###

# One explanatory variable #
mod_red <- lm(mean_red ~ treatment, data = data_dist, na.action = na.exclude)
summary(mod_red)

# Breusch-Pagan test
bptest(mod_red)   # p-value = 0.6184
# I can't reject the null hypothesis (homoscedasticity is present)

# Visual method
par(mfrow = c(2, 2))
plot(mod_red)


# Two explanatory variables #
mod_red2 <- lm(mean_red ~ restoration + management, data = mass_data_anova, na.action = na.exclude)
summary(mod_red2)

bptest(mod_red2)   # p-value = 0.4688

# Visual method
par(mfrow = c(2, 2))
plot(mod_red2)



