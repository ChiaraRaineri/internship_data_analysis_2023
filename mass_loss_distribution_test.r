library(ggplot2)
library(ggpubr)
library(tidyverse)
library(broom)
library(AICcmodavg)


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

# Homoscendasticity test

mod_litter <- lm(litter_massloss ~ treatment, data = data_dist)

# Doesn't work
residuals_plot <- ggplot(data = data_dist, aes(x = fitted(mod_litter), y = resid(mod_litter))) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE, linetype = "dashed") +
  labs(x = "Fitted Values", y = "Residuals") +
  ggtitle("Residuals vs Fitted Values") +
  theme_minimal()

print(residuals_plot)











