library(ggplot2)
library(ggpubr)
library(tidyverse)
library(broom)
library(dplyr)
library(car)
library(report)
library(viridis)
library(ggpmisc)
library(RColorBrewer)
# library(writexl)


### SOIL TEMPERATURE AND SOIL MOISTURE FROM FIELD DATA ###

mod <- read.csv("3_linear_models/model_data.csv", header = TRUE, colClasses = c("factor", "factor", "factor", "factor", "factor", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))

str(mod)
summary(mod)


## SOIL TEMPERATURE ##

# S
# Correlation test
cor.test(mod$soil_temp, mod$S, method = "pearson")
# p-value = 0.01576 (they are correlated)
# correlation coefficient = -0.2939484 (negative correlation)

cor.test(mod$soil_temp, mod$S, method = "kendall")


# Final scatter plot
scat1 <- ggplot(mod, aes(x = soil_temp, y = S, colour=Treatment, group = 1)) +
  geom_point(size = 2, aes(colour = Treatment)) +
  geom_smooth(method = "lm", se = FALSE, color = "black", fullrange=TRUE, linewidth = 0.5) +
  stat_poly_eq(use_label(c("p", "R2"))) +
  labs(y= "Stabilization factor (S)", x = "Soil temperature") +
  theme_classic()

reg_soil_temp <- scat1 + scale_colour_brewer(palette = "Paired")


png("3_linear_models/graphs/scatter_soil_temp_S.png", res = 150, width = 1500, height = 880)
reg_soil_temp
dev.off()



# k
# Correlation test
cor.test(mod$soil_temp, mod$k, method = "pearson")
# p-value = 0.008568 (they are correlated)
# correlation coefficient = -0.3259972 (negative correlation)

cor.test(mod$soil_temp, mod$S, method = "kendall")


# Final scatter plot
scat2 <- ggplot(mod, aes(x = soil_temp, y = k, colour=Treatment, group = 1)) +
  geom_point(size = 2, aes(colour = Treatment)) +
  geom_smooth(method = "lm", se = FALSE, color = "black", fullrange=TRUE, linewidth = 0.5) +
  stat_poly_eq(use_label(c("p", "R2"))) +
  labs(y= "Decomposition rate (k)", x = "Soil temperature") +
  theme_classic()

reg_soil_temp_k <- scat2 + scale_colour_brewer(palette = "Paired")


png("3_linear_models/graphs/scatter_soil_temp_k.png", res = 150, width = 1500, height = 880)
reg_soil_temp_k
dev.off()



# LITTER
# Correlation test
cor.test(mod$soil_temp, mod$litter_massloss, method = "pearson")
# p-value = 0.2685 (they are NOT correlated)


# Final scatter plot
ggplot(mod, aes(x = soil_temp, y = litter_massloss, colour=Treatment, group = 1)) +
  geom_point(size = 2, aes(colour = Treatment)) +
  geom_smooth(method = "lm", se = FALSE, color = "black", fullrange=TRUE, linewidth = 0.5) +
  stat_poly_eq(use_label(c("p", "R2"))) +
  labs(y= "Mass loss (%)", x = "Soil temperature") +
  theme_classic()




## SOIL MOISTURE ##

# S
# Correlation test
cor.test(mod$soil_moisture, mod$S, method = "pearson")
# p-value = 1.705e-08 (they are correlated)
# correlation coefficient = -0.6201177 (negative correlation)


# Final scatter plot
scat3 <- ggplot(mod, aes(x = soil_moisture, y = S, colour=Treatment, group = 1)) +
  geom_point(size = 2, aes(colour = Treatment)) +
  geom_smooth(method = "lm", se = FALSE, color = "black", fullrange=TRUE, linewidth = 0.5) +
  stat_poly_eq(use_label(c("p", "R2"))) +
  labs(y= "Stabilization factor (S)", x = "Soil moisture") +
  theme_classic()

reg_soil_moist_S <- scat3 + scale_colour_brewer(palette = "Paired")


png("3_linear_models/graphs/scatter_soil_moist_S.png", res = 150, width = 1500, height = 880)
reg_soil_moist_S
dev.off()



# k
# Correlation test
cor.test(mod$soil_moisture, mod$k, method = "pearson")
# p-value = 3.685e-08 (they are correlated)
# correlation coefficient = -0.6198061 (negative correlation)

cor.test(mod$soil_moisture, mod$S, method = "kendall")


# Final scatter plot
scat4 <- ggplot(mod, aes(x = soil_moisture, y = k, colour=Treatment, group = 1)) +
  geom_point(size = 2, aes(colour = Treatment)) +
  geom_smooth(method = "lm", se = FALSE, color = "black", fullrange=TRUE, linewidth = 0.5) +
  stat_poly_eq(use_label(c("p", "R2"))) +
  labs(y= "Decomposition rate (k)", x = "Soil moisture") +
  theme_classic()

reg_soil_moist_k <- scat4 + scale_colour_brewer(palette = "Paired")


png("3_linear_models/graphs/scatter_soil_moist_k.png", res = 150, width = 1500, height = 880)
reg_soil_moist_k
dev.off()



# LITTER
# Correlation test
cor.test(mod$soil_moisture, mod$litter_massloss, method = "pearson")
# p-value = 0.004898 (they are correlated)
# correlation coefficient = -0.3475201 (negative correlation)


# Final scatter plot
scatl <- ggplot(mod, aes(x = soil_moisture, y = litter_massloss, colour=Treatment, group = 1)) +
  geom_point(size = 2, aes(colour = Treatment)) +
  geom_smooth(method = "lm", se = FALSE, color = "black", fullrange=TRUE, linewidth = 0.5) +
  stat_poly_eq(use_label(c("p", "R2"))) +
  labs(y= "Mass loss (%)", x = "Soil temperature") +
  theme_classic()

reg_soil_moist_l <- scatl + scale_colour_brewer(palette = "Paired")




### ELLENBERG VALUES ###


## SOIL HUMIDITY OR MOISTURE ##

# S
# Correlation test
cor.test(mod$E_moisture, mod$S, method = "pearson")
# p-value = 0.0004541 (they are correlated)
# correlation coefficient = -0.4136705 (negative correlation)


# Final scatter plot
scat5 <- ggplot(mod, aes(x = E_moisture, y = S, colour=Treatment, group = 1)) +
  geom_point(size = 2, aes(colour = Treatment)) +
  geom_smooth(method = "lm", se = FALSE, color = "black", fullrange=TRUE, linewidth = 0.5) +
  stat_poly_eq(use_label(c("p", "R2"))) +
  labs(y= "Stabilization factor (S)", x = "Ellenberg F") +
  theme_classic()

reg_E_moist <- scat5 + scale_colour_brewer(palette = "Paired")


png("3_linear_models/graphs/scatter_E_moist_S.png", res = 150, width = 1500, height = 880)
reg_E_moist
dev.off()



# k
# Correlation test
cor.test(mod$E_moisture, mod$k, method = "pearson")
# p-value = 0.08278 (they are NOT correlated)
# Slight negative correlation


# Final scatter plot
scat6 <- ggplot(mod, aes(x = E_moisture, y = k, colour=Treatment, group = 1)) +
  geom_point(size = 2, aes(colour = Treatment)) +
  geom_smooth(method = "lm", se = FALSE, color = "black", fullrange=TRUE, linewidth = 0.5) +
  stat_poly_eq(use_label(c("p", "R2"))) +
  labs(y= "Decomposition rate (k)", x = "Ellenberg F") +
  theme_classic()

scat6 + scale_colour_brewer(palette = "Paired")



# LITTER
# Correlation test
cor.test(mod$E_moisture, mod$litter_massloss, method = "pearson")
# p-value = 0.006316 (they are correlated)
# correlation coefficient = -0.3379217 (negative correlation)


# Final scatter plot
scat7 <- ggplot(mod, aes(x = E_moisture, y = litter_massloss, colour=Treatment, group = 1)) +
  geom_point(size = 2, aes(colour = Treatment)) +
  geom_smooth(method = "lm", se = FALSE, color = "black", fullrange=TRUE, linewidth = 0.5) +
  stat_poly_eq(use_label(c("p", "R2"))) +
  labs(y= "Mass loss (%)", x = "Ellenberg F") +
  theme_classic()

reg_E_moist_l <- scat7 + scale_colour_brewer(palette = "Paired")


png("3_linear_models/graphs/scatter_E_moist_l.png", res = 150, width = 1500, height = 880)
reg_E_moist_l
dev.off()




## SOIL FERTILITY OR PRODUCTIVITY ##

# S
# Correlation test
cor.test(mod$E_soil_fertility, mod$S, method = "pearson")
# p-value = 8.866e-06 (they are correlated)
# correlation coefficient = 0.5100654  (positive correlation)


# Final scatter plot
scat8 <- ggplot(mod, aes(x = E_soil_fertility, y = S, colour=Treatment, group = 1)) +
  geom_point(size = 2, aes(colour = Treatment)) +
  geom_smooth(method = "lm", se = FALSE, color = "black", fullrange=TRUE, linewidth = 0.5) +
  stat_poly_eq(use_label(c("p", "R2"))) +
  labs(y= "Stabilization factor (S)", x = "Ellenberg N") +
  theme_classic()

reg_E_fert_S <- scat8 + scale_colour_brewer(palette = "Paired")


png("3_linear_models/graphs/scatter_E_fert_S.png", res = 150, width = 1500, height = 880)
reg_E_fert_S
dev.off()



# k
# Correlation test
cor.test(mod$E_soil_fertility, mod$k, method = "pearson")
# p-value = 1.447e-05 (they are NOT correlated)
# correlation coefficient = 0.5097086  (positive correlation)


# Final scatter plot
scat9 <- ggplot(mod, aes(x = E_soil_fertility, y = k, colour=Treatment, group = 1)) +
  geom_point(size = 2, aes(colour = Treatment)) +
  geom_smooth(method = "lm", se = FALSE, color = "black", fullrange=TRUE, linewidth = 0.5) +
  stat_poly_eq(use_label(c("p", "R2"))) +
  labs(y= "Decomposition rate (k)", x = "Ellenberg N") +
  theme_classic()

reg_E_fert_k <- scat9 + scale_colour_brewer(palette = "Paired")

png("3_linear_models/graphs/scatter_E_fert_k.png", res = 150, width = 1500, height = 880)
reg_E_fert_k
dev.off()



# LITTER
# Correlation test
cor.test(mod$E_soil_fertility, mod$litter_massloss, method = "pearson")
# p-value = 0.007847 (they are correlated)
# correlation coefficient = 0.3294824 (positive correlation)


# Final scatter plot
scat10 <- ggplot(mod, aes(x = E_soil_fertility, y = litter_massloss, colour=Treatment, group = 1)) +
  geom_point(size = 2, aes(colour = Treatment)) +
  geom_smooth(method = "lm", se = FALSE, color = "black", fullrange=TRUE, linewidth = 0.5) +
  stat_poly_eq(use_label(c("p", "R2"))) +
  labs(y= "Mass loss (%)", x = "Ellenberg N") +
  theme_classic()

reg_E_fert_l <- scat10 + scale_colour_brewer(palette = "Paired")


png("3_linear_models/graphs/scatter_E_fert_l.png", res = 150, width = 1500, height = 880)
reg_E_fert_l
dev.off()

