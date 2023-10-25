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


### TABLE WITH SOIL TEMPERATURE AND SOIL MOISTURE (FIELD DATA) ###

mod <- read.csv("3_linear_models/model_data.csv", header = TRUE, colClasses = c("factor", "factor", "factor", "factor", "factor", "numeric", "numeric", "numeric", "numeric"))

str(mod)
summary(mod)


## SOIL TEMPERATURE ##

# S
# Correlation test
cor.test(mod$soil_temp, mod$S, method = "pearson")
# p-value = 0.01576 (they are correlated)
# correlation coefficient = -0.2939484 (negative correlation)

# provisional scatter plot
ggscatter(mod, x = "soil_temp", y = "S", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Soil temperature", ylab = "Stabilization factor")


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

# provisional scatter plot
ggscatter(mod, x = "soil_temp", y = "k", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Soil temperature", ylab = "Decomposition rate")


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





## SOIL MOISTURE ##

# S
# Correlation test
cor.test(mod$soil_moisture, mod$S, method = "pearson")
# p-value = 1.705e-08 (they are correlated)
# correlation coefficient = -0.6201177 (negative correlation)

# provisional scatter plot
ggscatter(mod, x = "soil_moisture", y = "S", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Soil moisture", ylab = "Stabilization factor")


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

# provisional scatter plot
ggscatter(mod, x = "soil_moisture", y = "k", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Soil moisture", ylab = "Decomposition rate")


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

