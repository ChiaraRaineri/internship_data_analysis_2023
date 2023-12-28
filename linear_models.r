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
  labs(y= "Stabilization factor (S)", x = "Soil temperature (°C)") +
  theme_classic() +
  theme(legend.title = element_blank())

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
  labs(y= "Decomposition rate (k)", x = "Soil temperature (°C)") +
  theme_classic() +
  theme(legend.title = element_blank())

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
  theme_classic() +
  theme(legend.title = element_blank())




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
  theme_classic() +
  theme(legend.title = element_blank())

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
  theme_classic() +
  theme(legend.title = element_blank())

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
  theme_classic() +
  theme(legend.title = element_blank())

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
  labs(y= "Stabilization factor (S)", x = "Soil moisture (EIV-F)") +
  theme_classic() +
  theme(legend.title = element_blank())

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
  labs(y= "Decomposition rate (k)", x = "Soil moisture (EIV-F)") +
  theme_classic() +
  theme(legend.title = element_blank())

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
  labs(y= "Mass loss (%)", x = "Soil moisture (EIV-F)") +
  theme_classic() +
  theme(legend.title = element_blank())

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
  labs(y= "Stabilization factor (S)", x = "Soil fertility (EIV-N)") +
  theme_classic() +
  theme(legend.title = element_blank())

reg_E_fert_S <- scat8 + scale_colour_brewer(palette = "Paired")


png("3_linear_models/graphs/scatter_E_fert_S.png", res = 150, width = 1500, height = 880)
reg_E_fert_S
dev.off()



# k
# Correlation test
cor.test(mod$E_soil_fertility, mod$k, method = "pearson")
# p-value = 1.447e-05 (they are correlated)
# correlation coefficient = 0.5097086  (positive correlation)


# Final scatter plot
scat9 <- ggplot(mod, aes(x = E_soil_fertility, y = k, colour=Treatment, group = 1)) +
  geom_point(size = 2, aes(colour = Treatment)) +
  geom_smooth(method = "lm", se = FALSE, color = "black", fullrange=TRUE, linewidth = 0.5) +
  stat_poly_eq(use_label(c("p", "R2"))) +
  labs(y= "Decomposition rate (k)", x = "Soil fertility (EIV-N)") +
  theme_classic() +
  theme(legend.title = element_blank())

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
  labs(y= "Mass loss (%)", x = "Soil fertility (EIV-N)") +
  theme_classic() +
  theme(legend.title = element_blank()) 

reg_E_fert_l <- scat10 + scale_colour_brewer(palette = "Paired")


png("3_linear_models/graphs/scatter_E_fert_l.png", res = 150, width = 1500, height = 880)
reg_E_fert_l
dev.off()



all_litt <- ggarrange(reg_E_moist_l, reg_E_fert_l, labels = c("a.", "b."), ncol = 1, nrow = 2)
png("1_mass_loss/graphs/ellenberg_litter.png", res = 400, width = 3000, height = 3000)
all_litt
dev.off()

all_litt2 <- ggarrange(reg_E_moist_l, reg_E_fert_l, ncol = 2, nrow = 1)
png("1_mass_loss/graphs/ellenberg_litter2.png", res = 300, width = 3000, height = 1000)
all_litt2
dev.off()


all_tbi <- ggarrange(reg_soil_temp, reg_soil_temp_k, reg_E_fert_S, reg_E_fert_k, reg_E_moist,
                     labels = c("a.", "b.", "c.", "d.", "e."), ncol = 2, nrow = 3)
png("2_tea_indexes/graphs/ellenberg_tbi.png", res = 400, width = 4000, height = 3000)
all_tbi
dev.off()

all_tbi3 <- ggarrange(reg_E_fert_S, reg_E_fert_k, ncol = 2, nrow = 1)
png("2_tea_indexes/graphs/tbi_nitro.png", res = 300, width = 3000, height = 1000)
all_tbi3
dev.off()

all_tbi4 <- ggarrange(reg_E_fert_S, reg_E_fert_k, ncol = 2, nrow = 1)
png("2_tea_indexes/graphs/tbi_nitro.png", res = 300, width = 3000, height = 1000)
all_tbi4
dev.off()

all_tbi5 <- ggarrange(reg_E_moist, ncol = 1, nrow = 1)
png("2_tea_indexes/graphs/tbi_moist.png", res = 300, width = 2000, height = 1000)
all_tbi5
dev.off()










## NITROGEN ## Many outliers manually removed

# S
# Correlation test
cor.test(mod$N, mod$S, method = "pearson")
# p-value = 0.2311 (they are NOT correlated)


# Final scatter plot
ggplot(mod, aes(x = N, y = S, colour=Treatment, group = 1)) +
  geom_point(size = 2, aes(colour = Treatment)) +
  geom_smooth(method = "lm", se = FALSE, color = "black", fullrange=TRUE, linewidth = 0.5) +
  stat_poly_eq(use_label(c("p", "R2"))) +
  labs(y= "Stabilization factor (S)", x = "Available nitrogen") +
  theme_classic() +
  theme(legend.title = element_blank())



# k
# Correlation test
cor.test(mod$N, mod$k, method = "pearson")
# p-value = 0.3378 (they are NOT correlated)


# Final scatter plot
ggplot(mod, aes(x = N, y = k, colour=Treatment, group = 1)) +
  geom_point(size = 2, aes(colour = Treatment)) +
  geom_smooth(method = "lm", se = FALSE, color = "black", fullrange=TRUE, linewidth = 0.5) +
  stat_poly_eq(use_label(c("p", "R2"))) +
  labs(y= "Decomposition rate (k)", x = "Available nitrogen") +
  theme_classic() +
  theme(legend.title = element_blank())



# LITTER
# Correlation test
cor.test(mod$N, mod$litter_massloss, method = "pearson")
# p-value = 0.1486 (they are NOT correlated)


# Final scatter plot
ggplot(mod, aes(x = N, y = litter_massloss, colour=Treatment, group = 1)) +
  geom_point(size = 2, aes(colour = Treatment)) +
  geom_smooth(method = "lm", se = FALSE, color = "black", fullrange=TRUE, linewidth = 0.5) +
  stat_poly_eq(use_label(c("p", "R2"))) +
  labs(y= "Mass loss (%)", x = "Nitrogen availability") +
  theme_classic() +
  theme(legend.title = element_blank())





## PHOSPHORUS ## Outliers manually removed

# S
# Correlation test
cor.test(mod$P, mod$S, method = "pearson")
# p-value = 0.02221 (they are correlated)
# correlation coefficient = 0.2855021  (positive correlation)


# Final scatter plot
ggplot(mod, aes(x = P, y = S, colour=Treatment, group = 1)) +
  geom_point(size = 2, aes(colour = Treatment)) +
  geom_smooth(method = "lm", se = FALSE, color = "black", fullrange=TRUE, linewidth = 0.5) +
  stat_poly_eq(use_label(c("p", "R2"))) +
  labs(y= "Stabilization factor (S)", x = "Phosphorus") +
  theme_classic() +
  theme(legend.title = element_blank())



# k
# Correlation test
cor.test(mod$P, mod$k, method = "pearson")
# p-value = 0.00731 (they are correlated)
# correlation coefficient = 0.3374714  (positive correlation)


# Final scatter plot
ggplot(mod, aes(x = P, y = k, colour=Treatment, group = 1)) +
  geom_point(size = 2, aes(colour = Treatment)) +
  geom_smooth(method = "lm", se = FALSE, color = "black", fullrange=TRUE, linewidth = 0.5) +
  stat_poly_eq(use_label(c("p", "R2"))) +
  labs(y= "Decomposition rate (k)", x = "Phosphorus") +
  theme_classic() +
  theme(legend.title = element_blank())



# LITTER
# Correlation test
cor.test(mod$P, mod$litter_massloss, method = "pearson")
# p-value = 0.1492 (they are NOT correlated)


# Final scatter plot
ggplot(mod, aes(x = P, y = litter_massloss, colour=Treatment, group = 1)) +
  geom_point(size = 2, aes(colour = Treatment)) +
  geom_smooth(method = "lm", se = FALSE, color = "black", fullrange=TRUE, linewidth = 0.5) +
  stat_poly_eq(use_label(c("p", "R2"))) +
  labs(y= "Mass loss (%)", x = "Phosphorus") +
  theme_classic() +
  theme(legend.title = element_blank())




# Some boxplots to see the parameters in each treatment

eivf <- ggplot(mod, aes(Treatment, E_moisture, fill = Treatment)) + geom_boxplot(show.legend = FALSE) + 
  labs(x="Treatment", y="EIV-F") + 
  theme(axis.title.x = element_blank()) +
  scale_fill_brewer(palette="Paired")

eivn <- ggplot(mod, aes(Treatment, E_soil_fertility, fill = Treatment)) + geom_boxplot(show.legend = FALSE) + 
  labs(x="Treatment", y="EIV-N") + 
  theme(axis.title.x = element_blank()) +
  scale_fill_brewer(palette="Paired")

phosph <- ggplot(mod, aes(Treatment, P, fill = Treatment)) + geom_boxplot(show.legend = FALSE) + 
  labs(x="Treatment", y="P") + 
  theme(axis.title.x = element_blank()) +
  scale_fill_brewer(palette="Paired")

nitro <- ggplot(mod, aes(Treatment, N, fill = Treatment)) + geom_boxplot(show.legend = FALSE) + 
  labs(x="Treatment", y="N") + 
  theme(axis.title.x = element_blank()) +
  scale_fill_brewer(palette="Paired")


all2 <- ggarrange(eivf, eivn, phosph, nitro, labels = c("a.", "b.", "c.", "d."),
                 ncol = 2, nrow = 2)


png("1_mass_loss/graphs/environm.png", res = 400, width = 3400, height = 2900)
all2
dev.off()


################################################################################
# Two way anova for environmental variables

aov2_temp <- aov(soil_temp ~ restoration * management, data = mod)
aov2_moist <- aov(soil_moisture ~ restoration * management, data = mod)
aov2_N <- aov(E_moisture ~ restoration * management, data = mod)
aov2_F <- aov(E_soil_fertility ~ restoration * management, data = mod)


shapiro.test(aov2_temp$residuals) #no
shapiro.test(aov2_templog$residuals) #no
shapiro.test(aov2_moist$residuals) #no
shapiro.test(aov2_N$residuals) #ok
shapiro.test(aov2_F$residuals) #ok

leveneTest(aov2_temp) #ok
leveneTest(aov2_moist) #no
leveneTest(aov2_N) #ok
leveneTest(aov2_F) #ok

mod <- mutate(mod, logsoil_temp = log10(soil_temp))
aov2_templog <- aov(logsoil_temp ~ restoration * management, data = mod)

summary(aov2_templog)
summary(aov2_N)
summary(aov2_F)

TukeyHSD(aov2_N)
TukeyHSD(aov2_F)


################################################################################

