library(ggplot2)
library(ggpubr)
library(tidyverse)
library(broom)
library(dplyr)
library(car)
library(report)
library(viridis)
library(ggpmisc)

setwd("C:/internship/")

data <- read.csv("1_mass_loss/mass_loss_pearson.csv", header = TRUE, colClasses = c("factor", "factor", "factor", "factor", "numeric", "numeric"))

cor.test(data$litter_massloss, data$litter_cg_massloss, method = "pearson")
# p-value = 8.46e-06 (they are correlated)
# correlation coefficient = 0.5286027 (positive correlation)

ggscatter(data, x = "litter_massloss", y = "litter_cg_massloss", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Litter sites", ylab = "Litter common garden")


cor.test(data$litter_massloss, data$litter_cg_massloss, method = "kendall")
cor.test(data$litter_massloss, data$litter_cg_massloss, method = "spearman")


# https://www.datanovia.com/en/blog/pch-in-r-best-tips/
shapes = c(15, 16, 17, 18) 
shapes <- shapes[as.numeric(data$Treatment)]
plot(data$litter_cg_massloss ~ data$litter_massloss, pch= shapes, col=data$Treatment)


ggplot(data, aes(x = litter_massloss, y = litter_cg_massloss, shape = Treatment, colour=Treatment,group = treatment)) +
  geom_point(size = 3, aes(colour = factor(Treatment))) +
  scale_shape_manual(values = c(15, 16, 17, 18)) + 
  scale_color_viridis(discrete=TRUE) +
  theme_classic()


# DEFINITIVO
ggplot(data, aes(x = litter_massloss, y = litter_cg_massloss, colour=Treatment, group = 1)) +
  geom_point(size = 2, aes(colour = Treatment)) +
  scale_color_viridis(discrete=TRUE) +
  geom_smooth(method = "lm", se = FALSE, color = "black", fullrange=TRUE, linewidth = 0.5) +
  stat_poly_eq(use_label(c("p", "R2"))) +
  labs(y= "Common garden", x = "Sites") +
  theme_classic()
