# The first step of my analysis is to calculate the percentage mass loss from the litter in the litterbags and the tea in the teabags (green and rooibos)
# This way I will be able to compare them

# For the teas, initial and final weight without the bag are used


# install.packages(c("ggplot2", "ggpubr", "tidyverse", "broom", "AICcmodavg"))
library(ggplot2)
library(ggpubr)
library(tidyverse)
library(broom)
library(AICcmodavg)
library(lmtest)
library(viridis)
library(RColorBrewer)

setwd("C:/internship/")     # Same version is also on RStudio

########

# This data frame contains raw data from the field and the calculation of percentage mass loss
# Mass loss is calculated as (initial_weight -  final_weight) / initial_weight

data_dist <- read.csv("1_mass_loss/mass_loss.csv", header = TRUE, colClasses = c("factor", "factor", "factor", "factor", "numeric", "numeric", "numeric", "numeric"))

str(data_dist)
summary(data_dist)


# Let's start with some boxplots with the treatments for every parameter

# LITTER
# plitt2 <- ggplot(data_dist, aes(treatment, litter_massloss, fill = treatment)) + geom_boxplot(notch = TRUE, show.legend = FALSE) + labs(x="Treatment", y="Mass loss") + theme_bw() +
#   ggtitle("Litter from the sites") + theme(plot.title = element_text(hjust = 0.5)) + 
#   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
#   scale_fill_brewer(palette="Paired") + ylim(0,1)

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

png("1_mass_loss/graphs/treatment_litter.png", res = 300, width = 3400, height = 1900)
plitt2
dev.off()
png("1_mass_loss/graphs/treatment_cg.png", res = 300, width = 3400, height = 1900)
plittcg2
dev.off()
png("1_mass_loss/graphs/treatment_green.png", res = 300, width = 3400, height = 1900)
pmg2
dev.off()
png("1_mass_loss/graphs/treatment_red.png", res = 300, width = 3400, height = 1900)
pmr2
dev.off()



# Histograms # Frequency of mass loss intervals
par(mfrow = c(2,2))
hist(data_dist$litter_massloss, xlab = "Percentage of mass loss", main = "Litter from the sites")
hist(data_dist$litter_cg_massloss, xlab = "Percentage of mass loss", main = "Litter from common garden")
hist(data_dist$mean_green, xlab = "Percentage of mass loss", main = "Green tea")
hist(data_dist$mean_red, xlab = "Percentage of mass loss", main = "Red tea")




## UNITED GRAPHS ##

litt <- ggplot(data_dist, aes(treatment, litter_massloss, fill = treatment)) + geom_boxplot(show.legend = FALSE) + 
  theme_classic() +
  labs(x="Treatment", y="Mass loss (%)") + 
  theme(axis.title.x = element_blank()) +
  ggtitle("Litter at home site") + theme(plot.title = element_text(face = "bold")) + 
  scale_fill_brewer(palette="Paired")

cg <- ggplot(data_dist, aes(treatment, litter_cg_massloss, fill = treatment)) + geom_boxplot(show.legend = FALSE) + 
  theme_classic() +
  labs(x="Treatment", y="Mass loss (%)") + 
  theme(axis.title.x = element_blank()) +
  ggtitle("Litter in common garden") + theme(plot.title = element_text(face = "bold")) + 
  scale_fill_brewer(palette="Paired")

pg <- ggplot(data_dist, aes(treatment, mean_green, fill = treatment)) + geom_boxplot(show.legend = FALSE) + 
  theme_classic() +
  labs(x="Treatment", y="Mass loss (%)") + 
  theme(axis.title.x = element_blank()) +
  ggtitle("Green tea") + theme(plot.title = element_text(face = "bold")) + 
  scale_fill_brewer(palette="Paired")

pr <- ggplot(data_dist, aes(treatment, mean_red, fill = treatment)) + geom_boxplot(show.legend = FALSE) + 
  theme_classic() +
  labs(x="Treatment", y="Mass loss (%)") + 
  theme(axis.title.x = element_blank()) + 
  ggtitle("Rooibos tea") + theme(plot.title = element_text(face = "bold")) + 
  scale_fill_brewer(palette="Paired") 


all <- ggarrange(litt, cg, pg, pr, labels = c("a.", "b.", "c.", "d."),
          ncol = 2, nrow = 2)


png("1_mass_loss/graphs/mass_loss_boxplots.png", res = 400, width = 3400, height = 2900)
all
dev.off()


# png("1_mass_loss/graphs/mass_loss_boxplots.png", res = 300, width = 3400, height = 1900)



all2 <- ggarrange(litt, cg, ncol = 2, nrow = 1)
png("1_mass_loss/graphs/only_litter.png", res = 300, width = 3000, height = 1000)
all2
dev.off()

all3 <- ggarrange(pg, pr, ncol = 2, nrow = 1)
png("1_mass_loss/graphs/only_tea.png", res = 300, width = 3000, height = 1000)
all3
dev.off()






