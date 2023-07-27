# The first step of my analysis is to calculate the percentage mass loss from the litter in the litterbags and the tea in the teabags (green and rooibos)
# This way I will be able to compare them

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

mass_data <- read.csv("1_mass_loss/mass_loss_percentage.csv", header = TRUE, colClasses = c("factor", "factor", "factor", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric"))
mass_data

str(data)   

# Converting chr into factor using base R  # Not useful only practice
# mass_data$site <- factor(mass_data$site, levels = c("Ellebaekengen", "Stolpehuse", "Trolle", "Lille_Linde", "Skt_Klemens", "Klem_krat", "Skallebanke", "Brahetrolle", "Lyng", "Lyng_krat", "Ommevej", "Ommekrat"))
# mass_data$code <- factor(mass_data$code, levels = c("TRNM", "TRNU", "TRRM", "TRRU", "ODNM", "ODNU", "ODRM", "ODRU", "OMNM", "OMNU", "OMRM", "OMRU"))



# Summary statistics
summary(mass_data)


# What do I have to do with the NAs? Should I remove the rows that contain them?
# mass_data <- na.omit(mass_data)


# for the green and red tea "l" means that it's the tea buried in the North side (together with the litter), while "t" means it was buried in the South side
# cg means "common garden"



# boxplots 
# litter
plitt <- ggplot(mass_data, aes(code, litter_massloss, fill = code)) + geom_boxplot(show.legend = FALSE) + labs(x="Site", y="Mass loss") + theme_bw() +
ggtitle("Litter from the sites") + theme(plot.title = element_text(hjust = 0.5)) + 
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
scale_fill_brewer(palette="Paired") + ylim(0,1)
# litter common garden
plittcg <- ggplot(mass_data, aes(code, litter_cg_massloss, fill = code)) + geom_boxplot(show.legend = FALSE) + labs(x="Site", y="Mass loss") + theme_bw() +
ggtitle("Litter from the common garden") + theme(plot.title = element_text(hjust = 0.5)) + 
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
scale_fill_brewer(palette="Paired") + ylim(0,1)
# green tea (l)
pgl <- ggplot(mass_data, aes(code, green_l_massloss, fill = code)) + geom_boxplot(show.legend = FALSE) + labs(x="Site", y="Mass loss") + theme_bw() +
ggtitle("Green tea from North side") + theme(plot.title = element_text(hjust = 0.5)) + 
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
scale_fill_brewer(palette="Paired") + ylim(0,1)
# red tea (l)
prl <- ggplot(mass_data, aes(code, red_l_massloss, fill = code)) + geom_boxplot(show.legend = FALSE) + labs(x="Site", y="Mass loss") + theme_bw() +
ggtitle("Red tea from North side") + theme(plot.title = element_text(hjust = 0.5)) + 
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
scale_fill_brewer(palette="Paired") + ylim(0,1)
# green tea (t)
pgt <- ggplot(mass_data, aes(code, green_t_massloss, fill = code)) + geom_boxplot(show.legend = FALSE) + labs(x="Site", y="Mass loss") + theme_bw() + 
ggtitle("Green tea from South side") + theme(plot.title = element_text(hjust = 0.5)) + 
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
scale_fill_brewer(palette="Paired") + ylim(0,1)
# red tea (t)
prt <- ggplot(mass_data, aes(code, red_t_massloss, fill = code)) + geom_boxplot(show.legend = FALSE) + labs(x="Site", y="Mass loss") + theme_bw() + 
ggtitle("Red tea from South side") + theme(plot.title = element_text(hjust = 0.5)) + 
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
scale_fill_brewer(palette="Paired") + ylim(0,1)
# mean green teas from N and S
pmg <- ggplot(mass_data, aes(code, mean_green, fill = code)) + geom_boxplot(show.legend = FALSE) + labs(x="Site", y="Mass loss") + theme_bw() + 
  ggtitle("Mean values of green teas") + theme(plot.title = element_text(hjust = 0.5)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  scale_fill_brewer(palette="Paired") + ylim(0,1)
# mean red teas from N and S
pmr <- ggplot(mass_data, aes(code, mean_red, fill = code)) + geom_boxplot(show.legend = FALSE) + labs(x="Site", y="Mass loss") + theme_bw() + 
  ggtitle("Mean values of red teas") + theme(plot.title = element_text(hjust = 0.5)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  scale_fill_brewer(palette="Paired") + ylim(0,1)


pdf("plots_raw_data_same_scale.pdf", width = 12, height = 8)
plitt
plittcg
pgl
pgt
pmg
prl
prt
pmr
dev.off()








