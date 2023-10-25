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

data_dist <- read.csv("1_mass_loss/mass_loss.csv", header = TRUE, colClasses = c("factor", "factor", "factor", "factor", "numeric", "numeric", "numeric", "numeric"))



## UNITED GRAPHS ##

litt <- ggplot(data_dist, aes(treatment, litter_massloss, fill = treatment)) + geom_boxplot(show.legend = FALSE) + 
  theme_classic() +
  labs(x="Treatment", y="Mass loss (%)") + 
  theme(axis.title.x = element_blank()) +
  ggtitle("Litter from the sites") + theme(plot.title = element_text(face = "bold")) + 
  scale_fill_brewer(palette="Paired")

cg <- ggplot(data_dist, aes(treatment, litter_cg_massloss, fill = treatment)) + geom_boxplot(show.legend = FALSE) + 
  theme_classic() +
  labs(x="Treatment", y="Mass loss (%)") + 
  theme(axis.title.x = element_blank()) +
  ggtitle("Litter from the common garden") + theme(plot.title = element_text(face = "bold")) + 
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




# boxplots for treatment

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







