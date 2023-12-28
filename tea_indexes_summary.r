# This is the analysis for the S and k indexes of TBI
# Some outliers were removed manually

library(ggplot2)
library(ggpubr)
library(tidyverse)
library(broom)
library(AICcmodavg)
library(lmtest)


setwd("C:/internship/") 


# This data frame contains the mean values of S and k from the sites (mean of north and south values)
# S is the stabilization factor indicating how much of the material was stabilized
# k is decomposition speed, higher values indicate faster decomposition
# Indexes calculations and meaning are illustrated in the paper of Keuskamp et al.

tbi_data <- read.csv("2_tea_indexes/tea_indexes_sites.csv", header = TRUE, colClasses = c("factor", "factor", "factor", "factor", "factor", "factor", "numeric", "numeric"))
tbi_data

str(tbi_data)

# Summary statistics
summary(tbi_data)


# Histograms (skewedness) #
pdf("2_tea_indexes/graphs/histograms.pdf")
par(mfrow = c(1,2))
hist(tbi_data$S, xlab = "S", main = "Stabilization factor (S)")
hist(tbi_data$k, xlab = "k", main = "Decomposition speed (k)")
dev.off()



# PEARSON CORRELATION TEST # (outliers of k were removed)
cor.test(tbi_data$S, tbi_data$k, method = "pearson")

ggscatter(tbi_data, x = "S", y = "k", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Stabilization factor (S)", ylab = "Decomposition speed (k)")
# p-value = 2.336e-10 (S and k are correlated)
# correlation coefficient = 0.6923817 (positive correlation)


# Final scatter plot
scat <- ggplot(tbi_data, aes(x = S, y = k, colour=treatment, group = 1)) +
  geom_point(size = 2, aes(colour = treatment)) +
  geom_smooth(method = "lm", se = FALSE, color = "black", fullrange=TRUE, linewidth = 0.5) +
  stat_poly_eq(use_label(c("p", "R2"))) +
  labs(y= "Decomposition rate (k)", x = "Stabilization factor (S)") +
  theme_classic()

reg_S_k <- scat + scale_colour_brewer(palette = "Paired")


png("2_tea_indexes/graphs/scatter_Sk_reg.png", res = 150, width = 1500, height = 880)
reg_S_k
dev.off()



## k is not normally distributed ## (before outliers removal)

# Kendall rank correlation test
kendall <- cor.test(tbi_data$S, tbi_data$k,  method="kendall")
kendall    # p-value = 1.81e-09    # correlation coefficient = 0.5151365 

# Spearman rank correlation coefficient
spearman <-cor.test(tbi_data$S, tbi_data$k,  method = "spearman")
spearman   # p-value = 1.188e-11    # correlation coefficient = 0.7255432 



# BOXPLOTS #


# UNITED GRAPHS
pS <- ggplot(tbi_data, aes(treatment, S, fill = treatment)) + geom_boxplot(show.legend = FALSE) + 
  theme_classic() +
  labs(x="Treatment", y="S") + 
  theme(axis.title.x = element_blank()) +
  ggtitle("Stabilization factor (S)") + theme(plot.title = element_text(face = "bold")) + 
  scale_fill_brewer(palette="Paired")

pk <- ggplot(tbi_data, aes(treatment, k, fill = treatment)) + geom_boxplot(show.legend = FALSE) + 
  theme_classic() +
  labs(x="Treatment", y="k") +
  theme(axis.title.x = element_blank()) +
  ggtitle("Decomposition rate (k)") + theme(plot.title = element_text(face = "bold")) + 
  scale_fill_brewer(palette="Paired")

all_index <- ggarrange(pS, pk, labels = c("a.", "b."),
                 ncol = 2, nrow = 1)


png("2_tea_indexes/graphs/indexes_boxplots.png", res = 300, width = 3000, height = 1000)
all_index
dev.off()



all_green <- ggarrange(pS, pg, ncol = 2, nrow = 1)
png("2_tea_indexes/graphs/onlygreen.png", res = 300, width = 3000, height = 1000)
all_green
dev.off()


#####

# By code
pS_code <- ggplot(tbi_data, aes(code, S, fill = code)) + geom_boxplot(show.legend = FALSE) + labs(x="Site", y="S") + theme_bw() +
  ggtitle("Stabilization factor (S)") + theme(plot.title = element_text(hjust = 0.5)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  scale_fill_brewer(palette="Paired") + ylim(0,0.5)

pk_code <- ggplot(tbi_data, aes(code, k, fill = code)) + geom_boxplot(show.legend = FALSE) + labs(x="Site", y="k") + theme_bw() +
  ggtitle("Decomposition speed (k)") + theme(plot.title = element_text(hjust = 0.5)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  scale_fill_brewer(palette="Paired") + ylim(0,0.03)

# Exporting
pdf("2_tea_indexes/graphs/plots_by_code.pdf", width = 12, height = 8)
pS_code
pk_code
dev.off()




# By treatment
pS_treat <- ggplot(tbi_data, aes(treatment, S, fill = treatment)) + geom_boxplot(show.legend = FALSE) + labs(x="Treatment", y="S") + theme_bw() +
  ggtitle("Stabilization factor (S)") + theme(plot.title = element_text(hjust = 0.5)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  scale_fill_brewer(palette="Paired") + ylim(0,0.5)

pk_treat <- ggplot(tbi_data, aes(treatment, k, fill = treatment)) + geom_boxplot(show.legend = FALSE) + labs(x="Treatment", y="k") + theme_bw() +
  ggtitle("Decomposition speed (k)") + theme(plot.title = element_text(hjust = 0.5)) + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + 
  scale_fill_brewer(palette="Paired") + ylim(0,0.03)

# Exporting
pdf("2_tea_indexes/graphs/plots_by_treatment.pdf", width = 12, height = 8)
pS_treat
pk_treat
dev.off()

png("2_tea_indexes/graphs/treatment_S.png", res = 300, width = 3400, height = 1900)
pS_treat
dev.off()
png("2_tea_indexes/graphs/treatment_k.png", res = 300, width = 3400, height = 1900)
pk_treat
dev.off()

png("2_tea_indexes/graphs/pearson.png", res = 300, width = 3400, height = 1900)
ggscatter(tbi_data, x = "S", y = "k", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Stabilization factor (S)", ylab = "Decomposition speed (k)")
dev.off()








