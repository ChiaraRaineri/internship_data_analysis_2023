# This is the analysis for the S and k indexes of TBI

library(ggplot2)
library(ggpubr)
library(tidyverse)
library(broom)
library(AICcmodavg)
library(lmtest)


setwd("C:/internship/") 


# This data frame contains the mean values of S and k from the sites (mean of north and south values)
# Indexes calculations and meaning are illustrated in the paper of Keuskamp et al.

tbi_data <- read.csv("2_tea_indexes/tea_indexes_sites.csv", header = TRUE, colClasses = c("factor", "factor", "factor", "factor", "factor", "numeric", "numeric"))
tbi_data

str(tbi_data)

# Summary statistics
summary(tbi_data)


# Shapiro-Wilk test
shapiro.test(tbi_data$S)  # p-value = 0.08583
shapiro.test(tbi_data$k)  # p-value = 2.261e-06  (!)



# BOXPLOTS


