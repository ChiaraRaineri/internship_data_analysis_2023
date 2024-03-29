# First I'll perform a one way anova using treatment as an independent variable
# Then, I'll perform a two way anova using management and restoration as independent variables
# The data are mass loss of litter from the sites, litter from the common garden, green tea and red tea
# Outliers from litter common garden data and red tea data were removed manually



library(ggplot2)
library(ggpubr)
library(tidyverse)
library(broom)
library(dplyr)
library(car)
library(report)


data <- read.csv("1_mass_loss/mass_loss.csv", header = TRUE, colClasses = c("factor", "factor", "factor", "factor", "numeric", "numeric","numeric", "numeric"))



########################### ONE WAY ANOVA ###########################



#### Assumptions validation ####

# ANOVA model
aov1_litter <- aov(litter_massloss ~ treatment, data = data)
aov1_cg <- aov(litter_cg_massloss ~ treatment, data = data)
aov1_green <- aov(mean_green ~ treatment, data = data)
aov1_red <- aov(mean_red ~ treatment, data = data)


## Checking normality ##

# Visual method
par(mfrow = c(1, 2))

# Litter
hist(aov1_litter$residuals)  # histogram
qqPlot(aov1_litter$residuals, id = FALSE)  # QQ-plot

# Litter common garden
hist(aov1_cg$residuals)  # histogram
qqPlot(aov1_cg$residuals, id = FALSE)  # QQ-plot

# Green tea
hist(aov1_green$residuals)  # histogram
qqPlot(aov1_green$residuals, id = FALSE)  # QQ-plot

# Red tea
hist(aov1_red$residuals)  # histogram
qqPlot(aov1_red$residuals, id = FALSE)  # QQ-plot


# Shapiro-Wilk test
shapiro.test(aov1_litter$residuals)  # p-value = 0.8736
shapiro.test(aov1_cg$residuals)  # p-value = 0.007613 (!)
shapiro.test(aov1_green$residuals)  # p-value = 0.3145
shapiro.test(aov1_red$residuals)  # p-value = 0.4663


## Checking equality of variances (homogeneity) ##

# The Levene test is less sensitive than the Bartlett test to departures from normality. 
# If you have strong evidence that your data do in fact come from a normal, or nearly 
# normal, distribution, then Bartlett's test has better performance. 

# Bartlett test (most useful for litter, green tea and red tea)
bartlett.test(litter_massloss ~ treatment, data = data)  # p-value = 0.1722 (variances are equal)
bartlett.test(litter_cg_massloss ~ treatment, data = data)  # p-value = 0.004339 (variances are not equal)
bartlett.test(mean_green ~ treatment, data = data)  # p-value = 0.09873 (variances are equal)
bartlett.test(mean_red ~ treatment, data = data)  # p-value = 0.9708 (variances are equal)

# Levene's test (most useful for litter common garden)
leveneTest(litter_massloss ~ treatment, data = data)  # p-value = 0.08503 (variances are equal)
leveneTest(litter_cg_massloss ~ treatment, data = data)  # p-value = 0.06824 (variances are equal)
leveneTest(mean_green ~ treatment, data = data)  # p-value = 0.04074 (variances are not equal)
leveneTest(mean_red ~ treatment, data = data)  # p-value = 0.9564 (variances are equal)


# Visual method
par(mfrow = c(1, 2))

# Litter
plot(aov1_litter, which = 3)  # Homogeneity of variances
plot(aov1_litter, which = 2)  # Normality

# Litter common garden
plot(aov1_cg, which = 3)  # Homogeneity of variances
plot(aov1_cg, which = 2)  # Normality

# Green tea
plot(aov1_green, which = 3)  # Homogeneity of variances
plot(aov1_green, which = 2)  # Normality

# Red tea
plot(aov1_red, which = 3)  # Homogeneity of variances
plot(aov1_red, which = 2)  # Normality



#### Preliminary analyses ####

# Litter
aggregate(litter_massloss ~ treatment,
          data = data,
          function(x) round(c(mean = mean(x), sd = sd(x)), 4))
# Litter common garden
aggregate(litter_cg_massloss ~ treatment,
          data = data,
          function(x) round(c(mean = mean(x), sd = sd(x)), 4))
# Green tea
aggregate(mean_green ~ treatment,
          data = data,
          function(x) round(c(mean = mean(x), sd = sd(x)), 4))
# Red tea
aggregate(mean_red ~ treatment,
          data = data,
          function(x) round(c(mean = mean(x), sd = sd(x)), 4))



#### ANOVA ####

summary(aov1_litter)  # p-value = 0.0291
summary(aov1_cg)  # p-value = 1.63e-06 
summary(aov1_green)  # p-value = 0.00616
summary(aov1_red)  # p-value = 0.162


# log transformation of litter common garden
data <- mutate(data, loglitter_cg_massloss = log10(litter_cg_massloss))
aov1_cglog <- aov(loglitter_cg_massloss ~ treatment, data = data)
summary(aov1_cglog)  # p-value = 5.12e-06
shapiro.test(aov1_cglog$residuals)  # p-value = 0.4263


# Report results
report(aov1_litter)  # The effect of treatment is statistically significant and medium
report(aov1_cglog)  # The effect of treatment is statistically significant and large
report(aov1_green)  # The effect of treatment is statistically significant and large
report(aov1_red)  # The effect of treatment is statistically not significant and medium



#### Tukey HSD test ####

TukeyHSD(aov1_litter)  # Only RM-NM are significantly different (p adj = 0.0227204)
TukeyHSD(aov1_cglog)  # RM-NM (p adj = 0.0000506), RM-NU (p adj = 0.0000118) and RU-RM (p adj = 0.0032223) are significantly different 
TukeyHSD(aov1_green)  # RM-NM (p adj = 0.0068606) and RM-NU (p adj = 0.0281832) are significantly different
TukeyHSD(aov1_red)  # No combination is significant

# Plot
plot(TukeyHSD(aov1_litter))
plot(TukeyHSD(aov1_cglog))
plot(TukeyHSD(aov1_green))
plot(TukeyHSD(aov1_red))





########################### TWO WAY ANOVA ########################### 



#### Assumptions validation ####

# ANOVA model
aov2_litter <- aov(litter_massloss ~ restoration * management, data = data)
aov2_cg <- aov(litter_cg_massloss ~ restoration * management, data = data)
aov2_green <- aov(mean_green ~ restoration * management, data = data)
aov2_red <- aov(mean_red ~ restoration * management, data = data)


## Checking normality ##

# Visual method
par(mfrow = c(1, 2))

# Litter
hist(aov2_litter$residuals)  # histogram
qqPlot(aov2_litter$residuals, id = FALSE)  # QQ-plot

# Litter common garden
hist(aov2_cg$residuals)  # histogram
qqPlot(aov2_cg$residuals, id = FALSE)  # QQ-plot

# Green tea
hist(aov2_green$residuals)  # histogram
qqPlot(aov2_green$residuals, id = FALSE)  # QQ-plot

# Red tea
hist(aov2_red$residuals)  # histogram
qqPlot(aov2_red$residuals, id = FALSE)  # QQ-plot


# Shapiro-Wilk test
shapiro.test(aov2_litter$residuals)  # p-value = 0.8736
shapiro.test(aov2_cg$residuals)  # p-value = 0.007613 (!)
shapiro.test(aov2_green$residuals)  # p-value = 0.3145
shapiro.test(aov2_red$residuals)  # p-value = 0.4663


## Checking equality of variances (homogeneity) ##

# Levene's test
leveneTest(aov2_litter)  # p-value = 0.08503 (variances are equal)
leveneTest(aov2_cg)  # p-value = 0.06824 (variances are equal)
leveneTest(aov2_green)  # p-value = 0.04074 (variances are not equal)
leveneTest(aov2_red)  # p-value = 0.9564 (variances are equal)


# Outliers

# Litter
ggplot(data) + aes(x = restoration, y = litter_massloss) + geom_boxplot()
ggplot(data) + aes(x = management, y = litter_massloss) + geom_boxplot()

# Litter common garden
ggplot(data) + aes(x = restoration, y = litter_cg_massloss) + geom_boxplot()
ggplot(data) + aes(x = management, y = litter_cg_massloss) + geom_boxplot()

# Green tea
ggplot(data) + aes(x = restoration, y = mean_green) + geom_boxplot()
ggplot(data) + aes(x = management, y = mean_green) + geom_boxplot()

# Red tea
ggplot(data) + aes(x = restoration, y = mean_red) + geom_boxplot()
ggplot(data) + aes(x = management, y = mean_red) + geom_boxplot()



#### Preliminary analyses ####


# Litter
table_litter <- group_by(data, restoration, management) %>%
  summarise(
    mean = round(mean(litter_massloss, na.rm = TRUE), 4),
    sd = round(sd(litter_massloss, na.rm = TRUE), 4))
# Litter common garden
table_cg <- group_by(data, restoration, management) %>%
  summarise(
    mean = round(mean(litter_cg_massloss, na.rm = TRUE), 4),
    sd = round(sd(litter_cg_massloss, na.rm = TRUE), 4))
# Green tea
table_green <- group_by(data, restoration, management) %>%
  summarise(
    mean = round(mean(mean_green, na.rm = TRUE), 4),
    sd = round(sd(mean_green, na.rm = TRUE), 4))
# Red tea
table_red <- group_by(data, restoration, management) %>%
  summarise(
    mean = round(mean(mean_red, na.rm = TRUE), 4),
    sd = round(sd(mean_red, na.rm = TRUE), 4))


# Plots

# Litter
data %>%
  filter(!is.na(restoration)) %>%
  ggplot() +
  aes(x = management, y = litter_massloss, fill = restoration) +
  geom_boxplot()

data %>%
  filter(!is.na(management)) %>%
  ggplot() +
  aes(x = restoration, y = litter_massloss, fill = management) +
  geom_boxplot()

# Litter common garden
data %>%
  filter(!is.na(restoration)) %>%
  ggplot() +
  aes(x = management, y = litter_cg_massloss, fill = restoration) +
  geom_boxplot()

data %>%
  filter(!is.na(management)) %>%
  ggplot() +
  aes(x = restoration, y = litter_cg_massloss, fill = management) +
  geom_boxplot()

# Green tea
data %>%
  filter(!is.na(restoration)) %>%
  ggplot() +
  aes(x = management, y = mean_green, fill = restoration) +
  geom_boxplot()

data %>%
  filter(!is.na(management)) %>%
  ggplot() +
  aes(x = restoration, y = mean_green, fill = management) +
  geom_boxplot()

# Red tea
data %>%
  filter(!is.na(restoration)) %>%
  ggplot() +
  aes(x = management, y = mean_red, fill = restoration) +
  geom_boxplot()

data %>%
  filter(!is.na(management)) %>%
  ggplot() +
  aes(x = restoration, y = mean_red, fill = management) +
  geom_boxplot()



#### ANOVA ####


## Two-way ANOVA with interaction ##

summary(aov2_litter)  # p-value restoration:management = 0.013  (!)
summary(aov2_cg)  # p-value restoration:management = 0.02689  (!) 
summary(aov2_green)  # p-value restoration:management = 0.15350
summary(aov2_red)  # p-value restoration:management = 0.6698  

# log transformation of litter from common garden
aov2_cglog <- aov(loglitter_cg_massloss ~ restoration * management, data = data)
summary(aov2_cglog)  # p-value = 0.02495



## Two-way ANOVA without interaction ##

aov2_litter_noint <- aov(litter_massloss ~ restoration + management, data = data)
aov2_cg_noint <- aov(litter_cg_massloss ~ restoration + management, data = data)
aov2_green_noint <- aov(mean_green ~ restoration + management, data = data)
aov2_red_noint <- aov(mean_red ~ restoration + management, data = data)

summary(aov2_litter_noint)  # p-value restoration = 0.119  # p-value management = 0.567
summary(aov2_cg_noint)  # p-value restoration = 2.28e-05  # p-value management = 0.0032 
summary(aov2_green_noint)  # p-value restoration = 0.00189  # p-value management = 0.37930
summary(aov2_red_noint)  # p-value restoration = 0.0578  # p-value management = 0.2314

# log transformation of litter from common garden
aov2_cglog_noint <- aov(loglitter_cg_massloss ~ restoration + management, data = data)
summary(aov2_cglog_noint)  # p-value restoration = 4.49e-05   # p-value management = 0.048263



#### Tukey HSD test ####

TukeyHSD(aov2_litter)  # Only restored:managed-near_natural:managed are significantly different (p adj = 0.0227204)
TukeyHSD(aov2_cglog)  # restored:managed-near_natural:managed (p adj = 0.0000506), near_natural:unmanaged-restored:managed (p adj = 0.0000118) and restored:unmanaged-restored:managed (p adj = 0.0032223) are significantly different
TukeyHSD(aov2_green)  # restored:managed-near_natural:managed (p adj = 0.0068606) and near_natural:unmanaged-restored:managed (p adj = 0.0281832) are significantly different
TukeyHSD(aov2_red)  # No significant differences



### Visualization ### 1

plitter <- ggplot(table_litter, aes(x = factor(restoration), y = mean, fill = management, colour = management)) + 
  geom_bar(stat = "identity", position = "dodge") + theme(axis.title.x = element_blank()) + ggtitle("Litter from the sites")

pcg <- ggplot(table_cg, aes(x = factor(restoration), y = mean, fill = management, colour = management)) + 
  geom_bar(stat = "identity", position = "dodge") + theme(axis.title.x = element_blank()) + ggtitle("Litter from common garden")

pgreen <- ggplot(table_green, aes(x = factor(restoration), y = mean, fill = management, colour = management)) + 
  geom_bar(stat = "identity", position = "dodge") + theme(axis.title.x = element_blank()) + ggtitle("Green tea")

pred <- ggplot(table_red, aes(x = factor(restoration), y = mean, fill = management, colour = management)) + 
  geom_bar(stat = "identity", position = "dodge") + theme(axis.title.x = element_blank()) + ggtitle("Red tea")


# Plotting all the barplots together
pdf("1_mass_loss/graphs/mean_differences_2.pdf", width = 12, height = 8)
plitter
pcg
pgreen
pred
dev.off()


### Visualization ### 2

plitter2 <- ggplot(table_litter, aes(x = factor(management), y = mean, fill = restoration, colour = restoration)) + 
  geom_bar(stat = "identity", position = "dodge") + theme(axis.title.x = element_blank()) + ggtitle("Litter from the sites")

pcg2 <- ggplot(table_cg, aes(x = factor(management), y = mean, fill = restoration, colour = restoration)) + 
  geom_bar(stat = "identity", position = "dodge") + theme(axis.title.x = element_blank()) + ggtitle("Litter from common garden")

pgreen2 <- ggplot(table_green, aes(x = factor(management), y = mean, fill = restoration, colour = restoration)) + 
  geom_bar(stat = "identity", position = "dodge") + theme(axis.title.x = element_blank()) + ggtitle("Green tea")

pred2 <- ggplot(table_red, aes(x = factor(management), y = mean, fill = restoration, colour = restoration)) + 
  geom_bar(stat = "identity", position = "dodge") + theme(axis.title.x = element_blank()) + ggtitle("Red tea")


# Plotting all the barplots together
pdf("1_mass_loss/graphs/mean_differences_3.pdf", width = 12, height = 8)
plitter2
pcg2
pgreen2
pred2
dev.off()


# PLOTTING BOXPLOTS


conf_l <- data %>%
  filter(!is.na(management)) %>%
  ggplot() +
  aes(x = restoration, y = litter_massloss, fill = management) +
  geom_boxplot() +
  theme_classic() +
  theme(legend.title = element_blank()) +
  labs(y= "Mass loss (%)", x = " ") +
  theme(axis.title.x = element_blank()) +
  ggtitle("Litter from the sites") + theme(plot.title = element_text(face = "bold")) +
  scale_fill_brewer(palette="Set2") +
  scale_x_discrete(labels=c("Near natural","Restored"))
  

conf_cg <- data %>%
  filter(!is.na(management)) %>%
  ggplot() +
  aes(x = restoration, y = litter_cg_massloss, fill = management) +
  geom_boxplot() + 
  theme_classic() +
  theme(legend.title = element_blank()) +
  labs(y= "Mass loss (%)", x = " ") +
  theme(axis.title.x = element_blank()) +
  ggtitle("Litter from common garden") + theme(plot.title = element_text(face = "bold")) +
  scale_fill_brewer(palette="Set2") +
  scale_x_discrete(labels=c("Near natural","Restored"))


#ALTERNATIVO CONTORNO COMPLETO E RIGHE
#data %>%
#  filter(!is.na(management)) %>%
#  ggplot() +
#  aes(x = restoration, y = litter_massloss, fill = management) +
#  geom_boxplot() + theme_bw() +
#  theme(legend.title = element_blank()) +
#  labs(y= "Mass loss (%)", x = " ") +
#  theme(axis.title.x = element_blank()) +
#  ggtitle("Litter from the sites") + theme(plot.title = element_text(face = "bold")) +
#  scale_fill_brewer(palette="Set2") +
#  scale_x_discrete(labels=c("Near natural","Restored"))



conf_litt <- ggarrange(conf_l, conf_cg, labels = c("a.", "b."),
                       ncol = 2, nrow = 1)


png("1_mass_loss/graphs/confronto_litter.png", res = 300, width = 3000, height = 1000)
conf_litt
dev.off()



