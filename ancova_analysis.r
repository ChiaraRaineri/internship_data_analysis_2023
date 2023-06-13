library(ggplot2)
library(ggpubr)
library(tidyverse)
library(broom)
library(AICcmodavg)

# This spreadsheet contains the initial and final weight as well as the mass loss percentage
data_ancova <- read.csv("1_mass_loss/mass_loss_for_ancova.csv", header = TRUE, colClasses = c("factor", "factor", "factor", "factor", 
                                                                                            "numeric", "numeric", "numeric", "numeric", 
                                                                                            "numeric", "numeric", "numeric", "numeric", 
                                                                                            "numeric", "numeric", "numeric", "numeric", 
                                                                                            "numeric", "numeric", "numeric", "numeric", 
                                                                                            "numeric", "numeric", "numeric", "numeric"))

str(data_ancova)
summary(data_ancova)



# analysis of covariance (see p. 498), using final weight as the response variable and initial weight as a covariate, or
# by specifying the response variable as a relative growth rate, measured as log(final weight/initial weight),
# both of which can be analysed with normal errors without further transformation.

attach(data_ancova)   # So I can access variables of a DataFrame without invoking any function or method
names(data_ancova)                                                                                            

#LITTER (just as in the book)

# m stands for model
# The final weight is the response variable, restoration and management are two categorical explanatory variables and the initial weight is a continuous covariate
m1_litter <- lm(litter_final ~ restoration * litter_initial * management)   
# m1_litter is the maximal model (the most complicated model, then I will simplify it by removing 
# non-significant terms until I'm left with a minimal adequate model, in which all the parameters are significantly different from zero)
summary(m1_litter)
# No relationship is significant


# The next step is to delete the non-significant interaction terms from the model (I get the minimal adequate model)
m2_litter <- step(m1_litter)  # I don't understand this
summary(m2_litter)  # How do I interpret these?

anova(m2_litter)     # does this means that restoration and management alone do not have an effect on final weight, bu their combination does?


# After an analysis of covariance, it is useful to draw the fitted lines through a scatterplot, with each factor level represented by different plotting symbols and line types
plot(litter_initial, litter_final, col=as.numeric(management), pch=(15 + as.numeric(restoration)))
xv<-c(1, 5)
for (i in 1:2) {
  for (j in 1:4){
    a<-coef(m2_litter)[1] + (i>1) * coef(m2_litter)[2] + (j>1) * coef(m2_litter)[j+2]; b<-coef(m2_litter)[3]
    yv<-a + b * xv
    lines(xv, yv, lty = 2)
  } }
# I guess the black is "managed" and the red is "unmanaged"? (how do I know? levels(management))
# I guess the circle is "near_natural" and the triangle is "restored"?
# How do I interpret this graph?









# Another way from https://dzchilds.github.io/stats-for-bio/two-way-ancova-in-r.html 

plot(m1_litter, add.smooth = FALSE, which = 1)  # no evidence of a systematic trend here so the linearity assumption is fine
plot(m1_litter, which = 2)  # normal probability plot
plot(m1_litter, add.smooth = FALSE, which = 3)  # no systematic pattern in the size of the residuals
anova(m1_litter)













# GREEN TEA NORTH
m1_green_l <- lm(green_l_final ~ restoration * green_l_initial * management)
summary(m1_green_l)

m2_green_l <- step(m1_green_l)
summary(m2_green_l)

anova(m2_green_l)  

plot(green_l_initial, green_l_final, col=as.numeric(management), pch=(15 + as.numeric(restoration)))
xv<-c(1, 5)
for (i in 1:2) {
  for (j in 1:4){
    a<-coef(m2_green_l)[1] + (i>1) * coef(m2_green_l)[2] + (j>1) * coef(m2_green_l)[j+2]; b<-coef(m2_green_l)[3]
    yv<-a + b * xv
    lines(xv, yv, lty = 2)
  } }


# RED TEA NORTH
m1_red_l <- lm(red_l_final ~ restoration * red_l_initial * management)
summary(m1_red_l)

m2_red_l <- step(m1_red_l)
summary(m2_red_l)

anova(m2_red_l)    

plot(red_l_initial, red_l_final, col=as.numeric(management), pch=(15 + as.numeric(restoration)))
xv<-c(1, 5)
for (i in 1:2) {
  for (j in 1:4){
    a<-coef(m2_red_l)[1] + (i>1) * coef(m2_red_l)[2] + (j>1) * coef(m2_red_l)[j+2]; b<-coef(m2_red_l)[3]
    yv<-a + b * xv
    lines(xv, yv, lty = 2)
  } }







