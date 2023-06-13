library(ggplot2)
library(ggpubr)
library(tidyverse)
library(broom)
library(AICcmodavg)

data_ancova <- read.csv("1_mass_loss/mass_loss_for_ancova.csv", header = TRUE, colClasses = c("factor", "factor", "factor", "factor", 
                                                                                            "numeric", "numeric", "numeric", "numeric", 
                                                                                            "numeric", "numeric", "numeric", "numeric", 
                                                                                            "numeric", "numeric", "numeric", "numeric", 
                                                                                            "numeric", "numeric", "numeric", "numeric", 
                                                                                            "numeric", "numeric", "numeric", "numeric"))

str(data_ancova)
summary(data_ancova)



# analysis of covariance (see p. 489), using final weight as the response variable and initial weight as a covariate, or
# by specifying the response variable as a relative growth rate, measured as log(final weight/initial weight),
# both of which can be analysed with normal errors without further transformation.

attach(data_ancova)
names(data_ancova)                                                                                            

m1 <- lm(litter_final ~ restoration * litter_initial * management)
summary(m1)

m2 <- step(m1)
summary(m2)

anova(m2)     # does this means that restoration and management alone do not have an effect on final weight, bu their combination does?

plot(litter_initial, litter_final, col=as.numeric(management), pch=(15 + as.numeric(restoration)))
xv<-c(1, 5)
for (i in 1:2) {
  for (j in 1:4){
    a<-coef(m2)[1] + (i>1) * coef(m2)[2] + (j>1) * coef(m2)[j+2]; b<-coef(m2)[3]
    yv<-a + b * xv
    lines(xv, yv, lty = 2)
  } }

### Order matters?

m11 <- lm(litter_final~management*litter_initial*restoration)
summary(m11)

m22 <- step(m11)
summary(m22)

anova(m22)     # does this means that restoration and management alone do not have an effect on final weight, bu their combination does?

plot(litter_initial, litter_final, col=as.numeric(restoration),pch=(15+as.numeric(management)))
xv<-c(1,5)
for (i in 1:2) {
  for (j in 1:4){
    a<-coef(m22)[1]+(i>1)* coef(m22)[2]+(j>1)*coef(m22)[j+2];b<-coef(m22)[3]
    yv<-a+b*xv
    lines(xv,yv,lty=2)
  } }


# Another way from https://dzchilds.github.io/stats-for-bio/two-way-ancova-in-r.html 

plot(m1, add.smooth = FALSE, which = 1)  # no evidence of a systematic trend here so the linearity assumption is fine
plot(m1, which = 2)  # normal probability plot
plot(m1, add.smooth = FALSE, which = 3)  # no systematic pattern in the size of the residuals
anova(m1)



