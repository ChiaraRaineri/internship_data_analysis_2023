# The first step of my analysis is to calculate the percentage mass loss from the litter in the litterbags and the tea in the teabags (green and rooibos)
# This way I will be able to compare them

# No common garden, only data from the sites
# For the teas, initial and final weight without the bag are used



library(ggplot2)

setwd("C:/internship/")     # Same version is also on RStudio

mass_loss <- read.csv("1_mass_loss/mass_loss_sites_no_bag.csv")     # This is a data frame
mass_loss

str(mass_loss)   

# Should I convert the character values into factors? Is it useful?  # They can be categorical (factors are not just atomic vectors, they are objects)
unique(mass_loss$site)
mass_loss$site <- factor(mass_loss$site, levels = c("Ellebaekengen", "Stolpehuse", "Trolle", "Lille_Linde", "Skt_Klemens", "Klem_krat", "Skallebanke", "Brahetrolle", "Lyng", "Lyng_krat", "Ommevej", "Ommekrat"))
str(mass_loss$site)
levels(mass_loss$site)
unique(mass_loss$code)
mass_loss$code <- factor(mass_loss$code, levels = c("TRNM", "TRNU", "TRRM", "TRRU", "ODNM", "ODNU", "ODRM", "ODRU", "OMNM", "OMNU", "OMRM", "OMRU"))
str(mass_loss$code)
levels(mass_loss$code)


# Summary statistics
summary(mass_loss)


# What do I have to do with the NAs? Should I remove the rows that contain them?
# mass_loss <- na.omit(mass_loss)









