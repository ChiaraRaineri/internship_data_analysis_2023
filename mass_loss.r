# The first step of my analysis is to calculate the percentage mass loss from the litter in the litterbags and the tea in the teabags (green and rooibos)
# This way I will be able to compare them

# No common garden, only data from the sites
# For the teas, initial and final weight without the bag are used



library(ggplot2)

setwd("C:/internship/")     # Same version is also on RStudio

########

# This analysis take into account the categories "initial weight" and "final weight", so it's just descriptive
raw_data <- read.csv("1_mass_loss/mass_loss_sites_no_bag.csv")     # This is a data frame
raw_data

str(raw_data)   

# Should I convert the character values into factors? Is it useful?  # They can be categorical (factors are not just atomic vectors, they are objects)
unique(raw_data$site)
raw_data$site <- factor(raw_data$site, levels = c("Ellebaekengen", "Stolpehuse", "Trolle", "Lille_Linde", "Skt_Klemens", "Klem_krat", "Skallebanke", "Brahetrolle", "Lyng", "Lyng_krat", "Ommevej", "Ommekrat"))
str(raw_data$site)
levels(raw_data$site)
unique(raw_data$code)
raw_data$code <- factor(raw_data$code, levels = c("TRNM", "TRNU", "TRRM", "TRRU", "ODNM", "ODNU", "ODRM", "ODRU", "OMNM", "OMNU", "OMRM", "OMRU"))
str(raw_data$code)
levels(raw_data$code)


# Summary statistics
summary(raw_data)


# What do I have to do with the NAs? Should I remove the rows that contain them?
# mass_loss <- na.omit(raw_data)


########

# This analysis use a file in which I already subtracted the final weight from the initial weight and did the percentage mass loss




















