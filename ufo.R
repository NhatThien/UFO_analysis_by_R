#==================
# LOAD PACKAGES
#==================
library(ggmap)
library(ggplot2)
library(dplyr)
library(forcats)
library(ggrepel)

# D. Kahle and H. Wickham.
# ggmap: Spatial Visualization with ggplot2.
# The R Journal, 5(1), 144-161.
#URL http://journal.r-project.org/archive/2013-1/kahle-wickham.pdf

#=========================================
# install ggmap from its github repository
#=========================================
if(!requireNamespace("devtools")) install.packages("devtools")
devtools::install_github("dkahle/ggmap", ref = "tidyup")

#================
# Google API key
#================
register_google('AIzaSyAXCTrJ4tFykm-vdfvURGzhKVRkZISGnrY')

# read dataset
ufo <- read.csv(file="/home/xin/Documents/R_repository/insa/projet/UFO_analysis_by_R/complete.csv", header=T, sep=",")

# Number of ufo sightings by country and plot it
country_freq <- as.data.frame(table(ufo$country), col.names = c("country", "feq"), fit.empty.names = TRUE)
ggplot(country_freq, aes(Var1, Freq))+geom_bar(stat="identity", fill='blue') + labs(x = "Country") + labs(y = "Number")

# Number of ufo sightings by american state and plot it
state_freq <- as.data.frame(table(ufo$state), col.names = c("country", "feq"), fit.empty.names = TRUE)
ggplot(state_freq, aes(Var1, Freq))+geom_bar(stat="identity", fill='blue') + labs(x = "Country") + labs(y = "Number") + theme(axis.text.x = element_text(angle = 90, hjust = 1))


