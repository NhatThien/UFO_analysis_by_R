#==================
# LOAD PACKAGES
#==================
library(dplyr)
library(plyr)
library(ggmap)
library(ggplot2)
library(dplyr)
library(forcats)
library(ggrepel)
library(xts)

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

#================================================
# Number of ufo sightings by country and plot it
#================================================
ufo1 <- ufo # Copy databae
country_freq <- as.data.frame(table(ufo1$country), stringsAsFactors = FALSE) # counts at each combination of factor levels and convert table to dataframe
names(country_freq) <- c("country", "freq") # rename the column of the new dataframe
country_freq[country_freq$country == "",1] = "others" # rename blank space to others
ggplot(country_freq, aes(country, freq, fill = country))+geom_bar(stat="identity") + labs(x = "Country") + labs(y = "Number of ufo sightings")











