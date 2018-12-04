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
country_freq <- as.data.frame(table(ufo$country), stringsAsFactors = FALSE)
names(country_freq) <- c("country", "freq")
country_freq[country_freq$country == "",1] = "others"
ggplot(country_freq, aes(country, freq))+geom_bar(stat="identity", fill='pink') + labs(x = "Country") + labs(y = "Number")

# number of shape
shape_freq <- as.data.frame(table(ufo$shape), stringsAsFactors = FALSE)
names(shape_freq) <- c("shape", "freq")
shape_freq[shape_freq$shape == "",1] = "undefined"
ggplot(shape_freq, aes(shape, freq)) + geom_bar(stat="identity", fill='pink') + labs(x = "Shape") + labs(y = "Number") + theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Number of ufo sightings by american state and plot it
state_freq <- count(ufo1, c("state", "country"))
state_freq$state <- as.character(state_freq$state)
state_freq$country <- as.character(state_freq$country)
state_freq[state_freq$state == "",]$state = "unknownState" 
state_freq[state_freq$country == "",]$country = "unknownCountry"
p <- subset(ddply(state_freq, c("country"), transform, place = rank(-freq)), place < 10)
ggplot(p, aes(state, freq)) + geom_bar(stat="identity", , fill = 'pink') + facet_wrap("country", scales = "free") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

# Plot ufo sightings by its coordinates and its shapes
ufo1 <- select(ufo, latitude, longitude, shape)
ufo1 <- ufo1[complete.cases(ufo1), ]
ufo1$latitude <- as.numeric(as.character(ufo1$latitude))
ufo1$shape <- as.character(ufo1$shape)
ufo1[ufo1$shape == "",3] = "undefined"

ufo_map <- ggmap(get_googlemap(center = "us", maptype ='terrain',zoom = 1, scale = 2, color = 'color'))
ufo_map + geom_point(aes(x = longitude, y = latitude, colour = shape), data = ufo1)

# Plot by shape
ufo <- ufo[ufo$shape == "cone",]
ufo1 <- select(ufo, latitude, longitude, shape)
ufo1 <- ufo1[complete.cases(ufo1), ]
ufo1$latitude <- as.numeric(as.character(ufo1$latitude))

ufo_map <- ggmap(get_googlemap(center = "us", maptype ='terrain',zoom = 3, scale = 2, color = 'color'))
ufo_map + geom_point(aes(x = longitude, y = latitude, colour = shape), data = ufo1)

