#==================
# LOAD PACKAGES
#==================
library(dplyr)
library(plyr)
library(tidyverse)
library(ggmap)
library(ggplot2)
library(forcats)
library(ggrepel)
library(xts)
library(lubridate)

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
movies <- read.csv(file="/home/xin/Documents/R_repository/insa/projet/UFO_analysis_by_R/tmdb_5000_movies.csv", header=T, sep=",")

#================================================
# Nombre de l'observation d"ufo pour chaque pays
#================================================
ufo1 <- ufo # Copie la base de donnée
country_freq <- as.data.frame(table(ufo1$country), stringsAsFactors = FALSE) # compte les éléments appartenu à chaque niveau de factor
names(country_freq) <- c("country", "freq") # renomme les colonnes de dataframe
country_freq[country_freq$country == "",1] = "others" # nomme les case d'anonyme par "others"
ggplot(country_freq, aes(country, freq, fill = country))+geom_bar(stat="identity") + labs(x = "Country") + labs(y = "Number of ufo sightings")

#==========================================================================
# Visualise la position des ufo en forme de "lights" par google map
#==========================================================================
ufo2 <- select(ufo, latitude, longitude, shape)
ufo2 <- ufo2[complete.cases(ufo2), ]
ufo2$latitude <- as.numeric(as.character(ufo2$latitude))
ufo2 <- ufo2[ufo2$shape == "light",]

ufo_map <- ggmap(get_googlemap(center = c(lon = 0, lat = 0), maptype ='terrain',zoom = 1, scale = 2, color = 'color'))
ufo_map + geom_point(aes(x = longitude, y = latitude, colour = shape, alpha = 0.05, shape = "."), data = ufo2)

#=================================================
# Nombre de l'observation d"ufo pour chaque année
#=================================================
ufo3 <- ufo
ufo3$year <- year(mdy_hm(ufo3$datetime)) # création d'une colonne sur l'années du sighting
ufoByYear<-count(ufo3, "year") # on compte le nombre de sighting par an
ufoByYear<- filter(ufoByYear, year >=1980) # on s'interesse à ceux dont la date est après 1980
ggplot(ufoByYear, aes(year, freq)) + geom_col(fill='pink') + labs(x = "Year") + labs(y = "Number of ufo sightings")

#==============================================================
# Nombre de sortie des films sur la thématique extraterrestres
#==============================================================
#on filtre les films qui ont un rapport avec les extraterrestres
alienMovies <- filter(movies, str_detect(title,'Aliens?|aliens?') | str_detect(keywords,'space|aliens?|Space|Aliens?') )
#on rajoute une colonne qui contient l'année de sortie du film
alienMovies$year <- year(ymd(alienMovies$release_date))
alienMovies <- select(alienMovies, title, year)
#on compte le nombre de films d'aliens par année
alienMoviesByYear <- count(alienMovies, "year")
#on s'interesse à ceux sortis après 1980
alienMoviesByYear <- filter(alienMoviesByYear, year >=1980)
ggplot(alienMoviesByYear, aes(year, freq)) + geom_col(fill='blue')

#==============================================================
# Les formes le plus observés et leurs évolutions par temps
#==============================================================
ufo4 <- ufo
count_shape <- as.data.frame(table(ufo4$shape))
colnames(count_shape) <- c("shape", "freq")
most_shape <- arrange(count_shape, -freq)[1:5,]
info_mshape <- ufo4[ufo4$shape%in%most_shape$shape,]
info_mshape$datetime <- as.character(info_mshape$datetime)
info_mshape$datetime <- as.POSIXct(info_mshape$datetime, format = "%m/%d/%Y %H:%M")
info_mshape$datetime <- format(info_mshape$datetime, format = "%Y")
result <- select(info_mshape, datetime, shape)
result$nombre <- rep(1,nrow(result))
for_plot <- ddply(result, c("datetime", "shape"), summarise, nb_by_year = sum(nombre))
for_plot$datetime <- factor(for_plot$datetime)
ggplot(for_plot, aes(datetime , nb_by_year, fill = shape)) + geom_bar(stat="identity") + facet_wrap("shape", scale = "free", ncol = 1)+ theme(text = element_text(size=8), axis.text.x = element_text(angle = 90, hjust = 2))

#==================================
# Number of ufo sightings by shape
#==================================
ufo5 <- ufo
shape_freq <- as.data.frame(table(ufo5$shape), stringsAsFactors = FALSE)
names(shape_freq) <- c("shape", "freq")
shape_freq[shape_freq$shape == "",1] = "undefined"
ggplot(shape_freq, aes(shape, freq, fill = shape)) + geom_bar(stat="identity") + labs(x = "Shape") + labs(y = "Number of ufo sightings") + theme(axis.text.x = element_text(angle = 90, hjust = 1))








