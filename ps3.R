
library(streamR)
library(plyr)
library(sp)
library(RColorBrewer)
library(ggplot2)
library(grid)
library(maps)
library(maptools)
gpclibPermit()

#Parse the code, save it, filter it. All of this was done on a seperate computer in the library so I do not have a code for that
#LOAD THE RData Files (added to the materials that were turned in)


#filter Tweets for America
geo_tweetsUS <- filteredData[filteredData$country_code == "US",]

HC_US <- subset (geo_tweetsUS, grepl(pattern =  "Clinton | clinton | Hillary | hillary | iamwithher | Iamwithher", 
                                     geo_tweetsUS$text))
BS_US <- subset (geo_tweetsUS, grepl(pattern =  "Bernie | bernie | Sanders | sanders | feelthebern", 
                                     geo_tweetsUS$text))
DT_US <- subset (geo_tweetsUS, grepl(pattern =  "Donald | Trump | donald | trump", 
                                     geo_tweetsUS$text))
TC_US <- subset (geo_tweetsUS, grepl(pattern =  "Ted | Cruz | ted | cruz", 
                                     geo_tweetsUS$text))
MR_US <- subset (geo_tweetsUS, grepl(pattern =  "Marco | Rubio | marco | rubio", 
                                     geo_tweetsUS$text))

#HIllary Clinton Plot
map.data <- map_data("world")
points <- data.frame(x = as.numeric(HC$place_lon), y = as.numeric(HC$place_lat))
points <- points[points$y > 25, ]

ggplot(map.data) + geom_map(aes(map_id = region), map = map.data, fill = "white", 
                            color = "blue", size = 0.25) + expand_limits(x = map.data$long, y = map.data$lat) + 
  theme(axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), 
        axis.title = element_blank(), panel.background = element_blank(), panel.border = element_blank(), 
        panel.grid.major = element_blank(), plot.background = element_blank(), 
        plot.margin = unit(0 * c(-1.5, -1.5, -1.5, -1.5), "lines")) + geom_point(data = points, 
                                                                                 aes(x = x, y = y), size = 2, alpha = 1/4, color = "dark blue")

#HIllary Clinton American Plot
map.data <- map_data("state")
points <- data.frame(x = as.numeric(HC_US$place_lon), y = as.numeric(HC_US$place_lat))
  points <- points[points$y > 25, ]

ggplot(map.data) + geom_map(aes(map_id = region), map = map.data, fill = "white", 
                            color = "blue", size = 0.25) + expand_limits(x = map.data$long, y = map.data$lat) + 
  theme(axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), 
        axis.title = element_blank(), panel.background = element_blank(), panel.border = element_blank(), 
        panel.grid.major = element_blank(), plot.background = element_blank(), 
        plot.margin = unit(0 * c(-1.5, -1.5, -1.5, -1.5), "lines")) + geom_point(data = points, 
                                                                                 aes(x = x, y = y), size = 2, alpha = 1/4, color = "dark blue")


#Donald Trump Plot
map.data <- map_data("world")
points <- data.frame(x = as.numeric(DT$place_lon), y = as.numeric(DT$place_lat))
points <- points[points$y > 25, ]

ggplot(map.data) + geom_map(aes(map_id = region), map = map.data, fill = "white", 
                            color = "blue", size = 0.25) + expand_limits(x = map.data$long, y = map.data$lat) + 
  theme(axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), 
        axis.title = element_blank(), panel.background = element_blank(), panel.border = element_blank(), 
        panel.grid.major = element_blank(), plot.background = element_blank(), 
        plot.margin = unit(0 * c(-1.5, -1.5, -1.5, -1.5), "lines")) + geom_point(data = points, 

#Donald Trump American Plot
map.data <- map_data("state")
points <- data.frame(x = as.numeric(DT_US$place_lon), y = as.numeric(DT_US$place_lat))
points <- points[points$y > 25, ]
ggplot(map.data) + geom_map(aes(map_id = region), map = map.data, fill = "white", 
color = "blue", size = 0.25) + expand_limits(x = map.data$long, y = map.data$lat) + 
theme(axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), 
axis.title = element_blank(), panel.background = element_blank(), panel.border = element_blank(), 
panel.grid.major = element_blank(), plot.background = element_blank(), 
plot.margin = unit(0 * c(-1.5, -1.5, -1.5, -1.5), "lines")) + geom_point(data = points, 
aes(x = x, y = y), size = 2, alpha = 1/4, color = "dark blue")                                                                                 

#Marco Rubio Plot
map.data <- map_data("world")
points <- data.frame(x = as.numeric(MR$place_lon), y = as.numeric(MR$place_lat))
points <- points[points$y > 25, ]

ggplot(map.data) + geom_map(aes(map_id = region), map = map.data, fill = "white", 
                            color = "blue", size = 0.25) + expand_limits(x = map.data$long, y = map.data$lat) + 
  theme(axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), 
        axis.title = element_blank(), panel.background = element_blank(), panel.border = element_blank(), 
        panel.grid.major = element_blank(), plot.background = element_blank(), 
        plot.margin = unit(0 * c(-1.5, -1.5, -1.5, -1.5), "lines")) + geom_point(data = points, 
                                                                                 aes(x = x, y = y), size = 2, alpha = 1/4, color = "dark blue")

#Marco Rubio American Plot
map.data <- map_data("state")
points <- data.frame(x = as.numeric(MR_US$place_lon), y = as.numeric(MR_US$place_lat))
points <- points[points$y > 25, ]
ggplot(map.data) + geom_map(aes(map_id = region), map = map.data, fill = "white", 
                            color = "blue", size = 0.25) + expand_limits(x = map.data$long, y = map.data$lat) + 
  theme(axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), 
        axis.title = element_blank(), panel.background = element_blank(), panel.border = element_blank(), 
        panel.grid.major = element_blank(), plot.background = element_blank(), 
        plot.margin = unit(0 * c(-1.5, -1.5, -1.5, -1.5), "lines")) + geom_point(data = points, 
                                                                                 aes(x = x, y = y), size = 2, alpha = 1/4, color = "dark blue")                                                                                 


#Ted Cruz Plot
map.data <- map_data("world")
points <- data.frame(x = as.numeric(TC$place_lon), y = as.numeric(TC$place_lat))
points <- points[points$y > 25, ]

ggplot(map.data) + geom_map(aes(map_id = region), map = map.data, fill = "white", 
                            color = "blue", size = 0.25) + expand_limits(x = map.data$long, y = map.data$lat) + 
  theme(axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), 
        axis.title = element_blank(), panel.background = element_blank(), panel.border = element_blank(), 
        panel.grid.major = element_blank(), plot.background = element_blank(), 
        plot.margin = unit(0 * c(-1.5, -1.5, -1.5, -1.5), "lines")) + geom_point(data = points, 
                                                                                 aes(x = x, y = y), size = 2, alpha = 1/4, color = "dark blue")

#Ted Cruz in American Plot
map.data <- map_data("state")
points <- data.frame(x = as.numeric(TC_US$place_lon), y = as.numeric(TC_US$place_lat))
points <- points[points$y > 25, ]
ggplot(map.data) + geom_map(aes(map_id = region), map = map.data, fill = "white", 
                            color = "blue", size = 0.25) + expand_limits(x = map.data$long, y = map.data$lat) + 
  theme(axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), 
        axis.title = element_blank(), panel.background = element_blank(), panel.border = element_blank(), 
        panel.grid.major = element_blank(), plot.background = element_blank(), 
        plot.margin = unit(0 * c(-1.5, -1.5, -1.5, -1.5), "lines")) + geom_point(data = points, 
                                                                                 aes(x = x, y = y), size = 2, alpha = 1/4, color = "dark blue")                                                                                 


#Bernie Sanders Plot
map.data <- map_data("world")
points <- data.frame(x = as.numeric(BS$place_lon), y = as.numeric(BS$place_lat))
points <- points[points$y > 25, ]

ggplot(map.data) + geom_map(aes(map_id = region), map = map.data, fill = "white", 
                            color = "blue", size = 0.25) + expand_limits(x = map.data$long, y = map.data$lat) + 
  theme(axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), 
        axis.title = element_blank(), panel.background = element_blank(), panel.border = element_blank(), 
        panel.grid.major = element_blank(), plot.background = element_blank(), 
        plot.margin = unit(0 * c(-1.5, -1.5, -1.5, -1.5), "lines")) + geom_point(data = points, 
                                                                                 aes(x = x, y = y), size = 2, alpha = 1/4, color = "dark blue")



#Bernie Sanders in American Plot
map.data <- map_data("state")
points <- data.frame(x = as.numeric(BS_US$place_lon), y = as.numeric(BS_US$place_lat))
points <- points[points$y > 25, ]
ggplot(map.data) + geom_map(aes(map_id = region), map = map.data, fill = "white", 
                            color = "blue", size = 0.25) + expand_limits(x = map.data$long, y = map.data$lat) + 
  theme(axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), 
        axis.title = element_blank(), panel.background = element_blank(), panel.border = element_blank(), 
        panel.grid.major = element_blank(), plot.background = element_blank(), 
        plot.margin = unit(0 * c(-1.5, -1.5, -1.5, -1.5), "lines")) + geom_point(data = points, 
                                                                                 aes(x = x, y = y), size = 2, alpha = 1/4, color = "dark blue")                                                                                 



#HEAT MAPS
#Create a points data frame that just has lon and lat
US_pt <- geo_tweetsUS[c ("place_lon", "place_lat")]
HC_US_pt <- HC_US[c ("place_lon", "place_lat")]
BS_US_pt <- BS_US[c ("place_lon", "place_lat")]
DT_US_pt <- DT_US[c ("place_lon", "place_lat")]
TC_US_pt <- TC_US[c ("place_lon", "place_lat")]
MR_US_pt <- MR_US[c ("place_lon", "place_lat")]

#This function gives back a state name when given lon and lat
latlong2state <- function(pointsDF) {
  # Prepare SpatialPolygons object with one SpatialPolygon
  # per state (plus DC, minus HI & AK)
  states <- map('state', fill=TRUE, col="transparent", plot=FALSE)
  IDs <- sapply(strsplit(states$names, ":"), function(x) x[1])
  states_sp <- map2SpatialPolygons(states, IDs=IDs,
                                   proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # Convert pointsDF to a SpatialPoints object 
  pointsSP <- SpatialPoints(pointsDF, 
                            proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # Use 'over' to get _indices_ of the Polygons object containing each point 
  indices <- over(pointsSP, states_sp)
  
  # Return the state names of the Polygons object containing each point
  stateNames <- sapply(states_sp@polygons, function(x) x@ID)
  stateNames[indices]
}

#Create an additional column for state for latter use
HC_US_pt$states <- latlong2state(HC_US_pt)
BS_US_pt$states <- latlong2state(BS_US_pt)
DT_US_pt$states <- latlong2state(DT_US_pt)
TC_US_pt$states <- latlong2state(TC_US_pt)
MR_US_pt$states <- latlong2state(MR_US_pt)

#filter for the tweets without state
fHC <- HC_US_pt[!(is.na(HC_US_pt$state)),]
fBS <- BS_US_pt[!(is.na(BS_US_pt$state)),]
fDT <- DT_US_pt[!(is.na(DT_US_pt$state)),]
fTC <- TC_US_pt[!(is.na(TC_US_pt$state)),]
fMR <- MR_US_pt[!(is.na(MR_US_pt$state)),]

#Rename 

names(fHC)[names(fHC)=="state"] <- "states"
names(fBS)[names(fBS)=="state"] <- "states"
names(fDT)[names(fDT)=="state"] <- "states"
names(fTC)[names(fTC)=="state"] <- "states"
names(fMR)[names(fMR)=="state"] <- "states"

fHC <- count(fHC,"states")
fBS <- count(fBS,"states")
fDT <- count(fDT,"states")
fTC <- count(fTC,"states")
fMR <- count(fMR,"states")

#Due to the fact that the states have different populations we have created an CVS file with populations
#We are going to divide the number of tweets on the number of the population that can vote thus
#divising tweet per capita, which would be a much better indication than frequency or proportion

population <-read.csv("VotingPopulation.csv", header = TRUE, sep = ",", quote = "\"")

HC_pop <- merge(population,fHC)
HC_pop$freq <- HC_pop$freq/HC_pop$pop

#start mapping
mapUSA <- map('state',  fill = TRUE,  plot = FALSE)
nms <- sapply(strsplit(mapUSA$names,  ':'),  function(x)x[1])
USApolygons <- map2SpatialPolygons(mapUSA,  IDs = nms,  CRS('+proj=longlat'))


#Hillary
idx <- match(unique(nms),  HC_pop$states)
dat2 <- data.frame(value = HC_pop$freq[idx], state = unique(nms))
row.names(dat2) <- unique(nms)

USAsp <- SpatialPolygonsDataFrame(USApolygons,  data = dat2)
spplot(USAsp['value'], col.regions = rev(heat.colors(10, alpha = 1)))                                            


#BERNIE ADJUSTED FOR POPULATION
BS_pop <- merge(population,fBS)
BS_pop$freq <- BS_pop$freq/BS_pop$pop
mapUSA <- map('state',  fill = TRUE,  plot = FALSE)
nms <- sapply(strsplit(mapUSA$names,  ':'),  function(x)x[1])
USApolygons <- map2SpatialPolygons(mapUSA,  IDs = nms,  CRS('+proj=longlat'))
idx <- match(unique(nms),  BS_pop$states)
dat2 <- data.frame(value = BS_pop$freq[idx], state = unique(nms))
row.names(dat2) <- unique(nms)
USAsp <- SpatialPolygonsDataFrame(USApolygons,  data = dat2)
spplot(USAsp['value'], col.regions = rev(heat.colors(20, alpha = 1)))                                            


#Bernie
idx <- match(unique(nms),  fBS$states)
dat2 <- data.frame(value = fBS$freq[idx], state = unique(nms))
row.names(dat2) <- unique(nms)

USAsp <- SpatialPolygonsDataFrame(USApolygons,  data = dat2)
spplot(USAsp['value'], col.regions = rev(heat.colors(20, alpha = 1)))                                            


#Donald Trump
idx <- match(unique(nms),  fDT$states)
dat2 <- data.frame(value = fDT$freq[idx], state = unique(nms))
row.names(dat2) <- unique(nms)

USAsp <- SpatialPolygonsDataFrame(USApolygons,  data = dat2)
spplot(USAsp['value'], col.regions = rev(heat.colors(20, alpha = 1)))                                            

#DONALD TRUMP ADJUSTED FOR POPULATION
DT_pop <- merge(population,fDT)
DT_pop$freq <- DT_pop$freq/DT_pop$pop
mapUSA <- map('state',  fill = TRUE,  plot = FALSE)
nms <- sapply(strsplit(mapUSA$names,  ':'),  function(x)x[1])
USApolygons <- map2SpatialPolygons(mapUSA,  IDs = nms,  CRS('+proj=longlat'))
idx <- match(unique(nms),  DT_pop$states)
dat2 <- data.frame(value = DT_pop$freq[idx], state = unique(nms))
row.names(dat2) <- unique(nms)
USAsp <- SpatialPolygonsDataFrame(USApolygons,  data = dat2)
spplot(USAsp['value'], col.regions = rev(heat.colors(14, alpha = 1)))   


#Marco Rubio
idx <- match(unique(nms),  fMR$states)
dat2 <- data.frame(value = fMR$freq[idx], state = unique(nms))
row.names(dat2) <- unique(nms)

USAsp <- SpatialPolygonsDataFrame(USApolygons,  data = dat2)
spplot(USAsp['value'], col.regions = rev(heat.colors(20, alpha = 1)))                                            

#Ted Cruz
idx <- match(unique(nms),  fTC$states)
dat2 <- data.frame(value = fTC$freq[idx], state = unique(nms))
row.names(dat2) <- unique(nms)

USAsp <- SpatialPolygonsDataFrame(USApolygons,  data = dat2)
spplot(USAsp['value'], col.regions = rev(heat.colors(20, alpha = 1)))       

#Ted Cruz ADJUSTED FOR POPULATION
TC_pop <- merge(population,fTC)
TC_pop$freq <- TC_pop$freq/TC_pop$pop
mapUSA <- map('state',  fill = TRUE,  plot = FALSE)
nms <- sapply(strsplit(mapUSA$names,  ':'),  function(x)x[1])
USApolygons <- map2SpatialPolygons(mapUSA,  IDs = nms,  CRS('+proj=longlat'))
idx <- match(unique(nms),  TC_pop$states)
dat2 <- data.frame(value = TC_pop$freq[idx], state = unique(nms))
row.names(dat2) <- unique(nms)
USAsp <- SpatialPolygonsDataFrame(USApolygons,  data = dat2)
spplot(USAsp['value'], col.regions = rev(heat.colors(12, alpha = 1)))                                            


#MArco Rubio ADJUSTED FOR POPULATION
MR_pop <- merge(population,fMR)
MR_pop$freq <- MR_pop$freq/MR_pop$pop
mapUSA <- map('state',  fill = TRUE,  plot = FALSE)
nms <- sapply(strsplit(mapUSA$names,  ':'),  function(x)x[1])
USApolygons <- map2SpatialPolygons(mapUSA,  IDs = nms,  CRS('+proj=longlat'))
idx <- match(unique(nms),  MR_pop$states)
dat2 <- data.frame(value = MR_pop$freq[idx], state = unique(nms))
row.names(dat2) <- unique(nms)
USAsp <- SpatialPolygonsDataFrame(USApolygons,  data = dat2)
spplot(USAsp['value'], col.regions = rev(heat.colors(10, alpha = 1)))                                            



#Extra Credit Attempt
#Function returns the names of the counties according to lon and lat
latlong2county <- function(pointsDF) {
  # Prepare SpatialPolygons object with one SpatialPolygon
  # per state (plus DC, minus HI & AK)
  states <- map('county', fill=TRUE, col="transparent", plot=FALSE)
  IDs <- sapply(strsplit(states$names, ":"), function(x) x[1])
  states_sp <- map2SpatialPolygons(states, IDs=IDs,
                                   proj4string=CRS("+proj=longlat +datum=wgs84"))
  
  # Convert pointsDF to a SpatialPoints object 
  pointsSP <- SpatialPoints(pointsDF, 
                            proj4string=CRS("+proj=longlat +datum=wgs84"))
  
  # Use 'over' to get _indices_ of the Polygons object containing each point 
  indices <- over(pointsSP, states_sp)
  
  # Return the state names of the Polygons object containing each point
  stateNames <- sapply(states_sp@polygons, function(x) x@ID)
  stateNames[indices]
}

#import county list
data(county.fips)
county<-latlong2county(US_pt)

#convert county names to fip code
fips<-with(county.fips, fips[match(county, polyname)])
US_pt1 <- US_pt
US_pt1$fips <- fips  

#count fips
US_pt1 <- count(US_pt1,"fips")

#Transfer into a tab seperated value file to be used in D3
write.table(US_pt1, file='UStweets.tsv', quote=FALSE, sep='\t')


#Now let us try to do the same thing for each candidate: Hillary Clinton 

county<-latlong2county(HC_US_pt)

#convert county names to fip code
fips<-with(county.fips, fips[match(county, polyname)])
HC_US_pt1 <- HC_US_pt
HC_US_pt1$fips <- fips  

#count fips
HC_US_pt1 <- count(HC_US_pt1,"fips")

#Transfer into a tab seperated value file to be used in D3
write.table(HC_US_pt1, file='HCUStweets.tsv', quote=FALSE, sep='\t')


#Donald Trump
county<-latlong2county(DT_US_pt)

#convert county names to fip code
fips<-with(county.fips, fips[match(county, polyname)])
DT_US_pt1 <- DT_US_pt
DT_US_pt1$fips <- fips  

#count fips
DT_US_pt1 <- count(DT_US_pt1,"fips")

#Transfer into a tab seperated value file to be used in D3
write.table(DT_US_pt1, file='DTUStweets.tsv', quote=FALSE, sep='\t')
