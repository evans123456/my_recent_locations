library(jsonlite)
library(ggplot2)
#install.packages('Amelia') #for using missmap
library(Amelia)
#install.packages("leaflet")
library(leaflet)#for using mapdata

#check out googlemaps analysis -> https://shiring.github.io/maps/2016/12/30/Standortverlauf_post

history = read_json("./Takeout/Location History/Location History.json", simplifyVector = TRUE)
semantic.history = read_json("./Takeout/Location History/Semantic Location History/2021/2021_FEBRUARY.json", simplifyVector = TRUE)
history <- history[["locations"]]
history$coordinates <- cbind(history$latitudeE7,history$longitudeE7)
str(semantic.history)

activity.segment <- semantic.history[["timelineObjects"]][["activitySegment"]]
places.visited <- semantic.history[["timelineObjects"]][["placeVisit"]]

#remove NA rows
table(is.na(places.visited))
places.visited <- na.omit(places.visited)

dim(places.visited)
str(places.visited)

#convert timestamps
history$timestampMs = as.POSIXct(as.numeric(history$timestampMs)/1000, origin = "1970-01-01")
places.visited$location$sourceInfo$deviceTag = as.POSIXct(as.numeric(places.visited$location$sourceInfo$deviceTag )/1000, origin = "1970-01-01")
places.visited$location$locationConfidence = as.POSIXct(as.numeric(places.visited$location$locationConfidence)/1000, origin = "1970-01-01")
places.visited$duration$startTimestampMs = as.POSIXct(as.numeric(places.visited$duration$startTimestampMs)/1000, origin = "1970-01-01")
places.visited$duration$endTimestampMs = as.POSIXct(as.numeric(places.visited$duration$endTimestampMs)/1000, origin = "1970-01-01")

#convert the coordinates to gps
places.visited$location$latitudeE7 = places.visited$location$latitudeE7 /1e7
places.visited$location$longitudeE7 = places.visited$location$longitudeE7 /1e7

history$latitudeE7 = history$latitudeE7/1e7 
history$longitudeE7 = history$longitudeE7 / 1e7

leaflet(data = places.visited) %>% addTiles() %>%
  addMarkers(~location$longitudeE7, ~location$latitudeE7,popup = ~as.character(location$name), label = ~as.character(location$name))
  
  
a <- ggplot(places.visited,aes(location$name))
a <- a + geom_bar()
a <- a + theme(axis.text.x = element_text(angle = 90))
a


table(places.visited$location$name)








#library(psych)

#describe(places.visited)


missmap(places.visited$location$name)
























leaflet() %>%
  addTiles()%>%
  addMarkers(lng=74.5,lat=17.97,popup = "My native location", label="whachu sayin")

latitude <- history$latitudeE7
longitude <- history$longitudeE7
len <- length(latitude)
data(quakes)

leaflet(data = quakes[1:20,]) %>% addTiles() %>%
  addMarkers(~long, ~lat)

b <- ggplot(quakes, aes(x=depth,y=mag))
b <- b + geom_blank() + ylim(3,7) + xlim(0,700)
b <- b + geom_hline(yintercept = 5)
b <- b + geom_vline(xintercept = 500)
b <- b + geom_abline(intercept=4,slope=0.01)
b

c <- ggplot(quakes, aes(depth))
c <- c + geom_histogram()
c

d <- ggplot(quakes,aes(mag))
d <- d + geom_histogram()
d

e <- ggplot(quakes,aes(stations))
#e <- e + geom_histogram() 
#e <- e + geom_dotplot()
e <- e + geom_freqpoly()
e <- e + geom_density(kernel="gaussian")
e


#continuous x and continuous y
f <- ggplot(quakes,aes(depth,mag))
f <- f + geom_jitter()
f <- f + geom_label(aes(label=mag)) #shows the number ontop instead of the point
f

length(unique(quakes$stations))

h <- ggplot(quakes,aes(depth,mag))
h <- h + geom_jitter(aes(color=factor(stations)))
#h <- h + geom_jitter(aes(shape=factor(stations)))

h
