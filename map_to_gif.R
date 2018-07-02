# Create a .gif from png screenshots of leaflet map:

# code based upon code mentioned at 
# https://stackoverflow.com/questions/33508486/create-a-gif-from-a-series-of-leaflet-maps-in-r

# packages ----
library(webshot)
library(leaflet)
library(htmlwidgets)
library(tidyverse)
library(magick)

# create sequence of years ----
years <- seq(1950,2015,1)          # some zoom levels to animate

# forloop to create maps ----
for (i in seq_along(years)) {
  map <- leaflet(subset(all_dat, !is.na(code) & year == years[i]), options= leafletOptions( minZoom=2
                                                                                            , maxZoom=12) )%>% 
    addProviderTiles(providers$OpenStreetMap.Mapnik) %>%
    addLegend("bottomleft", pal = pal, values = at,
              title = paste0("Life Expectancy (yrs): ", years[i]),
              labFormat = labelFormat(prefix = ""),
              opacity = 1) %>%
    addPolygons(stroke=TRUE, weight=1, color="black", 
                fillColor = ~pal(life_expectancy), fillOpacity=0.5,  
                highlight = highlightOptions(
                  weight = 5,
                  color = "#666",
                  fillOpacity = 0.7,
                  bringToFront = TRUE))
  
  map$x$options = append(map$x$options, list("zoomControl" = FALSE)) # this removes zoom controls
  
  # save as png ----
  saveWidget(map, 'temp.html', selfcontained = FALSE)
  webshot('temp.html', file=paste0("map_avg_le",i,".png"),
          cliprect = 'viewport')
}

# create gif file ----
png.files <- sprintf("map_avg_le%01d.png", 1:66) #Mention the number of files to read
GIF.convert <- function(x, output = "global_life_expectancy.gif")#Create a function to read, animate and convert the files to gif
{
  image_read(x) %>%
    image_animate(fps = 5) %>%
    image_write(output)
}

GIF.convert(png.files)
