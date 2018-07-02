#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# packages ----
library(shiny)
library(sp)
library(leaflet)
library(fingertipsR)
library(tidyverse)
library(snakecase)
library(rgdal)
library(rgeos)
library(raster)
library(readxl)
library(rworldmap)

# Define UI for application that draws a world map 
# and displays life expectancy for each country between 1950 and 2015
ui <- fluidPage(
  
  tags$style(type = "text/css", "
             .irs-max {font-family: 'arial'; color: black;}
             .irs-min {font-family: 'arial'; color: black;}
             .irs-slider {width: 30px; height: 30px; top: 22px;}
             "),
   
   # Application title
   h2("Life expectancy around the globe: 1950 - 2015"),
  
   # leaflet map output (with adjusted height)
   leafletOutput("all_map", width = "100%", height = "530px"),
   
   hr(),
   
   fluidRow(
   
   # bottom row with a slider input for year
     shiny::column(4, offset = 4,
        sliderInput("year",
                    "Year:",
                    min = 1950,
                    max = 2015,
                    value = 1950,
                    step = 5,
                    sep = "",
                    width = "100%",
                    animate = animationOptions(interval = 1000, loop = FALSE)),
        div(style = "height:50px;padding:0px;")
      )
   )
)


# read in tidy tuesday data (week 14) ----
tidy_tues <- read.csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/week14_global_life_expectancy.csv")

# check data for missingness ----
check_years <- tidy_tues %>% 
  filter(code != "") %>% 
  arrange(country, year) %>% 
  group_by(country) %>% 
  slice(1) %>% # take first year present for each country
  ungroup()

summarise(check_years, max = max(year)) # 1950 is the maximum 'first' year out of all countries

check_missing <- tidy_tues %>% 
  filter(code != "" & year >= 1950) %>% 
  spread(key = year, value = life_expectancy, sep = "_") %>% 
  summarise_at(vars(year_1950:year_2015), funs(sum(is.na(.)))) # are any years missing data

# create dataset incl. years 1950 - 2015 ----
countries <- tidy_tues %>% 
  filter(code != "" & year >= 1950) %>% 
  mutate(NAME = country)

# get shapefile for world map----
world_map <- getMap()

# merge on data to shapefile (by country name) ----
all_dat <- merge(world_map, countries, by = "NAME", duplicateGeoms = TRUE)
some_dat <- subset(all_dat, year == 1950)
#  merge(world_map, countries[countries$year == 1950,], by = "NAME", duplicateGeoms = TRUE)

# assign sequential colour palette to life expectancies (min - max by increments of 10 years) ----
at <- seq(floor(min(countries$life_expectancy)*0.2)/0.2, ceiling(max(countries$life_expectancy)*0.2)/0.2, 10)
pal <- colorNumeric("YlGnBu", domain = at, reverse = TRUE)


# Define server logic required to draw a leaflet map ----
server <- function(input, output) {
  
  # Reactive expression for the data subsetted to what the user selected (years input) 
  filteredData <- reactive({
    dat <- countries %>%
      filter(year == as.character(input$year))
    dat_map <- merge(world_map, dat, by = "NAME", duplicateGeoms = TRUE)
    subset(dat_map, !is.na(code))
    
  })
  
  # create leaflet map for first year (1950)
  output$all_map <- renderLeaflet({
    leaflet(subset(some_dat, !is.na(code)), options= leafletOptions( minZoom=2
                                                                    , maxZoom=12) )%>% 
      addProviderTiles(providers$OpenStreetMap.Mapnik) %>%
      addLegend("bottomleft", pal = pal, values = at,
                title = "Average (yrs)",
                labFormat = labelFormat(prefix = ""),
                opacity = 1) %>%
      addPolygons(stroke=TRUE, weight=1, color="black", 
                  fillColor = ~pal(life_expectancy), fillOpacity=0.5,  
                  label = ~paste0(NAME, ": ", prettyNum(life_expectancy, big.mark = ",", format="f")),
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    #                   dashArray = "",
                    fillOpacity = 0.7,
                    bringToFront = TRUE)) 
  })
  
  # as the year input changes the code below will ensure the polygons change too:
  observe({

    proxy <- leafletProxy("all_map", data = filteredData())

    proxy %>%
      addPolygons(stroke=TRUE, weight=1, color="black",
                  fillColor = ~pal(life_expectancy), fillOpacity=0.6,
                  label = ~paste0(NAME, ": ", prettyNum(round(life_expectancy,2), big.mark = ",", format="f"), " (yrs)"),
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    fillOpacity = 0.6,
                    bringToFront = TRUE))

  })
}

# Run the application ----
shinyApp(ui = ui, server = server)

