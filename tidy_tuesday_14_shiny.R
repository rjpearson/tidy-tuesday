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
   h2("Life expectancy around the globe: 1950 - 1915"),
   
   leafletOutput("all_map", width = "100%"),
   
   hr(),
   
   fluidRow(
   
   # Sidebar with a slider input for number of bins 
     shiny::column(4, offset = 4,
        sliderInput("year",
                    "Year:",
                    min = 1950,
                    max = 2015,
                    value = 1950,
                    step = 5,
                    sep = "",
                    width = "100%",
                    animate = animationOptions(interval = 750, loop = TRUE))
      )
   )
)


# read in data ----
tidy_tues <- read.csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/week14_global_life_expectancy.csv")

# check data ----
check_years <- tidy_tues %>% 
  filter(code != "") %>% 
  arrange(country, year) %>% 
  group_by(country) %>% 
  slice(1) %>% 
  ungroup()

summarise(check_years, max = max(year)) # 1950

check_missing <- tidy_tues %>% 
  filter(code != "" & year >= 1950) %>% 
  spread(key = year, value = life_expectancy, sep = "_") %>% 
  summarise_at(vars(year_1950:year_2015), funs(sum(is.na(.))))

countries <- tidy_tues %>% 
  filter(code != "" & year >= 1950) %>% 
  mutate(NAME = country)

world_map <- getMap()
# shape_simp <- gSimplify(world_map, tol = 0.01)
# shape <- SpatialPolygonsDataFrame(shape_simp, data=shape_temp@data)

all_dat <- merge(world_map, countries, by = "NAME", duplicateGeoms = TRUE)
some_dat <- merge(world_map, countries[countries$year == 1950,], by = "NAME", duplicateGeoms = TRUE)

at <- seq(floor(min(countries$life_expectancy)*0.1)/0.1, ceiling(max(countries$life_expectancy)*0.1)/0.1, 10)
pal <- colorNumeric("YlGnBu", domain = at, reverse = TRUE)



# Define server logic required to draw a leaflet map
server <- function(input, output) {
  
  # Reactive expression for the data subsetted to what the user selected
  filteredData <- reactive({
    dat <- countries %>%
      filter(year == as.character(input$year))
    dat_map <- merge(world_map, dat, by = "NAME", duplicateGeoms = TRUE)
    subset(dat_map, !is.na(code))
    
  })
  
  output$all_map <- renderLeaflet({
    leaflet(subset(some_dat, !is.na(code)), options= leafletOptions( minZoom=1.2, maxZoom=12) )%>% 
      addProviderTiles(providers$OpenStreetMap.Mapnik) %>%
      addLegend("bottomleft", pal = pal, values = at,
                title = "Average life expectancy (years)",
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
  
  observe({

    proxy <- leafletProxy("all_map", data = filteredData())

    proxy %>%
      #clearShapes() %>%
      addPolygons(stroke=TRUE, weight=1, color="black",
                  fillColor = ~pal(life_expectancy), fillOpacity=0.5,
                  label = ~paste0(NAME, ": ", prettyNum(round(life_expectancy,2), big.mark = ",", format="f"), " (yrs)"),
                  highlight = highlightOptions(
                    weight = 5,
                    color = "#666",
                    #                    dashArray = "",
                    fillOpacity = 0.7,
                    bringToFront = TRUE))

  })
}

# Run the application 
shinyApp(ui = ui, server = server)

