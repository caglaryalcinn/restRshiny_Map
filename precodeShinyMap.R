list.of.packages.requare <- c("shiny",
                      "tidyverse","magrittr",
                      "xml2","leaflet",
                      "maps","mapdata","RgoogleMaps",
                      "rworldmap","dplyr","lubridate")
library(lubridate)
library(shiny)
library(leaflet)
library(mapdata)
library(RgoogleMaps)
library(rworldmap)
library(lubridate)
library(dplyr)
library(xml2)
library(magrittr)
library(tidyverse)



ispark <- read.csv("ispark.csv")
istasyon <- read.csv("istasyon.csv")


ui <- fluidPage(
    sidebarPanel(
        p("Created by Çağlarr YALÇIN", align = "center"),
        
        radioButtons(inputId = "Data", label = "choose data", 
                     choices = c("ispark","istasyon"), selected = "ispark"),
        
        selectInput(inputId = "County", label = "countyies",
                    choices = unique(ispark$CountyName)),
        
        sliderInput(inputId = "capacity", 
                    label = "min park capacity", 
                    value = 20, min = 0, max = 10000),
        
        checkboxGroupInput(inputId = "brand", 
                           label = "choose brand",
                           choices = unique(istasyon$FUEL_DISTRIBUTION_COMPANY_DESC),
                           selected = "Opet"),
    ),
    
    mainPanel(
        leafletOutput(outputId = "map"),
        verbatimTextOutput(outputId = "Openn")
    )
)

server <- function(input, output) {
    
    dataname <- reactive({input$Data})
    
    data <- reactive({
        switch (dataname(),
            
            ispark = ispark %>%
                filter(CountyName == input$County,
                       CAPACITY >= input$capacity),
            
            istasyon = istasyon %>% filter(County_Name == input$County,
                                           FUEL_DISTRIBUTION_COMPANY_DESC %in% input$brand)
        )
    })
    
    isparkmap <- reactive({
        data() %>% 
            leaflet() %>%
            addProviderTiles(providers$Stamen.TonerLite,
                             options = providerTileOptions(noWrap = TRUE)
            ) %>% 
            addMarkers(~LONGITUDE, ~LATITUDE,
                       popup = paste0("<b>Park Name: </b>", 
                                      data()$ParkName,
                                      "<br>","<b>Capacity: </b>",
                                      data()$CAPACITY,
                                      "<br>","<b>Working Hours: </b>",
                                      paste(data()$Open,data()$Close, sep="-"),
                                      "<br>","<b>County Name: </b>",
                                      data()$CountyName)) %>%
            setView(lng = median(data()$LONGITUDE),
                    lat = median(data()$LATITUDE),
                    zoom = 12)

    })
    
    istasyonmap <- reactive({
        data() %>% 
            leaflet() %>%
            addProviderTiles(providers$Stamen.TonerLite,
                             options = providerTileOptions(noWrap = TRUE)
            ) %>% 
            addMarkers(~LONGITUDE, ~LATITUDE,
                       popup = paste0("<b>Name: </b>", 
                                      data()$FUEL_DISTRIBUTION_COMPANY_DESC,
                                      "<br>","<b>Type </b>",
                                      data()$BUSINESS_TYPE_DESC,
                                      "<br>","<b>County Name  </b>",
                                      data()$County_Name,
                                      "<br>","<b>Neighborhood Name: </b>",
                                      data()$Neighborhood_Name )) %>%
                           setView(lng = median(data()$LONGITUDE),
                                   lat = median(data()$LATITUDE),
                                   zoom = 12)
        
    })
    
    output$Map <- renderLeaflet({
            switch (dataname(),
                    ispark = isparkmap(),
                    istasyon = istasyonmap()
            )
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
