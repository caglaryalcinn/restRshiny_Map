



library(dplyr)
library(leaflet)
library(shiny)
library(maps)
library(mapproj)

# Load data ----

ispark <- read.csv("ispark.csv",T,sep = ",")
#View(ispark)


istasyon <- read.csv("istasyon.csv",T,sep = ",")
#View(istasyon)




ui <- fluidPage(
    leafletOutput("ispark"),
    titlePanel("Stat 292 Quiz"),
    sidebarLayout(
        sidebarPanel(
            h1("Istanbul Shiny App",align = "center"),
            h6("Created by",align = "center"),
            h6("Caglar Yalcin- 2291003 ",align = "center"),
            sliderInput(
                "MPC", label = "Minimum Parking Capacity",
                min = min(ispark$CAPACITY), value = 30, max = 500
                
            ),
            radioButtons("RB", "Choose data:",
                         c("Ispark" = 1,
                           "Istasyon" = 2
                         )),
            
            selectInput("selection", h4("County"), 
                        choices = list("Arnavutkoy" = istasyon$County_Name[1]
                                       , "Silivri" = istasyon$County_Name[2]
                                       ,"Esenyurt" =istasyon$County_Name[3],
                                       "Pendik        " =istasyon$County_Name[4],
                                       "Kucukcekmece  " =istasyon$County_Name[5],
                                       "Uskudar      " =istasyon$County_Name[6],
                                       "Beykoz" =istasyon$County_Name[7],
                                       "Kadikoy       " =istasyon$County_Name[8],
                                       "Zeytinburnu   " =istasyon$County_Name[9],
                                       "Umraniye      " =istasyon$County_Name[10],
                                       "Besiktas      " =istasyon$County_Name[11],
                                       "Beyoglu      " =istasyon$County_Name[12],
                                       "Sultangazi" =istasyon$County_Name[13],
                                       "Kartal        " =istasyon$County_Name[14],
                                       "Beylikduzu    " =istasyon$County_Name[15],
                                       "Bagcilar      " =istasyon$County_Name[16],
                                       "Sisli         " =istasyon$County_Name[17],
                                       "Eyupsultan   " =istasyon$County_Name[18],
                                       "Gaziosmanpasa " =istasyon$County_Name[19],
                                       "Buyukcekmece  " =istasyon$County_Name[20],
                                       "Tuzla         " =istasyon$County_Name[21],
                                       "Kagithane     " =istasyon$County_Name[22],
                                       "Sile          " =istasyon$County_Name[23],
                                       "Atasehir     " =istasyon$County_Name[24],
                                       "Esenler       " =istasyon$County_Name[25],
                                       "Bahcelievler  " =istasyon$County_Name[26],
                                       "Gungoren      " =istasyon$County_Name[27],
                                       "Basaksehir    " =istasyon$County_Name[28],
                                       "Maltepe       " =istasyon$County_Name[29],
                                       "Sancaktepe   " =istasyon$County_Name[30],
                                       "Sultanbeyli   " =istasyon$County_Name[31],
                                       "Bayrampasa    " =istasyon$County_Name[32],
                                       "Cekmekoy      " =istasyon$County_Name[33],
                                       "Bakirkoy      " =istasyon$County_Name[34],
                                       "Catalca       " =istasyon$County_Name[35],
                                       "Avcilar      " =istasyon$County_Name[36],
                                       "Sariyer       " =istasyon$County_Name[37],
                                       "Fatih        " =istasyon$County_Name[38]
                        ), selected = 1),
            checkboxGroupInput(inputId = "stations",
                               label = strong("Choose a gas station brand"),
                               choices = list("Starpet" = 1,
                                              "Total" = 2,
                                              "BP" = 3,
                                              "LukOil" =4,
                                              "Petrol Ofisi" =5,
                                              "Opet" =6,
                                              "Alpet" = 7,
                                              "Shell-Turcas Petrol" = 8,
                                              "Turkiye Petrol" = 9,
                                              "Aytemiz" =10,
                                              "Moil" =11,
                                              "Pet-Line"=12,
                                              "Turkuaz"=13,
                                              "Sunpet"=14,
                                              "KadOil"=15
                               ),selected=1),
            checkboxInput("checkbox", "Show only open parks", value = TRUE)),
        
        mainPanel(
            leafletOutput(outputId = "mymap"))))      



server <- function(input, output,session) {
    
    
    output$mymap <- renderLeaflet({
        
        vec1<-ispark %>% filter(CountyName==input$selection)
        vec2<-istasyon%>% filter(County_Name==input$selection)
        
        if(input$RB==1){
            vec<-ispark %>% filter(CAPACITY>=input$MPC)
            
            
            leaflet(vec) %>% 
                addProviderTiles(providers$Esri.NatGeoWorldMap) %>% 
                addMarkers(~LONGITUDE, ~LATITUDE,
                           label = ~ParkName,
                           popup = paste0(
                               "<b>ParkName: </b>", 
                               vec$ParkName,
                               "<br>",
                               "<b>CountyName: </b>",
                               vec$CountyName))%>% 
                setView(lng = median(vec$LONGITUDE),
                        lat = median(vec$LATITUDE),
                        zoom = 10)
            
        }
        
        else {
            
            leaflet(istasyon) %>% 
                addProviderTiles("Esri") %>% 
                addMarkers(~LONGITUDE, ~LATITUDE,
                           label = ~FUEL_DISTRIBUTION_COMPANY_DESC,
                           popup = paste0(
                               "<b>ParkName: </b>", 
                               istasyon$FUEL_DISTRIBUTION_COMPANY_DESC,
                               "<br>",
                               "<b>CountyName: </b>",
                               istasyon$FUEL_DISTRIBUTION_COMPANY_DESC))%>% 
                setView(lng = median(istasyon$LONGITUDE),
                        lat = median(istasyon$LATITUDE),
                        zoom = 10)}
        
        
        
        
        
        
        
        
        
        
        
        
    })}
shinyApp(ui, server)
