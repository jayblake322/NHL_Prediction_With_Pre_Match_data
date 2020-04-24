library(shiny)
library(leaflet)
library(lubridate)
library(DT)
library(shinydashboard)

# set working directory
setwd("C:/Users/jaybl/OneDrive/DS_Projects/NHL_Prediction_With_Pre_Match_data/shiny")

matches <- read.csv("shiny_matches.csv") %>%
  mutate(date = dmy(date))

ranks <- read.csv("team_ranks_table.csv") %>% 
  mutate(Date = dmy(Date)) %>% arrange(Date, Rank)

random_date <- as.Date("2020-02-20")

ui <- bootstrapPage(
  
  titlePanel("NHL Game Tracker"),
  mainPanel(
    h5("Upcoming games not played due to Covid-19"),
    h2("")
  ),
  
  dateInput("Show Games and Rank Table for", inputId = "Game_Searcher", random_date),
  
  mainPanel("Team Rank Before Today's Match", width = 100,
            fluidRow(
              splitLayout(cellWidths = c("50%", "50%"), dataTableOutput("TeamRanks"), leafletOutput("mymap")
      )
    )
  )
)
  


server <- function(input, output) {
  
  ## Reactive expression for the data subsetted to what the user selected
  filteredData_rank <- reactive({
   ranks %>% filter(Date == input$Game_Searcher)
  })
  
  ## Reactive expression for the data subsetted to what the user selected
  filteredData_map <- reactive({
    matches %>% filter(date == input$Game_Searcher)
  })
  
  
  output$TeamRanks <- renderDataTable({
    filteredData_rank()
    })
  
  output$mymap <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles(providers$CartoDB.Positron) %>%
      fitBounds(-128.569703, 25.187022, -64.497437, 55.092184) %>%
      setMaxBounds(-128.569703, 25.187022, -64.497437, 55.092184)
  })

  observe({
   leafletProxy("mymap", data = filteredData_map()) %>%
    clearShapes() %>%
     addCircles(lng = ~location_lon, lat = ~location_lat, 
                color = "red", radius = 100000, stroke = FALSE, fillOpacity = 0.5,
                popup = ~label_content)
  })

}

shinyApp(ui, server)
