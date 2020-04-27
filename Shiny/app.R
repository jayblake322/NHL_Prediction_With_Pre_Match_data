
# libraries
library(shiny)
library(shinydashboard)
library(leaflet)
library(DT)
library(lubridate)
library(dplyr)


# global variables
date <- Sys.Date()

# data
matches <- read.csv("shiny_matches.csv") %>%
 mutate(date = dmy(date))

ranks <- read.csv("team_ranks_table.csv") %>% 
 mutate(Date = dmy(Date)) %>% arrange(Date, Rank)

season_date <- matches %>% select(c("date", "season")) %>%
  distinct() %>% 
  mutate(date = as.character(date)) %>% 
  mutate(season = as.character(season))


# Define UI for application that draws a histogram
ui <- dashboardPage(
  
  dashboardHeader(title = "NHL Game Tracker"),
  dashboardSidebar(
    
    h3("Chosen Date"),
    dateInput("Show Games and Rank Table", inputId = "Game_Searcher", date),
    h3("Season"),
    textOutput("season")
    
    ),
  
  dashboardBody(
    h3('NHL Games for Chosen Date'),
    leafletOutput("mymap", height = 350),
    h3("Rank of Teams Prior to the Match"),
    dataTableOutput("TeamRanks", height = 250)
  
  )
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  ## Reactive expression for the data subsetted to what the user selected
  season <- reactive({
    ifelse(month(input$Game_Searcher) > 6, 
           paste(year(input$Game_Searcher), "/", year(input$Game_Searcher) + 1),
           paste(year(input$Game_Searcher) - 1, "/", year(input$Game_Searcher)))
  
           })
  
  ## Reactive expression for the data subsetted to what the user selected
  filteredData_rank <- reactive({
    ranks %>% filter(Date == input$Game_Searcher) %>% select(-"Date")
  })
  
  ## Reactive expression for the data subsetted to what the user selected
  filteredData_map <- reactive({
    matches %>% filter(date == input$Game_Searcher)
  })
  
  output$season <- renderText({season()})
  
  output$mymap <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles(providers$CartoDB.Positron) %>%
      fitBounds(-128.569703, 25.187022, -64.497437, 55.092184) %>%
      setMaxBounds(-128.569703, 25.187022, -64.497437, 55.092184)
  })

  output$TeamRanks <- renderDataTable(
    datatable(filteredData_rank(), rownames = FALSE,
      options = list(
          pageLength = 5,
          pagingType = "simple",
          autoWidth = TRUE)
    )
  )  
  
  observe({
    leafletProxy("mymap", data = filteredData_map()) %>%
      clearShapes() %>%
      addCircles(lng = ~location_lon, lat = ~location_lat, 
                 color = "black", fillColor = "red", radius = 100000, stroke = TRUE, fillOpacity = 0.5,
                 popup = ~label_content,
                 popupOptions = popupOptions(maxWidth = 1000, closeOnClick = TRUE))
  })

}

# Run the application 
shinyApp(ui = ui, server = server)
