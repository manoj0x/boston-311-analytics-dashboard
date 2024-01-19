library(shiny)
library(DT)
library(ggplot2)
library(leaflet)
library(plotly)

# Read the data
data <- read.csv("boston_311_2023_data.csv")

# Define UI
ui <- fluidPage(
  titlePanel("Boston 311 Dashboard"),
  sidebarLayout(
    sidebarPanel(
      selectInput("status", "Status", choices = unique(data$case_status)),
      selectInput("neighborhood", "Neighborhood", choices = unique(data$neighborhood)),
      selectInput("reason", "Reason", choices = unique(data$reason))
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Data View", DTOutput("table")),
        tabPanel("Summary", plotOutput("plot")),
        tabPanel("Map", leafletOutput("map"))
      )
    )
  )
)

# Define server logic
# Define server logic
server <- function(input, output) {
  
  filteredData <- reactive({
    subset(data, 
           (case_status == input$status | input$status == "All") &
             (neighborhood == input$neighborhood | input$neighborhood == "All") &
             (reason == input$reason | input$reason == "All"))
  })
  
  output$table <- renderDT({
    datatable(filteredData())
  })
  
  output$plot <- renderPlot({
    # Check if there is data to plot
    if (nrow(filteredData()) > 0) {
      # Create a simple bar plot
      counts <- table(filteredData()$neighborhood)
      barplot(counts, main = "Number of Calls per Neighborhood", 
              xlab = "Neighborhood", ylab = "Count", col = "blue")
    } else {
      # Display a message if there is no data to plot
      plot.new()
      text(0.5, 0.5, "No data available for this selection")
    }
  })
  
  output$map <- renderLeaflet({
    leaflet(filteredData()) %>% 
      addTiles() %>% 
      addMarkers(~longitude, ~latitude)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

