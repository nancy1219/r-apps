# Install necessary packages
if (!require("shiny")) install.packages("shiny")
if (!require("leaflet")) install.packages("leaflet")
if (!require("plotly")) install.packages("plotly")
if (!require("DT")) install.packages("DT")

library(shiny)
library(leaflet)
library(plotly)
library(DT)

# Sample data for mental health resources
mental_health_data <- data.frame(
  Name = c("Community Counseling Center", "Mental Wellness Clinic", "Support Group HQ", "Therapy Center"),
  Latitude = c(37.7749, 37.7849, 37.7649, 37.7549),
  Longitude = c(-122.4194, -122.4094, -122.4294, -122.4394),
  Services = c("Counseling, Therapy", "Therapy", "Support Groups", "Counseling, Therapy"),
  Contact = c("555-1234", "555-5678", "555-8765", "555-4321"),
  stringsAsFactors = FALSE
)

# Define the UI
ui <- fluidPage(
  titlePanel("Mental Health Support Network"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Search for Resources"),
      textInput("search_name", "Search by Name or Service:", ""),
      actionButton("search_btn", "Search"),
      hr(),
      h3("Submit Feedback"),
      textAreaInput("feedback", "Your Feedback (Optional):", ""),
      actionButton("submit_feedback", "Submit Feedback")
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Map View", leafletOutput("map")),
        tabPanel("Resource List", DTOutput("resource_table")),
        tabPanel("Trends", plotlyOutput("service_trend"))
      )
    )
  )
)

# Define the Server
server <- function(input, output, session) {
  # Filtered data based on search
  filtered_data <- reactive({
    query <- tolower(input$search_name)
    if (query == "") {
      mental_health_data
    } else {
      mental_health_data[
        grepl(query, tolower(mental_health_data$Name)) | 
          grepl(query, tolower(mental_health_data$Services)), 
        ]
    }
  })
  
  # Render the map
  output$map <- renderLeaflet({
    leaflet(data = filtered_data()) %>%
      addTiles() %>%
      addMarkers(
        ~Longitude, ~Latitude,
        popup = ~paste("<b>", Name, "</b><br>",
                       "Services: ", Services, "<br>",
                       "Contact: ", Contact)
      )
  })
  
  # Render the resource table
  output$resource_table <- renderDT({
    datatable(filtered_data(), 
              options = list(pageLength = 5),
              rownames = FALSE)
  })
  
  # Dummy trend data
  service_trends <- data.frame(
    Service = c("Counseling", "Therapy", "Support Groups"),
    Count = c(150, 200, 100)
  )
  
  # Render the trends plot
  output$service_trend <- renderPlotly({
    plot_ly(service_trends, x = ~Service, y = ~Count, type = "bar") %>%
      layout(title = "Popular Mental Health Services",
             xaxis = list(title = "Service Type"),
             yaxis = list(title = "Count"))
  })
  
  # Feedback submission
  observeEvent(input$submit_feedback, {
    if (input$feedback != "") {
      showNotification("Thank you for your feedback!", type = "message")
    } else {
      showNotification("Feedback is optional, but feel free to share your thoughts!", type = "warning")
    }
  })
}

# Run the App
shinyApp(ui = ui, server = server)