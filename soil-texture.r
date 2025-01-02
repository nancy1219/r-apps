install.packages("shiny")
install.packages("ggplot2")
install.packages("soiltexture")
   
# Load necessary libraries
library(shiny)
library(soiltexture)
library(ggplot2)

# Define the UI for the Shiny app
ui <- fluidPage(
  titlePanel("Soil Texture Identifier"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("sand", "Percentage of Sand:", min = 0, max = 100, value = 40),
      sliderInput("silt", "Percentage of Silt:", min = 0, max = 100, value = 40),
      sliderInput("clay", "Percentage of Clay:", min = 0, max = 100, value = 20),
      actionButton("calculate", "Identify Soil Texture")
    ),
    mainPanel(
      plotOutput("texturePlot"),
      verbatimTextOutput("textureClass")
    )
  )
)

# Define server logic
server <- function(input, output) {
  # Observe the button to calculate soil texture
  observeEvent(input$calculate, {
    # Get user input values
    sand <- input$sand
    silt <- input$silt
    clay <- input$clay
    
    # Check if percentages add up to 100
    if ((sand + silt + clay) != 100) {
      output$textureClass <- renderText("The percentages of sand, silt, and clay must add up to 100.")
      output$texturePlot <- renderPlot(NULL)
      return()
    }
    
    # Create a data frame with input values
    soil_data <- data.frame(
      "CLAY" = clay,
      "SILT" = silt,
      "SAND" = sand
    )
    
    # Calculate the soil texture class
    texture_class <- TT.points.in.classes(
      tri.data = soil_data,
      class.sys = "USDA.TT",
      PiC.type = "t"
    )
    
    # Display the texture class
    output$textureClass <- renderText({
      paste("Soil Texture Class:", texture_class[[1]])
    })
    
    # Generate the soil texture triangle plot
    output$texturePlot <- renderPlot({
      TT.plot(
        class.sys = "USDA.TT",
        main = "Soil Texture Triangle",
        tri.data = soil_data,
        col = "blue", 
        pch = 19, 
        cex = 2
      )
    })
  })
}

# Run the application
shinyApp(ui = ui, server = server)