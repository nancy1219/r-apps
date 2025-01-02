library(shiny)
library(neuralnet)
library(caret)

# Define UI for the application
ui <- fluidPage(
  titlePanel("ANN Model for Predicting MPG"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Enter Car Features"),
      numericInput("cyl", "Number of Cylinders (cyl):", value = 4, min = 4, max = 8, step = 1),
      numericInput("disp", "Displacement (disp):", value = 160, min = 50, max = 500, step = 1),
      numericInput("hp", "Horsepower (hp):", value = 100, min = 50, max = 300, step = 1),
      numericInput("wt", "Weight (wt):", value = 2.5, min = 1.5, max = 5, step = 0.1),
      
      actionButton("predictBtn", "Predict MPG")
    ),
    
    mainPanel(
      h3("Predicted MPG:"),
      verbatimTextOutput("mpgOutput")
    )
  )
)

# Define server logic required to train the ANN and make predictions
server <- function(input, output) {
  
  # Prepare data
  data(mtcars)
  features <- c("cyl", "disp", "hp", "wt") 
  X <- mtcars[, features] 
  y <- mtcars$mpg
  
  # Split data into training and testing sets
  set.seed(123) 
  trainIndex <- createDataPartition(y, p = 0.8, list = FALSE)
  train_X <- X[trainIndex, ]
  train_y <- y[trainIndex]
  
  # Train the neural network model
  nn_model <- reactive({
    neuralnet(mpg ~ ., data = data.frame(train_X, mpg = train_y), 
              hidden = c(5), 
              linear.output = TRUE) 
  })
  
  # Make predictions based on user input
  predict_mpg <- eventReactive(input$predictBtn, {
    new_data <- data.frame(
      cyl = input$cyl,
      disp = input$disp,
      hp = input$hp,
      wt = input$wt
    )
    
    prediction <- compute(nn_model(), new_data)$net.result
    prediction
  })
  
  # Output predicted MPG
  output$mpgOutput <- renderText({
    paste("The predicted MPG is:", round(predict_mpg(), 2))
  })
}

# Run the application
shinyApp(ui = ui, server = server)