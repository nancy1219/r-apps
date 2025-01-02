library(shiny)


ui <- fluidPage(
  titlePanel("Skincare Analysis for All Skin Types"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Personalize your skincare"),
      
      
      selectInput("skin_type", 
                  label = "Select your skin type:",
                  choices = c("Oily", "Dry", "Combination", "Sensitive")),
      
      
      sliderInput("age", 
                  label = "Select your age group:",
                  min = 18, max = 65, value = 30),
      
      
      checkboxGroupInput("concerns", 
                         label = "Select your skin concerns:",
                         choices = c("Acne", "Wrinkles", "Hyperpigmentation", 
                                     "Dullness", "Pores", "Dryness")),
      

      radioButtons("routine", 
                   label = "How often do you follow a skincare routine?",
                   choices = c("Daily", "Occasionally", "Rarely")),
      
      actionButton("submit", "Analyze")
    ),
    
    mainPanel(
      h3("Product Recommendations"),
      verbatimTextOutput("recommendation"),
      
      h3("Suggested Routine"),
      verbatimTextOutput("routine_output")
    )
  )
)


server <- function(input, output) {
  
  
  recommendations <- reactive({
    skin_type <- input$skin_type
    concerns <- input$concerns
    age <- input$age
    
    
    if (skin_type == "Oily") {
      rec <- "Oil-free cleansers, Salicylic acid, Lightweight moisturizers"
    } else if (skin_type == "Dry") {
      rec <- "Hydrating cleansers, Hyaluronic acid, Thick moisturizers"
    } else if (skin_type == "Combination") {
      rec <- "Balancing cleansers, Niacinamide, Lightweight moisturizers"
    } else if (skin_type == "Sensitive") {
      rec <- "Gentle cleansers, Fragrance-free products, Soothing creams"
    }
    
    if ("Acne" %in% concerns) {
      rec <- paste(rec, "+ Benzoyl Peroxide for Acne")
    }
    if ("Wrinkles" %in% concerns) {
      rec <- paste(rec, "+ Retinol for Wrinkles")
    }
    if ("Hyperpigmentation" %in% concerns) {
      rec <- paste(rec, "+ Vitamin C for Hyperpigmentation")
    }
    
    return(rec)
  })
  
  
  routine_suggestion <- reactive({
    routine <- input$routine
    
    if (routine == "Daily") {
      return("Cleanse -> Tone -> Moisturize -> Sunscreen (AM) / Cleanse -> Treat -> Moisturize (PM)")
    } else if (routine == "Occasionally") {
      return("Cleanse -> Moisturize -> Sunscreen (AM)")
    } else if (routine == "Rarely") {
      return("Cleanse -> Moisturize")
    }
  })
  
  output$recommendation <- renderText({
    input$submit
    isolate(recommendations())
  })
  
  output$routine_output <- renderText({
    input$submit
    isolate(routine_suggestion())
  })
}


shinyApp(ui = ui, server = server)