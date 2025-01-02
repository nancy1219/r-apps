library(shiny)
library(colourpicker)
library(colorspace)

# Define UI
ui <- fluidPage(
  titlePanel("Color Palette Generator"),
  
  sidebarLayout(
    sidebarPanel(
      colourInput("base_color", "Choose Base Color", value = "#3498db"),
      selectInput(
        "palette_type",
        "Select Palette Type",
        choices = c("Complementary", "Monochromatic", "Analogous", "Triadic", "Tetradic"),
        selected = "Complementary"
      ),
      sliderInput("num_colors", "Number of Colors", min = 2, max = 10, value = 5),
      actionButton("generate", "Generate Palette")
    ),
    
    mainPanel(
      h4("Generated Palette"),
      uiOutput("color_boxes"),
      h4("Color Codes"),
      tableOutput("color_table")
    )
  )
)

# Define Server
server <- function(input, output, session) {
  
  # Generate palette based on input
  palette_generator <- reactive({
    req(input$base_color)
    
    # Convert base color to hexadecimal and generate palette
    base_color <- hex2RGB(input$base_color)
    num_colors <- input$num_colors
    
    palette <- switch(
      input$palette_type,
      "Complementary" = qualitative_hcl(n = num_colors, h = as.numeric(base_color@coords[1]) + 180),
      "Monochromatic" = sequential_hcl(n = num_colors, h = base_color@coords[1]),
      "Analogous" = qualitative_hcl(n = num_colors, h = seq(base_color@coords[1] - 30, base_color@coords[1] + 30, length.out = num_colors)),
      "Triadic" = qualitative_hcl(n = num_colors, h = c(base_color@coords[1], base_color@coords[1] + 120, base_color@coords[1] + 240)),
      "Tetradic" = qualitative_hcl(n = num_colors, h = c(base_color@coords[1], base_color@coords[1] + 90, base_color@coords[1] + 180, base_color@coords[1] + 270))
    )
    
    palette
  })
  
  # Render color boxes
  output$color_boxes <- renderUI({
    palette <- palette_generator()
    div(
      style = "display: flex; gap: 10px;",
      lapply(palette, function(color) {
        div(
          style = paste("width: 50px; height: 50px; background-color:", color, "; border: 1px solid black;"),
          ""
        )
      })
    )
  })
  
  # Render color codes table
  output$color_table <- renderTable({
    palette <- palette_generator()
    data.frame(
      Color = palette,
      HEX = palette
    )
  })
}

# Run the app
shinyApp(ui = ui, server = server)