library(shiny)
library(shinyWidgets)
library(dplyr)



breast <- read.xlsx("C:/Users/HP/Downloads/breast-cancer-pls-da.xlsx",sheetIndex=1,header=T)
print(breast)




# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Breast Cancer"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Inputs: Select variables to plot ----
    sidebarPanel(
      
      # Select variable for y-axis
      selectInput(
        inputId = "y",
        label = "Y-axis:",
        choices = c("clump", "ucellsize", "ucellshape", "mgadhesion", "sepics","bnuclei", "bchromatin", "normnucl","mitoses","classe"),
        selected = "classe"
      ),
      # Select variable for x-axis
      selectInput(
        inputId = "x",
        label = "X-axis:",
        choices = c("clump", "ucellsize", "ucellshape", "mgadhesion", "sepics","bnuclei", "bchromatin", "normnucl","mitoses","classe"),
        selected = "clump"
      )
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Scree plot ----
      plotOutput(outputId = "Screeplot")
      
    )
  )
)

# Define server logic required to draw a scree plot ----
server <- function(input, output) {
  output$Screeplot <- renderPlot({
    plsda.scree(fit)
  })
}  


# Create Shiny app ----
shinyApp(ui = ui, server = server)
