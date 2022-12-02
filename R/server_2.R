
server <- function(input, output) {
  output$preview <-  renderDataTable({
    
    req(input$dataFile)
    
    df <- read.csv(input$dataFile$datapath,
                   header = as.logical(input$header),
                   sep = input$sep,
                   quote = input$quote,
                   nrows=10
    )
    df[, c(input$x, input$y), drop = FALSE]
  },  options = list(scrollX = TRUE , dom = 't'))

  
#server <- function(input,output){
  
#}
  
}  
shinyApp(ui, server)

