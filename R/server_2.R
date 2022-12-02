
server <- function(input, output, session) {
  table <- reactive({
    
    req(input$dataFile)
    
    df <- read.csv(input$dataFile$datapath,
                   header = as.logical(input$header),
                   sep = input$sep,
                   quote = input$quote
    )
  })

  output$preview <-  renderDataTable({
    table()
  },  options = list(scrollX = TRUE , dom = 't'))
  
  observeEvent({
    c(input$dataFile , input$header , input$sep , input$quote)
  },{
    updateSelectInput(session, "x", "X:", choices = colnames(table()))
    updateSelectInput(session, "y", "Y:", choices = colnames(table()))
  })
  
  formula <- eventReactive({input$submitSelection},{
    X <- input$x
    Y <- input$y
    
    formula1 <- as.formula(paste(Y, "~", paste(X, collapse = " + ")))
    
  })
  
  PLSfit <- eventReactive({input$launchFit}, {
    object <- plsda.fit(formula = formula(), data = table())
  })
  
  output$fitResults <- renderPrint({
    summary(PLSfit())
  })
  
  output$fitResultsSelect <- renderPrint({
    PLSfit()[input$fitSelect]
  })
  
  table2 <- reactive({
    
    req(input$dataFile2)
    
    df <- read.csv(input$dataFile2$datapath,
                   header = as.logical(input$header2),
                   sep = input$sep2,
                   quote = input$quote2
    )
  })
  
  output$preview2 <-  renderDataTable({
    table2()
  },  options = list(scrollX = TRUE , dom = 't'))
  
  PLSpredict <- eventReactive({input$submitPred}, {
    objectpred <- plsda.predict(PLSfit(), table2()[,input$x])
  })
  
  output$predResults <- renderPrint({
    PLSpredict()[1:min(length(PLSpredict()), 100)]
  })
  
}

shinyApp(ui, server)
