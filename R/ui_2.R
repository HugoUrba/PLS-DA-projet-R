library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "PLSDA Package"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("importation du data base", tabName = "readData", icon = icon("readme")),
      menuItem("select the target variable", tabName = "y"),
      selectInput(
        inputId = "y",
        label = "y:",
        choices = c()),
      menuItem("select explicative variables", tabName = "x"),
      selectInput(
        inputId = "x",
        label = "X:",
        choices = c(),
        multiple=TRUE),
      actionButton("submitSelection", "Submit Selection"),
      menuItem("fit the model", tabName = "fit"),
      menuItem("Data for prediction", tabName = "dataPred"),
      menuItem("Predict", tabName = "pred"),
      menuItem("graphiques", tabName = "graphiques", icon = icon("poll"))
    )
  ),
  dashboardBody(
    tabItems(
      # Read data (Pour importer un fichier, shiny propose la fonction fileInput)
      tabItem(tabName = "readData",
              h1("Lecture des données"),
              fileInput("dataFile",label = NULL,
                        buttonLabel = "Browse...",
                        placeholder = "No file selected"),
              
              h3("Parameters"),
              
              # Input: Checkbox if file has header
              radioButtons("header", 
                           label = "Header",
                           choices = c("Yes" = TRUE,
                                       "No" = FALSE),
                           selected = TRUE, inline=T),
              
              # Input: Select separator ----
              radioButtons("sep", 
                           label = "Separator",
                           choices = c(Comma = ",",
                                       Semicolon = ";",
                                       Tab = "\t"),
                           selected = ";", inline=T),
              
              # Input: Select quotes ----
              radioButtons("quote", 
                           label= "Quote",
                           choices = c(None = "",
                                       "Double Quote" = '"',
                                       "Single Quote" = "'"),
                           selected = "", inline=T),
              #créer une zone d'affichage 
              h3("File preview"),
              dataTableOutput(outputId = "preview")
              
      ),
      
      #fit the model
      tabItem(tabName = "fit",
              h1("Fit the model"),
              actionButton("launchFit", "Start Fit"),
              verbatimTextOutput("fitResults"),
              h4(radioButtons("fitSelect", "Results of Fit Training",
                              choices = c(Coefficients = "coef",
                                          Intercept = "intercept",
                                          X_weights = "Xweights",
                                          Y_weights="Yweigths",
                                          X_loadings="Xloadings",
                                          Y_loadings="Yloadings",
                                          X_scores="Xscores",
                                          Y_scores="Yscores",
                                          Modalities = "modalities",
                                          Ncomp = "ncomp"))),
              verbatimTextOutput("fitResultsSelect")
      ),
      
      
      tabItem(tabName = "dataPred",
              
              h1("Lecture des données à prédire"),
              fileInput("dataFile2",label = NULL,
                        buttonLabel = "Browse...",
                        placeholder = "No file selected"),
              
              h3("Parameters"),
              
              # Input: Checkbox if file has header
              radioButtons("header2", 
                           label = "Header",
                           choices = c("Yes" = TRUE,
                                       "No" = FALSE),
                           selected = TRUE, inline=T),
              
              # Input: Select separator ----
              radioButtons("sep2", 
                           label = "Separator",
                           choices = c(Comma = ",",
                                       Semicolon = ";",
                                       Tab = "\t"),
                           selected = ";", inline=T),
              
              # Input: Select quotes ----
              radioButtons("quote2", 
                           label= "Quote",
                           choices = c(None = "",
                                       "Double Quote" = '"',
                                       "Single Quote" = "'"),
                           selected = "", inline=T),
              #créer une zone d'affichage 
              h3("File preview"),
              dataTableOutput(outputId = "preview2")
      ),
      
      #predict the data
      tabItem(tabName = "pred",
              h1("Predict the new data"),
              actionButton("submitPred", "Submit for prediction"),
              verbatimTextOutput("predResults")
      ),
              
      # visualization
      tabItem(tabName = "graphics",
              h1("Graphics")
      )
    )
  )
)





