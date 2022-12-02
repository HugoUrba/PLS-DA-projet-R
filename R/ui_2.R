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
        #label = "y:",
        choices = c("clump", "ucellsize", "ucellshape", "mgadhesion", "sepics","bnuclei", "bchromatin", "normnucl","mitoses","classe"),
        selected = "clump"),
      menuItem("select explicative variables", tabName = "x"),
      selectInput(
        inputId = "x",
        label = "X:",
        choices = c("clump", "ucellsize", "ucellshape", "mgadhesion", "sepics","bnuclei", "bchromatin", "normnucl","mitoses","classe"),
        selected = "clump",multiple=TRUE),
      actionButton("viewx", "Submit X Selection"),
      br(),
      br(),
      actionButton("viewy", "Submit Y Selection"),
      
      menuItem("fit the model", tabName = "fit"),
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
      

      #visualisation des données 
      tabItem(tabName = "visualization",
              h1("Visualisation des données"),
              h2("Exploration du tableau"),
              dataTableOutput('dataTable')
      ),
      #afficher x 
      tabItem(tabName='x',
              h1("explicative variables")),
      #afficher y
      tabItem(tabName='y',
              h1("target variable")),
      
      
      #fit the model
      tabItem(tabName = "fit",
              h1("fit the model")
      ),
      #predict the model
      tabItem(tabName = "pred",
              h1("predict the model")
      ),
      
      # visualization
      tabItem(tabName = "graphiques",
              h1("graphiques")
      )
    )
  )
)


