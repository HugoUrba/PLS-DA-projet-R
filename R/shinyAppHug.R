library(shiny)
library(semantic.dashboard)
library(DT)
library(fontawesome)
ui <- dashboardPage(theme = shinytheme("slate"),
  dashboardHeader(title="PLS-DA"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Your DataSet",tabName = "data",icon=icon("table")),
      menuItem("Fit PLSDA",tabName = "fit",icon=icon("refresh")),
      menuItem("Prediction",tabName = "predict",icon=icon("crystal-ball")),
      menuItem("Graphics",tabName = "plots",icon=icon("chart-simple"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem("data",
        fluidPage(
          h1("Your DataSet"),
          fileInput("file", "Input CSV File", accept = ".csv")
        )
      ),
      tabItem("fit",
        fluidPage(
          h1("Fit PLSDA")
        )
      ),
      tabItem("predict",
        fluidPage(
          h1("Prediction")
        )
      ),
      tabItem("plots",
        fluidPage(
          h1("Graphics")
        )
      )
    )
  )
)

server <- function(input, output){

}

shinyApp(ui,server)

