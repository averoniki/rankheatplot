library(shiny)
library(rmarkdown)
library(shinycssloaders)

source("./scripts/rankHeatCircos.R")

# Define UI for application that draws a histogram
ui <- navbarPage(
  "Rank-Heat Plot",
  tabPanel(
    title = "Upload data",
    fluidRow(column(
      12, fileInput(
        accept = c(
          "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
          "application/vnd.ms-excel",
          ".csv"
        ),
        inputId = "userData",
        label = h3("Upload a preprocessed csv or .xlsx file")
      )
    )),
    fluidRow(column(
      12, shinycssloaders::withSpinner(plotOutput("heatmap"))
    )),
    fluidRow(column(
      12, shinycssloaders::withSpinner(dataTableOutput("basicTable"))
    ))
  ),
  
  tabPanel(title = "About",
           fluidRow(htmlOutput("about"))),
)

processInput <- function(pth) {
  if (!is.na(excel_format(pth))) {
    tbl <- read_xlsx(pth, 1)
  } else {
    tbl <- read.csv(pth)
  }
  tbl
}

server <- function(input, output) {
  userData <- reactive(input$userData)
  userTbl <- reactive({
    if (!is.null(userData())) {
      processInput(userData()$datapath)
    }
  })
  
  heatmap <- reactive({
    if (!is.null(userTbl())) {
      rhp.rankheatplotCircos(userTbl())
    }
  })
  
  output$basicTable <- renderDataTable({
    req(userTbl())
    userTbl()
  })
  
  output$heatmap <- renderPlot({
    #req(heatmap())
    heatmap()
  })
  
  output$about <- renderUI({
    includeHTML(path = "about.html")
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
