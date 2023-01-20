library(shiny)
library(rmarkdown)
library(shinycssloaders)

source("./scripts/rankHeatCircos.R")

# Shiny options:
# toggle which outcomes are visible
# select palette (ryg, greyscale, colorblind)
# toggle treatment labels and values
# show outcomes as abbreviations with guide/legend outside plot

# Define UI for application that draws a histogram
ui <- shinyUI(navbarPage(
  "Rank-Heat Plot",
  tabPanel(title = "Upload data",
           fluidRow(
             column(4, fluidRow(column(
               12,
               fileInput(
                 accept = c(
                   "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
                   "application/vnd.ms-excel",
                   ".csv"
                 ),
                 inputId = "userData",
                 label = h3("Upload a preprocessed csv or .xlsx file")
               )
             )), column(
               12,
               checkboxGroupInput(inputId = "outcomeFilters", label = "")
             )),
             column(8, shinycssloaders::withSpinner(
               plotOutput("heatmap", width = "100%", height = "700px")
             ))
           )),
  
  tabPanel(title = "About",
           fluidRow(htmlOutput("about"))),
))

processInput <- function(pth) {
  if (!is.na(excel_format(pth))) {
    tbl <- read_xlsx(pth, 1)
  } else {
    tbl <- read.csv(pth)
  }
  as.data.frame(tbl)
}

server <- function(input, output) {
  userData <- reactive(input$userData)
  
  userTbl <- reactive({
    if (!is.null(userData())) {
      processed <- processInput(userData()$datapath)
      rhp.prepData(processed)
    }
  })
  
  outcomeFilters <- reactive(input$outcomeFilters)
  
  filteredUserTbl <- reactive({
    if (!is.null(userTbl()) &&
        !is.null(input$outcomeFilters)) {
      #select only rows with filter names selected
      if (length(outcomeFilters()) == 0) {
        
      } else {
        userTbl()[, colnames(userTbl()) %in% input$outcomeFilters]
      }
    }
  })
  
  heatmap <- reactive({
    if (!is.null(filteredUserTbl())) {
      rhp.rankheatplotCircos(filteredUserTbl())
    }
  })
  
  output$heatmap <- renderPlot({
    heatmap()
  })
  
  output$about <- renderUI({
    includeHTML(path = "about.html")
  })
  
  observe({
    if (!is.null(userTbl())) {
      updateCheckboxGroupInput(
        inputId = "outcomeFilters",
        label = "Outcome Filters",
        choices = colnames(userTbl()),
        selected = colnames(userTbl())
      )
    }
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
