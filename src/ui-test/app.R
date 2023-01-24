library(shiny)
library(shinyjs)

#' Simple demo of hide/show with shinyjs 
#' Can remove once dynamic-tabs is a little more mature

ui <- shinyUI(navbarPage(
  "Rank-Heat Plot",
  tabPanel(title = "Upload data",
           useShinyjs(),
           fluidRow(
             column(
               4,
               fluidRow(column(
                 12,
                 radioButtons(
                   inputId = "option1",
                   label = "select type",
                   choices = c("a", "b", "c")
                 )
               )),
               fluidRow(column(12,
                               div(
                                 id = "a_options",
                                 selectInput(
                                   inputId = "option",
                                   label = "Select a options",
                                   choices = c("d", "e", "f"),
                                 )
                               ))),
               fluidRow(column(12,
                               div(
                                 id = "b_options",
                                 radioButtons(
                                   inputId = "option3",
                                   label = "Select b option",
                                   choices = c("yes", "no")
                                 )
                               ))),
               fluidRow(column(
                 12,
                 radioButtons(
                   inputId = "option4",
                   label = "Select required option 4",
                   choices = c("yes", "no"),
                   selected = character(0)
                 )
               )),
               actionButton("submit", "Submit")
             )
           )),
  
  tabPanel(title = "About",
           fluidRow(htmlOutput("about")))
))

# Define server logic required to draw a histogram
server <- function(input, output) {
  output$about <- renderUI({
    includeHTML(path = "about.html")
  })
  
  # disable based on input id /shinyjs
  # in this case, option4 is required
  observe({
    if (is.null(input$option4)) {
      shinyjs::disable("submit")
    } else {
      shinyjs::enable("submit")
    }
  })
  
  shinyjs::hide("a_options")
  shinyjs::hide("b_options")
  
  observe({
    if (input$option1 == "a") {
      shinyjs::show("a_options")
      shinyjs::hide("b_options")
    } else if (input$option1 == "b") {
      shinyjs::show("b_options")
      shinyjs::hide("a_options")
    }
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)
