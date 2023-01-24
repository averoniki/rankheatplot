library(shiny)
library(rmarkdown)
library(shinycssloaders)

#' This is a test layout for a dynamic ui with a tab per upload sheet
#' We will merge it into main app once it's minimally tested and operative


ui <-
  shinyUI(fluidPage(
    useShinyjs(),
    titlePanel("Dynamic Tab Test!"),
    fluidRow(
      column(4,
             fluidRow(column(
               12,
               fileInput(
                 accept = c(
                   "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
                   "application/vnd.ms-excel"
                 ),
                 inputId = "userData",
                 label = h3("Upload a preprocessed csv or .xlsx file")
               )
             )), fluidRow(column(
               12, actionButton("submit", "Submit")
             ))),
      column(8,
             shiny::tabsetPanel(id = "dynamicTabs"))
    )
  ))




server <- function(input, output) {
  userData <- reactive(input$userData)
  
  sheetList <- reactive({
    if (!is.null(userData())) {
      extractSheets(userData()$datapath)
    }
  })
  
  # BUILDING THE FORM
  
  #' This will create the UI with an identical tab for each uploaded sheet
  observe({
    for (name in names(sheetList())) {
      shiny::prependTab(inputId = "dynamicTabs", shiny::tabPanel(
        title = name,
        div(
          id = paste0(name, "_option_a"),
          radioButtons(
            inputId = paste0(name, "_option_a"),
            label = "Select option A",
            choices = c("yes", "no"),
            selected = character(0)
          )
        ),
        div(
          id = paste0(name, "_option_b"),
          radioButtons(
            inputId = paste0(name, "_option_b"),
            label = "Select option B",
            choices = c("yes", "no"),
            selected = character(0)
          )
        ),
        div(
          id = paste0(name, "_option_c"),
          radioButtons(
            inputId = paste0(name, "_option_c"),
            label = "Select option C",
            choices = c("yes", "no"),
            selected = character(0)
          )
        )
      ))
    }
  })
  
  
  # Observe changes (via loop) and update visibility/choices
  observe({
    # group options together
    options <- groupValues(input, names(sheetList()))
    # loop through options and update ui as needed
    for (sheetName in names(options)) {
      for (option in names(visibilityUpdaters)) {
        visibilityUpdaters[[option]]$update(options, sheetName, output)
      }
      
    }
  })
  

  # on submit, we'll build the option group and validate
  observeEvent(input$submit, {
    
  })
  
}


### HELPERS

validators <- list(
  a = list(
    key = "a",
    validate = function(allValues, sheetName) {
      NULL
    }
  ),
  b = list(
    key = "b",
    validate = function(allValues, sheetName) {
      if (allValues['a'] == "no" && allValues['b'] == "no") {
        "Error in field \"b\": A and B cannot both be 'no'!"
      }
    }
  ),
  c = list(
    key = "c",
    validate = function(allValues, sheetName) {
      NULL
    }
  )
)

visibilityUpdaters <- list(
  a = list(
    key = "a",
    update = function(allValues, sheetName) {
      NULL
    }
  ),
  b = list(
    key = "b",
    update = function(allValues, sheetName) {
      NULL
    }
  ),
  c = list(
    key = "c",
    update = function(allValues, sheetName) {
      id <- paste0(sheetName, "_option_c")
      if (!is.null(allValues[[sheetName]][['a']]) &&
          allValues[[sheetName]][['a']] == "no") {
        updateRadioButtons(inputId = id, selected=character(0))
        shinyjs::hide(id)
      } else {
        shinyjs::show(id)
      }
    }
  )
)


#' @param valueList the list of variables, probably reactiveValues
#' @param prefixVec vector of prefixes, probably list of sheets
groupValues <- function(valueList, prefixVec) {
  ret <- list()
  for (prefix in prefixVec) {
    ret[[prefix]] = list()
  }
  
  for (key in names(valueList)) {
    # first, does the key contain the {prefixVec}_option_ string? if so, extract and save value
    prefix <- str_remove(key, "_.+")
    # first, check if key is in prefix list by stripping everything else
    if (prefix %in% prefixVec &&
        str_detect(key, paste0(prefix, "_option_"))) {
      optionName <- str_remove(key, paste0(prefix, "_option_"))
      ret[[prefix]][[optionName]] <- valueList[[key]]
    }
  }
  ret
}


#' @param pth path to excel sheet
extractSheets <- function(pth) {
  sheets <- excel_sheets(pth)
  output <- list()
  for (i in 1:length(sheets)) {
    df <- read_excel(path = pth, sheet = sheets[i])
    output[[sheets[i]]] <- df
  }
  output
}

# Run the application
shinyApp(ui = ui, server = server)
