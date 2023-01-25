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
                 label = h3("Upload an .xlsx file")
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
      shiny::prependTab(inputId = "dynamicTabs",
                        shiny::tabPanel(title = name,
                                        fluidRow(
                                          column(
                                            6,
                                            div(
                                              id = paste0(name, "_option_dataFormat"),
                                              radioButtons(
                                                inputId = paste0(name, "_option_dataFormat"),
                                                label = "Select Data Format",
                                                choiceNames = c("Arm Level", "Contrast Level"),
                                                choiceValues = c("arm", "contrast"),
                                                selected = character(0)
                                              )
                                            ),
                                            div(
                                              id = paste0(name, "_option_outcomeType"),
                                              radioButtons(
                                                inputId = paste0(name, "_option_outcomeType"),
                                                label = "Select Outcome Type",
                                                choiceNames = c("Binary", "Continuous", "Time to Event", "Survival"),
                                                choiceValues = c("binary", "continuous", "tte", "survival"),
                                                selected = character(0)
                                              )
                                            ),
                                            div(
                                              id = paste0(name, "_option_sm"),
                                              radioButtons(
                                                inputId = paste0(name, "_option_sm"),
                                                label = "Select Effect Size",
                                                choiceNames = c("HR", "IRR", "MD", "OR", "RD", "ROM", "RR", "SMD"),
                                                choiceValues = c("hr", "irr", "md", "or", "rd", "rom", "rr", "smd"),
                                                selected = character(0)
                                              )
                                            )
                                          ),
                                          column(
                                            6,
                                            div(
                                              id = paste0(name, "_option_smallValues"),
                                              radioButtons(
                                                inputId = paste0(name, "_option_smallValues"),
                                                label = "Select Small Values",
                                                choiceNames = c("Good", "Bad"),
                                                choiceValues = c("good", "bad"),
                                                selected = character(0)
                                              )
                                            ),
                                            div(
                                              id = paste0(name, "_option_model"),
                                              radioButtons(
                                                inputId = paste0(name, "_option_model"),
                                                label = "Select Model",
                                                choiceNames = c("Common effect", "Random effects"),
                                                choiceValues = c("common", "random"),
                                                selected = character(0)
                                              )
                                            ),
                                            div(
                                              id = paste0(name, "_option_methodTau"),
                                              radioButtons(
                                                inputId = paste0(name, "_option_methodTau"),
                                                label = "Select Method.tau",
                                                choiceNames = c("REML", "ML", "DL"),
                                                choiceValues = c("reml", "ml", "dl"),
                                                selected = character(0)
                                              )
                                            ),
                                            div(
                                              id = paste0(name, "_option_method"),
                                              radioButtons(
                                                inputId = paste0(name, "_option_method"),
                                                label = "Select Rank Statistic",
                                                choiceNames = c("SUCRA", "P-Score"),
                                                choiceValues = c("SUCRA", "P-score"),
                                                selected = character(0)
                                              )
                                            )
                                          )
                                        )))
    }
    updateTabsetPanel(inputId = "dynamicTabs", selected = rev(names(sheetList()))[1])
  })
  
  # Observe changes and update visibility/choices
  observe({
    shinyjs::disable("submit")
    # group options together and key by sheet name
    options <- groupValues(input, names(sheetList()))
    # loop through options and update ui as needed
    for (sheetName in names(options)) {
      for (option in names(visibilityUpdaters)) {
        visibilityUpdaters[[option]]$update(options, sheetName)
      }
    }
    # if there are no missing fields, enable submit button
    if (length(getMissingOptions(options)) == 0) {
      shinyjs::enable('submit')
    }
    
  })
  
  # on submit, we'll make sure that the sheets have the right columns
  observeEvent(input$submit, {
    
  })
  
}


### HELPERS

#' TODO: functions should be called with corresponding DFs b/c the main validation
#' will be involve ensuring that the dataframes have the correct columns for the specified
#' outcome type
validators <- list(
  dataFormat = list(
    key = "a",
    validate = function(allValues, sheetName) {
      NULL
    }
  ),
  outcomeType = list(
    key = "b",
    validate = function(allValues, sheetName) {
      if (allValues['a'] == "no" && allValues['b'] == "no") {
        "Error in field \"b\": A and B cannot both be 'no'!"
      }
    }
  ),
  effectSize = list(
    key = "c",
    validate = function(allValues, sheetName) {
      NULL
    }
  )
)

#' list of functions, one per possible input
#' each is in charge of showing/hiding itself and setting its value or options
#' according to 'upstream' selections
visibilityUpdaters <- list(
  outcomeType = list(
    update = function(allValues, sheetName) {
      id <- paste0(sheetName, "_option_outcomeType")
      dataFormat <- allValues[[sheetName]][['dataFormat']]
      outcomeType <- allValues[[sheetName]][['outcomeType']]
      #enable by default
      shinyjs::enable(selector = paste0("#", id))
      # if user has selected 'arm', apply constraints
      if (!is.null(dataFormat) && dataFormat == "arm") {
        disableOptions(id, "survival", selected = outcomeType)
      }
    }
  ),
  effectSize = list(
    update = function(allValues, sheetName) {
      id <- paste0(sheetName, "_option_sm")
      outcomeType = allValues[[sheetName]][['outcomeType']]
      selected = allValues[[sheetName]][['sm']]
      # always start by enabling everything...
      shinyjs::enable(selector = paste0("#", id))
      if (!is.null(outcomeType)) {
        if (outcomeType == 'binary') {
          disableOptions(id, c('smd', 'hr', 'md', 'rom', 'irr'), selected)
        } else if (outcomeType == 'continuous') {
          disableOptions(id, c('rr', 'hr', 'or', 'rd', 'irr'), selected)
        } else if (outcomeType == 'tte') {
          disableOptions(id, c('rr', 'hr', 'or', 'rd', 'rom', 'md', 'smd'), selected)
        } else if (outcomeType == 'survival') {
          disableOptions(id, c('rr', 'irr', 'or', 'rd', 'rom', 'md', 'smd'), selected)
        }
      }
    }
  ),
  methodTau = list(
    update = function(allValues, sheetName) {
      id <- paste0(sheetName, "_option_methodTau")
      model <- allValues[[sheetName]][['model']]
      selector <- paste0("#", id)
      # always start by enabling everything...
      shinyjs::show(selector = selector)
      if (!is.null(model)) {
        if (model == 'common') {
          updateRadioButtons(inputId = id, selected = character(0))
          shinyjs::hide(selector = selector)
        }
      }
    }
  )
)

#' Determine whether the given set of options is complete
#' if not complete, return a list of sheets and missing fields
#' @param optionSet list, the list of set options, keyed by sheet name
getMissingOptions <- function(optionSet) {
  missing <- list()
  options <-
    c("dataFormat",
      "methodTau",
      "method",
      "model",
      "outcomeType",
      "sm",
      "smallValues")
  
  for (sheetName in names(optionSet)) {
    for (i in 1:length(options)) {
      if (options[i] == 'methodTau' &&
          !is.null(optionSet[[sheetName]][['model']]) &&
          optionSet[[sheetName]][['model']] == 'common') {
        
      } else if (is.null(optionSet[[sheetName]][[options[i]]])) {
        if (is.null(missing[[sheetName]])) {
          missing[[sheetName]] <- list()
        }
        missing[[sheetName]][[options[i]]] <- TRUE
      }
    }
  }
  missing
}

#' @param id string, the id (not selector) of the radio group
#' @param values vector, the choices that are NOT allowed
#' @param selected string | NULL, the current selection value
#' @returns void
disableOptions <- function(id, values, selected) {
  # if selected value is in the disallowed values, set to null
  if (!is.null(selected) && selected %in% values) {
    updateRadioButtons(inputId = id, selected = character(0))
  }
  sapply(values, disableOption, id)
}

#' disable an option within a radio group
#' @param value string, the value of the option
#' @param id string, the id (not selector) of the option group
#' @returns void
disableOption <- function(value, id) {
  selector <- getRadioInputSelector(id, value)
  shinyjs::disable(selector = selector)
}

# return the selector for the radio option w/ given value and parent id
#' @param id string, the id (not selector) of the radio group
#' @param value string, the value of the option
#' @returns string
getRadioInputSelector <- function(id, value) {
  paste0("#", id, " [type=radio][value=", value, "]")
}

#' @param valueList the list of variables, probably reactiveValues
#' @param prefixVec vector of prefixes, probably list of sheets
#' @returns list
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
#' @returns list of dataframes
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