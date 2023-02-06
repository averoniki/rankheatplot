library(shiny)
library(rmarkdown)
library(shinybusy)
library(shinyjs)
library(stringr)
library(readxl)
source("/home/rstudio/src/scripts/rankHeatCircos.R")

ui <-
  shinyUI(navbarPage(
    tags$head(tags$link(
      rel = "stylesheet", type = "text/css", href = "app.css"
    )),
    title = "RankHeat Plot",
    useShinyjs(),
    tabPanel(
      title = "Run",
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
               )), fluidRow(column(
                 12, div(
                   class = "mt-5 mb-5 alert-danger",
                   uiOutput("sheetValidationHeading")
                   ,
                   tags$ul(uiOutput("sheetValidationMsg"))
                 )
               ))),
        column(8,
               fluidRow(column(
                 12,
                 shiny::tabsetPanel(id = "dynamicTabs")
               )))
      ),
      fluidRow(tags$hr()),
      fluidRow(column(
        6,
        plotOutput("heatmap", width = "100%", height = "700px")
      ),
      column(6,
             tableOutput("dataTable")))
    ),
    tabPanel(title = "About",
             fluidRow(htmlOutput("about"))),
  ))

server <- function(input, output) {
  output$about <- renderUI({
    includeHTML(path = "about.html")
  })
  
  userData <- reactive(input$userData)
  
  sheetList <- reactive({
    # reset ui on upload
    output$sheetValidationHeading <- NULL
    output$sheetValidationMsg <- NULL
    output$dataTable <- NULL
    output$heatmap <- NULL
    if (!is.null(userData())) {
      extractSheets(userData()$datapath)
    }
  })
  
  # BUILDING THE FORM
  
  #' This will create the UI with an identical tab for each uploaded sheet
  observe({
    shinyjs::runjs("$('#dynamicTabs li').remove()")
    for (name in names(sheetList())) {
      shiny::prependTab(inputId = "dynamicTabs",
                        shiny::tabPanel(
                          title = name,
                          class = "mt-5",
                          fluidRow(
                            column(
                              4,
                              radioButtons(
                                inputId = paste0(name, "_option_dataFormat"),
                                label = "Select Data Format",
                                choiceNames = c("Arm Level", "Contrast Level"),
                                choiceValues = c("arm", "contrast"),
                                selected = character(0)
                              ),
                              radioButtons(
                                inputId = paste0(name, "_option_outcomeType"),
                                label = "Select Outcome Type",
                                choiceNames = c("Binary", "Continuous", "Time to Event", "Survival"),
                                choiceValues = c("binary", "continuous", "tte", "survival"),
                                selected = character(0)
                              ),
                            ),
                            column(
                              4,
                              radioButtons(
                                inputId = paste0(name, "_option_sm"),
                                label = "Select Effect Size",
                                choiceNames = c("OR", "RR", "RD", "MD", "SMD", "ROM", "IRR", "HR"),
                                choiceValues = c("or", "rr", "rd", "md", "smd", "rom", "irr", "hr"),
                                selected = character(0)
                              ),
                              radioButtons(
                                inputId = paste0(name, "_option_smallValues"),
                                label = "Select Small Values",
                                choiceNames = c("Good", "Bad"),
                                choiceValues = c("good", "bad"),
                                selected = character(0)
                              ),
                            ),
                            column(
                              4,
                              radioButtons(
                                inputId = paste0(name, "_option_model"),
                                label = "Select Model",
                                choiceNames = c("Common effect", "Random effects"),
                                choiceValues = c("common", "random"),
                                selected = character(0)
                              ),
                              radioButtons(
                                inputId = paste0(name, "_option_methodTau"),
                                label = "Select Method.tau",
                                choiceNames = c("REML", "ML", "DL"),
                                choiceValues = c("reml", "ml", "dl"),
                                selected = character(0)
                              ),
                              radioButtons(
                                inputId = paste0(name, "_option_method"),
                                label = "Select Rank Statistic",
                                choiceNames = c("SUCRA", "P-Score"),
                                choiceValues = c("SUCRA", "P-score"),
                                selected = character(0)
                                ,
                              )
                            )
                          )
                        ))
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
    options <- groupValues(input, names(sheetList()))
    invalid <- validateSheets(options, sheetList())
    if (length(invalid)) {
      msg <- list()
      for (sheetName in names(invalid)) {
        msg[[sheetName]] <- tags$li(paste0(
          str_to_upper(sheetName),
          ": ",
          paste0(invalid[[sheetName]], collapse = ", "),
          "\n"
        ))
      }
      output$sheetValidationHeading <-
        renderText("The following sheets have errors:")
      output$sheetValidationMsg <- renderUI(msg)
    }  else {
      output$sheetValidationHeading <- NULL
      output$sheetValidationMsg <- renderUI(NULL)
      
      tryCatch({
        show_modal_spinner()
        submitAnalysis(options, sheetList(), output)
      }, finally = {
        remove_modal_spinner()
      })
    }
  })
  
}

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

#' validate that sheets have the appropriate columns for the options selected
#' @param optionSet list, list of selected options, keyed by sheet name
#' @param sheets list, list of dataframes, keyed by sheet name
#' @returns list, list of errors, possibly empty. Each sheet that has an error
#' will have an entry in the list, keyed by sheet name, with the string message as content.
validateSheets <- function(optionSet, sheets) {
  results <- list()
  for (sheetName in names(sheets)) {
    errors <-
      validateSheet(optionSet[[sheetName]], sheets[[sheetName]])
    if (length(errors)) {
      results[[sheetName]] <- errors
    }
  }
  results
}

#' Confirm that the sheet has the appropriate columns for the options selected
#' @param options list, list of selected options
#' @param sheet dataframe, a user-submitted sheet
#' @returns vector, a vector of the missing field names
validateSheet <- function(options, sheet) {
  # if dataType is arm, then we have to be sure they have the right columns for
  # the outcomeType they selected, as this will be passed on to pairwise
  # we also need to confirm, for all sheets, that the base columns are present
  
  results <- list()
  required <- list("study_id" = list())
  
  if (options[['dataFormat']] == "arm") {
    required <- c(required, list("t" = list()))
    
    if (options[['outcomeType']] == "binary") {
      tm <- list(n = list(type = "integer", gt = "0"),
                 r = list(type = "integer", gt = "e0"))
      results <- checkTypeAndExistence(sheet, c(required, tm))
    }
    if (options[['outcomeType']] == "continuous") {
      tm <- list(
        n = list(type = "integer", gt = "0"),
        sd = list(type = "numeric", gt = "0"),
        m = list(type = "numeric")
      )
      results <- checkTypeAndExistence(sheet, c(required, tm))
    }
    if (options[['outcomeType']] == "tte") {
      tm <- list(
        d = list(type = "integer", gt = "e0"),
        time = list(type = "numeric", gt = "0")
      )
      results <- checkTypeAndExistence(sheet, c(required, tm))
    }
  } else {
    tm <- list(
      TE = list(type = "numeric"),
      seTE = list(type = "numeric", gt = "0"),
      treat1 = list(),
      treat2 = list()
    )
    results <- checkTypeAndExistence(sheet, c(required, tm))
  }
  
  results
}

#' @param cols data.frame, a subset of relevant columns
#' @param typeMap list, keyed by (expected) colname in df, with optional keys 'type' and 'gt'
#' @returns list, keyed by sheet with error message or empty if no errors
checkTypeAndExistence <- function(df, typeMap) {
  errors <- list()
  for (fieldName in names(typeMap)) {
    # first make sure column exists
    if (fieldName %in% names(df)) {
      # then check for NAs
      if (length(df[[fieldName]][!is.na(df[[fieldName]])]) < length(df[[fieldName]])) {
        errors[[fieldName]] <- paste0(fieldName, ' has empty values')
      }
      # then check the type
      else {
        if (!is.null(typeMap[[fieldName]]$type)) {
          type <- typeMap[[fieldName]]$type
          if (type == "integer") {
            if (!all(compareNA(df[[fieldName]], as.integer(df[[fieldName]])))) {
              errors[[fieldName]] <- paste0(fieldName, " must be an integer")
            }
          } else if (type == "numeric") {
            if (!all(compareNA(df[[fieldName]], as.numeric(df[[fieldName]])))) {
              errors[[fieldName]] <- paste0(fieldName, " must be a number")
            }
          }
        }
        if (!length(errors)) {
          if (!is.null(typeMap[[fieldName]]$gt)) {
            # then check values
            if (typeMap[[fieldName]]$gt == "0" &&
                !all(df[[fieldName]] > 0)) {
              errors[[fieldName]] <- paste0(fieldName, " must be greater than 0")
            }
            if (typeMap[[fieldName]]$gt == "e0" &&
                !all(df[[fieldName]] >= 0)) {
              errors[[fieldName]] <-
                paste0(fieldName, " must be greater than or equal to 0")
            }
          }
        }
      }
    } else {
      errors[[fieldName]] <- paste0(fieldName, " is required")
    }
  }
  errors
}

#' Determine whether the given set of options is complete
#' @param optionSet list, the list of set options, keyed by sheet name
#' @returns list, a list of missing fields, keyed by sheet name
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
    prefix <- str_remove(key, "_option.+")
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

submitAnalysis <- function(options, sheets, output) {
  results <- list()
  
  for (sheetName in names(sheets)) {
    df <- sheets[[sheetName]]
    opts <- options[[sheetName]]
    if (opts$dataFormat == "arm") {
      transformed <- armToContrast(df, opts)
    } else {
      transformed <- df
    }
    
    netmetaArgs <-
      list(
        data = transformed,
        sm = opts$sm,
        common = opts$model == "common",
        random = opts$model != "common",
        TE = transformed[['TE']],
        seTE = transformed[['seTE']],
        studlab = transformed[['studlab']],
        treat1 = transformed[['treat1']],
        treat2 = transformed[['treat2']]
      )
    
    if (opts$model == "common") {
      netmetaArgs$method.tau <- opts$methodTau
    }
    
    netmetaRes <- do.call(netmeta, netmetaArgs)
    
    cons <-
      netconnection(netmetaRes[['treat1']], netmetaRes[['treat2']], netmetaRes[['studlab']], data = netmetaRes)
    if (cons$n.subnets > 1) {
      # TODO: this needs proper handling
      stop("User should update their data!")
    }
    
    ranked <- netrank(
      x = netmetaRes,
      method = opts$method,
      small.values = opts$smallValues,
      common = opts$model == "common",
      random = opts$model != "common"
    )
    
    if (opts$model == "common") {
      ranking = ranked$ranking.common
    } else {
      ranking = ranked$ranking.random
    }
    
    results[[sheetName]] <- rankToDf(ranking, sheetName)
    
  }
  
  allResults <-
    Reduce(function(a, b)
      merge(a, b, by = "Treatment", all = TRUE),  results)
  
  formatted <- rhp.prepData(allResults)
  
  output$heatmap <- renderPlot({
    rhp.rankheatplotCircos(formatted)
  })
  
  output$dataTable <- renderTable(formatted, rownames = T)
  
  
}

#' @param ranking named vector, typically retrieved from netrank() result as
#' result$ranking.common or result$ranking.random
#' @param treament string
rankToDf <- function(ranking, outcome) {
  df <- data.frame(names(ranking), as.numeric(ranking))
  names(df) <- c("Treatment", outcome)
  df
}

#' transform arm data to contrast data if needed
#' @param option list, a list of arguments selected in the ui
#' @param sheet df, a dataframe derived from a sheet of the uploaded excel book
armToContrast <- function(sheet, options) {
  # set globally require args
  args <- list(studlab = sheet[['study_id']], treat = sheet[['t']])
  
  if (options[['outcomeType']] == "binary") {
    args$n <- sheet[['n']]
    args$event <- sheet[['r']]
    args$data <- sheet
    args$sm <- options$ms
  }
  if (options[['outcomeType']] == "continuous") {
    args$n <- sheet[['n']]
    args$m <- sheet[['m']]
    args$sd <- sheet[['sd']]
  }
  if (options[['outcomeType']] == "tte") {
    args$d <- sheet[['d']]
    args$t <- sheet[['t']]
    args$time <- sheet[['time']]
  }
  
  do.call(pairwise, args = args, quote = F)
  
}

# This function returns TRUE wherever elements are the same, including NA's,
# and FALSE everywhere else.
# http://www.cookbook-r.com/Manipulating_data/Comparing_vectors_or_factors_with_NA/
compareNA <- function(v1, v2) {
  same <- (v1 == v2) | (is.na(v1) & is.na(v2))
  same[is.na(same)] <- FALSE
  return(same)
}

# Run the application
shinyApp(ui = ui, server = server)
