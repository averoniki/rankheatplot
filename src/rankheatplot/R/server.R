env = Sys.getenv()
projectRoot = ifelse(!is.na(env['PROJECT_ROOT']), env['PROJECT_ROOT'], getwd())
source(file.path(projectRoot, 'rankHeatCircos.R'))

shiny::shinyServer(function(input, output, session) {
  output$about <- renderUI({
    includeHTML(path = file.path(projectRoot, "about.html"))
  })

  output$plotShot <- renderImage({
    list(src = file.path(projectRoot,
                         "www",
                         "plot-shot.png"))
  }, deleteFile = F)

  # define this server-side so we can have a dependency on startOver button
  # so that it resets properly
  output$file_input <- renderUI({
    input$startOver
    shiny::fileInput(
      accept = c(
        "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
        "application/vnd.ms-excel"
      ),
      inputId = "userData",
      label = shiny::div(
        shiny::h3("Upload an .xlsx file"),
        shiny::p(
          "Each sheet should contain data for a single outcome. For more information, see the About Page."
        )
      )
    )
  })

  output$exampleFileDownload <- downloadHandler(
    filename = "AD-data.xlsx",
    content = function(file) {
      file.copy(file.path(projectRoot,
                          "www",
                          "safety-ad-data-rtc.xlsx"),
                file)
    }
  )

  # we'll make the user explicitly start over
  # so we can cleanly remove tabs w/o timing issues from relying on sheetList changes
  observeEvent(input$startOver, {
    unsetLoadedUI()
    if (!is.null(sheetList())) {
      for (tab in names(sheetList())) {
        removeTab('dynamicTabs', target = tab)
      }
      output$sheetValidationHeading <- NULL
      output$sheetValidationMsg <- NULL
      output$dataTable <- NULL
      output$heatmap <- NULL
      hideDisplayControls()
    }
  })

  # dependency on userData causes this to fire with every upload
  sheetList <- reactive({
    if (!is.null(input$userData)) {
      setLoadedUI()
      extractSheets(input$userData$datapath)
    } else {
      unsetLoadedUI()
      NULL
    }
  })

  output$treatmentList <- bindEvent(shiny::renderUI({
    req(sheetList())
    tl <- getTreatmentList(sheetList())
    sortable::rank_list(input_id = "treatmentList",
                        labels = tl,
                        text = "Drag the labels below to order the treatments in the plot")
  }), sheetList(), ignoreNULL = T)

  # BUILDING THE FORM
  # This will create the UI with an identical tab for each uploaded sheet
  observeEvent(sheetList(), {
    # https://community.rstudio.com/t/how-do-i-use-for-loop-in-rendering-outputs/35761/2
    lapply(names(sheetList()), function(name) {
      output[[paste0(name, 'Table')]] <-
        renderDataTable(sheetList()[[name]], options = list(pageLength = 10))
    })

    for (name in names(sheetList())) {
      shiny::prependTab(
        inputId = "dynamicTabs",
        shiny::tabPanel(
          title = name,
          class = "mt-5",
          fluidRow(
            column(
              4,
              radioButtons(
                inputId = paste0(name, "_option_dataFormat"),
                inline = T,
                label = "Select Data Format",
                choiceNames = c("Arm Level", "Contrast Level"),
                choiceValues = c("arm", "contrast"),
                selected = character(0)
              ),
              radioButtons(
                inputId = paste0(name, "_option_outcomeType"),
                inline = T,
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
                inline = T,
                label = "Select Effect Size",
                choiceNames = c("OR", "RR", "RD", "MD", "SMD", "ROM", "IRR", "HR"),
                choiceValues = c("or", "rr", "rd", "md", "smd", "rom", "irr", "hr"),
                selected = character(0)
              ),
              radioButtons(
                inputId = paste0(name, "_option_smallValues"),
                inline = T,
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
                inline = T,
                label = "Select Model",
                choiceNames = c("Common effect", "Random effects"),
                choiceValues = c("common", "random"),
                selected = character(0)
              ),
              radioButtons(
                inputId = paste0(name, "_option_methodTau"),
                inline = T,
                label = "Select Method.tau",
                choiceNames = c("REML", "ML", "DL"),
                choiceValues = c("reml", "ml", "dl"),
                selected = character(0)
              ),
              radioButtons(
                inputId = paste0(name, "_option_method"),
                inline = T,
                label = "Select Rank Statistic",
                choiceNames = c("SUCRA", "P-Score"),
                choiceValues = c("SUCRA", "P-score"),
                selected = character(0)
              )
            )
          ),
          shiny::fluidRow(shiny::column(
            12,
            shiny::dataTableOutput(outputId = paste0(name, 'Table'))
          ),)
        )
      )
    }
    updateTabsetPanel(inputId = "dynamicTabs", selected = rev(names(sheetList()))[1])

  })

  # Observe *all* changes to input and update visibility/choices
  observe({
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

  observeEvent(input$useAll, {
    options <- groupValues(input, names(isolate(sheetList())))

    selected = options[[input$dynamicTabs]]

    for (sheetName in names(isolate(sheetList()))) {
      for (option in names(selected)) {
        updateRadioButtons(
          inputId = paste0(sheetName, "_option_", option),
          selected = paste0(selected[[option]])
        )
      }
    }
  })

  # bind formatted data object to submit button
  formattedValues <- bindEvent(reactive({
    options <-
      groupValues(input, names(sheetList()))
    invalid <-
      validateSheets(options, sheetList())
    if (length(invalid)) {
      createErrorDisplayList(errorList = invalid, output = output)
      NULL
    }  else {
      output$sheetValidationHeading <- NULL
      output$sheetValidationMsg <-
        renderUI(NULL)

      tryCatch({
        shinybusy::show_modal_spinner()
        formatted <-
          getFormattedData(options, sheetList(), input)
        formatted <- formatted[input$treatmentList, , drop = F]

        shinybusy::remove_modal_spinner()
        # check if we got an error list back
        if (!is.data.frame(formatted)) {
          createErrorDisplayList(formatted, ouput)
          NULL
        } else {
          formatted
        }
      }, error = {
        shinybusy::remove_modal_spinner()
        NULL
      })
    }
  }), input$submit)

  # render plot if formattedValues are valid
  observe({
    if (!is.null(formattedValues())) {
      output$heatmap <- renderPlot({
        rhp.rankheatplotCircos(
          formattedValues(),
          cexLabel = input$cexLabel,
          cexValue = input$cexValue,
          cexSector = input$cexSector
        )
      })
      output$heatmapDownload <- downloadHandler(
        filename = function() {
          "rankheat.png"
        },
        content = function(file) {
          png(file) # open the png device
          rhp.rankheatplotCircos(
            formattedValues(),
            cexLabel = input$cexLabel,
            cexValue = input$cexValue,
            cexSector = input$cexSector
          )
          dev.off()  # turn the device off
        }
      )
      output$dataTable <-
        renderTable(formattedValues(), rownames = T)
      showDisplayControls()
      updateTabsetPanel(session, inputId = "navtabs", selected = "viewPlot")
    } else {
      hideDisplayControls()
    }
  })
})

# put UI in state for selecting options
setLoadedUI <- function() {
  shinyjs::disable('userData')
  shinyjs::show('startOver')
  shinyjs::show(selector = ".dynamic-tabs-container")
  shinyjs::show('submit')
  shinyjs::show('treatmentList')
  shinyjs::disable('submit')
}

# put UI in state for uploading data
unsetLoadedUI <- function() {
  shinyjs::enable('userData')
  shinyjs::hide('startOver')
  shinyjs::hide('submit')
  shinyjs::hide("treatmentList")
  shinyjs::hide(selector = ".dynamic-tabs-container")
}


#' @param errorList list, a list of error messages, typically keyed by sheet, errors can be a vector of message
#' @param output reactiveValues, the output to update
#' @return void
createErrorDisplayList <- function(errorList, output) {
  msg <- list()
  for (sheetName in names(errorList)) {
    msg[[sheetName]] <- tags$li(paste0(
      stringr::str_to_upper(sheetName),
      ": ",
      paste0(errorList[[sheetName]], collapse = ", "),
      "\n"
    ))
  }
  output$sheetValidationHeading <-
    renderText("The following sheets have errors:")
  output$sheetValidationMsg <-
    renderUI(msg)
}

hideDisplayControls <- function() {
  shinyjs::hide(selector = ".plot-panel")
}

showDisplayControls <- function() {
  shinyjs::show(selector = ".plot-panel")
}

#' @param sheetList, list, list of dataframes, keyed by outcome
#' @return vector, vector of unique treatments
getTreatmentList <- function(sheetList) {
  # sheet may not be validated yet, so can't assume `t` column exists
  ret <- c()
  for (i in 1:length(sheetList)) {
    if ('t' %in% names(sheetList[[i]])) {
      ret <- c(ret, sheetList[[i]][['t']])
    }
  }

  unique(ret)
}

# list of functions, one per possible input
# each is in charge of showing/hiding itself and setting its value or options
# according to 'upstream' selections
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
          disableOptions(id,
                         c('rr', 'hr', 'or', 'rd', 'rom', 'md', 'smd'),
                         selected)
        } else if (outcomeType == 'survival') {
          disableOptions(id,
                         c('rr', 'irr', 'or', 'rd', 'rom', 'md', 'smd'),
                         selected)
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

#' Validate that sheets have the appropriate columns for the options selected
#' @param optionSet list, list of selected options, keyed by sheet name
#' @param sheets list, list of dataframes, keyed by sheet name
#' @return list, list of errors, possibly empty. Each sheet that has an error
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

#' Confirm that the sheet has the appropriate columns, types, and contents
#' for the options selected
#' @param options list, list of selected options
#' @param sheet dataframe, a user-submitted sheet
#' @return list, a list of errors keyed by field name
validateSheet <- function(options, sheet) {
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
    # ensure that we have at least 2 unique study_ids, but ignore if there's
    # already an error there
    if (!"study_id" %in% names(results)) {
      if (length(unique(sheet[['study_id']])) < 2) {
        results[['study_id']] <-
          "study_id field must contain at least two unique values"
      }
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
#' @return list, keyed by field name with error message or empty if no errors
checkTypeAndExistence <- function(df, typeMap) {
  errors <- list()
  for (fieldName in names(typeMap)) {
    # first make sure column exists
    if (fieldName %in% names(df)) {
      # then check for NAs
      if (length(df[[fieldName]][!is.na(df[[fieldName]])]) < length(df[[fieldName]])) {
        errors[[fieldName]] <- paste0(fieldName, ' has empty values')
      }
      # if existence check passes, check the type
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
#' @return list, a list of missing fields, keyed by sheet name
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
#' @return void
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
#' @return string
getRadioInputSelector <- function(id, value) {
  paste0("#", id, " [type=radio][value=", value, "]")
}

#' @param valueList the list of variables, probably reactiveValues
#' @param prefixVec vector of prefixes, probably list of sheets
#' @return list
groupValues <- function(valueList, prefixVec) {
  ret <- list()
  for (prefix in prefixVec) {
    ret[[prefix]] = list()
  }

  for (key in names(valueList)) {
    # first, does the key contain the {prefixVec}_option_ string? if so, extract and save value
    prefix <- stringr::str_remove(key, "_option.+")
    # first, check if key is in prefix list by stripping everything else
    if (prefix %in% prefixVec &&
        stringr::str_detect(key, paste0(prefix, "_option_"))) {
      optionName <- stringr::str_remove(key, paste0(prefix, "_option_"))
      ret[[prefix]][[optionName]] <- valueList[[key]]
    }
  }
  ret
}

#' @param pth path to excel sheet
#' @returns list of dataframes
extractSheets <- function(pth) {
  sheets <- readxl::excel_sheets(pth)
  output <- list()
  for (i in 1:length(sheets)) {
    df <- readxl::read_excel(path = pth, sheet = sheets[i])
    names(df) <- stringr::str_trim(names(df))
    output[[sheets[i]]] <- df
  }
  output
}

#' transform arm data to contrast data if needed
#' @param option list, a list of arguments selected in the ui
#' @param sheet df, a dataframe derived from a sheet of the uploaded excel book
armToContrast <- function(sheet, options) {
  # set globally require args
  args <-
    list(studlab = sheet[['study_id']], treat = sheet[['t']])

  if (options[['outcomeType']] == "binary") {
    args$n <- sheet[['n']]
    args$event <- sheet[['r']]
    args$data <- sheet
    args$sm <- options$sm
  }
  if (options[['outcomeType']] == "continuous") {
    args$n <- sheet[['n']]
    args$m <- sheet[['m']]
    args$sd <- sheet[['sd']]
  }
  if (options[['outcomeType']] == "tte") {
    args$event <- sheet[['d']]
    args$time <- sheet[['time']]
    args$data <- sheet
    args$sm <- options$sm
  }

  do.call(netmeta::pairwise, args = args, quote = F)

}


#' Run netmeta function with user values converting arm to contrast
#' @param options list, user-provided arguments to pass to netmeta
#' @param sheet dataframe, data from a user sheet
#' @return dataframe, dataframe that can be passed to rank heat plot function
getNetmeta <- function(options, sheet) {
  if (options$dataFormat == "arm") {
    transformed <- armToContrast(sheet, options)
  } else {
    transformed <- sheet
  }

  netmetaArgs <-
    list(
      data = transformed,
      sm = options$sm,
      common = options$model == "common",
      random = options$model != "common",
      TE = transformed[['TE']],
      seTE = transformed[['seTE']],
      studlab = transformed[['studlab']],
      treat1 = transformed[['treat1']],
      treat2 = transformed[['treat2']]
    )

  if (options$model == "common") {
    netmetaArgs$method.tau <- options$methodTau
  }

  do.call(netmeta::netmeta, netmetaArgs)

}

#' get rankings
#' @param options list, user-provided arguments to pass to netmeta
#' @param netmetaRes netmeta object, results of netmeta function
#' @param sheetName char, the name of the sheet
#' @return dataframe, dataframe that can be passed to rank heat plot function
getRanks <- function(options, netmetaRes, sheetName) {
  ranked <- netmeta::netrank(
    x = netmetaRes,
    method = options$method,
    small.values = options$smallValues,
    common = options$model == "common",
    random = options$model != "common"
  )

  if (options$model == "common") {
    ranking = ranked$ranking.common
  } else {
    ranking = ranked$ranking.random
  }

  rankToDf(ranking, sheetName)
}

#' format data to be passed to plotting function
#' @param options list, list keyed by sheet of arguments to pass to functions
#' @param sheets list, list keyed by sheet name of user-supplied dataframes
#' @param input reactiveValues, the list of inputs, which will contain formatting values
#' @return dataframe, dataframe that can be passed to rank heat plot function if successful,
#' else a list of fields that failed the connectivity test
getFormattedData <- function(options, sheets, input) {
  errors = list()
  # before passing to mapply, we have to make sure lists are in the same order
  opts <- options[names(sheets)]
  netmetaRes <-
    mapply(getNetmeta,
           opts,
           sheets,
           USE.NAMES = T,
           SIMPLIFY = F)

  for (res in names(netmetaRes)) {
    cons <-
      netmeta::netconnection(netmetaRes[[res]][['treat1']],
                             netmetaRes[[res]][['treat2']],
                             netmetaRes[[res]][['studlab']],
                             data = as.data.frame(netmetaRes[[res]]))
    if (cons$n.subnets > 1) {
      errors[[res]] <- paste0("Data in ", res, " is unconnected!")
    }
  }

  if (length(errors)) {
    errors
  } else {
    rankRes <-
      mapply(
        getRanks,
        opts,
        netmetaRes,
        names(sheets),
        USE.NAMES = T,
        SIMPLIFY = F
      )

    allResults <-
      Reduce(function(a, b)
        merge(a, b, by = "Treatment", all = TRUE),  rankRes)

    rhp.prepData(allResults)
  }

}

#' @param ranking named vector, typically retrieved from netrank() result as
#' result$ranking.common or result$ranking.random
#' @param treament string
#' @return dataframe
rankToDf <- function(ranking, outcome) {
  df <- data.frame(names(ranking), as.numeric(ranking))
  names(df) <- c("Treatment", outcome)
  df
}


# This function returns TRUE wherever elements are the same, including NA's,
# and FALSE everywhere else.
# http://www.cookbook-r.com/Manipulating_data/Comparing_vectors_or_factors_with_NA/
compareNA <- function(v1, v2) {
  same <- (v1 == v2) | (is.na(v1) & is.na(v2))
  same[is.na(same)] <- FALSE
  return(same)
}
