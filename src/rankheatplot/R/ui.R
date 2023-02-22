shiny::shinyUI(
  shiny::navbarPage(
    shiny::tags$head(
      shiny::tags$link(rel = "stylesheet", type = "text/css", href = "app.css"),
      shiny::tags$link(rel = "shortcut icon", href = "favicon.ico")
    ),
    title = "RankHeat Plot",
    shinyjs::useShinyjs(),
    shiny::tabPanel(
      title = "Run",
      shiny::fluidRow(
        shiny::column(
          4,
          shiny::fluidRow(shiny::column(
            12,
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
                ),
              )
            )
          )),
          shiny::fluidRow(
            shiny::column(
              class = "mt-5",
              12,
              shiny::actionButton("submit", "Submit", class = "submit-button"),
              shiny::actionButton("startOver", "Start Over", class = "restart-button")
            )
          ),
          shiny::fluidRow(shiny::column(
            12,
            shiny::div(
              class = "mt-5 mb-5 alert-danger",
              shiny::uiOutput("sheetValidationHeading")
              ,
              shiny::tags$ul(shiny::uiOutput("sheetValidationMsg"))
            )
          )),
          shiny::fluidRow(
            shiny::column(
              12,
              class = "mt-5 mb-5 display-controls",
              shiny::div(
                shiny::p(shiny::em("Use the controls below to adjust the label fonts in the graphic")),
                shiny::div(
                  shiny::sliderInput(
                    "cexValue",
                    "Value Label Size:",
                    min = .1,
                    max = 2,
                    value = .75
                  ),
                ),
                shiny::div(
                  shiny::sliderInput(
                    "cexLabel",
                    "Outcome Label Size:",
                    min = .1,
                    max = 2,
                    value = .65
                  ),
                ),
                shiny::div(
                  shiny::sliderInput(
                    "cexSector",
                    "Treatment Label Size:",
                    min = .1,
                    max = 2,
                    value = 1
                  ),
                )
              )
            )
          )
        ),
        shiny::column(8,
                      shiny::fluidRow(shiny::column(
                        12,
                        shiny::tabsetPanel(
                          id = "dynamicTabs",
                          header = shiny::p(
                            class = "tab-heading",
                            em("Use the controls below to configure the analysis for this sheet.")
                          )
                        )
                      )))
      ),
      shiny::fluidRow(shiny::tags$hr()),
      shiny::div(
        class = "results-area",
        shiny::fluidRow(shiny::column(
          12,
          shiny::downloadButton("heatmapDownload")
        ),),
        shiny::fluidRow(
          shiny::column(6,
                        shiny::plotOutput(
                          "heatmap", width = "100%", height = "700px"
                        )),
          shiny::column(6,
                        shiny::tableOutput("dataTable"))
        )
      )
    ),
    shiny::tabPanel(title = "About",
                    shiny::fluidRow(shiny::htmlOutput("about"))),
  )
)
