shiny::shinyUI(
  shiny::div(
    shiny::navbarPage(
      id = "navtabs",
      shiny::tags$head(
        shiny::tags$link(rel = "stylesheet", type = "text/css", href = "app.css"),
        shiny::tags$link(rel = "shortcut icon", href = "favicon.ico")
      ),
      title = "Rank-Heat Plot",
      shinyjs::useShinyjs(),
      shiny::tabPanel(title = "Get Started",
        shiny::fluidRow(
          shiny::column(3,
            shiny::fluidRow(
              shiny::column(12, shiny::h1("Rank-Heat Plot")),
              shiny::column(12, shiny::imageOutput("plotShot", height = NULL))
            )
          ),
          shiny::column(8, shiny::fluidRow(shiny::htmlOutput("about")))
        )
      ),
      shiny::tabPanel(
        title = "Upload Data",
        shiny::fluidRow(
          shiny::column(
            4,
            shiny::fluidRow(
              shiny::column(12,
                            uiOutput('file_input')),
              shiny::column(
                12,
                shiny::downloadButton("exampleFileDownload", label = "Download Example Dataset")
              ),
              shiny::column(
                12,
                shiny::p(
                  class = "citation",
                  "(Tricco, A.C., Ashoor, H.M., Soobiah, C., Rios, P., Veroniki, A.A., Hamid, J.S., Ivory, J.D., Khan, P.A., Yazdi, F., Ghassemi, M., Blondal, E., Ho, J.M., Ng, C.H., Hemmelgarn, B., Majumdar, S.R., Perrier, L. and Straus, S.E. (2018), Comparative Effectiveness and Safety of Cognitive Enhancers for Treating Alzheimer's Disease: Systematic Review and Network Metaanalysis. J Am Geriatr Soc, 66: 170-178. https://doi.org/10.1111/jgs.15069)"
                )
              )
            ),
            shiny::fluidRow(class="submit-buttons",
              shiny::column(
                12,
                p(class="small", strong("All options on the right must be selected for all sheets before submitting."))
              ),
              shiny::column(
                class = "mt-5",
                12,
                shiny::actionButton("submit", "Submit", class = "submit-button"),
                shiny::actionButton("startOver", "Start Over", class = "restart-button")
              )
            ),
            shiny::fluidRow(shiny::column(
              12,
              shiny::div(class = "mt-5 mb-5 alert-danger",
                  shiny::uiOutput("sheetValidationHeading"),
                  shiny::tags$ul(shiny::uiOutput("sheetValidationMsg"))
                )
              )
            ),
            shiny::fluidRow(class = "treatment-list",
              shiny::column(
                9, class = "mt-5 mb-5",
                shiny::uiOutput('treatmentList')
              )
            )
          ),
          shiny::column(
            8,
            class = "dynamic-tabs-container",
            shiny::fluidRow(
              shiny::column(12,
                shiny::div(
                  shiny::tabsetPanel(
                    id = "dynamicTabs",
                    header = shiny::div(
                      shiny::p(
                        class = "tab-heading",
                        em(strong("Use the controls below to configure the analysis for this sheet."))
                      ),
                      shiny::h4(actionLink(inputId = "useAll", label = "Apply these settings to all sheets")),
                    )
                  )
                )
              )
            )
          )
        )
      ),
      shiny::tabPanel(
        title = "View Plot",
        value = "viewPlot",
        shiny::fluidRow(
          class = "plot-panel",
          shiny::column(3,
            shiny::fluidRow(
              shiny::column( 12, class = "mt-5 mb-5 display-controls",
                shiny::div(
                  shiny::p(
                    shiny::em("Use the controls below to adjust the label fonts in the graphic")
                  ),
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
          shiny::column(9,
            shiny::fluidRow(
              shiny::column(12, shiny::downloadButton("heatmapDownload")),
              shiny::fluidRow(
                shiny::column(12,
                  shiny::plotOutput("heatmap", width = "100%", height = "700px")
                )
              ),
              shiny::fluidRow(
                shiny::column(
                  width = 12,
                  class = "viewer-data-table-row",
                  shiny::tableOutput("dataTable")
                )
              )
            ) # end outer fluid row in rhs column
          ) # end rhs column
        ) # end outer plot page fluid row
      ), # end outer plot page tab panel
      shiny::tabPanel(
        title = "Tutorial and FAQ",
        value = "tutorial_faq",
        shiny::fluidRow(
          class = "tutoria-faq-panel",
          shiny::column(12,
            shiny::h1("Video Tutorial"),
            shiny::p("The video below provides a step-by-step demonstration of
                     how to use the Rank-Heat Plot with example data."),
            shiny::tags$iframe(width="642",
                               height="300",
                               src="https://www.youtube.com/embed/uKVedOjznYc",
                               title="Rankheatplot",
                               frameborder="0",
                               allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share",
                               allowfullscreen="allowfullscreen"
                              )
          ),
          shiny::column(12,
            shiny::h1("FAQ"),
            shiny::p("Coming soon...")
          )
        ) # end fluid row
      ) # end FAQ tab panel
    ), # end navbar page
    shiny::div(
      class = "footer",
      paste0(
        "Copyright ",
        substr(date(), nchar(date()) - 3, nchar(date())),
        " Knowledge Translation Program"
      )
    )
  ) # end outer div
)
