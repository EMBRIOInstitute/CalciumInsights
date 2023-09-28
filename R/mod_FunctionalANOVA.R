#' FunctionalANOVA UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_FunctionalANOVA_ui <- function(id){
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(width = 4,
                   fileInput(ns("data_calcium"),
                             accept = c('text/csv',
                                        'text/comma-separated-values,text/plain',
                                        '.csv'),
                             label = h5("Dataset")),
                   numericInput(ns("fac"),label = "¿How many layers of cells does the experiment have?", value = 3),
                   textInput(ns("niv"),label = "¿How many cells does each layer have? ('Enter a vector comma delimited')", value = "3,5,5"),
                   ),
      mainPanel(
        tabsetPanel(
          type = "tabs",
          tabPanel("SummaryData",
                   DT::DTOutput(ns("Summaryfunctional")),
                   DT::DTOutput(ns("datafunctional"))
          ),
          tabPanel("Descriptive statistics",
                   plotOutput(ns("graph4")),
                   plotOutput(ns("m4"))
                   )

        )
      )

    )

  )
}

#' FunctionalANOVA Server Functions
#'
#' @noRd
mod_FunctionalANOVA_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    filedata <- reactive({
      req(input$data_calcium)
      fileInput <- load_file(input$data_calcium$name, input$data_calcium$datapath)
      fileInput <- as.data.frame(fileInput)
      return(list(fileInput = fileInput))
    })




    data_info <- reactive({
      req(filedata()$fileInput)
      Nobservations <- nrow(filedata()$fileInput)
      Ncells <- ncol(filedata()$fileInput)-1
      SummaryData <- data.frame(list(Number = c(Ncells, Nobservations)))
      rownames(SummaryData) <- c("cells", "Time observations")
      list(SummaryData = SummaryData, data = data.frame(filedata()$fileInput,row.names = NULL))
    })

    ####Output normal
    output$datafunctional <- DT::renderDataTable({
      df <- data_info()$data
      DT::datatable(df)
    })

    output$Summaryfunctional <- DT::renderDataTable({
      df <- data_info()$SummaryData
      DT::datatable(df, options = list(
        pagingType = 'simple',
        dom = 't'
      ))
    })

    descriptive_info <- reactive({
      m4 <- model_functional(data = data_info()$data, fac = input$fac, niv = input$niv)$m4
      g5 <- model_functional(data = data_info()$data, fac = input$fac, niv = input$niv)$g5
      return(list(m4 = m4, g5 = g5))
    })

    output$m4 <- renderPlot({
      descriptive_info()$m4
    })

    output$graph4 <- renderPlot({
      descriptive_info()$g5
    })


  })
}

## To be copied in the UI
# mod_FunctionalANOVA_ui("FunctionalANOVA_1")

## To be copied in the server
# mod_FunctionalANOVA_server("FunctionalANOVA_1")
