#' Smoothed_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Smoothed_data_ui <- function(id){
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(width = 4,
                   fileInput(ns("fileBcsv1"),
                             accept = c('text/csv',
                                        'text/comma-separated-values,text/plain',
                                        '.csv'),
                             label = h5("Dataset")),
                   numericInput(inputId = ns("Cell1"),
                                label = "Select a cell:",
                                value = 1, min = 1),
                   numericInput(inputId = ns("point_impact1"),
                                label = "points of impact:",
                                value = 0),

                   tags$hr(style="border-top: 1px solid #ccc;"),

                   tags$h4("Savitzky-Golay Smoothing Function Arguments",
                           style = "color: gray; margin-top: 10px;"),
                   numericInput(inputId = ns("p"),
                                label = " The degree of the polynomial",
                                value = 5, min = 1),
                   numericInput(inputId = ns("w"),
                                label = "Odd Window Size (Integer)",
                                value = 17, min = 3,step = 2),

                   tags$hr(style="border-top: 1px solid #ccc;"),

                   tags$h4("findpeaks Function Arguments",
                           style = "color: gray; margin-top: 10px;"),

                   numericInput(inputId = ns("minpeakheight1"),
                                label = "Minimum Absolute Peak Height",
                                value = 0.1, min = 0, max = 100,step = 0.1),
                   numericInput(inputId = ns("minpeakdistance1"),
                                label = "Minimum Peak Distance (Index-Based)",
                                value = 2, min = 0, max = 100),
                   numericInput(inputId = ns("nups1"),
                                label = "Minimum Increasing Steps to Reach a Peak",
                                value = 1, min = 0, max = 100),
                   numericInput(inputId = ns("ndowns1"),
                                label = "Minimum Decreasing Steps After the Peak",
                                value = 1, min = 0, max = 100),
                   downloadButton(ns("downloadData.one1"),
                                  "Save My Results")

      ),
      mainPanel(
        tabsetPanel(
          type = "tabs",
          tabPanel("SummaryData1",
                   DT::DTOutput(ns("infotable1")),
                   DT::DTOutput(ns("data1"))
          ),
          tabPanel("Peaks",
                   DT::DTOutput(ns("table_peaks1")),
                   plotOutput(ns("plot_peak2")),
                   plotOutput(ns("plot_peak1"))


          ),
          tabPanel("Derivatives",
                   plotOutput(ns("plot_smoothed")),
                   plotOutput(ns("plot_derivative1")),
                   plotOutput(ns("plot_derivative2"))


          )
        )
      )


    )


  )
}

#' Smoothed_data Server Functions
#'
#' @noRd
mod_Smoothed_data_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    filedata <- reactive({
      req(input$fileBcsv1)
      fileInput <- load_file(input$fileBcsv1$name, input$fileBcsv1$datapath)
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

    output$data1 <- DT::renderDataTable({
      df <- data_info()$data
      DT::datatable(df)
    })

    output$infotable1 <- DT::renderDataTable({
      df <- data_info()$SummaryData
      DT::datatable(df, options = list(
        pagingType = 'simple',
        dom = 't'
      ))
    })


    peaks_df1 <- reactive({

      req(filedata()$fileInput)
      data = filedata()$fileInput
      data = t(data)
      colnames(data) = data[1,]
      data = data[-1,]
      cell = as.numeric(input$Cell1)
      data_raw = data.frame(Time = as.numeric(colnames(data)),
                            signal = as.numeric(data[cell,]))
      data_raw  = data_raw[data_raw$Time>=input$point_impact1,]

  #datos siavizados y derivadaas
      data_suavizada <- Savitzky_Golay(data = data, p = input$p, w = input$w, Cell = cell)$data.suav_P
      data_suavizada = data_suavizada[data_suavizada$Time >= input$point_impact1,]
      primera_derivada <- Savitzky_Golay(data = data, p = input$p, w = input$w, Cell = cell)$data.1nd_P
      primera_derivada = primera_derivada[primera_derivada$Time >= input$point_impact1,]
      segunda_derivada <- Savitzky_Golay(data = data, p = input$p, w = input$w, Cell = cell)$data.2nd_P
      segunda_derivada = segunda_derivada[segunda_derivada$Time >= input$point_impact1,]

  #picos con datos suavizados
      peaks_found <- peaks(data = data_suavizada, nups=input$nups1,
                           ndowns = input$ndowns1, minpeakheight = input$minpeakheight1,
                           minpeakdistance = input$minpeakdistance1)

      table_peak <- peaks_found$p_eak
      table_positions_peaks <- peaks_found$peak

      return(list(table_peak = table_peak,
                  table_positions_peaks  = table_positions_peaks,
                  data_raw = data_raw, data_suavizada = data_suavizada,
                  primera_derivada = primera_derivada,
                  segunda_derivada = segunda_derivada  ))
      })


    peaks_plot1 <- reactive({
      table_peak = peaks_df1()$table_peak  #tabla que muestra los piko
      table_positions_peaks = peaks_df1()$table_positions_peaks # tabla de las posiciones de los piko

      data_raw = peaks_df1()$data_raw  #data con la celula analizada
      data_suavizada <- peaks_df1()$data_suavizada  #data siavizada con la celula analizada



      primera_derivada <- peaks_df1()$primera_derivada #data primera derivada con la celula analizada
      segunda_derivada <- peaks_df1()$segunda_derivada#data segunda derivada con la celula analizada

      peaks <- table_positions_peaks[,2]   # Índices donde se encuentran los picos
      data_putos_pekas = data.frame(x = data_suavizada[,1][peaks], y = data_suavizada[,2][peaks]) #puntos de los picos
      vertical_segments <- data.frame(x = data_suavizada[,1][peaks],
                                      yend = table_positions_peaks[,1])   # posicion del piko y su altura
      data_min <- prominens(data = data_suavizada, peak = table_positions_peaks)$data_min # puntos minimos donde empiezan los prominents
      df_peaks_parcia <- prominens(data = data_suavizada, peak = table_positions_peaks)$df_peaks_parcia # el segmento del prominens

      Puntos_medios <- FWHM(peaks = data_suavizada[,1][peaks], firts_peak_div2 = table_positions_peaks[,1][1]/2,
                            df_peaks_parcia = df_peaks_parcia)$Puntos_medios  # puntos medios de los prominances

      table_peak$prominence <- round(prominens(data = data_suavizada, peak = table_positions_peaks)$prominens_amplitud,3)  # valor de los prominens
      table_peak$Prominence_Midpoint <- Puntos_medios$p_eak_mediun # valor medio de las promineces



      gg2 <- ggplot2::ggplot(data_suavizada, ggplot2::aes(x = Time, y = data.suav)) +
        ggplot2::geom_line() +
        ggplot2::geom_hline(yintercept = input$minpeakheight1, linetype = "dashed", color = "blue") +
        ggplot2::geom_point(data = data_putos_pekas,
                            ggplot2::aes(x = x, y = y), color = "red", size = 1) +
        ggplot2::geom_segment(data = vertical_segments,
                              ggplot2::aes(x = x, xend = x, y = 0, yend = yend),
                              linetype = "dashed", color = "red") +
        ggplot2::geom_segment(data = df_peaks_parcia,
                              ggplot2::aes(x = p_ini1, xend = p_ini2, y = p_fin1 , yend = p_fin2),
                              linetype = "dashed", color = "blue") +
        ggplot2::geom_point(data = Puntos_medios, ggplot2::aes(x = posiscion_medio,
                                                               y = p_eak_mediun), color = "blue", size = 1) +
        ggplot2::theme_minimal()



      gg1 <- ggplot2::ggplot(data_raw, ggplot2::aes(x = data_raw[,1], y = data_raw[,2])) +
          ggplot2::geom_line() +
        ggplot2::geom_hline(yintercept = input$minpeakheight1, linetype = "dashed", color = "blue") +
        ggplot2::geom_point(data = data_putos_pekas,
                            ggplot2::aes(x = x, y = y), color = "red", size = 1) +
        ggplot2::geom_segment(data = vertical_segments,
                              ggplot2::aes(x = x, xend = x, y = 0, yend = yend),
                              linetype = "dashed", color = "red") +
        ggplot2::geom_segment(data = df_peaks_parcia,
                              ggplot2::aes(x = p_ini1, xend = p_ini2, y =p_fin1 , yend = p_fin2),
                              linetype = "dashed", color = "blue") +
        ggplot2::geom_point(data = Puntos_medios, ggplot2::aes(x = posiscion_medio,
                                                               y = p_eak_mediun), color = "blue", size = 1) +
        ggplot2::theme_minimal()

      #gradico solo siavizado
      gg3 <- ggplot2::ggplot(data_suavizada, ggplot2::aes(x = Time, y = data.suav)) +
        ggplot2::geom_line() +
        ggplot2::theme_minimal()

      #primera derivada
      gg4 <- ggplot2::ggplot(primera_derivada, ggplot2::aes(x = Time, y = data_1nd)) +
        ggplot2::geom_line() +
        ggplot2::theme_minimal()

      #segunda derivada
      gg5 <- ggplot2::ggplot(segunda_derivada, ggplot2::aes(x = Time, y = data_2nd)) +
        ggplot2::geom_line() +
        ggplot2::theme_minimal()






      return(list(gg1 = gg1, gg2 = gg2, gg3 = gg3,gg4 = gg4, gg5 = gg5, table_peak=table_peak))
    })

    output$plot_peak1 <- renderPlot({
      peaks_plot1()$gg1
    })

    output$plot_peak2 <- renderPlot({
      peaks_plot1()$gg2
    })

    output$plot_smoothed <- renderPlot({
      peaks_plot1()$gg3
    })

    output$plot_derivative1 <- renderPlot({
      peaks_plot1()$gg4
    })

    output$plot_derivative2 <- renderPlot({
      peaks_plot1()$gg5
    })

    output$table_peaks1 <- DT::renderDataTable({
      df <- peaks_plot1()$table_peak

      column_order <- c("absolute_amplitude", "prominence","Prominence_Midpoint" , "posision_peaks", "l_inf", "l_sup")



      DT::datatable(df[, column_order])
    })


  })
}

## To be copied in the UI
# mod_Smoothed_data_ui("Smoothed_data_1")

## To be copied in the server
# mod_Smoothed_data_server("Smoothed_data_1")
