#' Special_Case_Circular_Scanning UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Special_Case_Circular_Scanning_ui <- function(id){
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(width = 4,
                   fileInput(ns("fileCasecsv"),
                             accept = c('text/csv',
                                        'text/comma-separated-values,text/plain',
                                        '.csv'),
                             label = h5("Dataset")),
                   numericInput(inputId = ns("Cell_case"),
                                label = "Select a Cell:",
                                value = 1, min = 1),
                   numericInput(inputId = ns("span_case"),
                                label = "Smoothness Control:",
                                value = 0.1,min = 0, max = 1,step = 0.01),
                   numericInput(inputId = ns("point_impact_case"),
                                label = "Stimulus Onset Time:",
                                value = 0),
                   ),
      mainPanel(
        tabsetPanel(
          type = "tabs",
          tabPanel("SummaryData",
                   DT::DTOutput(ns("infotable_case")),
                   DT::DTOutput(ns("data_case"))
          ),
          tabPanel("Peaks",
                   DT::DTOutput(ns("table_peaks_case")),
                   DT::DTOutput(ns("half_prominece")),
                   DT::DTOutput(ns("table_AUC_case")),
                   plotOutput(ns("plot_peak_case")),
                   DT::DTOutput(ns("df_radius")),
                   plotOutput(ns("plot_regresion")),
                   verbatimTextOutput(ns("summary_output"))

          )
        )
      )
      )



  )
}

#' Special_Case_Circular_Scanning Server Functions
#'
#' @noRd
mod_Special_Case_Circular_Scanning_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    filedata <- reactive({
      req(input$fileCasecsv)
      fileInput <- load_file(input$fileCasecsv$name, input$fileCasecsv$datapath)
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

    output$data_case <- DT::renderDataTable({
      df <- data_info()$data
      DT::datatable(df)
    })

    output$infotable_case <- DT::renderDataTable({
      df <- data_info()$SummaryData
      DT::datatable(df, options = list(
        pagingType = 'simple',
        dom = 't'
      ))
    })


    peaks_df <- reactive({
      req(filedata()$fileInput)
      data = filedata()$fileInput
      data = t(data)
      colnames(data) = data[1,]
      data = data[-1,]
      cell = as.numeric(input$Cell_case)
      data_raw = data.frame(Time = as.numeric(colnames(data)),
                            signal = as.numeric(data[cell,]))
      data_raw  = data_raw[data_raw$Time>=0,]

      ##### function loess for smoothed
      smoothed <- loess(signal ~ Time, data = data_raw , span = input$span_case)
      predictions <- predict(smoothed)
      df_smoothed <- data.frame(Time = data_raw$Time, signal = predictions)
      #####

      ### El pico mas alto
      picos <- as.data.frame(pracma::findpeaks(x = as.numeric(df_smoothed[,2])))
      picos_ordenados <- picos[order(picos[,1]), ]
      peak <- picos_ordenados[nrow(picos_ordenados), ]
      #peak = picos
      posision_peak = df_smoothed[,1][peak[,2]]
      l_inf = df_smoothed[,1][peak[,3]]
      l_sup = df_smoothed[,1][peak[,4]]
      #
      p_eak = data.frame(Absolute_Amplitude =round(peak[,1],3),
                        Peak_Time = posision_peak,  L_inf = l_inf,
                        L_sup = l_sup )
      # #########
       return(list(table_peak = p_eak, table_positions_peaks = peak,
                   data_raw = data_raw, df_smoothed = df_smoothed ))
    })

    peaks_plot <- reactive({
      table_peak = peaks_df()$table_peak  #tabla que muestra los piko
      table_positions_peaks = peaks_df()$table_positions_peaks # tabla de las posiciones de los piko
      data_raw = peaks_df()$data_raw  #data con la celula analizada
      data_smoothed = peaks_df()$df_smoothed   # data suavizada

      peaks <- table_positions_peaks[,2]   # Índices donde se encuentran los picos
      data_putos_pekas = data.frame(x = data_smoothed[,1][peaks], y = data_smoothed[,2][peaks])
      vertical_segments <- data.frame(x = data_smoothed[,1][peaks],
                                      yend = table_positions_peaks[,1])

      df_peaks_parcia <- prominens_case(data = data_smoothed, peak = table_positions_peaks)$df_peaks_parcia # el segmento del prominens
      table_peak$prominence <- round(prominens_case(data = data_smoothed, peak = table_positions_peaks)$prominens_amplitud,3)  # valor de los prominens
      Puntos_medios <- FWHM_case(peaks = data_smoothed[,1][peaks], df_peaks_parcia = df_peaks_parcia)$Puntos_medios
      #table_peak$Prominence_Midpoint <- Puntos_medios$p_eak_mediun # valor medio de las promineces
      table_peak$Time_to_peak <- table_peak[,2]- input$point_impact_case

      half_prominence <- line_half_prominence(data = data_smoothed, peak = table_positions_peaks, Puntos_medios = Puntos_medios$p_eak_mediun)$half_prominence

      gg <- ggplot2::ggplot(data_smoothed, ggplot2::aes(x = data_smoothed[,1], y = data_smoothed[,2])) +
        ggplot2::geom_line() +
        ggplot2::geom_point(data = data_putos_pekas,
                            ggplot2::aes(x = x, y = y), color = "red", size = 2) +
        ggplot2::geom_segment(data = vertical_segments,
                              ggplot2::aes(x = x, xend = x, y = 0, yend = yend),
                              linetype = "dashed", color = "red") +
        ggplot2::geom_segment(data = df_peaks_parcia,
                              ggplot2::aes(x = p_ini1, xend = p_ini2, y =p_fin1 , yend = p_fin2),
                              linetype = "dashed", color = "blue", size = 1) +
        ggplot2::geom_segment(data = half_prominence,
                              ggplot2::aes(x = x1, xend = x2, y = y,
                                           yend = y),linetype = "dashed", color = "orange", size = 1) +
        # ggplot2::geom_point(data = Puntos_medios, ggplot2::aes(x = posiscion_medio,
        #                                                        y = p_eak_mediun), color = "blue", size = 1) +
          ggplot2::theme_minimal()


      return(list(gg = gg, table_peak = table_peak, half_prominence = half_prominence))
    })


    output$half_prominece <- DT::renderDataTable({
      df <- peaks_plot()$half_prominence
      df$FWHM <- df[,2]-df[,1]
      df <- df[,-3]
      colnames(df) <- c("time_1_FWHM", "time_1_FWHM", "FWHM" )
      DT::datatable(df, options = list(
        pagingType = 'simple',
        dom = 't'
      ))
    })

     output$table_peaks_case <- DT::renderDataTable({
     df <- peaks_plot()$table_peak
     DT::datatable(df, options = list(
       pagingType = 'simple',
       dom = 't'
     ))
     })

     output$plot_peak_case <- renderPlot({
       peaks_plot()$gg
     })

     peaks_radius <- reactive({

       req(filedata()$fileInput)
       data = filedata()$fileInput
       data = t(data)
       colnames(data) = data[1,]
       data = data[-1,]
       n_cell = dim(data)[1]

       time_peak <- c()
       for (i in 1:n_cell) {
         data_raw = data.frame(Time = as.numeric(colnames(data)),
                               signal = as.numeric(data[i,]))
         data_raw  = data_raw[data_raw$Time>=0,]

         ##
         smoothed <- loess(signal ~ Time, data = data_raw , span = input$span_case)
         predictions <- predict(smoothed)
         df_smoothed <- data.frame(Time = data_raw$Time, signal = predictions)
         ##
         picos <- as.data.frame(pracma::findpeaks(x = as.numeric(df_smoothed[,2])))
         picos_ordenados <- picos[order(picos[,1]), ]
         peak <- picos_ordenados[nrow(picos_ordenados), ]
         #peak = picos
         time_peak[i] = df_smoothed[,1][peak[,2]]-input$point_impact_case

       }
       df_radius <- data.frame(radius = as.numeric(row.names(data)), Time_to_peak = time_peak )

       lm_model <- lm(radius ~ Time_to_peak, data = df_radius)


       #r_squared <- summary(lm_model)$r.squared
       anova_result <- summary(lm_model)

       # Crear una gráfica de dispersión con el modelo ajustado y el R^2
       g <- ggplot2::ggplot(data = df_radius, ggplot2::aes(x = Time_to_peak, y = radius)) +
         ggplot2::geom_point() +  # Gráfico de dispersión de los datos
         ggplot2::geom_smooth(method = "lm", formula = y ~ x, se = FALSE, color = "blue") +  # Modelo ajustado
         #ggplot2::annotate("text", x = max(df_radius$Time_to_peak), y = max(df_radius$Time_to_peak), label = paste("R^2 =", round(r_squared, 2)), color = "red") +  # Texto del R^2
         ggplot2::labs(title = "linear regression", x = "Peak time (s)", y = "Distance (um)")  # Títulos de la gráfica




       return(list(df_radius = df_radius, g=g, anova_result = anova_result))

     })

     output$df_radius <- DT::renderDataTable({
       df <- peaks_radius()$df_radius
       DT::datatable(df)
     })

     output$plot_regresion <- renderPlot({
       peaks_radius()$g
     })



     output$summary_output <- renderPrint({
       peaks_radius()$anova_result
     })

  })
}

## To be copied in the UI
# mod_Special_Case_Circular_Scanning_ui("Special_Case_Circular_Scanning_1")

## To be copied in the server
# mod_Special_Case_Circular_Scanning_server("Special_Case_Circular_Scanning_1")
