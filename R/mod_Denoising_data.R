#' Denoising_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Denoising_data_ui <- function(id){
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(width = 4,
                   fileInput(ns("fileBcsv2"),
                             accept = c('text/csv',
                                        'text/comma-separated-values,text/plain',
                                        '.csv'),
                             label = h5("Dataset")),
                   numericInput(inputId = ns("Cell2"),
                                label = "Select a Cell:",
                                value = 1, min = 1),
                   numericInput(inputId = ns("point_impact2"),
                                label = "Points of Impact:",
                                value = 0),
                   numericInput(inputId = ns("span"),
                                label = "Smoothness Control:",
                                value = 0.1,min = 0, max = 1,step = 0.01),
                   tags$h4("Find Peaks Function Arguments",
                           style = "color: gray; margin-top: 10px;"),
                   numericInput(inputId = ns("minpeakheight2"),
                                label = "Minimum Absolute Peak Height",
                                value = 0, min = 0, max = 100,step = 0.1),
                   numericInput(inputId = ns("minpeakdistance2"),
                                label = "Minimum Peak Distance (Index-Based)",
                                value = 0, min = 0, max = 100),
                   numericInput(inputId = ns("nups2"),
                                label = "Minimum Increasing Steps to Reach a Peak",
                                value = 1, min = 0, max = 100),
                   numericInput(inputId = ns("ndowns2"),
                                label = "Minimum Decreasing Steps After the Peak",
                                value = 1, min = 0, max = 100),
                   selectInput(ns("raw_data"),
                               label = "Raw Data",
                               choices = list("no"=1,
                                              "yes"=2
                               )
                   ),
                   selectInput(ns("auc2"),
                               label = "AUC",
                               choices = list("no"=1,
                                              "yes"=2
                               )
                   )


                   ),

      mainPanel(
        tabsetPanel(
          type = "tabs",
          tabPanel("SummaryData2",
                   DT::DTOutput(ns("infotable2")),
                   DT::DTOutput(ns("data2"))
          ),
          tabPanel("Peaks",
                   DT::DTOutput(ns("table_peaks2")),
                   DT::DTOutput(ns("table_AUC2")),
                   plotOutput(ns("plot_peak2")),
                   plotOutput(ns("plot_raw_smoothed"))


          )
        )
      )

    )

  )
}

#' Denoising_data Server Functions
#'
#' @noRd
mod_Denoising_data_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    filedata <- reactive({
      req(input$fileBcsv2)
      fileInput <- load_file(input$fileBcsv2$name, input$fileBcsv2$datapath)
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

    output$data2 <- DT::renderDataTable({
      df <- data_info()$data
      DT::datatable(df)
    })

    output$infotable2 <- DT::renderDataTable({
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
      cell = as.numeric(input$Cell2)
      data_raw = data.frame(Time = as.numeric(colnames(data)),
                            signal = as.numeric(data[cell,]))
      data_raw  = data_raw[data_raw$Time>=input$point_impact2,]

      ##### function loess for smoothed
      smoothed <- loess(signal ~ Time, data = data_raw , span = input$span)
      predictions <- predict(smoothed)
      df_smoothed <- data.frame(Time = data_raw$Time, signal = predictions)
      #####

      peaks_found <- peaks(data = df_smoothed, nups=input$nups2,
                           ndowns = input$ndowns2, minpeakheight = input$minpeakheight2,
                           minpeakdistance = input$minpeakdistance2)

      table_peak <- peaks_found$p_eak
      table_positions_peaks <- peaks_found$peak

      return(list(table_peak = table_peak,
                  table_positions_peaks  = table_positions_peaks,
                  data_raw = data_raw, df_smoothed = df_smoothed ))
    })


    peaks_plot <- reactive({
      table_peak = peaks_df()$table_peak  #tabla que muestra los piko
      table_positions_peaks = peaks_df()$table_positions_peaks # tabla de las posiciones de los piko
      data_raw = peaks_df()$data_raw  #data con la celula analizada
      data_smoothed = peaks_df()$df_smoothed   # data suavizada

      peaks <- table_positions_peaks[,2]   # Índices donde se encuentran los picos
      data_putos_pekas = data.frame(x = data_smoothed[,1][peaks], y = data_smoothed[,2][peaks]) #puntos de los picos
      vertical_segments <- data.frame(x = data_smoothed[,1][peaks],
                                      yend = table_positions_peaks[,1])   # posicion del piko y su altura

      data_min <- prominens(data = data_smoothed, peak = table_positions_peaks)$data_min # puntos minimos donde empiezan los prominents
      df_peaks_parcia <- prominens(data = data_smoothed, peak = table_positions_peaks)$df_peaks_parcia # el segmento del prominens

      Puntos_medios <- FWHM(peaks = data_smoothed[,1][peaks], firts_peak_div2 = table_positions_peaks[,1][1]/2,
                            df_peaks_parcia = df_peaks_parcia)$Puntos_medios  # puntos medios de los prominances

      table_peak$prominence <- round(prominens(data = data_smoothed, peak = table_positions_peaks)$prominens_amplitud,3)  # valor de los prominens
      table_peak$Prominence_Midpoint <- Puntos_medios$p_eak_mediun # valor medio de las promineces

      first_time <- as.data.frame(response_time(data = data_smoothed, peak = table_positions_peaks,
                                                Puntos_medios = Puntos_medios)$first_time )  #primer tiempo
      second_time <- as.data.frame(response_time(data = data_smoothed, peak = table_positions_peaks,
                                                 Puntos_medios = Puntos_medios)$second_time)  #segundo tiempo
      Tiempo_respose <- response_time(data = data_smoothed, peak = table_positions_peaks,
                                      Puntos_medios = Puntos_medios)$Tiempo_respose #tiempo de respuesta

      data_segmento_tiempo <- data.frame(x1 = first_time[1,1], x2 = second_time[1,1])

      if(input$auc2==2){


        AUC <- AUC2(datos = data_smoothed, P_min = first_time[1,1] , P_max = second_time[1,1])$area
        AUC_abs_error <- AUC2(datos = data_smoothed, P_min = first_time[1,1] , P_max = second_time[1,1])$with_absolute_error

        tabla_AUC <- data.frame(AUC = AUC, AUC_abs_error = AUC_abs_error, P_min = first_time[1,1], P_max = second_time[1,1])
      }
      else {tabla_AUC <- data.frame()}


      gg <- ggplot2::ggplot(data_smoothed, ggplot2::aes(x = data_smoothed[,1], y = data_smoothed[,2])) +
        ggplot2::geom_line() +
      ggplot2::geom_hline(yintercept = input$minpeakheight2, linetype = "dashed", color = "blue") +
        ggplot2::geom_point(data = data_putos_pekas,
                            ggplot2::aes(x = x, y = y), color = "red", size = 1)+
        ggplot2::geom_segment(data = vertical_segments,
                              ggplot2::aes(x = x, xend = x, y = 0, yend = yend),
                              linetype = "dashed", color = "red") +
        #ggplot2::geom_point(data = data_min, ggplot2::aes(x = x, y = y), color = "blue", size = 1) +
        ggplot2::geom_segment(data = df_peaks_parcia,
                              ggplot2::aes(x = p_ini1, xend = p_ini2, y =p_fin1 , yend = p_fin2),
                              linetype = "dashed", color = "blue") +
        ggplot2::geom_point(data = Puntos_medios, ggplot2::aes(x = posiscion_medio,
                                                               y = p_eak_mediun), color = "blue", size = 1) +
        ggplot2::geom_point(data = first_time,
                            ggplot2::aes(x = first_time[1,1], y = 0), color = "green", size = 2) +
        ggplot2::geom_point(data = second_time,
                            ggplot2::aes(x = second_time[1,1], y = 0), color = "green", size = 2) +
        ggplot2::geom_segment(data = data_segmento_tiempo,
                              ggplot2::aes(x = x1, xend = x2, y = 0, yend = 0),
                              linetype = "solid", color = "green") +
        ggplot2::geom_text(data = data_segmento_tiempo,  # Utiliza el mismo conjunto de datos para asegurarte de que 'Tiempo_respose' esté disponible
                           ggplot2::aes(x = (x1 + x2) / 2, y = 0, label = Tiempo_respose),  # Ubicación del texto en el medio del segmento
                           vjust = 1.5,  # Alineación vertical
                           hjust = 0.5,  # Alineación horizontal (centro)
                           color = "black",  # Color del texto
                           size = 5) +  # Tamaño del texto
        ggplot2::theme_minimal()


      if (input$raw_data == 2){
        gg <- gg + ggplot2::geom_line(data = data_raw, ggplot2::aes(x = data_raw[,1], y = data_raw[,2]),color = "red" )
      }
      else{gg <- gg + ggplot2::geom_line( )}


      df_raw_smoothed <- data.frame(data_smoothed = data_smoothed[,2], data_raw = data_raw[,2] )
      modelo <- lm(data_smoothed ~ data_raw, data = df_raw_smoothed)
      r_cuadrado <- summary(modelo)$r.squared



      gg2 <- ggplot2::ggplot(df_raw_smoothed, ggplot2::aes(x = data_raw  , y = data_smoothed)) +
        ggplot2::geom_point() +              # Para mostrar los puntos de datos
        ggplot2::geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +  # Para agregar la línea de regresión lineal
        ggplot2::labs(title = "Linear Regression Plot", x = "Raw data", y = "Smoothed data") +
        ggplot2::theme_minimal()

      gg2 <- gg2 +
        ggplot2::geom_text(x = max(df_raw_smoothed$data_raw) * 0.8, y = max(df_raw_smoothed$data_smoothed) * 0.5,
                  label = paste("R^2 =", round(r_cuadrado, 4)),
                  parse = TRUE, hjust = 0, vjust = 0, size = 5)






      return(list(gg = gg, gg2 = gg2, table_peak = table_peak, tabla_AUC = tabla_AUC))


    })

    output$plot_peak2 <- renderPlot({
      peaks_plot()$gg
    })

    output$plot_raw_smoothed <- renderPlot({
      peaks_plot()$gg2
    })

    output$table_peaks2 <- DT::renderDataTable({
      df <- peaks_plot()$table_peak
      #colnames(df) <- c("absolute_amplitude", "prominence","prominence_midpoint" , "position_peaks", "l_inf", "l_sup")

      column_order <- c("absolute_amplitude", "prominence","Prominence_Midpoint" , "posision_peaks", "l_inf", "l_sup")



      DT::datatable(df[, column_order])
    })

    output$table_AUC2 <- DT::renderDataTable({
      df <- peaks_plot()$tabla_AUC
      DT::datatable(df, options = list(
        pagingType = 'simple',
        dom = 't'
      ))
    })




  })
}

## To be copied in the UI
# mod_Denoising_data_ui("Denoising_data_1")

## To be copied in the server
# mod_Denoising_data_server("Denoising_data_1")
