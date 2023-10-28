#' Denoising_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#library(shinyjs)
mod_Denoising_data_ui <- function(id){
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(width = 4,
                   useShinyjs(),
                   tags$style(HTML(".param-label {display: flex; align-items: flex-start;}.small-button { font-size: 10px; padding: 2px 2px; }")),

                   fileInput(ns("fileBcsv2"),
                             accept = c('text/csv',
                                        'text/comma-separated-values,text/plain',
                                        '.csv'),
                             label = h5("Dataset")),
                   numericInput(inputId = ns("Cell2"),
                                label = "Components:",
                                value = 1, min = 1),
                   numericInput(inputId = ns("point_impact2"),
                                label = "Stimulus Onset Time:",
                                value = 0),
                   numericInput(inputId = ns("span"),
                                label = "Smoothness Control:",
                                value = 0.05, min = 0, max = 1,step = 0.01),
                   tags$h4("Find Peaks Function Arguments",
                           style = "color: gray; margin-top: 10px;"),
                   numericInput(inputId = ns("minpeakheight2"),
                                label = "Peak Height",
                                value = 0, min = 0, max = 100,step = 0.1),
                   numericInput(inputId = ns("minpeakdistance2"),
                                label = "Peak Distance:",
                                value = 0, min = 0, max = 100),
                   numericInput(inputId = ns("nups2"),
                                label = "Pre-Peak Ascent:",
                                value = 1, min = 0, max = 100),
                   numericInput(inputId = ns("ndowns2"),
                                label = "Post-Peak Descent",
                                value = 1, min = 0, max = 100),
                   numericInput(inputId = ns("min_FWHM"),
                                label = "FWHM (min)",
                                value = 1, min = 0),
                   selectInput(ns("raw_data"),
                               label = "Raw Data",
                               choices = list("no"=1,
                                              "yes"=2
                               )
                   ),
                   radioButtons(
                     inputId = ns("auc2"),
                     label = "Area under the curve:",
                     choices = c("No" = 1, "Yes" = 2), selected = 1
                   ),

                   conditionalPanel(condition = "input.auc2==2", ns=ns,
                                    numericInput(inputId = ns("Integration_Reference1"),
                                                 label = "Reference Level:",
                                                 value = 0, step = 0.1),
                   ),

                   div(class = "param-label",
                       actionButton(ns("param_info_button11"), "Help", class = "small-button")
                   )



                   ),


      mainPanel(
        tabsetPanel(
          type = "tabs",
          tabPanel("SummaryData",
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

    #####
    observeEvent(input$param_info_button11, {
      showModal(modalDialog(
        title = "Application Parameter Guide",
        HTML("<p style='text-align: justify;'> This section is focused on elucidating a set of fundamental tools that
    enhance the data analysis experience. Ranging from the uncomplicated and
    efficient data import to the meticulous control over analysis parameters,
    these functionalities furnish a sturdy framework for data exploration and
    investigation. Each element, from loading data files in data exchange formats
    to setting thresholds and criteria, is engineered to bestow simplicity
    and effectiveness upon the analytical process.</p>"),
        HTML("<p style='text-align: justify;'><strong> Dataset: </strong> this button serves as a straightforward
      and efficient instrument facilitating data import and the commencement of
      exploratory endeavors. It enables the loading of files in both
      CSV and TSV data interchange formats. A mere activation of this button
      permits the selection of a data file in the corresponding format, thereby
      initiating analytical pursuits in a manner characterized by simplicity
      and efficacy.</p>"),
        HTML("<p style='text-align: justify;'>
        <strong> Components: </strong> when selecting this parameter,
      choose the cell you wish to analyze, which corresponds to the columns
      in the loaded dataset.</p>"),
        HTML("<p style='text-align: justify;'><strong> Stimulus Onset Time: </strong> here, you can select the
      moment at which the experiment's stimulus commenced, marking the
      point at which the analysis of the calcium signal begins.</p>"),
        HTML("<p style='text-align: justify;'><strong> Smoothness Control:</strong> The parameter
             that controls the degree of smoothing.</p>"),
        HTML("<p style='text-align: justify;'><strong> Peak Height:</strong>
        the minimum (absolute) height
      a peak has to have to be recognized as such.</p>"),
        HTML("<p style='text-align: justify;'><strong> Peak Distance:</strong> the minimum distance
      (in indices) peaks have to have to be counted.</p>"),
        HTML("<p style='text-align: justify;'><strong> Pre-Peak Ascent:</strong> minimum number of increasing
      steps before a peak is reached.</p>"),
        HTML("<p style='text-align: justify;'><strong> Post-Peak Descent:</strong> minimum number of
      decreasing steps after the peak.</p>"),
        HTML("<p style='text-align: justify;'><strong> Area under the curve,</strong> you have the option to choose:</p>"),
        HTML("<p style='text-align: justify;'>no: if you do not wish to calculate the area under the curve.</p>"),
        HTML("<p style='text-align: justify;'><strong> Raw Data:</strong>...</p>"),
        HTML("<p style='text-align: justify;'>yes: if you wish to calculate the area under the curve.</p>"),
        HTML("<p style='text-align: justify;'><strong> Reference Level:</strong> the assigned value represents
      a threshold indicating the calculation of the area under the curve
      above this value, depicted on the graph by a green line.</p>"),
        footer = modalButton("Close")
      ))
    })

#Data


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


      MSCPFP = Time_of_the_first_peak(data1 = data_smoothed, peak = table_positions_peaks)$cambios_menor_que_pfp # posicion donde hay un cambio en la primera dericada
                                                                                                            # para el primer pico

      data_min <- prominens2(data = data_smoothed, peak = table_positions_peaks, MSCPFP = MSCPFP)$data_min # puntos minimos donde empiezan los prominents
      df_peaks_parcia <- prominens2(data = data_smoothed, peak = table_positions_peaks, MSCPFP = MSCPFP)$df_peaks_parcia # el segmento del prominens

      time_start_increasin_peak <- prominens2(data = data_smoothed,
                                              peak = table_positions_peaks,
                                              MSCPFP = MSCPFP)$time_start_increasin_peak


      Puntos_medios <- FWHM2(peaks = data_smoothed[,1][peaks], df_peaks_parcia = df_peaks_parcia)$Puntos_medios  # puntos medios de los prominances

      table_peak$prominence <- round(prominens2(data = data_smoothed, peak = table_positions_peaks, MSCPFP = MSCPFP)$prominens_amplitud,3)  # valor de los prominens
      table_peak$Prominence_Midpoint <- Puntos_medios$p_eak_mediun # valor medio de las promineces

      first_time <- as.data.frame(response_time(data = data_smoothed, peak = table_positions_peaks,
                                                Puntos_medios = Puntos_medios)$first_time )  #primer tiempo
      second_time <- as.data.frame(response_time(data = data_smoothed, peak = table_positions_peaks,
                                                 Puntos_medios = Puntos_medios)$second_time)  #segundo tiempo
      Tiempo_respose <- response_time(data = data_smoothed, peak = table_positions_peaks,
                                      Puntos_medios = Puntos_medios)$Tiempo_respose #tiempo de respuesta

      data_segmento_tiempo <- data.frame(x1 = first_time[1,1], x2 = second_time[1,1])



      right_left_FWHM <- right_left_FWHM(data1=data_smoothed, peak = table_positions_peaks,
                                         P_M = Puntos_medios)
      left_FWHM <- right_left_FWHM$df
      right_FWHM <- right_left_FWHM$df2

      table_peak$Time_left_FWHM <- left_FWHM$Time_left_FWHM
      table_peak$Time_right_FWHM <- right_FWHM$Time_right_FWHM

      table_peak$FWHM <- right_FWHM$Time_right_FWHM -left_FWHM$Time_left_FWHM

      table_peak$Time_to_peak <- table_peak$posision_peak - time_start_increasin_peak$Time

      table_FWHM <- data.frame(t1 = left_FWHM$Time_left_FWHM, t2 = right_FWHM$Time_right_FWHM, y_FWHM = Puntos_medios$p_eak_mediun)







      if(input$auc2==2){
        Integration_Reference <- input$Integration_Reference1

        AUC <- AUC2(datos = data_smoothed, Integration_Reference = Integration_Reference)
        area <- AUC$area
        AUC_abs_error <- AUC$with_absolute_error
        P_min = AUC$P_min
        P_max = AUC$P_max

        tabla_AUC <- data.frame(AUC = area, P_min = P_min, P_max = P_max)
      }
      else {tabla_AUC <- data.frame()}


      gg <- ggplot2::ggplot(data_smoothed, ggplot2::aes(x = data_smoothed[,1], y = data_smoothed[,2])) +
        ggplot2::geom_line() +
      ggplot2::geom_hline(yintercept = input$minpeakheight2, linetype = "dashed", color = "blue") +
        ggplot2::geom_point(data = data_putos_pekas,
                            ggplot2::aes(x = x, y = y), color = "red", size = 2)+
        ggplot2::geom_segment(data = vertical_segments,
                              ggplot2::aes(x = x, xend = x, y = 0, yend = yend),
                              linetype = "dashed", color = "red") +
        #ggplot2::geom_point(data = data_min, ggplot2::aes(x = x, y = y), color = "blue", size = 1) +
        ggplot2::geom_segment(data = df_peaks_parcia,
                              ggplot2::aes(x = p_ini1, xend = p_ini2, y =p_fin1 , yend = p_fin2),
                              linetype = "dashed", color = "blue") +
        ggplot2::geom_point(data = Puntos_medios, ggplot2::aes(x = posiscion_medio,
                                                               y = p_eak_mediun), color = "blue", size = 1) +
        ggplot2::geom_point(data = left_FWHM, ggplot2::aes(x = Time_left_FWHM, y = y),size = 1) +
        ggplot2::geom_point(data = right_FWHM, ggplot2::aes(x = Time_right_FWHM, y = y), size = 1)+
        ggplot2::geom_segment(data =  table_FWHM,ggplot2::aes(x = t1, xend = t2, y = y_FWHM , yend = y_FWHM),
                              linetype = "solid", color = "orange") +
        ggplot2::ggtitle("Metrics Graph") +
        ggplot2::labs(x = "Time", y = "Signal") +
        # ggplot2::geom_point(data = first_time,
        #                     ggplot2::aes(x = first_time[1,1], y = 0), color = "green", size = 2) +
        # ggplot2::geom_point(data = second_time,
        #                     ggplot2::aes(x = second_time[1,1], y = 0), color = "green", size = 2) +
        # ggplot2::geom_segment(data = data_segmento_tiempo,
        #                       ggplot2::aes(x = x1, xend = x2, y = 0, yend = 0),
        #                       linetype = "solid", color = "green") +
        # ggplot2::geom_text(data = data_segmento_tiempo,  # Utiliza el mismo conjunto de datos para asegurarte de que 'Tiempo_respose' esté disponible
        #                    ggplot2::aes(x = (x1 + x2) / 2, y = 0, label = Tiempo_respose),  # Ubicación del texto en el medio del segmento
        #                    vjust = 1.5,  # Alineación vertical
        #                    hjust = 0.5,  # Alineación horizontal (centro)
        #                    color = "black",  # Color del texto
        #                    size = 5) +  # Tamaño del texto
        ggplot2::theme_minimal()


      if (input$raw_data == 2){
        gg <- gg + ggplot2::geom_line(data = data_raw, ggplot2::aes(x = data_raw[,1], y = data_raw[,2]),color = "red" )
      }
      else{gg <- gg + ggplot2::geom_line( )}

      if(input$auc2==2){
        gg <- gg +
          ggplot2::geom_hline(yintercept = input$Integration_Reference1, linetype = "dashed", color = "green")
      }
      else {gg <- gg}


      df_raw_smoothed <- data.frame(data_smoothed = data_smoothed[,2], data_raw = data_raw[,2] )
      modelo <- lm(data_smoothed ~ data_raw, data = df_raw_smoothed)
      r_cuadrado <- summary(modelo)$r.squared


      gg2 <- ggplot2::ggplot(df_raw_smoothed, ggplot2::aes(x = data_raw, y = data_smoothed)) +
        ggplot2::geom_point() +
        ggplot2::geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
        ggplot2::labs(title = "Linear Regression Plot", x = "Raw data", y = "Smoothed data") +
        ggplot2::theme_minimal()

      # Calcular la ecuación de la recta de regresión

      intercept <- coef(modelo)[1]
      slope <- coef(modelo)[2]

      equation_text <- sprintf("y = %.2fx + %.2f", slope, intercept)

      gg2 <- gg2 +
        ggplot2::geom_text(x = max(df_raw_smoothed$data_raw) * 0.2, y = max(df_raw_smoothed$data_smoothed) * 0.9,
                           label = equation_text,
                           hjust = 0, vjust = 0, size = 5)
      gg2 <- gg2 +
          ggplot2::geom_text(x = max(df_raw_smoothed$data_raw) * 0.2, y = max(df_raw_smoothed$data_smoothed) * 0.8,
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
      colnames(df) <- c("Absolute_Amplitude", "Peak_Time", "L_inf", "L_sup", "Prominence", "Prominence_Midpoint", "Time_left_FWHM","Time_right_FWHM", "FWHM", "Time_to_peak")

      column_order <- c("Absolute_Amplitude","Prominence", "Prominence_Midpoint", "Peak_Time", "Time_to_peak", "L_inf", "L_sup", "Time_left_FWHM","Time_right_FWHM","FWHM")



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
