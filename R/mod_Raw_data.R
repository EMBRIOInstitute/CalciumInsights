#' Raw_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList

library(shinyjs)
mod_Raw_data_ui <- function(id){
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(width = 4,
                   useShinyjs(),
                   tags$style(HTML(".param-label {display: flex; align-items: flex-start;}.small-button { font-size: 10px; padding: 2px 2px; }")),


                   fileInput(ns("fileBcsv"),
                             accept = c('text/csv',
                                        'text/comma-separated-values,text/plain',
                                        '.csv'),
                             label = h5("Dataset")),

                   numericInput(inputId = ns("Cell"),
                                label = "Components::",
                                value = 1, min = 1),

                   numericInput(inputId = ns("point_impact"),
                                label = "Stimulus Onset Time:",
                                value = 0),
                   div(
                     style = "border-top: 1px solid #ccc; margin-top: 10px; margin-bottom: 10px;"
                   ),

                   tags$h4("Find Peaks Function Arguments",
                           style = "color: gray; margin-top: 10px;"),

                   numericInput(inputId = ns("minpeakheight"),
                                label = "Peak Height:",
                                value = 0.1, min = 0, step = 0.1),
                   numericInput(inputId = ns("minpeakdistance"),
                                label = "Peak Distance:",
                                value = 1, min = 0),

                   numericInput(inputId = ns("nups"),
                                label = "Pre-Peak Ascent:",
                                value = 1, min = 0),

                   numericInput(inputId = ns("ndowns"),
                                label = "Post-Peak Descent:",
                                value = 1, min = 0),
                   numericInput(inputId = ns("min_FWHM2"),
                                label = "FWHM (min)",
                                value = 1, min = 0),

                   div(
                     style = "border-top: 1px solid #ccc; margin-top: 10px; margin-bottom: 10px;"
                   ),

                   radioButtons(
                       inputId = ns("auc"),
                       label = "Area under the curve:",
                       choices = c("No" = 1, "Yes" = 2), selected = 1
                     ),

                   conditionalPanel(condition = "input.auc==2", ns=ns,
                   numericInput(inputId = ns("Integration_Reference"),
                                label = "Reference Level:",
                                value = 0, step = 0.1),
                   ),

                   downloadButton(ns("downloadData.one"),
                                  "Save My Results"),

                   HTML("<div style='margin-bottom: 10px;'></div>"),

                   div(class = "param-label",
                       actionButton(ns("param_info_button10"), "Help", class = "small-button")
                   ),
                   ),

      mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel("SummaryData",
                 DT::DTOutput(ns("infotable")),
                 DT::DTOutput(ns("data"))
                 ),
        tabPanel("Peaks",
                 DT::DTOutput(ns("table_peaks")),
                 DT::DTOutput(ns("table_AUC")),
                 #plotOutput(ns("plot_peak")),
                 plotOutput(ns("plot_peak3"))

        )
        )
      )


    )
  )
}

#' Raw_data Server Functions
#'
#' @noRd
mod_Raw_data_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns


 #####
    observeEvent(input$param_info_button10, {
      showModal(modalDialog(
        title = "Application Parameter Guide",
        size = "l",
        HTML("
      <p style='text-align: justify;'>
        This section provides an overview of essential tools that enhance the data analysis experience.
        From simple and efficient data import to precise control over analysis parameters,
        these features establish a robust framework for data exploration and investigation.
        Each element, from loading data files in different formats to configuring thresholds and criteria,
        is designed to simplify and enhance the analytical process.
      </p>
    "),
        HTML("
      <p style='text-align: justify;'>
        <strong>Dataset:</strong> This button simplifies data import and the start of exploratory work.
        It allows you to load files in CSV and TSV formats with ease,
        initiating your analytical tasks with simplicity and efficiency.
      </p>
    "),
        HTML("
      <p style='text-align: justify;'>
        <strong>Components:</strong> Choose the cell you want to analyze, corresponding to the columns in the loaded dataset.
      </p>
    "),
        HTML("
      <p style='text-align: justify;'>
        <strong>Stimulus Onset Time:</strong> Here, you can select the moment when the experiment's stimulus began,
        marking the point at which the analysis of the calcium signal starts.
      </p>
    "),
        HTML("
      <p style='text-align: justify;'>
        <strong>Peak Height:</strong> This is the minimum (absolute) height required for a peak to be recognized.
      </p>
    "),
        HTML("
      <p style='text-align: justify;'>
        <strong>Peak Distance:</strong> This is the minimum distance (in indices) that peaks must have to be counted.
      </p>
    "),
        HTML("
      <p style='text-align: justify;'>
        <strong>Pre-Peak Ascent:</strong> This is the minimum number of increasing steps required before a peak is reached.
      </p>
    "),
        HTML("
      <p style='text-align: justify;'>
        <strong>Post-Peak Descent:</strong> This is the minimum number of decreasing steps required after a peak.
      </p>
    "),
        HTML("
      <p style='text-align: justify;'>
        <strong>Area under the curve:</strong> You can choose to calculate the area under the curve or not.
      </p>
    "),
        HTML("
      <p style='text-align: justify;'>
        - 'No': If you do not wish to calculate the area under the curve.
      </p>
      <p style='text-align: justify;'>
        - 'Yes': If you wish to calculate the area under the curve.
      </p>
    "),
        HTML("
      <p style='text-align: justify;'>
        <strong>Reference Level:</strong> The assigned value represents a threshold for calculating the area under the curve
        above this value, depicted on the graph by a green line.
      </p>
    "),
        HTML("
  <div class='container'>
    <h2>Metrics Definitions</h2>
    <div style='overflow-x: auto;'> <!-- Agregamos un contenedor con desplazamiento horizontal -->
      <table class='table table-striped table-bordered custom-width' style='max-width: 60%;'>
        <colgroup>
          <col style='width: 10%;'> <!-- Ajusta el ancho de la columna Metric según tus preferencias -->
          <col style='width: 40%;'> <!-- Ajusta el ancho de la columna Definition según tus preferencias -->
          <col style='width: 30%;'> <!-- Ajusta el ancho de la columna Reference según tus preferencias -->
        </colgroup>
        <thead>
          <tr>
            <th>Metric</th>
            <th>Definition</th>
            <th>Reference</th>
          </tr>
        </thead>
        <tbody>
          <tr>
            <td>Amplitude</td>
            <td>
              The maximum distance a wave travels from its resting or equilibrium position. Usually reported as the spatial spread (fullwidth at half-maximal amplitude: FWHM).
            </td>
            <td><cite> [1] </cite><br /><cite> [2] </cite></td>
          </tr>
          <td>Amplitude</td>
            <td>
              The maximum distance a wave travels from its resting or equilibrium position. Usually reported as the spatial spread (fullwidth at half-maximal amplitude: FWHM).
            </td>
            <td><cite> [1] </cite><br /><cite> [2] </cite></td>
          </tr>
          <!-- Agrega las otras filas de la tabla aquí -->
        </tbody>
      </table>
    </div>
  </div>
"),

footer = modalButton("Close")
      ))
    })




#data
    filedata <- reactive({
      req(input$fileBcsv)
      fileInput <- load_file(input$fileBcsv$name, input$fileBcsv$datapath)
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

    output$data <- DT::renderDataTable({
      df <- data_info()$data
      DT::datatable(df)
    })

    output$infotable <- DT::renderDataTable({
      df <- data_info()$SummaryData
      DT::datatable(df, options = list(
        pagingType = 'simple',
        dom = 't'
      ))
    })

### peaks

    peaks_df <- reactive({

      req(filedata()$fileInput)
      data = filedata()$fileInput
      data = t(data)
      colnames(data) = data[1,]
      data = data[-1,]
      cell = as.numeric(input$Cell)
      data_raw = data.frame(Time = as.numeric(colnames(data)),
                            signal = as.numeric(data[cell,]))
      data_raw  = data_raw[data_raw$Time>=input$point_impact,]

      smoothed <- loess(signal ~ Time, data = data_raw , span = 0.06)
      predictions <- predict(smoothed)
      df_smoothed <- data.frame(Time = data_raw$Time, signal = predictions)


      peaks_found <- peaks(data = data_raw, nups=input$nups,
                           ndowns = input$ndowns,
                           minpeakheight = input$minpeakheight,
                           minpeakdistance = input$minpeakdistance)

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
      df_smoothed = peaks_df()$df_smoothed # para encontrar el primer cambio de la derivada en el primer pico

      peaks <- table_positions_peaks[,2]   # Índices donde se encuentran los picos
      data_putos_pekas = data.frame(x = data_raw[,1][peaks], y = data_raw[,2][peaks]) #puntos de los picos
      vertical_segments <- data.frame(x = data_raw[,1][peaks],
                                      yend = table_positions_peaks[,1])   # posicion del piko y su altura

      MSCPFP = Time_of_the_first_peak(data1 = df_smoothed,
                                      peak = table_positions_peaks)$cambios_menor_que_pfp # posicion donde hay un cambio en la primera dericada
                                                                                                            # para el primer pico

      data_min <- prominens2(data = data_raw, peak = table_positions_peaks,
                             MSCPFP = MSCPFP)$data_min # puntos minimos donde empiezan los prominents
      df_peaks_parcia <- prominens2(data = data_raw, peak = table_positions_peaks,
                                    MSCPFP = MSCPFP)$df_peaks_parcia # el segmento del prominens

      time_start_increasin_peak <- prominens2(data = data_raw,
                                              peak = table_positions_peaks,
                                              MSCPFP = MSCPFP)$time_start_increasin_peak

      # Puntos_medios <- FWHM(peaks = data_raw[,1][peaks], firts_peak_div2 = table_positions_peaks[,1][1]/2,
      #                       df_peaks_parcia = df_peaks_parcia)$Puntos_medios  # puntos medios de los prominances

      Puntos_medios <- FWHM2(peaks = data_raw[,1][peaks],df_peaks_parcia = df_peaks_parcia)$Puntos_medios  # puntos medios de los prominances

      table_peak$prominence <- round(prominens2(data = data_raw, peak = table_positions_peaks, MSCPFP = MSCPFP)$prominens_amplitud,3)  # valor de los prominens
      table_peak$Prominence_Midpoint <- Puntos_medios$p_eak_mediun # valor medio de las promineces

       first_time <- as.data.frame(response_time(data = data_raw, peak = table_positions_peaks,
                                      Puntos_medios = Puntos_medios)$first_time )  #primer tiempo
       second_time <- as.data.frame(response_time(data = data_raw, peak = table_positions_peaks,
                                      Puntos_medios = Puntos_medios)$second_time)  #segundo tiempo
       Tiempo_respose <- response_time(data = data_raw, peak = table_positions_peaks,
                                     Puntos_medios = Puntos_medios)$Tiempo_respose #tiempo de respuesta

       data_segmento_tiempo <- data.frame(x1 = first_time[1,1], x2 = second_time[1,1])




       right_left_FWHM <- right_left_FWHM(data1=data_raw, peak = table_positions_peaks,
                                     P_M = Puntos_medios)
       left_FWHM <- right_left_FWHM$df
       right_FWHM <- right_left_FWHM$df2

       table_peak$Time_left_FWHM <- left_FWHM$Time_left_FWHM
       table_peak$Time_right_FWHM <- right_FWHM$Time_right_FWHM

       table_peak$FWHM <- right_FWHM$Time_right_FWHM -left_FWHM$Time_left_FWHM


       table_peak$Time_to_peak <- table_peak$posision_peak - time_start_increasin_peak$Time

       table_peak$puntominimo_y <- prominens2(data = data_raw,
                                              peak = table_positions_peaks,
                                              MSCPFP = MSCPFP)$df_peaks_parcia$p_fin1

       table_FWHM <- data.frame(t1 = left_FWHM$Time_left_FWHM, t2 = right_FWHM$Time_right_FWHM, y_FWHM = Puntos_medios$p_eak_mediun)

       if(input$auc==2){

         Integration_Reference <- input$Integration_Reference

        AUC <- AUC(datos = data_raw, Integration_Reference = Integration_Reference)
        area <- AUC$area
        AUC_abs_error <- AUC$with_absolute_error
        P_min = AUC$P_min
        P_max = AUC$P_max

        tabla_AUC <- data.frame(AUC = area, P_min = P_min, P_max = P_max)
       }
       else {tabla_AUC <- data.frame()}

       data_raw1 <- data_raw
       colnames(data_raw1) <- c("Time", "sig" )

      gg <- ggplot2::ggplot(data_raw1, ggplot2::aes(x = Time, y = sig)) +
        ggplot2::geom_line() +
        ggplot2::geom_hline(yintercept = input$minpeakheight, linetype = "dashed", color = "purple") +
        ggplot2::geom_point(data = data_putos_pekas,
                            ggplot2::aes(x = x, y = y), color = "red", size = 2)+
        ggplot2::geom_segment(data = vertical_segments,
                              ggplot2::aes(x = x, xend = x, y = 0, yend = yend),
                     linetype = "dashed", color = "red") +
        ggplot2::geom_segment(data = df_peaks_parcia,
                              ggplot2::aes(x = p_ini1, xend = p_ini2, y =p_fin1 , yend = p_fin2),
                                  linetype = "dashed", color = "blue") +
        ggplot2::geom_segment(data =  table_FWHM,ggplot2::aes(x = t1, xend = t2, y = y_FWHM , yend = y_FWHM),
                                                    linetype = "solid", color = "orange") +

        ggplot2::annotate(geom = "text", x = 1500, y = 1, label = "---- Peak height",
                 color = "red", size = 5) +
        ggplot2::annotate("path", x = 1200, y = 1, xend = 1400, yend = 1,
                 colour = "red", size = 1.5, alpha = 0.4) +

        # ggplot2::geom_text(
        #   label="Peak heigth",
        #   nudge_x = 0.25, nudge_y = 0.25,
        #   check_overlap = FALSE, color = "red",
        # ) +

        ggplot2::ggtitle("Metrics Graph") +
        ggplot2::labs(x = "Time", y = "Signal") +


        # ggplot2::geom_point(data = time_start_increasin_peak,
        #                     ggplot2::aes(x = Time, y = y), color = "red", size = 1)+
        # ggplot2::geom_point(data = left_FWHM, ggplot2::aes(x = Time_left_FWHM, y = y),size = 1) +
        # ggplot2::geom_point(data = right_FWHM, ggplot2::aes(x = Time_right_FWHM, y = y), size = 1)+
        # ggplot2::geom_point(data = Puntos_medios, ggplot2::aes(x = posiscion_medio,
        #                                      y = p_eak_mediun), color = "blue", size = 1) +
        #ggplot2::geom_point(data = data_min, ggplot2::aes(x = x, y = y), color = "blue", size = 1) +
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

      if(input$auc==2){
        gg <- gg +
          ggplot2::geom_hline(yintercept = input$Integration_Reference, linetype = "dashed", color = "green")
        }
      else {gg <- gg}

      return(list(gg = gg, table_peak = table_peak, tabla_AUC = tabla_AUC))

    })

    output$plot_peak3 <- renderPlot({

      data_smoothed = peaks_df()$df_smoothed
      data_raw = peaks_df()$data_raw
      colnames(data_raw) <- c("Time","Sing")
      df_p <- peaks_plot()$table_peak
      colnames(df_p) <- c("Absolute_Amplitude", "Peak_Time", "L_inf", "L_sup", "Prominence", "Prominence_Midpoint", "Time_left_FWHM","Time_right_FWHM", "FWHM", "Time_to_peak","puntominimo_y")
      column_order <- c("Absolute_Amplitude","Prominence", "Prominence_Midpoint", "Peak_Time", "Time_to_peak", "L_inf", "L_sup", "Time_left_FWHM","Time_right_FWHM","FWHM","puntominimo_y")
      df_p <- df_p[, column_order]
      df_p <- df_p[df_p$FWHM > input$min_FWHM2, ]


      gg3 <- ggplot2::ggplot(data_raw, ggplot2::aes(x = Time, y = Sing)) +
        ggplot2::geom_line() +
        ggplot2::geom_hline(yintercept = input$minpeakheight, linetype = "dashed", color = "purple") +
        ggplot2::geom_point(data = df_p,
                            ggplot2::aes(x = Peak_Time, y = Absolute_Amplitude), color = "red", size = 2) +
        ggplot2::geom_segment(data = df_p,
                              ggplot2::aes(x = Peak_Time, xend = Peak_Time, y = 0, yend = Absolute_Amplitude),
                              linetype = "dashed", color = "red") +
        ggplot2::geom_segment(data = df_p,
                              ggplot2::aes(x = Peak_Time, xend = Peak_Time, y = puntominimo_y  , yend = Absolute_Amplitude),
                              linetype = "dashed", color = "blue") +
        ggplot2::geom_segment(data =  df_p, ggplot2::aes(x = Time_left_FWHM, xend = Time_right_FWHM, y = Prominence_Midpoint , yend = Prominence_Midpoint),
                              linetype = "solid", color = "orange") +
        ggplot2::theme_minimal()


      if(input$auc==2){
        gg3 <- gg3 +
          ggplot2::geom_hline(yintercept = input$Integration_Reference, linetype = "dashed", color = "green")
      }
      else {gg3 <- gg3}

      gg3

    })





    output$plot_peak <- renderPlot({
      peaks_plot()$gg
    })


    output$table_peaks <- DT::renderDataTable({
      df_p <- peaks_plot()$table_peak
      colnames(df_p) <- c("Absolute_Amplitude", "Peak_Time", "L_inf", "L_sup", "Prominence", "Prominence_Midpoint", "Time_left_FWHM","Time_right_FWHM", "FWHM", "Time_to_peak","puntominimo_y")
      column_order <- c("Absolute_Amplitude","Prominence", "Prominence_Midpoint", "Peak_Time", "Time_to_peak", "L_inf", "L_sup", "Time_left_FWHM","Time_right_FWHM","FWHM","puntominimo_y")
      df_p <- df_p[, column_order]
      df_p <- df_p[df_p$FWHM > input$min_FWHM2, ]
      df_p <- df_p[,-11]
      DT::datatable(df_p)
    })


    output$downloadData.one <- downloadHandler(
      filename = function() {
        paste("Metrics", ".csv", sep = "")

      },
      content = function(file) {
        df_p <- peaks_plot()$table_peak
        colnames(df_p) <- c("Absolute_Amplitude", "Peak_Time", "L_inf", "L_sup", "Prominence", "Prominence_Midpoint", "Time_left_FWHM","Time_right_FWHM", "FWHM", "Time_to_peak","puntominimo_y")
        column_order <- c("Absolute_Amplitude","Prominence", "Prominence_Midpoint", "Peak_Time", "Time_to_peak", "L_inf", "L_sup", "Time_left_FWHM","Time_right_FWHM","FWHM","puntominimo_y")
        df_p <- df_p[, column_order]
        df_p <- df_p[df_p$FWHM > input$min_FWHM2, ]
        df_p <- df_p[,-11]
        write.csv(df_p, file, row.names = FALSE)

      }
    )








    output$table_AUC <- DT::renderDataTable({
      df <- peaks_plot()$tabla_AUC
      DT::datatable(df, options = list(
        pagingType = 'simple',
        dom = 't'
      ))
    })


  })
}

## To be copied in the UI
# mod_Raw_data_ui("Raw_data_1")

## To be copied in the server
# mod_Raw_data_server("Raw_data_1")
