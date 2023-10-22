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
                   tags$style(HTML(".param-label {display: flex; align-items: center;}.small-button { font-size: 8px; padding: 1px 1px; }")),

                   div(class = "param-label",
                   fileInput(ns("fileBcsv"),
                             accept = c('text/csv',
                                        'text/comma-separated-values,text/plain',
                                        '.csv'),
                             label = h5("Dataset")),
                   actionButton(ns("param_info_button0"), "?", class = "small-button") ),
                   div(class = "param-label",
                   numericInput(inputId = ns("Cell"),
                                label = "Select a Cell:",
                                value = 1, min = 1),
                   actionButton(ns("param_info_button"), "?", class = "small-button") ),


                   div(class = "param-label",
                   numericInput(inputId = ns("point_impact"),
                                label = "Stimulus Onset Time:",
                                value = 0),
                   actionButton(ns("param_info_button2"), "?", class = "small-button")
                   ),

                   div(
                     style = "border-top: 1px solid #ccc; margin-top: 10px; margin-bottom: 10px;"
                   ),
                   tags$h4("Find Peaks Function Arguments",
                           style = "color: gray; margin-top: 10px;"),

                   div(class = "param-label",
                   numericInput(inputId = ns("minpeakheight"),
                                label = "Peak Height:",
                                value = 0.1, min = 0, max = 100,step = 0.1),
                   actionButton(ns("param_info_button3"), "?", class = "small-button")
                   ),

                   div(class = "param-label",
                   numericInput(inputId = ns("minpeakdistance"),
                                label = "Peak Distance:",
                                value = 1, min = 0, max = 100),
                   actionButton(ns("param_info_button4"), "?", class = "small-button")
                   ),
                   div(class = "param-label",
                   numericInput(inputId = ns("nups"),
                                label = "Pre-Peak Ascent:",
                                value = 1, min = 0, max = 100),
                   actionButton(ns("param_info_button5"), "?", class = "small-button")
                   ),

                   div(class = "param-label",
                   numericInput(inputId = ns("ndowns"),
                                label = "Post-Peak Descent:",
                                value = 1, min = 0, max = 100),
                   actionButton(ns("param_info_button6"), "?", class = "small-button")
                   ),

                   div(
                     style = "border-top: 1px solid #ccc; margin-top: 10px; margin-bottom: 10px;"
                   ),

                   div(class = "param-label",
                   selectInput(ns("auc"),
                               label = "Area under the curve:",
                               choices = list("no"=1,
                                              "yes"=2
                               )
                   ),
                   actionButton(ns("param_info_button7"), "?", class = "small-button")
                   ),
                   conditionalPanel(condition = "input.auc==2", ns=ns,
                   div(class = "param-label",
                   numericInput(inputId = ns("Integration_Reference"),
                                label = "Reference Level:",
                                value = 0, step = 0.1),
                   actionButton(ns("param_info_button8"), "?", class = "small-button")
                   )),


                   downloadButton(ns("downloadData.one"),
                                  "Save My Results"),


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
                 plotOutput(ns("plot_peak"))

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


    observeEvent(input$param_info_button0, {
      showModal(modalDialog(
        title = "Button for Data Import and Exploration in CSV and TSV Formats'",
        "This button serves as a straightforward and efficient instrument facilitating data import and the commencement of exploratory endeavors. It enables the loading of files in both CSV and TSV data interchange formats. A mere activation of this button permits the selection of a data file in the corresponding format, thereby initiating analytical pursuits in a manner characterized by simplicity and efficacy.",
        footer = modalButton("Close")
      ))
    })

    observeEvent(input$param_info_button, {
      showModal(modalDialog(
        title = "Ayuda para el parámetro 'Número de muestras'",
        "El parámetro 'Número de muestras' te permite elegir cuántas muestras aleatorias se generarán.",
        "Ajusta el valor utilizando el control deslizante y luego haz clic en el botón 'Generar' para realizar el cálculo.",
        footer = modalButton("Close")
      ))
    })

    observeEvent(input$param_info_button2, {
      showModal(modalDialog(
        title = "Stimulus Onset Time",
        "Here, you can select the moment at which the experiment's stimulus commenced, marking the point at which the analysis of the calcium signal begins.",
        footer = modalButton("Close")
      ))
    })

    observeEvent(input$param_info_button3, {
      showModal(modalDialog(
        title = "Minimum Absolute Peak Height",
        "The minimum (absolute) height a peak has to have to be recognized as such",
        footer = modalButton("Close")
      ))
    })

    observeEvent(input$param_info_button4, {
      showModal(modalDialog(
        title = "Minimum Peak Distance",
        "The minimum distance (in indices) peaks have to have to be counted",
        footer = modalButton("Close")
      ))
    })

    observeEvent(input$param_info_button5, {
      showModal(modalDialog(
        title = "Minimum Steps to Reach a Peak",
        "Minimum number of increasing steps before a peak is reached",
        footer = modalButton("Close")
      ))
    })

    observeEvent(input$param_info_button6, {
      showModal(modalDialog(
        title = "Decreasing Steps After Peak",
        "Minimum number of decreasing steps after the peak",
        footer = modalButton("Close")
      ))
    })

    observeEvent(input$param_info_button7, {
      showModal(modalDialog(
        title = "Area Under the Curve",
        "You have the option to choose:",
        "no: if you do not wish to calculate the area under the curve.",
        "yes: if you wish to calculate the area under the curve.",
        footer = modalButton("Close")
      ))
    })

    observeEvent(input$param_info_button8, {
      showModal(modalDialog(
        title = "Area Under the Curve: Reference Level",
        "The assigned value represents a threshold indicating the calculation of the area under the curve above this value, depicted on the graph by a green line.",
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

      data_min <- prominens2(data = data_raw, peak = table_positions_peaks, MSCPFP = MSCPFP)$data_min # puntos minimos donde empiezan los prominents
      df_peaks_parcia <- prominens2(data = data_raw, peak = table_positions_peaks, MSCPFP = MSCPFP)$df_peaks_parcia # el segmento del prominens

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
                            ggplot2::aes(x = x, y = y), color = "red", size = 1)+
        ggplot2::geom_segment(data = vertical_segments,
                              ggplot2::aes(x = x, xend = x, y = 0, yend = yend),
                     linetype = "dashed", color = "red") +
        ggplot2::geom_segment(data = df_peaks_parcia,
                              ggplot2::aes(x = p_ini1, xend = p_ini2, y =p_fin1 , yend = p_fin2),
                                  linetype = "dashed", color = "blue") +
        ggplot2::geom_segment(data =  table_FWHM,ggplot2::aes(x = t1, xend = t2, y = y_FWHM , yend = y_FWHM),
                                                    linetype = "solid", color = "orange") +

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

    output$plot_peak <- renderPlot({
      peaks_plot()$gg
    })


    output$table_peaks <- DT::renderDataTable({
      df <- peaks_plot()$table_peak
      #df <- df[,-6]
      colnames(df) <- c("Absolute_Amplitude", "Peak_Time", "L_inf", "L_sup", "Prominence", "Prominence_Midpoint", "Time_left_FWHM","Time_right_FWHM", "FWHM")

      column_order <- c("Absolute_Amplitude","Prominence", "Prominence_Midpoint", "Peak_Time", "L_inf", "L_sup", "Time_left_FWHM","Time_right_FWHM","FWHM")



      DT::datatable(df[,column_order])
    })


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
