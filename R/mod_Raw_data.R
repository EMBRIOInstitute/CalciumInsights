#' Raw_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Raw_data_ui <- function(id){
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(width = 3,
                   fileInput(ns("fileBcsv"),
                             accept = c('text/csv',
                                        'text/comma-separated-values,text/plain',
                                        '.csv'),
                             label = h5("Dataset")),
                   numericInput(inputId = ns("Cell"),
                                label = "Select a Cell:",
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
                                label = "Minimum Absolute Peak Height",
                                value = 0.1, min = 0, max = 100,step = 0.1),
                   numericInput(inputId = ns("minpeakdistance"),
                                label = "Minimum Peak Distance (Index-Based)",
                                value = 30, min = 0, max = 100),
                   numericInput(inputId = ns("nups"),
                                label = "Minimum Increasing Steps to Reach a Peak",
                                value = 1, min = 0, max = 100),
                   numericInput(inputId = ns("ndowns"),
                                label = "Minimum Decreasing Steps After the Peak",
                                value = 1, min = 0, max = 100),
                   div(
                     style = "border-top: 1px solid #ccc; margin-top: 10px; margin-bottom: 10px;"
                   ),
                   selectInput(ns("auc"),
                               label = "AUC",
                               choices = list("no"=1,
                                              "yes"=2
                               )
                   ),
                   numericInput(inputId = ns("Integration_Reference"),
                                label = "AUC Reference",
                                value = 0, step = 0.1),

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


      peaks_found <- peaks(data = data_raw, nups=input$nups,
                           ndowns = input$ndowns, minpeakheight = input$minpeakheight,
                           minpeakdistance = input$minpeakdistance)

      table_peak <- peaks_found$p_eak
      table_positions_peaks <- peaks_found$peak

      return(list(table_peak = table_peak,
                  table_positions_peaks  = table_positions_peaks,
                  data_raw = data_raw ))
      })


    peaks_plot <- reactive({

      table_peak = peaks_df()$table_peak  #tabla que muestra los piko
      table_positions_peaks = peaks_df()$table_positions_peaks # tabla de las posiciones de los piko
      data_raw = peaks_df()$data_raw  #data con la celula analizada

      peaks <- table_positions_peaks[,2]   # Índices donde se encuentran los picos
      data_putos_pekas = data.frame(x = data_raw[,1][peaks], y = data_raw[,2][peaks]) #puntos de los picos
      vertical_segments <- data.frame(x = data_raw[,1][peaks],
                                      yend = table_positions_peaks[,1])   # posicion del piko y su altura

      data_min <- prominens(data = data_raw, peak = table_positions_peaks)$data_min # puntos minimos donde empiezan los prominents
      df_peaks_parcia <- prominens(data = data_raw, peak = table_positions_peaks)$df_peaks_parcia # el segmento del prominens

      Puntos_medios <- FWHM(peaks = data_raw[,1][peaks], firts_peak_div2 = table_positions_peaks[,1][1]/2,
                            df_peaks_parcia = df_peaks_parcia)$Puntos_medios  # puntos medios de los prominances

      table_peak$prominence <- round(prominens(data = data_raw, peak = table_positions_peaks)$prominens_amplitud,3)  # valor de los prominens
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

      gg <- ggplot2::ggplot(data_raw, ggplot2::aes(x = data_raw[,1], y = data_raw[,2])) +
        ggplot2::geom_line() +
        ggplot2::geom_hline(yintercept = input$minpeakheight, linetype = "dashed", color = "blue") +
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
        ggplot2::geom_point(data = left_FWHM, ggplot2::aes(x = Time_left_FWHM, y = y),size = 1) +
        ggplot2::geom_point(data = right_FWHM, ggplot2::aes(x = Time_right_FWHM, y = y), size = 1)+
        ggplot2::geom_segment(data =  table_FWHM,ggplot2::aes(x = t1, xend = t2, y = y_FWHM , yend = y_FWHM),
                                                    linetype = "solid", color = "orange") +
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
