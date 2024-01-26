#' Denoising_data UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
library(shinyjs)
mod_Denoising_data_ui <- function(id){
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(width = 4,
                   shinyjs::useShinyjs(),
                   tags$style(HTML(".param-label {display: flex; align-items: flex-start;}.small-button { font-size: 10px; padding: 2px 2px; }")),

                   actionButton(ns("param_info_button11"), "Help",
                                class = "btn-sm",
                                style = "position: absolute; top: 0; right: 15px; margin: 5px;"),

                   radioButtons(ns("data_simulate"), "Example Data",
                                choices = c("Yes"=1, "No"=0),
                                selected = 0),

                       fileInput(ns("fileBcsv2"),
                                 accept = c('text/csv',
                                            'text/comma-separated-values,text/plain',
                                            '.csv'),
                                 label = h5("Dataset")),

                   div(style = "border-top: 1px solid #ccc; margin-top: 10px; margin-bottom: 10px;"),

                   numericInput(inputId = ns("Cell2"),
                                label = "Region of Interest (ROI):",
                                value = 1, min = 1),
                   numericInput(inputId = ns("span"),
                                label = "Smoothness Control:",
                                value = 0.05, min = 0, max = 1,step = 0.01),

                   div(style = "border-top: 1px solid #ccc; margin-top: 10px; margin-bottom: 10px;"),

                   tags$h4("Find Peaks Function Arguments",
                           style = "color: gray; margin-top: 10px;"),

                   fluidRow(
                     column(width = 6,
                            numericInput(inputId = ns("minpeakheight2"),
                                         label = "1.Peak Height (min)",
                                         value = 0, min = 0, max = 100,
                                         step = 0.1),
                            numericInput(inputId = ns("ndowns2"),
                                         label = "3.Peak Descent",
                                         value = 1, min = 0, max = 100)
                     ),

                     column(width = 6,
                            numericInput(inputId = ns("nups2"),
                                         label = "2.Peak Ascent:",
                                         value = 1, min = 0, max = 100),
                            numericInput(inputId = ns("minpeakdistance2"),
                                         label = "4.Min Peak Distance:",
                                         value = 0, min = 0, max = 100)

                     ),

                     column(width = 6,
                            numericInput(inputId = ns("min_FWHP"),
                                         label = "5.FWHP (min)",
                                         value = 0, min = 0, step = 0.1)
                     ),

                     column(width = 6,
                            numericInput(inputId = ns("min_prominence"),
                                         label = "6.Prominence (min)",
                                         value = 0, min = 0, step = 0.1)
                     )
                   ),

                   div(style = "border-top: 1px solid #ccc; margin-top: 10px; margin-bottom: 10px;"),

                   radioButtons(
                     inputId = ns("auc2"),
                     label = "Area Under the Curve (AUC):",
                     choices = c("No" = 1, "Yes" = 2), selected = 1
                   ),

                   selectInput(inputId = ns("Baseline"),
                               label = "Baseline:",
                               choices = c("Reference Level 0" = 1,
                                           "Standard definition" = 2,
                                           "Interval" = 3,
                                           "Your own baseline"=4,
                                           "Min"=5
                               ),
                               selected = 1,
                               multiple = FALSE),

                   conditionalPanel(condition = "input.Baseline==3", ns = ns,
                                    fluidRow(
                                      column(6, textInput(ns("Lim_inf"), "Lim inf:", value = "0")),
                                      column(6, textInput(ns("Lim_sup"), "Lim sup:", value = "20"))
                                    ),
                   ),

                   conditionalPanel(condition = "input.Baseline==4", ns = ns,
                                    numericInput(inputId = ns("own_baseline"),
                                                 label = "Own Baseline:",
                                                 value = 0, step = 0.1),
                   ),

                   radioButtons(inputId = ns("raw_data"),
                                label = "Raw Data",
                                choices = list("no"=1,
                                               "yes"=2
                                )
                   ),

                   radioButtons(
                     inputId = ns("FWHM"),
                     label = "Full Width at Half Maximum:",
                     choices = c("No" = 1, "Yes" = 2), selected = 1
                   ),




                   downloadButton(ns("descargarP"), "Trace Metrics"),
                   downloadButton(ns("descargar"), "Transient Metrics"),




      ),


      mainPanel(
        tabsetPanel(
          type = "tabs",
          tabPanel("SummaryData",
                   DT::DTOutput(ns("infotable2")),
                   DT::DTOutput(ns("data2"))
          ),
          tabPanel("Peaks",
                   tabsetPanel(
                     type = "tabs",
                     tabPanel("Metrics",
                              DT::DTOutput(ns("table_peaks2")),
                              DT::DTOutput(ns("table_peaks22"))
                     ),
                     tabPanel("Metric plots",
                              plotOutput(ns("plot_peak3")),
                              plotOutput(ns("derivative")),
                              plotOutput(ns("plot_raw_smoothed"))
                     ),
                     tabPanel("Components",
                              #plotOutput(ns("plot_component")),
                              plotOutput(ns("panel")),
                              verbatimTextOutput(ns("outputList")),
                              plotOutput(ns("plot_ls")),
                              plotOutput(ns("plot_box_ls")),
                              plotOutput(ns("plot_ls1")),
                              plotOutput(ns("plot_box_ls1")),
                              plotOutput(ns("plot_ls2")),
                              plotOutput(ns("plot_box_ls2")),
                     )
                   )


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

    #########Help #########

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

    #########load Data #########


    filedata <- reactive({
      if (input$data_simulate > 0) {
        # Cargar los datos de ejemplo
        data_example <- readRDS(system.file("data", "Data_Simulate_Calcium.rds", package = "CalciumInsights"))
        fileInput <- data_example
        fileInput2 <- NULL  # O ajusta según tus necesidades
      }



      else{
      req(input$fileBcsv2)
      ext <- tools::file_ext(input$fileBcsv2$name)
      fileInput1 <- load_file(input$fileBcsv2$name,
                              input$fileBcsv2$datapath,
                              ext)

      if (ext %in% c("csv", "tsv")) {
        fileInput <- as.data.frame(fileInput1)
        fileInput2 <- NULL
      }

      else if (ext == "json") {
        fileInput2 <- fileInput1
        comp <- fileInput2$components
        com <- t(fileInput2$components)
        time <- seq(0, fileInput2$image_data[2]-1,
                    by = 1)*fileInput2$image_data[1]
        com <- cbind(time, com)
        fileInput <- com
      }
      }
      return(list(fileInput = fileInput, fileInput2 = fileInput2))
    })

    #########Summary of Data #########

    data_info <- reactive({
      req(filedata()$fileInput)
      Nobservations <- nrow(filedata()$fileInput)
      Ncells <- ncol(filedata()$fileInput)-1
      SummaryData <- data.frame(list(Number = c(Ncells, Nobservations)))
      rownames(SummaryData) <- c("Components", "Time observations")
      list(SummaryData = SummaryData,
           data = data.frame(filedata()$fileInput,
                             row.names = NULL))
    })

    output$data2 <- DT::renderDataTable({
      df <- data_info()$data
      DT::datatable(df,options = list(
        pagingType = 'simple'
      ), caption = tags$caption(tags$strong("Dataset:")))
    })

    output$infotable2 <- DT::renderDataTable({
      df <- data_info()$SummaryData
      DT::datatable(df, options = list(
        pagingType = 'simple',
        dom = 't'
      ), caption = tags$caption(tags$strong("Dataset Summary:")))
    })


    #########Loess function #########

    peaks_df <- reactive({

      req(filedata()$fileInput)
      data = filedata()$fileInput
      data = t(data)
      colnames(data) = data[1,]
      data = data[-1,]
      cell = as.numeric(input$Cell2)
      data_raw = data.frame(Time = as.numeric(colnames(data)),
                            signal = as.numeric(data[cell,]))


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
                  data_raw = data_raw, df_smoothed = df_smoothed, data = data))
    })

    ######### Extraction of metrics #########

    peaks_plot <- reactive({
      table_peak = peaks_df()$table_peak  #tabla que muestra los piko
      table_positions_peaks = peaks_df()$table_positions_peaks # tabla de las posiciones de los piko
      data_raw = peaks_df()$data_raw  #data con la celula analizada
      data_smoothed = peaks_df()$df_smoothed   # data suavizada
      peaks <- table_positions_peaks[,2]   # Índices donde se encuentran los picos
      data_putos_pekas = data.frame(x = data_smoothed[,1][peaks],
                                    y = data_smoothed[,2][peaks]) #puntos de los picos
      vertical_segments <- data.frame(x = data_smoothed[,1][peaks],
                                      yend = table_positions_peaks[,1]) # posicion del piko y su altura
      MSCPFP = Time_of_the_first_peak(data1 = data_smoothed,
                                      peak = table_positions_peaks)$cambios_menor_que_pfp # posicion donde
                                                                                          #hay un cambio en la primera derivada
                                                                                          # para el primer pico
      data_min <- prominens2(data = data_smoothed,
                             peak = table_positions_peaks,
                             MSCPFP = MSCPFP)$data_min # puntos minimos donde empiezan los prominents

      df_peaks_parcia <- prominens2(data = data_smoothed,
                                    peak = table_positions_peaks,
                                    MSCPFP = MSCPFP)$df_peaks_parcia    # el segmento del prominens

      time_start_increasin_peak <- prominens2(data = data_smoothed,
                                              peak = table_positions_peaks,
                                              MSCPFP = MSCPFP)$time_start_increasin_peak
      #####

      Puntos_medios <- FWHP2(peaks = data_smoothed[,1][peaks],
                             df_peaks_parcia = df_peaks_parcia)$Puntos_medios  # puntos medios de los prominances

      table_peak$prominence <- prominens2(data = data_smoothed,
                                          peak = table_positions_peaks,
                                          MSCPFP = MSCPFP)$prominens_amplitud  # valor de los prominens

      table_peak$Prominence_Midpoint <- Puntos_medios$p_eak_mediun # valor medio de las promineces

      first_time <- as.data.frame(response_time(data = data_smoothed,
                                                peak = table_positions_peaks,
                                                Puntos_medios = Puntos_medios)$first_time )  #primer tiempo
      second_time <- as.data.frame(response_time(data = data_smoothed,
                                                 peak = table_positions_peaks,
                                                 Puntos_medios = Puntos_medios)$second_time)  #segundo tiempo
      Tiempo_respose <- response_time(data = data_smoothed,
                                      peak = table_positions_peaks,
                                      Puntos_medios = Puntos_medios)$Tiempo_respose #tiempo de respuesta

      data_segmento_tiempo <- data.frame(x1 = first_time[1,1],
                                         x2 = second_time[1,1])

      right_left_FWHP <- right_left_FWHP(data1=data_smoothed,
                                         peak = table_positions_peaks,
                                         P_M = Puntos_medios)
      left_FWHP <- right_left_FWHP$df
      right_FWHP <- right_left_FWHP$df2

      table_peak$Time_left_FWHP <- left_FWHP$Time_left_FWHP
      table_peak$Time_right_FWHP <- right_FWHP$Time_right_FWHP

      table_peak$FWHP <- right_FWHP$Time_right_FWHP - left_FWHP$Time_left_FWHP

      table_peak$Time_to_peak <- table_peak$posision_peak - time_start_increasin_peak$Time



      table_peak$puntominimo_y <- prominens2(data = data_smoothed,
                                             peak = table_positions_peaks,
                                             MSCPFP = MSCPFP)$df_peaks_parcia$p_fin1


      table_FWHP <- data.frame(t1 = left_FWHP$Time_left_FWHP,
                               t2 = right_FWHP$Time_right_FWHP,
                               y_FWHP = Puntos_medios$p_eak_mediun)


      if(input$Baseline==1){ baseline1 = 0}
      if(input$Baseline==2){
        Time_One_set <- time_start_increasin_peak$Time[1]
        posicion_Time_One_set <- which(data_smoothed$Time  == Time_One_set)
        baseline1 <- mean(data_smoothed$signal[1:posicion_Time_One_set] )
      }
      if(input$Baseline==3){
        Lim_inf <- as.numeric(input$Lim_inf)
        Lim_sup <- as.numeric(input$Lim_sup)
        df_filtrado <- data_smoothed[data_smoothed$Time >= Lim_inf & data_smoothed$Time <= Lim_sup, ]
        baseline1 <- mean(df_filtrado$signal)
      }
      if(input$Baseline==4){
        baseline1 <- input$own_baseline
      }
      if(input$Baseline==5){
        baseline1 <- min(data_smoothed$signal)
      }

      if(input$auc2==2){
        AUC <- AUC2(datos = data_smoothed,
                    Integration_Reference = baseline1)
        area <- AUC$area
        AUC_abs_error <- AUC$with_absolute_error
        P_min = AUC$P_min
        P_max = AUC$P_max
        tabla_AUC <- data.frame(AUC = area, P_min = P_min, P_max = P_max)
      }
      else {tabla_AUC <- data.frame()}

      ####### Graph of the Linear Model Adjustment between data_smoothed and raw data

      df_raw_smoothed <- data.frame(data_smoothed = data_smoothed[,2],
                                    data_raw = data_raw[,2] )
      modelo <- lm(data_smoothed ~ data_raw,
                   data = df_raw_smoothed)
      r_cuadrado <- summary(modelo)$r.squared

      gg2 <- ggplot2::ggplot(df_raw_smoothed,
                             ggplot2::aes(x = data_raw, y = data_smoothed)) +
        ggplot2::geom_point() +
        ggplot2::geom_smooth(method = "lm", formula = y ~ x, se = FALSE) +
        ggplot2::labs(title = "Linear Regression",
                      x = "Raw data",
                      y = "Smoothed data") +
        ggplot2::theme_minimal() +
        ggplot2::theme_classic() +
        ggplot2::theme(
          plot.title = ggplot2::element_text(size = 28, face = "bold"),
          axis.title = ggplot2::element_text(size = 28, face = "bold"),
          axis.text = ggplot2::element_text(size = 16, face = "bold"),
          legend.title = ggplot2::element_text(size = 28, face = "bold"),
          legend.text = ggplot2::element_text(size = 28, face = "bold")
        )


      ###### Calculate the linear regression equation

      intercept <- coef(modelo)[1]
      slope <- coef(modelo)[2]

      equation_text <- sprintf("y = %.2fx + %.2f", slope, intercept)


      gg2 <- gg2 +
        ggplot2::geom_text(x = mean(df_raw_smoothed$data_raw) - sd(df_raw_smoothed$data_raw),
                           y = mean(df_raw_smoothed$data_smoothed) + sd(df_raw_smoothed$data_raw),
                           label = equation_text,
                           hjust = 0, vjust = 0, size = 5)
      gg2 <- gg2 +
        ggplot2::geom_text(x = mean(df_raw_smoothed$data_raw) - sd(df_raw_smoothed$data_raw),
                           y = mean(df_raw_smoothed$data_smoothed) + 2*sd(df_raw_smoothed$data_raw),
                           label = paste("R^2 =", round(r_cuadrado, 4)),
                           parse = TRUE, hjust = 0, vjust = 0, size = 5)



      table_peak$Transient_Ocurrence_Time <- time_start_increasin_peak$Time


      ######## Funcion de Rise #####
      data_minimos_crecientes <- data.frame(x1 = time_start_increasin_peak$Time,
                                            y1 = data_min$y,
                                            x2 = table_peak$posision_peaks,
                                            y2 = table_peak$absolute_amplitude )
      cell = as.numeric(input$Cell2)
      primera_derivada <- Savitzky_Golay(data = peaks_df()$data,
                                         p = 2,
                                         w = 5,
                                         Cell = cell)$data.1nd_P

      primera_derivada1 <- data.frame(Time = primera_derivada$Time,
                                      deri1 = prospectr::savitzkyGolay(X=data_smoothed$signal,
                                                                       m=1,
                                                                       p = 2,
                                                                       w = 5))
      slope <- c()
      times_predition <- list()
      for (i in 1:length(data_minimos_crecientes$x1)) {
        resultados_filtrados <- primera_derivada1[primera_derivada1$Time >= data_minimos_crecientes$x1[i] & primera_derivada1$Time <= data_minimos_crecientes$x2[i], ]
        slope[i] <- max(resultados_filtrados$deri1)
      }
      table_peak$slope <- slope




      ################################## FWHM
      return(list(gg2 = gg2, table_peak = table_peak, tabla_AUC = tabla_AUC,
                  baseline1 = baseline1,
                  primera_derivada1 = primera_derivada1))
    })


    output$derivative <- renderPlot({

      data_derivative <- peaks_plot()$primera_derivada1

      derivative <- ggplot2::ggplot(data_derivative,
                                    ggplot2::aes(x = Time, y = deri1)) +
        ggplot2::geom_line(linetype = "solid",size = 1.5, color = "black") +
        ggplot2::geom_hline(yintercept = 0,
                            linetype = "dashed", color = "purple") +
        ggplot2::labs(title = "First Derivative",
                      x = "Time [s]",
                      y = "Fluorescence [a.u.]") +

        ggplot2::theme_classic() +

        ggplot2::theme(plot.title = ggplot2::element_text(size = 28,
                                                          face = "bold")) +

        ggplot2::theme(axis.title.y = ggplot2::element_text(size = 28,
                                                            face = "bold")) +
        ggplot2::theme(axis.title.x = ggplot2::element_text(size = 28,
                                                            face = "bold")) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(size = 16,
                                                           face = "bold")) +
        ggplot2::theme(axis.text.y = ggplot2::element_text(size = 16,
                                                           face = "bold"))


      derivative


    })


    peaks_FWHM <- reactive({

      baseline1 <- peaks_plot()$baseline1
      table_positions_peaks = peaks_df()$table_positions_peaks # tabla de las posiciones de los piko
      data_raw = peaks_df()$data_raw  #data con la celula analizada
      data_smoothed = peaks_df()$df_smoothed   # data suavizada
      peaks <- table_positions_peaks[,2]   # Índices donde se encuentran los picos
      p_eak_mediun <- c((table_positions_peaks[,1] + baseline1)/2)  #absolute_amplitude dividido en 2
      Puntos_medios <- data.frame(posiscion_medio = data_smoothed[,1][peaks],
                                  p_eak_mediun = p_eak_mediun)
      right_left_FWHM <- right_left_FWHP(data1=data_smoothed,
                                         peak = table_positions_peaks,
                                         P_M = Puntos_medios)
      left_FWHM <- right_left_FWHM$df
      right_FWHM <- right_left_FWHM$df2
      FWHM <- right_FWHM$Time_right_FWHP - left_FWHM$Time_left_FWHP
      Time_left_FWHM <- left_FWHM$Time_left_FWHP
      Time_right_FWHM <- right_FWHM$Time_right_FWHP
      Amplitude_Midpoint <- p_eak_mediun
      df_FWHM <- data.frame(Time_left_FWHM = Time_left_FWHM,
                            Time_right_FWHM = Time_right_FWHM,
                            Amplitude_Midpoint = Amplitude_Midpoint)
      return(list(df_FWHM = df_FWHM, FWHM = FWHM))

    })




    Peaks_Data_Final <- reactive({
      df_p <- peaks_plot()$table_peak
      df_FWHM1 <- peaks_FWHM()$df_FWHM

      df_p$FWHM <- peaks_FWHM()$FWHM

      colnames(df_p) <- c("Amplitude", "Peak_Occurence_Time", "L_inf", "L_sup",
                          "Prominence", "Prominence_Midpoint", "Time_left_FWHP",
                          "Time_right_FWHP", "FWHP", "Peak_Rise_Time",
                          "puntominimo_y", "Transient_Ocurrence_Time",
                          "Rise_Rate", "FWHM")

      df_FWHM2 <- cbind(df_p,df_FWHM1) #union de la data y df_FWHM1

      df_FWHM2 <- df_FWHM2[df_FWHM2$FWHP > input$min_FWHP, ]             # Filter for minimun FWHP
      df_FWHM2 <- df_FWHM2[df_FWHM2$Prominence > input$min_prominence, ] #filter for minimun prominence
      df_FWHM2 <- df_FWHM2[df_FWHM2$Amplitude > peaks_plot()$baseline1, ] #filter for minimun prominence
      df_p <- df_FWHM2
      return(list(df_p = df_p))
    })



    output$plot_peak3 <- renderPlot({
      data_smoothed = peaks_df()$df_smoothed
      data_raw = peaks_df()$data_raw
      colnames(data_smoothed) <- c("Time","Sing")
      df_p <- Peaks_Data_Final()$df_p

      data_derivative <- peaks_plot()$primera_derivada1

      gg3 <- ggplot2::ggplot(data_smoothed,
                             ggplot2::aes(x = Time, y = Sing)) +
        ggplot2::geom_line(linetype = "solid",size = 1.5, color = "black") +

        ggplot2::geom_hline(yintercept = input$minpeakheight2,
                            linetype = "dashed", color = "purple") +
        ggplot2::geom_point(data = df_p,
                            ggplot2::aes(x = Peak_Occurence_Time, y = Amplitude),
                            color = "red", size = 6) +
        ggplot2::geom_segment(data = df_p,
                              ggplot2::aes(x = Peak_Occurence_Time,
                                           xend = Peak_Occurence_Time,
                                           y = peaks_plot()$baseline1,
                                           yend = Amplitude),
                              linetype = "dashed",size = 1.5, color = "red") +
        ggplot2::geom_segment(data = df_p,
                              ggplot2::aes(x = Peak_Occurence_Time,
                                           xend = Peak_Occurence_Time,
                                           y = puntominimo_y, yend = Amplitude),
                              linetype = "dashed",size = 1.5, color = "blue") +
        ggplot2::geom_segment(data =  df_p,
                              ggplot2::aes(x = Time_left_FWHP,
                                           xend = Time_right_FWHP,
                                           y = Prominence_Midpoint,
                                           yend = Prominence_Midpoint),
                              linetype = "solid",size = 1.5, color = "orange") +

        ggplot2::labs(title = "Calcium Trace",
                      x = "Time [s]",
                      y = "Fluorescence [a.u.]") +

        ggplot2::theme_classic() +

        ggplot2::theme(plot.title = ggplot2::element_text(size = 28,
                                                          face = "bold")) +

        ggplot2::theme(axis.title.y = ggplot2::element_text(size = 28,
                                                            face = "bold")) +
        ggplot2::theme(axis.title.x = ggplot2::element_text(size = 28,
                                                            face = "bold")) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(size = 16,
                                                           face = "bold")) +
        ggplot2::theme(axis.text.y = ggplot2::element_text(size = 16,
                                                           face = "bold"))


      if (input$raw_data == 2){
        gg3 <- gg3 + ggplot2::geom_line(data = data_raw,
                                        ggplot2::aes(x = data_raw[,1],
                                                     y = data_raw[,2]),
                                        color = "red" )
      }
      else{gg3 <- gg3 + ggplot2::geom_line( )}

      if(input$auc2==2){
        Integration_Reference <- peaks_plot()$baseline1
        gg3 <- gg3 +
          ggplot2::geom_hline(yintercept = Integration_Reference,
                              linetype = "dashed", color = "green") +
          ggplot2::geom_ribbon(data = subset(data_smoothed,
                                             Sing > Integration_Reference),
                               ggplot2::aes(ymax = Sing ,
                                            ymin = Integration_Reference),
                               fill = "green", alpha = 0.1)
      }
      else {gg3 <- gg3}

      if (input$FWHM==2){
        gg3 <- gg3 + ggplot2::geom_segment(data = df_p,
                                           ggplot2::aes(x = Time_left_FWHM,
                                                        xend = Time_right_FWHM,
                                                        y = Amplitude_Midpoint,
                                                        yend = Amplitude_Midpoint),
                                           linetype = "solid",size = 1.5, color = "Maroon 1")
      }
      else {gg3 <- gg3}
      gg3
    })




    # output$plot_component <- renderPlot({
    #   fileInput2 <- filedata()$fileInput2
    #   if (is.null(fileInput2)) {
    #     # Si fileInput2 es NULL, muestra un mensaje de error o información
    #     error_msg <- "Data insufficient for component visualization."
    #     print(error_msg)
    #     plot(0, type = "n", ann = TRUE, axes = TRUE)
    #     text(1, 0, error_msg, col = "red", cex = 1.5)
    #   }  else {
    #     contour_plot <- fileInput2$contour_plot
    #     primera_matriz <- contour_plot[, , 3]
    #     par(pty = "s")  # Ajusta los márgenes si es necesario
    #     # Personaliza la paleta de colores con un blanco más intenso
    #     colormap <- colorRampPalette(c("black", "red", "yellow", "white"), space = "rgb")(256)
    #     image(primera_matriz, col = colormap, axes = FALSE, xaxt = "n", yaxt = "n")
    #   }
    # })


    output$plot_raw_smoothed <- renderPlot({
      peaks_plot()$gg2
    })

    ###########Tabla que muestras en la aplicacion al ususario(for peaks)
    ###########Debe ser la misma tabla que imprime

    output$table_peaks2 <- DT::renderDataTable({
      df_p <- Peaks_Data_Final()$df_p
      df_p <- subset(df_p, select = c("Amplitude", "Peak_Occurence_Time",
                                      "Prominence", "FWHP", "FWHM", "Peak_Rise_Time",
                                      "Transient_Ocurrence_Time", "Rise_Rate"))
      df_p$Amplitude <- df_p$Amplitude - peaks_plot()$baseline1
      DT::datatable(df_p, caption = tags$caption(tags$strong("Transient Metrics")))
    })



    output$table_peaks22 <- DT::renderDT({  # tabla de las metricas que se muestran al ususario(for transiens)
      df_p <- Peaks_Data_Final()$df_p       # Debe ser la misma tabla que imprime
      time1 <- min(peaks_df()$data_raw$Time)
      time2 <- max(peaks_df()$data_raw$Time)
      Time_OnSet <- df_p$Transient_Ocurrence_Time[1]
      Frequency <- length(df_p$Amplitude)/(time2-time1)
      Baseline <- peaks_plot()$baseline1
      number_of_peaks <- length(df_p$Amplitude)
      df_p2 <- data.frame(Time_Onset = Time_OnSet, Frequency = Frequency, Baseline,
                          Number_of_Peaks= number_of_peaks)

      if (input$auc2==2){            #AUC TABLE
        Transient_Metrics <- cbind(df_p2, peaks_plot()$tabla_AUC)
      }
      else {Transient_Metrics <- df_p2}

      DT::datatable(Transient_Metrics,  options = list(
        pagingType = 'simple',
        dom = 't',
        autoWidth = TRUE
      ),caption = tags$caption(tags$strong("Trace Metrics")))
    })

    ###################################################
    ##### funcion de analisis de todos los transitorios
    ####################################################
    ####################################################

    all_trasien_peaks_df <- reactive({

      req(filedata()$fileInput)
      data = filedata()$fileInput
      data = t(data)
      colnames(data) = data[1,]
      data = data[-1,]

      ls <- list()
      ls1 <- list()
      ls2 <- list()
      for (i in 1:dim(data)[1]) {


        cell = i
        data_raw = data.frame(Time = as.numeric(colnames(data)),
                              signal = as.numeric(data[cell,]))


        ##### function loess for smoothed
        smoothed <- loess(signal ~ Time, data = data_raw ,
                          span = input$span)
        predictions <- predict(smoothed)
        df_smoothed <- data.frame(Time = data_raw$Time,
                                  signal = predictions)
        #####

        peaks_found <- peaks(data = df_smoothed,
                             nups=input$nups2,
                             ndowns = input$ndowns2,
                             minpeakheight = input$minpeakheight2,
                             minpeakdistance = input$minpeakdistance2)

        table_peak <- peaks_found$p_eak
        table_positions_peaks <- peaks_found$peak

        table_peak = table_peak  #tabla que muestra los piko
        table_positions_peaks = table_positions_peaks # tabla de las posiciones de los piko
        data_raw = data_raw  #data con la celula analizada
        data_smoothed = df_smoothed   # data suavizada

        peaks <- table_positions_peaks[,2]   # Índices donde se encuentran los picos
        data_putos_pekas = data.frame(x = data_smoothed[,1][peaks],
                                      y = data_smoothed[,2][peaks]) #puntos de los picos
        vertical_segments <- data.frame(x = data_smoothed[,1][peaks],
                                        yend = table_positions_peaks[,1])   # posicion del piko y su altura


        MSCPFP = Time_of_the_first_peak(data1 = data_smoothed,
                                        peak = table_positions_peaks)$cambios_menor_que_pfp # posicion donde hay un cambio en la primera dericada
        # para el primer pico

        data_min <- prominens2(data = data_smoothed,
                               peak = table_positions_peaks,
                               MSCPFP = MSCPFP)$data_min # puntos minimos donde empiezan los prominents
        df_peaks_parcia <- prominens2(data = data_smoothed,
                                      peak = table_positions_peaks,
                                      MSCPFP = MSCPFP)$df_peaks_parcia # el segmento del prominens

        time_start_increasin_peak <- prominens2(data = data_smoothed,
                                                peak = table_positions_peaks,
                                                MSCPFP = MSCPFP)$time_start_increasin_peak

        Puntos_medios <- FWHP2(peaks = data_smoothed[,1][peaks],
                               df_peaks_parcia = df_peaks_parcia)$Puntos_medios  # puntos medios de los prominances

        table_peak$prominence <- prominens2(data = data_smoothed,
                                            peak = table_positions_peaks,
                                            MSCPFP = MSCPFP)$prominens_amplitud  # valor de los prominens
        table_peak$Prominence_Midpoint <- Puntos_medios$p_eak_mediun # valor medio de las promineces

        first_time <- as.data.frame(response_time(data = data_smoothed,
                                                  peak = table_positions_peaks,
                                                  Puntos_medios = Puntos_medios)$first_time )  #primer tiempo
        second_time <- as.data.frame(response_time(data = data_smoothed,
                                                   peak = table_positions_peaks,
                                                   Puntos_medios = Puntos_medios)$second_time)  #segundo tiempo
        Tiempo_respose <- response_time(data = data_smoothed,
                                        peak = table_positions_peaks,
                                        Puntos_medios = Puntos_medios)$Tiempo_respose #tiempo de respuesta

        data_segmento_tiempo <- data.frame(x1 = first_time[1,1],
                                           x2 = second_time[1,1])

        right_left_FWHP <- right_left_FWHP(data1=data_smoothed, peak = table_positions_peaks,
                                           P_M = Puntos_medios)
        left_FWHP <- right_left_FWHP$df
        right_FWHP <- right_left_FWHP$df2

        table_peak$Time_left_FWHP <- left_FWHP$Time_left_FWHP
        table_peak$Time_right_FWHP <- right_FWHP$Time_right_FWHP

        table_peak$FWHP <- right_FWHP$Time_right_FWHP -left_FWHP$Time_left_FWHP

        table_peak$Time_to_peak <- table_peak$posision_peak - time_start_increasin_peak$Time

        table_peak$puntominimo_y <- prominens2(data = data_smoothed,
                                               peak = table_positions_peaks,
                                               MSCPFP = MSCPFP)$df_peaks_parcia$p_fin1

        df_p1 <- table_peak

        colnames(df_p1) <- c("Absolute_Amplitude", "Peak_Time", "L_inf", "L_sup",
                             "Prominence", "Prominence_Midpoint", "Time_left_FWHP",
                             "Time_right_FWHP", "FWHP", "Time_to_peak","puntominimo_y")
        ls[[i]] <- df_p1$Peak_Time
        ls1[[i]] <- df_p1$Time_to_peak
        ls2[[i]] <- df_p1$FWHP
      }

      ################### Peak_Time
      data_list <- ls
      data_df <- data.frame(Grupo = rep(1:length(data_list), sapply(data_list, length)), Valor = unlist(data_list))

      # Crear el panel de boxplots
      ls_plot <- ggplot2::ggplot(data_df, ggplot2::aes(x = factor(Grupo), y = Valor, group = factor(Grupo))) +
        ggplot2::geom_boxplot() +
        ggplot2::labs(x = "Components", y = "Peak Occurrence Time") +
        ggplot2::ggtitle("Box plot of all the components") +
        ggplot2::scale_x_discrete(breaks = seq(1, dim(data)[1], by = 10)) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
        #ggplot2::theme(axis.text = ggplot2::element_text(face = "bold")) +
        ggplot2::theme(
          plot.title = ggplot2::element_text(size = 20, face = "bold"),  # Tamaño y estilo del título
          axis.title.x = ggplot2::element_text(size = 18),               # Tamaño del título del eje x
          axis.title.y = ggplot2::element_text(size = 18)                # Tamaño del título del eje y
        ) + ggplot2::theme_minimal()
      #ggplot2::ggsave("f_13.png", plot = ls_plot, device = "png")


      ls_Box_plot <- ggplot2::ggplot(data_df, ggplot2::aes(x = Valor)) +
        ggplot2::geom_histogram(binwidth = 50, fill = "blue", color = "black") +
        ggplot2::labs(title = "Histogram of all Peak Occurrence Time", x = "Peak Occurrence Time", y = "Frequency") +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
        #ggplot2::theme(axis.text = ggplot2::element_text(face = "bold")) +
        ggplot2::theme(
          plot.title = ggplot2::element_text(size = 20, face = "bold"),  # Tamaño y estilo del título
          axis.title.x = ggplot2::element_text(size = 18),               # Tamaño del título del eje x
          axis.title.y = ggplot2::element_text(size = 18)                # Tamaño del título del eje y
        ) + ggplot2::theme_minimal()
      #ggplot2::ggsave("f_23.png", plot = ls_Box_plot, device = "png")
      ##################

      ################### Time_to_peak
      data_list1 <- ls1
      data_df1 <- data.frame(Grupo = rep(1:length(data_list1), sapply(data_list1, length)), Valor = unlist(data_list1))

      # Crear el panel de boxplots
      ls_plot1 <- ggplot2::ggplot(data_df1, ggplot2::aes(x = factor(Grupo), y = Valor, group = factor(Grupo))) +
        ggplot2::geom_boxplot() +
        ggplot2::labs(x = "Components", y = "Peak Rise Time") +
        ggplot2::ggtitle("Box plot of all the components") +
        ggplot2::scale_x_discrete(breaks = seq(1, dim(data)[1], by = 10)) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
        ggplot2::theme(
          plot.title = ggplot2::element_text(size = 20, face = "bold"),  # Tamaño y estilo del título
          axis.title.x = ggplot2::element_text(size = 18),               # Tamaño del título del eje x
          axis.title.y = ggplot2::element_text(size = 18)                # Tamaño del título del eje y
        ) + ggplot2::theme_minimal()
      #ggplot2::ggsave("f_33.png", plot = ls_plot1, device = "png")

      ls_Box_plot1 <- ggplot2::ggplot(data_df1, ggplot2::aes(x = Valor)) +
        ggplot2::geom_histogram(binwidth = 50, fill = "blue", color = "black") +
        ggplot2::labs(title = "Histogram of all Peak Rise Time", x = "Peak Rise Time", y = "Frequency") +
        #ggplot2::theme(axis.text = ggplot2::element_text(face = "bold")) +
        ggplot2::theme(
          plot.title = ggplot2::element_text(size = 20, face = "bold"),  # Tamaño y estilo del título
          axis.title.x = ggplot2::element_text(size = 18),               # Tamaño del título del eje x
          axis.title.y = ggplot2::element_text(size = 18)                # Tamaño del título del eje y
        ) + ggplot2::theme_minimal()
      #ggplot2::ggsave("f_43.png", plot = ls_Box_plot1, device = "png")
      ##################

      ################### FWHP
      data_list2 <- ls2
      data_df2 <- data.frame(Grupo = rep(1:length(data_list2), sapply(data_list2, length)), Valor = unlist(data_list2))

      # Crear el panel de boxplots
      ls_plot2 <- ggplot2::ggplot(data_df2, ggplot2::aes(x = factor(Grupo), y = Valor, group = factor(Grupo))) +
        ggplot2::geom_boxplot() +
        ggplot2::labs(x = "Components", y = "FWHP") +
        ggplot2::ggtitle("Box plot of all the components") +
        ggplot2::scale_x_discrete(breaks = seq(1, dim(data)[1], by = 10)) +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
        ggplot2::theme(
          plot.title = ggplot2::element_text(size = 20, face = "bold"),  # Tamaño y estilo del título
          axis.title.x = ggplot2::element_text(size = 18),               # Tamaño del título del eje x
          axis.title.y = ggplot2::element_text(size = 18)                # Tamaño del título del eje y
        ) + ggplot2::theme_minimal()
      #ggplot2::ggsave("f_53.png", plot = ls_plot2, device = "png")


      ls_Box_plot2 <- ggplot2::ggplot(data_df2, ggplot2::aes(x = Valor)) +
        ggplot2::geom_histogram(binwidth = 50, fill = "blue", color = "black") +
        ggplot2::labs(title = "Histogram of all FWHP", x = "FWHP", y = "Frequency") +
        #ggplot2::theme(axis.text = ggplot2::element_text(face = "bold")) +
        ggplot2::theme(
          plot.title = ggplot2::element_text(size = 20, face = "bold"),  # Tamaño y estilo del título
          axis.title.x = ggplot2::element_text(size = 18),               # Tamaño del título del eje x
          axis.title.y = ggplot2::element_text(size = 18)                # Tamaño del título del eje y
        ) + ggplot2::theme_minimal()
      #ggplot2::ggsave("f_63.png", plot = ls_Box_plot2, device = "png")
      ##################


      panel <- gridExtra::grid.arrange(
        ggplot2::ggplotGrob(ls_plot), ggplot2::ggplotGrob(ls_Box_plot),
        ggplot2::ggplotGrob(ls_plot1), ggplot2::ggplotGrob(ls_Box_plot1),
        ggplot2::ggplotGrob(ls_plot2), ggplot2::ggplotGrob(ls_Box_plot2),
        ncol = 2
      )
      #ggplot2::ggsave("panel3.png", plot = panel, device = "png", dpi = 300)


      return(list(ls2 = ls2, ls_plot = ls_plot, ls_Box_plot = ls_Box_plot,
                  ls_plot1 = ls_plot1, ls_Box_plot1 = ls_Box_plot1,
                  ls_plot2 = ls_plot2, ls_Box_plot2 = ls_Box_plot2, panel = panel))
    })


    output$panel <- renderPlot({
      all_trasien_peaks_df()$panel
    })

    # output$outputList <- renderPrint({
    #   #all_trasien_peaks_df()$ls2
    # })

    # output$plot_ls <- renderPlot({
    #   all_trasien_peaks_df()$ls_plot
    # })
    #
    # output$plot_box_ls <- renderPlot({
    #   all_trasien_peaks_df()$ls_Box_plot
    # })
    #
    # output$plot_ls1 <- renderPlot({
    #   all_trasien_peaks_df()$ls_plot1
    # })
    #
    # output$plot_box_ls1 <- renderPlot({
    #   all_trasien_peaks_df()$ls_Box_plot1
    # })
    #
    # output$plot_ls2 <- renderPlot({
    #   all_trasien_peaks_df()$ls_plot2
    # })
    #
    # output$plot_box_ls2 <- renderPlot({
    #   all_trasien_peaks_df()$ls_Box_plot2
    # })


    #downloadData Transient_Metrics

    output$descargar <- downloadHandler(
      filename = function() {
        paste("Transient_Metrics", ".csv", sep = "")

      },
      content = function(file) {

        df_p <- Peaks_Data_Final()$df_p
        time1 <- min(peaks_df()$data_raw$Time)
        time2 <- max(peaks_df()$data_raw$Time)
        Time_OnSet <- df_p$Transient_Ocurrence_Time[1]
        Frequency <- length(df_p$Amplitude)/(time2-time1)
        Baseline <- peaks_plot()$baseline1
        number_of_peaks <- length(df_p$Amplitude)
        id <- 1
        df_p2 <- data.frame(id = id, Time_Onset = Time_OnSet, Frequency = Frequency,
                            Baseline, Number_of_Peaks = number_of_peaks)

        if (input$auc2==2){            #AUC TABLE
          Transient_Metrics <- cbind(df_p2, peaks_plot()$tabla_AUC)
        }
        else {Transient_Metrics <- df_p2}

        write.csv(Transient_Metrics, file, row.names = FALSE)
      }
    )



    #download Trace_Metrics
    output$descargarP <- downloadHandler(
      filename = function() {
        paste("Trace_Metrics", ".csv", sep = "")

      },
      content = function(file) {
        df_p <- Peaks_Data_Final()$df_p
        df_p$Amplitude <- df_p$Amplitude - peaks_plot()$baseline1
        df_p$id <- seq(1:length(df_p$Amplitude))
        df_p <- subset(df_p, select = c("id","Amplitude", "Peak_Occurence_Time",
                                        "Prominence", "FWHP", "FWHM", "Peak_Rise_Time",
                                        "Transient_Ocurrence_Time", "Rise_Rate"))
        write.csv(df_p, file, row.names = FALSE)
      }
    )




  })
}

## To be copied in the UI
# mod_Denoising_data_ui("Denoising_data_1")

## To be copied in the server
# mod_Denoising_data_server("Denoising_data_1")
