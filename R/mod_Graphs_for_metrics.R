#' Graphs_for_metrics UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Graphs_for_metrics_ui <- function(id){
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(width = 4,
                   fileInput(ns("replica1"),
                             accept = c('text/csv',
                                        'text/comma-separated-values,text/plain',
                                        '.csv'),
                             label = h5("Replica1")),

                   fileInput(ns("replica2"),
                             accept = c('text/csv',
                                        'text/comma-separated-values,text/plain',
                                        '.csv'),
                             label = h5("Replica2")),

                   fileInput(ns("replica3"),
                             accept = c('text/csv',
                                        'text/comma-separated-values,text/plain',
                                        '.csv'),
                             label = h5("Replica3")),
                   radioButtons(inputId = ns("Log_Data"),
                                label = "Log Data",
                                choices = list("yes" = 1,"no" = 2)
                   ),
                   numericInput(inputId = ns("span1"),
                                label = "Smoothness Control:",
                                value = 0.05, min = 0, max = 1,step = 0.01),

                   textInput(ns("transiens"), label = "Transiens", value = "1,2,3"),
                   ),
      mainPanel(
        tabsetPanel(type = "tabs",
                    tabPanel("SummaryData",
                             DT::DTOutput(ns("SummaryData1")),
                             DT::DTOutput(ns("SummaryData2")),
                             DT::DTOutput(ns("SummaryData3"))

                    ),
                    tabPanel("Graphs",
                             plotOutput(ns("Graph_1")),
                             DT::DTOutput(ns("GraphData1"))

                             )

        )
        )
      )

  )
}

#' Graphs_for_metrics Server Functions
#'
#' @noRd
mod_Graphs_for_metrics_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    filedata1 <- reactive({
      req(input$replica1)
      ext <- tools::file_ext(input$replica1$name)
      fileInput1 <- load_file(input$replica1$name, input$replica1$datapath, ext)

      if (ext %in% c("csv", "tsv")) {
        fileInput <- as.data.frame(fileInput1)
        fileInput2 <- NULL
      }

      else if (ext == "json") {
        fileInput2 <- fileInput1
        comp <- fileInput2$components
        com <- t(fileInput2$components)
        time <- seq(0, fileInput2$image_data[2]-1, by = 1)*fileInput2$image_data[1]
        com <- cbind(time, com)
        fileInput <- com
      }
      return(list(fileInput = fileInput, fileInput2 = fileInput2))
    })

    filedata2 <- reactive({
      req(input$replica2)
      ext <- tools::file_ext(input$replica2$name)
      fileInput1 <- load_file(input$replica2$name, input$replica2$datapath, ext)

      if (ext %in% c("csv", "tsv")) {
        fileInput <- as.data.frame(fileInput1)
        fileInput2 <- NULL
      }

      else if (ext == "json") {
        fileInput2 <- fileInput1
        comp <- fileInput2$components
        com <- t(fileInput2$components)
        time <- seq(0, fileInput2$image_data[2]-1, by = 1)*fileInput2$image_data[1]
        com <- cbind(time, com)
        fileInput <- com
      }
      return(list(fileInput = fileInput, fileInput2 = fileInput2))
    })

    filedata3 <- reactive({
      req(input$replica3)
      ext <- tools::file_ext(input$replica3$name)
      fileInput1 <- load_file(input$replica3$name, input$replica3$datapath, ext)

      if (ext %in% c("csv", "tsv")) {
        fileInput <- as.data.frame(fileInput1)
        fileInput2 <- NULL
      }

      else if (ext == "json") {
        fileInput2 <- fileInput1
        comp <- fileInput2$components
        com <- t(fileInput2$components)
        time <- seq(0, fileInput2$image_data[2]-1, by = 1)*fileInput2$image_data[1]
        com <- cbind(time, com)
        fileInput <- com
      }
      return(list(fileInput = fileInput, fileInput2 = fileInput2))
    })

    data_info1 <- reactive({
      req(filedata1()$fileInput)
      Nobservations <- nrow(filedata1()$fileInput)
      Ncells <- ncol(filedata1()$fileInput)-1
      SummaryData <- data.frame(list(Number = c(Ncells, Nobservations)))
      rownames(SummaryData) <- c("Components", "Time observations")
      list(SummaryData = SummaryData, data = data.frame(filedata1()$fileInput,row.names = NULL))
    })

    data_info2 <- reactive({
      req(filedata2()$fileInput)
      Nobservations <- nrow(filedata2()$fileInput)
      Ncells <- ncol(filedata2()$fileInput)-1
      SummaryData <- data.frame(list(Number = c(Ncells, Nobservations)))
      rownames(SummaryData) <- c("Components", "Time observations")
      list(SummaryData = SummaryData, data = data.frame(filedata2()$fileInput,row.names = NULL))
    })

    data_info3 <- reactive({
      req(filedata3()$fileInput)
      Nobservations <- nrow(filedata3()$fileInput)
      Ncells <- ncol(filedata3()$fileInput)-1
      SummaryData <- data.frame(list(Number = c(Ncells, Nobservations)))
      rownames(SummaryData) <- c("Components", "Time observations")
      list(SummaryData = SummaryData, data = data.frame(filedata3()$fileInput,row.names = NULL))
    })

    output$SummaryData1 <- DT::renderDataTable({
      df <- data_info1()$data
      DT::datatable(df)
    })

    output$SummaryData2 <- DT::renderDataTable({
      df <- data_info2()$data
      DT::datatable(df)
    })

    output$SummaryData3 <- DT::renderDataTable({
      df <- data_info3()$data
      DT::datatable(df)
    })

    peaks_df1 <- reactive({

    graphg <- Metrics(fileInput_1 = filedata1()$fileInput, nups3 = 1, ndowns3 = 1,
                       minpeakheight3 = 0, minpeakdistance3 = 1, span = input$span1)

    Peak_Time1 <- graphg$ls
    Peak_to_Time1 <- graphg$ls1
    FWHM1 <- graphg$ls2
    Transient_Ocurrence_Time1 <- graphg$ls3
    Time_OnSet1 <- graphg$ls4
    Frequency1 <- graphg$ls5
    Baseline1 <- graphg$ls6
    AUC1 <- graphg$ls7
    Amplitude1 <- graphg$ls8
    Number_of_peaks1 <- graphg$ls9
    Rise1 <- graphg$ls10



    Peak_Time <- unlist(Peak_Time1)
    Peak_to_Time <- unlist(Peak_to_Time1)
    FWHM <- unlist(FWHM1)
    Transient_Ocurrence_Time <- unlist(Transient_Ocurrence_Time1)
    Time_OnSet <- unlist(Time_OnSet1)
    Frequency <- unlist(Frequency1)
    Baseline <- unlist(Baseline1)
    AUC <- unlist(AUC1)
    Amplitude <- unlist(Amplitude1)
    Number_of_peaks <- unlist(Number_of_peaks1)
    Rise <- unlist(Rise1)

    df_grap <- data.frame(Peak_Time = Peak_Time, Peak_to_Time  = Peak_to_Time,
                FWHM = FWHM, Transient_Ocurrence_Time = Transient_Ocurrence_Time,
                Baseline = Baseline, Amplitude = Amplitude, Rise = Rise)


    return(list(Peak_Time = Peak_Time, Peak_to_Time  = Peak_to_Time,
                FWHM = FWHM, df_grap = df_grap, Time_OnSet = Time_OnSet,
                Frequency = Frequency, AUC = AUC, Number_of_peaks = Number_of_peaks))
    })
    peaks_df2 <- reactive({

      graphg <- Metrics(fileInput_1 = filedata2()$fileInput, nups3 = 1, ndowns3 = 1,
                        minpeakheight3 = 0, minpeakdistance3 = 1, span = input$span1)

      Peak_Time1 <- graphg$ls
      Peak_to_Time1 <- graphg$ls1
      FWHM1 <- graphg$ls2
      Transient_Ocurrence_Time1 <- graphg$ls3
      Time_OnSet1 <- graphg$ls4
      Frequency1 <- graphg$ls5
      Baseline1 <- graphg$ls6
      AUC1 <- graphg$ls7
      Amplitude1 <- graphg$ls8
      Number_of_peaks1 <- graphg$ls9
      Rise1 <- graphg$ls10


      Peak_Time <- unlist(Peak_Time1)
      Peak_to_Time <- unlist(Peak_to_Time1)
      FWHM <- unlist(FWHM1)
      Transient_Ocurrence_Time <- unlist(Transient_Ocurrence_Time1)
      Time_OnSet <- unlist(Time_OnSet1)
      Frequency <- unlist(Frequency1)
      Baseline <- unlist(Baseline1)
      AUC <- unlist(AUC1)
      Amplitude <- unlist(Amplitude1)
      Number_of_peaks <- unlist(Number_of_peaks1)
      Rise <- unlist(Rise1)

      df_grap <- data.frame(Peak_Time = Peak_Time, Peak_to_Time  = Peak_to_Time,
                            FWHM = FWHM, Transient_Ocurrence_Time = Transient_Ocurrence_Time,
                            Baseline = Baseline, Amplitude = Amplitude, Rise = Rise)

      return(list(Peak_Time = Peak_Time, Peak_to_Time  = Peak_to_Time,
                  FWHM = FWHM, df_grap = df_grap, Time_OnSet = Time_OnSet,
                  Frequency = Frequency, AUC = AUC, Number_of_peaks = Number_of_peaks))
    })
    peaks_df3 <- reactive({

      graphg <- Metrics(fileInput_1 = filedata3()$fileInput, nups3 = 1, ndowns3 = 1,
                        minpeakheight3 = 0, minpeakdistance3 = 1, span = input$span1)

      Peak_Time1 <- graphg$ls
      Peak_to_Time1 <- graphg$ls1
      FWHM1 <- graphg$ls2
      Transient_Ocurrence_Time1 <- graphg$ls3
      Time_OnSet1 <- graphg$ls4
      Frequency1 <- graphg$ls5
      Baseline1 <- graphg$ls6
      AUC1 <- graphg$ls7
      Amplitude1 <- graphg$ls8
      Number_of_peaks1 <- graphg$ls9
      Rise1 <- graphg$ls10


      Peak_Time <- unlist(Peak_Time1)
      Peak_to_Time <- unlist(Peak_to_Time1)
      FWHM <- unlist(FWHM1)
      Transient_Ocurrence_Time <- unlist(Transient_Ocurrence_Time1)
      Time_OnSet <- unlist(Time_OnSet1)
      Frequency <- unlist(Frequency1)
      Baseline <- unlist(Baseline1)
      AUC <- unlist(AUC1)
      Amplitude <- unlist(Amplitude1)
      Number_of_peaks <- unlist(Number_of_peaks1)
      Rise <- unlist(Rise1)


      df_grap <- data.frame(Peak_Time = Peak_Time, Peak_to_Time  = Peak_to_Time,
                            FWHM = FWHM, Transient_Ocurrence_Time = Transient_Ocurrence_Time,
                            Baseline = Baseline, Amplitude = Amplitude, Rise = Rise)

      return(list(Peak_Time = Peak_Time, Peak_to_Time  = Peak_to_Time,
                  FWHM = FWHM, df_grap = df_grap, Time_OnSet = Time_OnSet,
                  Frequency = Frequency, AUC = AUC, Number_of_peaks = Number_of_peaks))
    })

    grahpg_Peak_Rise_Time <- reactive({
      Peak_Rise_Time_n1 <- peaks_df1()$df_grap$Peak_to_Time
      Peak_Rise_Time_n2 <- peaks_df2()$df_grap$Peak_to_Time
      Peak_Rise_Time_n3 <- peaks_df3()$df_grap$Peak_to_Time

      grupo1 <- data.frame(Replica = "n = 1", Valor = Peak_Rise_Time_n1)
      grupo2 <- data.frame(Replica = "n = 2", Valor = Peak_Rise_Time_n2)
      grupo3 <- data.frame(Replica = "n = 3", Valor = Peak_Rise_Time_n3)

      datos_Peak_Rise_Time <- bind_rows(grupo1, grupo2, grupo3)

      graphip1 <- ggplot2::ggplot(datos_Peak_Rise_Time, ggplot2::aes(x = Replica, y = Valor, fill = Replica)) +
        ggplot2::geom_boxplot() +
        ggplot2::labs(title = "Peak Rise Time",
             x = "",
             y = "[s]") +
        ggplot2::theme_minimal()

      graphip1 +
        ggplot2::theme(axis.text.x = ggplot2::element_text(face = "bold", size = 20),
              axis.text.y = ggplot2::element_text(face = "bold", size = 20),
              axis.title = ggplot2::element_text(face = "bold", size = 20))
      ##########################################################################
      Peak_Ocurrence_Time_n1 <- peaks_df1()$df_grap$Peak_Time
      Peak_Ocurrence_Time_n2 <- peaks_df2()$df_grap$Peak_Time
      Peak_Ocurrence_Time_n3 <- peaks_df3()$df_grap$Peak_Time

      grupo12 <- data.frame(Replica = "n = 1", Valor = Peak_Ocurrence_Time_n1)
      grupo22 <- data.frame(Replica = "n = 2", Valor = Peak_Ocurrence_Time_n2)
      grupo32 <- data.frame(Replica = "n = 3", Valor = Peak_Ocurrence_Time_n3)

      datos_Peak_Ocurrence_Time <- bind_rows(grupo12, grupo22, grupo32)

      graphip12 <- ggplot2::ggplot(datos_Peak_Ocurrence_Time, ggplot2::aes(x = Replica, y = Valor, fill = Replica)) +
        ggplot2::geom_boxplot() +
        ggplot2::labs(title = "Peak Ocurrence Time",
                      x = "",
                      y = "[s]") +
        ggplot2::theme_minimal()

      graphip12 +
        ggplot2::theme(axis.text.x = ggplot2::element_text(face = "bold", size = 20),
                       axis.text.y = ggplot2::element_text(face = "bold", size = 20),
                       axis.title = ggplot2::element_text(face = "bold", size = 20))
      ##########################################################################
      FWHM_n1 <- peaks_df1()$df_grap$FWHM
      FWHM_n2 <- peaks_df2()$df_grap$FWHM
      FWHM_n3 <- peaks_df3()$df_grap$FWHM

      grupo13 <- data.frame(Replica = "n = 1", Valor = FWHM_n1)
      grupo23 <- data.frame(Replica = "n = 2", Valor = FWHM_n2)
      grupo33 <- data.frame(Replica = "n = 3", Valor = FWHM_n3)

      datos_FWHM <- bind_rows(grupo13, grupo23, grupo33)

      graphip13 <- ggplot2::ggplot(datos_FWHM, ggplot2::aes(x = Replica, y = Valor, fill = Replica)) +
        ggplot2::geom_boxplot() +
        ggplot2::labs(title = "FWHM",
                      x = "",
                      y = "[s]") +
        ggplot2::theme_minimal()

      graphip13 +
        ggplot2::theme(axis.text.x = ggplot2::element_text(face = "bold", size = 20),
                       axis.text.y = ggplot2::element_text(face = "bold", size = 20),
                       axis.title = ggplot2::element_text(face = "bold", size = 20))

      ##########################################################################
      Transient_Ocurrence_Time_n1 <- peaks_df1()$df_grap$Transient_Ocurrence_Time
      Transient_Ocurrence_Time_n2 <- peaks_df2()$df_grap$Transient_Ocurrence_Time
      Transient_Ocurrence_Time_n3 <- peaks_df3()$df_grap$Transient_Ocurrence_Time

      grupo14 <- data.frame(Replica = "n = 1", Valor = Transient_Ocurrence_Time_n1)
      grupo24 <- data.frame(Replica = "n = 2", Valor = Transient_Ocurrence_Time_n2)
      grupo34 <- data.frame(Replica = "n = 3", Valor = Transient_Ocurrence_Time_n3)

      datos_Transient_Ocurrence_Time <- bind_rows(grupo14, grupo24, grupo34)

      graphip14 <- ggplot2::ggplot(datos_Transient_Ocurrence_Time,
                                   ggplot2::aes(x = Replica, y = Valor, fill = Replica)) +
        ggplot2::geom_boxplot() +
        ggplot2::labs(title = "Transient Ocurrence Time",
                      x = "",
                      y = "[s]") +
        ggplot2::theme_minimal()

      graphip14 +
        ggplot2::theme(axis.text.x = ggplot2::element_text(face = "bold", size = 20),
                       axis.text.y = ggplot2::element_text(face = "bold", size = 20),
                       axis.title = ggplot2::element_text(face = "bold", size = 20))
      ##########################################################################

      Time_OnSet_n1 <- peaks_df1()$Time_OnSet
      Time_OnSet_n2 <- peaks_df2()$Time_OnSet
      Time_OnSet_n3 <- peaks_df3()$Time_OnSet

      grupo15 <- data.frame(Replica = "n = 1", Valor = Time_OnSet_n1)
      grupo25 <- data.frame(Replica = "n = 2", Valor = Time_OnSet_n2)
      grupo35 <- data.frame(Replica = "n = 3", Valor = Time_OnSet_n3)

      datos_Time_OnSet <- bind_rows(grupo15, grupo25, grupo35)

      graphip15 <- ggplot2::ggplot(datos_Time_OnSet, ggplot2::aes(x = Replica, y = Valor, fill = Replica)) +
        ggplot2::geom_boxplot() +
        ggplot2::labs(title = "Time OnSet",
                      x = "",
                      y = "[s]") +
        ggplot2::theme_minimal()

      graphip15 +
        ggplot2::theme(axis.text.x = ggplot2::element_text(face = "bold", size = 20),
                       axis.text.y = ggplot2::element_text(face = "bold", size = 20),
                       axis.title = ggplot2::element_text(face = "bold", size = 20))

      ##########################################################################

      Frequency_n1 <- peaks_df1()$Frequency
      Frequency_n2 <- peaks_df2()$Frequency
      Frequency_n3 <- peaks_df3()$Frequency

      grupo16 <- data.frame(Replica = "n = 1", Valor = Frequency_n1)
      grupo26 <- data.frame(Replica = "n = 2", Valor = Frequency_n2)
      grupo36 <- data.frame(Replica = "n = 3", Valor = Frequency_n3)

      datos_Time_OnSet <- bind_rows(grupo16, grupo26, grupo36)

      graphip16 <- ggplot2::ggplot(datos_Time_OnSet, ggplot2::aes(x = Replica, y = Valor, fill = Replica)) +
        ggplot2::geom_boxplot() +
        ggplot2::labs(title = "Frequency",
                      x = "",
                      y = "[s]") +
        ggplot2::theme_minimal()

      graphip16 +
        ggplot2::theme(axis.text.x = ggplot2::element_text(face = "bold", size = 20),
                       axis.text.y = ggplot2::element_text(face = "bold", size = 20),
                       axis.title = ggplot2::element_text(face = "bold", size = 20))

      ##########################################################################
      Baseline_n1 <- peaks_df1()$df_grap$Baseline
      Baseline_n2 <- peaks_df2()$df_grap$Baseline
      Baseline_n3 <- peaks_df3()$df_grap$Baseline

      grupo17 <- data.frame(Replica = "n = 1", Valor = Baseline_n1)
      grupo27 <- data.frame(Replica = "n = 2", Valor = Baseline_n2)
      grupo37 <- data.frame(Replica = "n = 3", Valor = Baseline_n3)

      datos_Baseline <- bind_rows(grupo17, grupo27, grupo37)

      graphip17 <- ggplot2::ggplot(datos_Baseline, ggplot2::aes(x = Replica, y = Valor, fill = Replica)) +
        ggplot2::geom_boxplot() +
        ggplot2::labs(title = "Baseline",
                      x = "",
                      y = expression(paste(Delta * F / F[0]))) +
        ggplot2::theme_minimal()

      graphip17 +
        ggplot2::theme(axis.text.x = ggplot2::element_text(face = "bold", size = 20),
                       axis.text.y = ggplot2::element_text(face = "bold", size = 20),
                       axis.title = ggplot2::element_text(face = "bold", size = 20))

      ##########################################################################
      AUC_n1 <- peaks_df1()$AUC
      AUC_n2 <- peaks_df2()$AUC
      AUC_n3 <- peaks_df3()$AUC

      grupo18 <- data.frame(Replica = "n = 1", Valor = AUC_n1)
      grupo28 <- data.frame(Replica = "n = 2", Valor = AUC_n2)
      grupo38 <- data.frame(Replica = "n = 3", Valor = AUC_n3)

      datos_AUC <- bind_rows(grupo18, grupo28, grupo38)

      graphip18 <- ggplot2::ggplot(datos_AUC, ggplot2::aes(x = Replica, y = Valor, fill = Replica)) +
        ggplot2::geom_boxplot() +
        ggplot2::labs(title = "AUC",
                      x = "",
                      y = expression(paste(Delta * F / F[0] , 'x', s))) +
        ggplot2::theme_minimal()

      graphip18 +
        ggplot2::theme(axis.text.x = ggplot2::element_text(face = "bold", size = 30),
                       axis.text.y = ggplot2::element_text(face = "bold", size = 30),
                       axis.title = ggplot2::element_text(face = "bold", size = 30))

      ##########################################################################
      Amplitude_n1 <- peaks_df1()$df_grap$Amplitude
      Amplitude_n2 <- peaks_df2()$df_grap$Amplitude
      Amplitude_n3 <- peaks_df3()$df_grap$Amplitude

      grupo19 <- data.frame(Replica = "n = 1", Valor = Amplitude_n1)
      grupo29 <- data.frame(Replica = "n = 2", Valor = Amplitude_n2)
      grupo39 <- data.frame(Replica = "n = 3", Valor = Amplitude_n3)

      datos_Amplitude <- bind_rows(grupo19, grupo29, grupo39)

      graphip19 <- ggplot2::ggplot(datos_Amplitude, ggplot2::aes(x = Replica, y = Valor, fill = Replica)) +
        ggplot2::geom_boxplot() +
        ggplot2::labs(title = "Amplitude",
                      x = "",
                      y = expression(paste(Delta * F / F[0]))) +
        ggplot2::theme_minimal()

      graphip19 +
        ggplot2::theme(axis.text.x = ggplot2::element_text(face = "bold", size = 20),
                       axis.text.y = ggplot2::element_text(face = "bold", size = 20),
                       axis.title = ggplot2::element_text(face = "bold", size = 20))
      ##########################################################################
      Number_of_peaks_n1 <- peaks_df1()$Number_of_peaks
      Number_of_peaks_n2 <- peaks_df2()$Number_of_peaks
      Number_of_peaks_n3 <- peaks_df3()$Number_of_peaks

      grupo110 <- data.frame(Replica = "n = 1", Valor = Number_of_peaks_n1)
      grupo210 <- data.frame(Replica = "n = 2", Valor = Number_of_peaks_n2)
      grupo310 <- data.frame(Replica = "n = 3", Valor = Number_of_peaks_n3)

      datos_Number_of_peaks <- bind_rows(grupo110, grupo210, grupo310)

      graphip110 <- ggplot2::ggplot(datos_Number_of_peaks, ggplot2::aes(x = Replica, y = Valor, fill = Replica)) +
        ggplot2::geom_boxplot() +
        ggplot2::labs(title = "Number of peaks",
                      x = "",
                      y = "Peaks") +
        ggplot2::theme_minimal()

      graphip110 +
        ggplot2::theme(axis.text.x = ggplot2::element_text(face = "bold", size = 20),
                       axis.text.y = ggplot2::element_text(face = "bold", size = 20),
                       axis.title = ggplot2::element_text(face = "bold", size = 20))
      ##########################################################################
      Rise_n1 <- peaks_df1()$df_grap$Rise
      Rise_n2 <- peaks_df2()$df_grap$Rise
      Rise_n3 <- peaks_df3()$df_grap$Rise

      grupo111 <- data.frame(Replica = "n = 1", Valor = Rise_n1)
      grupo211 <- data.frame(Replica = "n = 2", Valor = Rise_n2)
      grupo311 <- data.frame(Replica = "n = 3", Valor = Rise_n3)

      datos_Rise <- bind_rows(grupo111, grupo211, grupo311)

      graphip111 <- ggplot2::ggplot(datos_Rise, ggplot2::aes(x = Replica, y = Valor, fill = Replica)) +
        ggplot2::geom_boxplot() +
        ggplot2::labs(title = "Rise",
                      x = "",
                      y = expression(paste(Delta * F / F[0] , 'x', 1/s))) +
        ggplot2::theme_minimal()

      graphip111 +
        ggplot2::theme(axis.text.x = ggplot2::element_text(face = "bold", size = 20),
                       axis.text.y = ggplot2::element_text(face = "bold", size = 20),
                       axis.title = ggplot2::element_text(face = "bold", size = 20))

      ########### Transient graph ###############################################

      cadena_texto <- input$transiens

      # Convertir la cadena en un vector numérico
      numeros_separados <- unlist(strsplit(cadena_texto, split = ","))
      numeros_numericos <- as.numeric(numeros_separados)

      # Verificar si los números se pueden convertir correctamente
      if (!any(is.na(numeros_numericos))) {
        cell1 <- numeros_numericos[1]
      } else {
        "Error: Por favor, introduce números separados por comas."
      }



      req(filedata1()$fileInput)
      data1 = filedata1()$fileInput
      data1 = t(data1)
      colnames(data1) = data1[1,]
      data1 = data1[-1,]
      data_raw1 = data.frame(Time = as.numeric(colnames(data1)),
                            signal = as.numeric(data1[cell1,]))
      ##### function loess for smoothed
      smoothed1 <- loess(signal ~ Time, data = data_raw1 , span = 0.05)
      predictions1 <- predict(smoothed1)
      df_smoothed1 <- data.frame(Time = data_raw1$Time, signal = predictions1)
      ########################################################################

      if (!any(is.na(numeros_numericos))) {
        cell2 <- numeros_numericos[2]
      } else {
        "Error: Por favor, introduce números separados por comas."
      }

      req(filedata2()$fileInput)
      data2 = filedata2()$fileInput
      data2 = t(data2)
      colnames(data2) = data2[1,]
      data2 = data2[-1,]
      data_raw2 = data.frame(Time = as.numeric(colnames(data2)),
                             signal = as.numeric(data2[cell2,]))
      ##### function loess for smoothed
      smoothed2 <- loess(signal ~ Time, data = data_raw2 , span = 0.05)
      predictions2 <- predict(smoothed2)
      df_smoothed2 <- data.frame(Time = data_raw2$Time, signal = predictions2)
      ########################################################################
      if (!any(is.na(numeros_numericos))) {
        cell3 <- numeros_numericos[3]
      } else {
        "Error: Por favor, introduce números separados por comas."
      }


      req(filedata3()$fileInput)
      data3 = filedata3()$fileInput
      data3 = t(data3)
      colnames(data3) = data3[1,]
      data3 = data3[-1,]
      data_raw3 = data.frame(Time = as.numeric(colnames(data3)),
                             signal = as.numeric(data3[cell3,]))
      ##### function loess for smoothed
      smoothed3 <- loess(signal ~ Time, data = data_raw3 , span = 0.05)
      predictions3 <- predict(smoothed3)
      df_smoothed3 <- data.frame(Time = data_raw3$Time, signal = predictions3)

      ########################################################################

      df_smoothed11 <- data.frame(Valor = df_smoothed1$signal, Time = df_smoothed1$Time, Curve = "1")
      df_smoothed12 <- data.frame(Valor = df_smoothed2$signal, Time = df_smoothed2$Time, Curve = "2")
      df_smoothed13 <- data.frame(Valor = df_smoothed3$signal, Time = df_smoothed3$Time, Curve = "3")

      data_Transient <- bind_rows(df_smoothed11, df_smoothed12, df_smoothed13)

      graphip21 <- ggplot2::ggplot(data = data_Transient, ggplot2::aes(x = Time, y = Valor, color = Curve)) +
        ggplot2::geom_line() +
        ggplot2::labs(title = "",
             x = "Time",
             y = expression(paste(Delta * F / F[0])),
             color = "Curves") +
        ggplot2::theme_minimal()

      graphip21 +
        ggplot2::theme(axis.text.x = ggplot2::element_text(face = "bold", size = 20),
                       axis.text.y = ggplot2::element_text(face = "bold", size = 20),
                       axis.title = ggplot2::element_text(face = "bold", size = 20))



      panel_Transient_metrics2 <- ggpubr::ggarrange(graphip21, graphip1,
                                                    graphip12, graphip13,
                                                    graphip14, graphip15,
                                                    graphip16, graphip17,
                                                    graphip18, graphip19,
                                                    graphip110, graphip111,
                                                    nrow = 3, ncol = 4 )
      ggplot2::ggsave("Panel_metric_Shelly.png", plot = panel_Transient_metrics2, device = "png", dpi = 300)

      return(list(panel_Transient_metrics2 = panel_Transient_metrics2))
    })

    grahpg_Peak_Rise_Time_log <- reactive({
      Peak_Rise_Time_n1 <- log(peaks_df1()$df_grap$Peak_to_Time)
      Peak_Rise_Time_n2 <- log(peaks_df2()$df_grap$Peak_to_Time)
      Peak_Rise_Time_n3 <- log(peaks_df3()$df_grap$Peak_to_Time)

      grupo1 <- data.frame(Replica = "n = 1", Valor = Peak_Rise_Time_n1)
      grupo2 <- data.frame(Replica = "n = 2", Valor = Peak_Rise_Time_n2)
      grupo3 <- data.frame(Replica = "n = 3", Valor = Peak_Rise_Time_n3)

      datos_Peak_Rise_Time <- bind_rows(grupo1, grupo2, grupo3)

      graphip1 <- ggplot2::ggplot(datos_Peak_Rise_Time, ggplot2::aes(x = Replica, y = Valor, fill = Replica)) +
        ggplot2::geom_boxplot() +
        ggplot2::labs(title = "Peak Rise Time",
                      x = "",
                      y = "[log(s)]") +
        ggplot2::theme_minimal()

      graphip1 +
        ggplot2::theme(axis.text.x = ggplot2::element_text(face = "bold", size = 20),
                       axis.text.y = ggplot2::element_text(face = "bold", size = 20),
                       axis.title = ggplot2::element_text(face = "bold", size = 20))
      ##########################################################################
      Peak_Ocurrence_Time_n1 <- log(peaks_df1()$df_grap$Peak_Time)
      Peak_Ocurrence_Time_n2 <- log(peaks_df2()$df_grap$Peak_Time)
      Peak_Ocurrence_Time_n3 <- log(peaks_df3()$df_grap$Peak_Time)

      grupo12 <- data.frame(Replica = "n = 1", Valor = Peak_Ocurrence_Time_n1)
      grupo22 <- data.frame(Replica = "n = 2", Valor = Peak_Ocurrence_Time_n2)
      grupo32 <- data.frame(Replica = "n = 3", Valor = Peak_Ocurrence_Time_n3)

      datos_Peak_Ocurrence_Time <- bind_rows(grupo12, grupo22, grupo32)

      graphip12 <- ggplot2::ggplot(datos_Peak_Ocurrence_Time, ggplot2::aes(x = Replica, y = Valor, fill = Replica)) +
        ggplot2::geom_boxplot() +
        ggplot2::labs(title = "Peak Ocurrence Time",
                      x = "",
                      y = "[log(s)]") +
        ggplot2::theme_minimal()

      graphip12 +
        ggplot2::theme(axis.text.x = ggplot2::element_text(face = "bold", size = 20),
                       axis.text.y = ggplot2::element_text(face = "bold", size = 20),
                       axis.title = ggplot2::element_text(face = "bold", size = 20))
      ##########################################################################
      FWHM_n1 <- log(peaks_df1()$df_grap$FWHM)
      FWHM_n2 <- log(peaks_df2()$df_grap$FWHM)
      FWHM_n3 <- log(peaks_df3()$df_grap$FWHM)

      grupo13 <- data.frame(Replica = "n = 1", Valor = FWHM_n1)
      grupo23 <- data.frame(Replica = "n = 2", Valor = FWHM_n2)
      grupo33 <- data.frame(Replica = "n = 3", Valor = FWHM_n3)

      datos_FWHM <- bind_rows(grupo13, grupo23, grupo33)

      graphip13 <- ggplot2::ggplot(datos_FWHM, ggplot2::aes(x = Replica, y = Valor, fill = Replica)) +
        ggplot2::geom_boxplot() +
        ggplot2::labs(title = "FWHM",
                      x = "",
                      y = "[log(s)]") +
        ggplot2::theme_minimal()

      graphip13 +
        ggplot2::theme(axis.text.x = ggplot2::element_text(face = "bold", size = 20),
                       axis.text.y = ggplot2::element_text(face = "bold", size = 20),
                       axis.title = ggplot2::element_text(face = "bold", size = 20))

      ##########################################################################
      Transient_Ocurrence_Time_n1 <- log(peaks_df1()$df_grap$Transient_Ocurrence_Time)
      Transient_Ocurrence_Time_n2 <- log(peaks_df2()$df_grap$Transient_Ocurrence_Time)
      Transient_Ocurrence_Time_n3 <- log(peaks_df3()$df_grap$Transient_Ocurrence_Time)

      grupo14 <- data.frame(Replica = "n = 1", Valor = Transient_Ocurrence_Time_n1)
      grupo24 <- data.frame(Replica = "n = 2", Valor = Transient_Ocurrence_Time_n2)
      grupo34 <- data.frame(Replica = "n = 3", Valor = Transient_Ocurrence_Time_n3)

      datos_Transient_Ocurrence_Time <- bind_rows(grupo14, grupo24, grupo34)

      graphip14 <- ggplot2::ggplot(datos_Transient_Ocurrence_Time, ggplot2::aes(x = Replica, y = Valor, fill = Replica)) +
        ggplot2::geom_boxplot() +
        ggplot2::labs(title = "Transient Ocurrence Time",
                      x = "",
                      y = "[log(s)]") +
        ggplot2::theme_minimal()

      graphip14 +
        ggplot2::theme(axis.text.x = ggplot2::element_text(face = "bold", size = 20),
                       axis.text.y = ggplot2::element_text(face = "bold", size = 20),
                       axis.title = ggplot2::element_text(face = "bold", size = 20))
      ##########################################################################

      Time_OnSet_n1 <- log(peaks_df1()$Time_OnSet)
      Time_OnSet_n2 <- log(peaks_df2()$Time_OnSet)
      Time_OnSet_n3 <- log(peaks_df3()$Time_OnSet)

      grupo15 <- data.frame(Replica = "n = 1", Valor = Time_OnSet_n1)
      grupo25 <- data.frame(Replica = "n = 2", Valor = Time_OnSet_n2)
      grupo35 <- data.frame(Replica = "n = 3", Valor = Time_OnSet_n3)

      datos_Time_OnSet <- bind_rows(grupo15, grupo25, grupo35)

      graphip15 <- ggplot2::ggplot(datos_Time_OnSet, ggplot2::aes(x = Replica, y = Valor, fill = Replica)) +
        ggplot2::geom_boxplot() +
        ggplot2::labs(title = "Time OnSet",
                      x = "",
                      y = "[log(s)]") +
        ggplot2::theme_minimal()

      graphip15 +
        ggplot2::theme(axis.text.x = ggplot2::element_text(face = "bold", size = 20),
                       axis.text.y = ggplot2::element_text(face = "bold", size = 20),
                       axis.title = ggplot2::element_text(face = "bold", size = 20))

      ##########################################################################

      Frequency_n1 <- log(peaks_df1()$Frequency)
      Frequency_n2 <- log(peaks_df2()$Frequency)
      Frequency_n3 <- log(peaks_df3()$Frequency)

      grupo16 <- data.frame(Replica = "n = 1", Valor = Frequency_n1)
      grupo26 <- data.frame(Replica = "n = 2", Valor = Frequency_n2)
      grupo36 <- data.frame(Replica = "n = 3", Valor = Frequency_n3)

      datos_Time_OnSet <- bind_rows(grupo16, grupo26, grupo36)

      graphip16 <- ggplot2::ggplot(datos_Time_OnSet, ggplot2::aes(x = Replica, y = Valor, fill = Replica)) +
        ggplot2::geom_boxplot() +
        ggplot2::labs(title = "Frequency",
                      x = "",
                      y = "[log(1/s)]") +
        ggplot2::theme_minimal()

      graphip16 +
        ggplot2::theme(axis.text.x = ggplot2::element_text(face = "bold", size = 20),
                       axis.text.y = ggplot2::element_text(face = "bold", size = 20),
                       axis.title = ggplot2::element_text(face = "bold", size = 20))

      ##########################################################################
      Baseline_n1 <- log(peaks_df1()$df_grap$Baseline)
      Baseline_n2 <- log(peaks_df2()$df_grap$Baseline)
      Baseline_n3 <- log(peaks_df3()$df_grap$Baseline)

      grupo17 <- data.frame(Replica = "n = 1", Valor = Baseline_n1)
      grupo27 <- data.frame(Replica = "n = 2", Valor = Baseline_n2)
      grupo37 <- data.frame(Replica = "n = 3", Valor = Baseline_n3)

      datos_Baseline <- bind_rows(grupo17, grupo27, grupo37)

      graphip17 <- ggplot2::ggplot(datos_Baseline, ggplot2::aes(x = Replica, y = Valor, fill = Replica)) +
        ggplot2::geom_boxplot() +
        ggplot2::labs(title = "Baseline",
                      x = "",
                      y = expression(log(paste(Delta*F/F[0]))))+
        ggplot2::theme_minimal()

      graphip17 +
        ggplot2::theme(axis.text.x = ggplot2::element_text(face = "bold", size = 20),
                       axis.text.y = ggplot2::element_text(face = "bold", size = 20),
                       axis.title = ggplot2::element_text(face = "bold", size = 20))

      ##########################################################################
      AUC_n1 <- log(peaks_df1()$AUC)
      AUC_n2 <- log(peaks_df2()$AUC)
      AUC_n3 <- log(peaks_df3()$AUC)

      grupo18 <- data.frame(Replica = "n = 1", Valor = AUC_n1)
      grupo28 <- data.frame(Replica = "n = 2", Valor = AUC_n2)
      grupo38 <- data.frame(Replica = "n = 3", Valor = AUC_n3)

      datos_AUC <- bind_rows(grupo18, grupo28, grupo38)

      graphip18 <- ggplot2::ggplot(datos_AUC, ggplot2::aes(x = Replica, y = Valor, fill = Replica)) +
        ggplot2::geom_boxplot() +
        ggplot2::labs(title = "AUC",
                      x = "",
                      y = expression(log(paste(Delta * F / F[0] , 'x', s))))  +
        ggplot2::theme_minimal()

      graphip18 +
        ggplot2::theme(axis.text.x = ggplot2::element_text(face = "bold", size = 30),
                       axis.text.y = ggplot2::element_text(face = "bold", size = 30),
                       axis.title = ggplot2::element_text(face = "bold", size = 30))

      ##########################################################################
      Amplitude_n1 <- log(peaks_df1()$df_grap$Amplitude)
      Amplitude_n2 <- log(peaks_df2()$df_grap$Amplitude)
      Amplitude_n3 <- log(peaks_df3()$df_grap$Amplitude)

      grupo19 <- data.frame(Replica = "n = 1", Valor = Amplitude_n1)
      grupo29 <- data.frame(Replica = "n = 2", Valor = Amplitude_n2)
      grupo39 <- data.frame(Replica = "n = 3", Valor = Amplitude_n3)

      datos_Amplitude <- bind_rows(grupo19, grupo29, grupo39)

      graphip19 <- ggplot2::ggplot(datos_Amplitude, ggplot2::aes(x = Replica, y = Valor, fill = Replica)) +
        ggplot2::geom_boxplot() +
        ggplot2::labs(title = "Amplitude",
                      x = "",
                      y = expression(log(paste(Delta*F/F[0])))) +
        ggplot2::theme_minimal()

      graphip19 +
        ggplot2::theme(axis.text.x = ggplot2::element_text(face = "bold", size = 20),
                       axis.text.y = ggplot2::element_text(face = "bold", size = 20),
                       axis.title = ggplot2::element_text(face = "bold", size = 20))
      ##########################################################################
      Number_of_peaks_n1 <- log(peaks_df1()$Number_of_peaks)
      Number_of_peaks_n2 <- log(peaks_df2()$Number_of_peaks)
      Number_of_peaks_n3 <- log(peaks_df3()$Number_of_peaks)

      grupo110 <- data.frame(Replica = "n = 1", Valor = Number_of_peaks_n1)
      grupo210 <- data.frame(Replica = "n = 2", Valor = Number_of_peaks_n2)
      grupo310 <- data.frame(Replica = "n = 3", Valor = Number_of_peaks_n3)

      datos_Number_of_peaks <- bind_rows(grupo110, grupo210, grupo310)

      graphip110 <- ggplot2::ggplot(datos_Number_of_peaks, ggplot2::aes(x = Replica, y = Valor, fill = Replica)) +
        ggplot2::geom_boxplot() +
        ggplot2::labs(title = "Number of peaks",
                      x = "",
                      y = "log(Peaks)") +
        ggplot2::theme_minimal()

      graphip110 +
        ggplot2::theme(axis.text.x = ggplot2::element_text(face = "bold", size = 20),
                       axis.text.y = ggplot2::element_text(face = "bold", size = 20),
                       axis.title = ggplot2::element_text(face = "bold", size = 20))

      ##########################################################################
      Rise_n1 <- log(peaks_df1()$df_grap$Rise)
      Rise_n2 <- log(peaks_df2()$df_grap$Rise)
      Rise_n3 <- log(peaks_df3()$df_grap$Rise)

      grupo111 <- data.frame(Replica = "n = 1", Valor = Rise_n1)
      grupo211 <- data.frame(Replica = "n = 2", Valor = Rise_n2)
      grupo311 <- data.frame(Replica = "n = 3", Valor = Rise_n3)

      datos_Rise <- bind_rows(grupo111, grupo211, grupo311)

      graphip111 <- ggplot2::ggplot(datos_Rise, ggplot2::aes(x = Replica, y = Valor, fill = Replica)) +
        ggplot2::geom_boxplot() +
        ggplot2::labs(title = "Rise",
                      x = "",
                      y = expression(log(paste(Delta * F / F[0] , 'x', 1/s)))) +
        ggplot2::theme_minimal()

      graphip111 +
        ggplot2::theme(axis.text.x = ggplot2::element_text(face = "bold", size = 20),
                       axis.text.y = ggplot2::element_text(face = "bold", size = 20),
                       axis.title = ggplot2::element_text(face = "bold", size = 20))

      ########### Transient graph ###############################################
      cadena_texto <- input$transiens

      # Convertir la cadena en un vector numérico
      numeros_separados <- unlist(strsplit(cadena_texto, split = ","))
      numeros_numericos <- as.numeric(numeros_separados)

      # Verificar si los números se pueden convertir correctamente
      if (!any(is.na(numeros_numericos))) {
        cell1 <- numeros_numericos[1]
      } else {
        "Error: Por favor, introduce números separados por comas."
      }



      req(filedata1()$fileInput)
      data1 = filedata1()$fileInput
      data1 = t(data1)
      colnames(data1) = data1[1,]
      data1 = data1[-1,]
      data_raw1 = data.frame(Time = as.numeric(colnames(data1)),
                             signal = as.numeric(data1[cell1,]))
      ##### function loess for smoothed
      smoothed1 <- loess(signal ~ Time, data = data_raw1 , span = 0.05)
      predictions1 <- predict(smoothed1)
      df_smoothed1 <- data.frame(Time = data_raw1$Time, signal = predictions1)
      ########################################################################

      req(filedata2()$fileInput)
      data2 = filedata2()$fileInput
      data2 = t(data2)
      colnames(data2) = data2[1,]
      data2 = data2[-1,]
      cell2 <- numeros_numericos[2]
      data_raw2 = data.frame(Time = as.numeric(colnames(data2)),
                             signal = as.numeric(data2[cell2,]))
      ##### function loess for smoothed
      smoothed2 <- loess(signal ~ Time, data = data_raw2 , span = 0.05)
      predictions2 <- predict(smoothed2)
      df_smoothed2 <- data.frame(Time = data_raw2$Time, signal = predictions2)
      ########################################################################

      req(filedata3()$fileInput)
      data3 = filedata3()$fileInput
      data3 = t(data3)
      colnames(data3) = data3[1,]
      data3 = data3[-1,]
      cell3 <- numeros_numericos[3]
      data_raw3 = data.frame(Time = as.numeric(colnames(data3)),
                             signal = as.numeric(data3[cell3,]))
      ##### function loess for smoothed
      smoothed3 <- loess(signal ~ Time, data = data_raw3 , span = 0.05)
      predictions3 <- predict(smoothed3)
      df_smoothed3 <- data.frame(Time = data_raw3$Time, signal = predictions3)

      ########################################################################

      df_smoothed11 <- data.frame(Valor = df_smoothed1$signal, Time = df_smoothed1$Time, Curve = "1")
      df_smoothed12 <- data.frame(Valor = df_smoothed2$signal, Time = df_smoothed2$Time, Curve = "2")
      df_smoothed13 <- data.frame(Valor = df_smoothed3$signal, Time = df_smoothed3$Time, Curve = "3")

      data_Transient <- bind_rows(df_smoothed11, df_smoothed12, df_smoothed13)

      graphip21 <- ggplot2::ggplot(data = data_Transient, ggplot2::aes(x = Time, y = Valor, color = Curve)) +
        ggplot2::geom_line() +
        ggplot2::labs(title = "",
                      x = "Time[s]",
                      y = expression(paste(Delta*F/F[0])),
                      color = "Curves") +
        ggplot2::theme_minimal()

      graphip21 +
        ggplot2::theme(axis.text.x = ggplot2::element_text(face = "bold", size = 20),
                       axis.text.y = ggplot2::element_text(face = "bold", size = 20),
                       axis.title = ggplot2::element_text(face = "bold", size = 20))



      panel_Transient_metrics2 <- ggpubr::ggarrange(graphip21, graphip1,
                                                    graphip12, graphip13,
                                                    graphip14, graphip15,
                                                    graphip16, graphip17,
                                                    graphip18, graphip19,
                                                    graphip110, graphip111,
                                                    nrow = 3, ncol = 4 )
      ggplot2::ggsave("Panel_metric_Shelly_log.png", plot = panel_Transient_metrics2, device = "png", dpi = 300)

      return(list(panel_Transient_metrics2 = panel_Transient_metrics2))
    })

    output$Graph_1 <- renderPlot({
      if (input$Log_Data == 1){
      grahpg_Peak_Rise_Time_log()$panel_Transient_metrics2
      }
      else{grahpg_Peak_Rise_Time()$panel_Transient_metrics2}
    })


  })
}

## To be copied in the UI
# mod_Graphs_for_metrics_ui("Graphs_for_metrics_1")

## To be copied in the server
# mod_Graphs_for_metrics_server("Graphs_for_metrics_1")
