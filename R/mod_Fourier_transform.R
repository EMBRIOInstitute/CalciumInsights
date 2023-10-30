#' Fourier_transform UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Fourier_transform_ui <- function(id){
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(width = 4,
                   fileInput(ns("fileBcsvF"),
                             accept = c('text/csv',
                                        'text/comma-separated-values,text/plain',
                                        '.csv'),
                             label = h5("Dataset")),
                   numericInput(inputId = ns("Cell3"),
                                label = "Components:",
                                value = 1, min = 1),
                   numericInput(inputId = ns("NF"),
                                label = "NF:",
                                value = 50, min = 1),

                   ),
      mainPanel(
        tabsetPanel(
          type = "tabs",
          tabPanel("SummaryData",
                   DT::DTOutput(ns("infotableF")),
                   DT::DTOutput(ns("dataF"))


          ),
          tabPanel("Peaks",
                   DT::DTOutput(ns("table_peaksF")),
                   plotOutput(ns("ff_combined")),
                   plotOutput(ns("graf")),

                   )
        )
      )


    )
  )
}

#' Fourier_transform Server Functions
#'
#' @noRd
mod_Fourier_transform_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    filedata <- reactive({
      req(input$fileBcsvF)
      fileInput <- load_file(input$fileBcsvF$name, input$fileBcsvF$datapath)
      fileInput <- as.data.frame(fileInput)
      return(list(fileInput = fileInput))
    })

    data_info <- reactive({
      req(filedata()$fileInput)
      Nobservations <- nrow(filedata()$fileInput)
      Ncells <- ncol(filedata()$fileInput)-1
      SummaryData <- data.frame(list(Number = c(Ncells, Nobservations)))
      rownames(SummaryData) <- c("Components", "Time observations")
      list(SummaryData = SummaryData, data = data.frame(filedata()$fileInput,row.names = NULL))
    })

    output$dataF <- DT::renderDataTable({
      df <- data_info()$data
      DT::datatable(df)
    })

    output$infotableF <- DT::renderDataTable({
      df <- data_info()$SummaryData
      DT::datatable(df, options = list(
        pagingType = 'simple',
        dom = 't'
      ))
    })

    F_df_component <- reactive({
      req(filedata()$fileInput)
      data = filedata()$fileInput
      data = t(data)
      colnames(data) = data[1,]
      data = data[-1,]
      cell = as.numeric(input$Cell3)
      data_raw = data.frame(time = as.numeric(colnames(data)),
                            value = as.numeric(data[cell,]))
      # data_raw  = data_raw[data_raw$Time>=input$point_impact2,]
      data_raw  = data_raw[data_raw$time>= 0,]
      return(list(data_raw = data_raw))
    })

    FFT <- reactive({
      data1 <- F_df_component()$data_raw

      time <- data1$time
      value <- data1$value
      data <- data.frame(time = time, value = value)

      fft_result <- fft(data$value)


      amplitude <- Mod(fft_result)
      phase <- Arg(fft_result)

      # Create a data frame with frequency, amplitude, and phase
      fft_data <- data.frame(
        frequency = 1:length(amplitude),
        amplitude = amplitude,
        phase = phase,
        fft_result = fft_result
      )

      g1 <- ggplot2::ggplot(fft_data, ggplot2::aes(x = frequency, y = amplitude/length(time))) +
        ggplot2::geom_bar(stat = "identity") +
        ggplot2::xlab("Frequency") +
        ggplot2::ylab("Amplitude")

      g2 <- ggplot2::ggplot(fft_data, ggplot2::aes(x = frequency, y = phase)) +
        ggplot2::geom_bar(stat = "identity") +
        ggplot2::xlab("Frequency") +
        ggplot2::ylab("Phase")

      reconstruct_time_series <- function(frequencies, amplitudes, phases, time) {
        N <- length(frequencies)
        result <- rep(0, length(time))

        for (k in 1:N) {
          result <- result + amplitudes[k] * sin(2 * pi * frequencies[k] * time/length(time) + phases[k])
        }
        result
      }

      reconstructed_data <- reconstruct_time_series(
        fft_data$frequency,
        fft_data$amplitude,
        fft_data$phase,
        time
      )

      reconstruction_plot_data <- data.frame(
        time = time,
        original_data = data$value,
        reconstructed_data = reconstructed_data
      )

      fft_data <- dplyr::arrange(fft_data, desc(amplitude))
      n <- length(fft_data$frequency[1:input$NF])
      df <- data.frame(x = time)
      function_list_cos <- lapply(1:n, function(i) fft_data$amplitude[i] * cos(2 * pi * fft_data$frequency[i] * time/length(time) + fft_data$phase[i])/length(time))
      df_cos <- cbind(df, as.data.frame(function_list_cos))
      df_long_cos <- tidyr::pivot_longer(df_cos, cols = -x, names_to = "Function", values_to = "y")

      p_cos <- ggplot2::ggplot(data = df_long_cos, ggplot2::aes(x = x, y = y, color = Function)) +
        ggplot2::geom_line(show.legend = FALSE) +
        ggplot2::labs(title = "Multiple Cos Functions", x = "x-axis", y = "y-axis") +
        ggplot2::scale_color_manual(values = rainbow(n))  +
        ggplot2::theme_minimal()

      # Create a list of functions
      function_list_sin <- lapply(1:n, function(i) fft_data$amplitude[i] * sin(2 * pi * fft_data$frequency[i] * time/length(time) + fft_data$phase[i])/length(time))

      # Combine the functions into a data frame
      df_sin <- cbind(df, as.data.frame(function_list_sin))
      # Reshape the data frame into a long format
      df_long_sin <- tidyr::pivot_longer(df_sin, cols = -x, names_to = "Function", values_to = "y")
      # Create the plot

      p_sin <- ggplot2::ggplot(data = df_long_sin, ggplot2::aes(x = x, y = y, color = Function)) +
        ggplot2::geom_line(show.legend = FALSE) +
        ggplot2::labs(title = "Multiple Sin Functions", x = "x-axis", y = "y-axis") +
        ggplot2::scale_color_manual(values = rainbow(n))  +
        ggplot2::theme_minimal()


      df_combined <- data.frame(x = time[1:n]*length(time)/n)

      #df_combined$y <- abs(combined_function)

      fft_data_combined <- df_combined
      fft_data_combined <- dplyr::arrange(fft_data[1:n,], frequency)
      df_combined$y <- Mod(fft(fft_data_combined$fft_result[1:n], inverse = TRUE))/length(time)
      # Create the plot for combined_function
      p_combined <- ggplot2::ggplot(data = df_combined, ggplot2::aes(x = x, y = y)) +
        ggplot2::geom_line(color = "red", size = 1, linetype = "solid") +
        ggplot2::geom_line(data = reconstruction_plot_data, ggplot2::aes(x = time, y = original_data, color = "red")) +
        ggplot2::labs(title = "Combined Function", x = "x-axis", y = "y-axis") +
        ggplot2::theme_minimal() +
        ggplot2::scale_color_manual(values = c("blue", "red"), name = "Legend Title", labels = c("Raw data", "Red Line"))




      graf = gridExtra::grid.arrange(g1, g2, p_cos, p_sin, nrow = 2)

      return(list(graf = graf, p_combined = p_combined ))
    })

    output$graf <- renderPlot({
      FFT()$graf
    })

    output$ff_combined <- renderPlot({
      FFT()$p_combined
    })




  })
}

## To be copied in the UI
# mod_Fourier_transform_ui("Fourier_transform_1")

## To be copied in the server
# mod_Fourier_transform_server("Fourier_transform_1")
