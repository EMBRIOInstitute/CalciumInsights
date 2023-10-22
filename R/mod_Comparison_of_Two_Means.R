#' Comparison_of_Two_Means UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_Comparison_of_Two_Means_ui <- function(id){
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(width = 4,
                   fileInput(ns("file_hipotesis"),
                             accept = c('text/csv',
                                        'text/comma-separated-values,text/plain',
                                        '.csv'),
                             label = h5("Dataset")),

                   numericInput(inputId = ns("bins"),
                                label = "Bins",
                                value = 5, min = 1, max = 100,step = 1),

                   ),
      mainPanel(
        tabsetPanel(
          type = "tabs",
          tabPanel("Summary",

                   div(DT::DTOutput(ns("sumarydata")), style = "margin-top: 20px;"),
                   div(DT::DTOutput(ns("data")), style = "margin-top: 20px;"),
          ),
          tabPanel("Descriptive Analysis",
                   tabsetPanel(
                     type = "tabs",
                   tabPanel("Histogram",
                   plotOutput(ns("histo1")),
                   plotOutput(ns("histo2")),
                   ),
                   tabPanel("Normality Plot",
                   plotOutput(ns("qqplot1")),
                   plotOutput(ns("qqplot2")),
                   ),
                   tabPanel("Normality Test",
                   verbatimTextOutput(ns("normality_test_result1")),
                   verbatimTextOutput(ns("normality_test_result2")),
                   )
                   )



          ),
          tabPanel("Tests",
                   verbatimTextOutput(ns("ttest_result")),
                   verbatimTextOutput(ns("mann_whitney_result"))


          )
        )
      )
      )

  )
}

#' Comparison_of_Two_Means Server Functions
#'
#' @noRd
mod_Comparison_of_Two_Means_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    filedata <- reactive({
      req(input$file_hipotesis)
      fileInput <- load_file(input$file_hipotesis$name, input$file_hipotesis$datapath)
      fileInput <- as.data.frame(fileInput)
      return(list(fileInput = fileInput))
    })

    data_info <- reactive({
      req(filedata()$fileInput)
      data <- filedata()$fileInput
      Nobservations1 <- length(na.omit(data[,1]))
      Nobservations2 <- length(na.omit(data[,2]))
      SummaryData <- data.frame(list(Number = c(Nobservations1, Nobservations2)))
      rownames(SummaryData) <- c("n1", "n2")


      list(SummaryData = SummaryData, data = data)
    })

    output$data <- DT::renderDataTable({
      df <- data_info()$data
      DT::datatable(df)
    })

    output$sumarydata <- DT::renderDataTable({
      df <- data_info()$SummaryData
      DT::datatable(df, options = list(
        pagingType = 'simple',
        dom = 't'
      ))
    })

    output$histo1 <- renderPlot({
      df <- data_info()$data
      x_range1 <- range(df[, 1])
      x_range <- range(df[, 2])

      hist(df[,1], breaks = input$bins,  main = "Group 1", xlab = colnames(df)[1])
    })

    output$histo2 <- renderPlot({
      df <- data_info()$data
      x_range1 <- range(df[, 1])
      x_range <- range(df[, 2])

      hist(df[,2],  breaks = input$bins, main = "Group 2", xlab = colnames(df)[2])
    })

    output$qqplot1 <- renderPlot({
      df <- data_info()$data
      qqnorm(df[,1], main = "Normality Plot Group 1")
      qqline(df[,1])
    })

    output$qqplot2 <- renderPlot({
      df <- data_info()$data
      qqnorm(df[,2], main = "Normality Plot Group 1")
      qqline(df[,2])
    })


    output$normality_test_result1 <- renderPrint({
      df <- data_info()$data
      group1 <- df[,1]

        normality_test <- shapiro.test(group1)
        p_value <- normality_test$p.value

        cat("Resultado de la prueba de Shapiro-Wilk:\n")
        print(normality_test)

        hypothesis <- "The null hypothesis is that your data follows a normal distribution."
        cat("Hipótesis:\n")
        cat(hypothesis, "\n\n")

        if (p_value > 0.05) {
          conclusion <- "Conclusion: The data follows a normal distribution (p-value > 0.05)."
        } else {
          conclusion <- "Conclusion: The data does not follow a normal distribution (p-value <= 0.05)."
        }

        cat(conclusion)

    })


    output$normality_test_result2 <- renderPrint({
      df <- data_info()$data
      group2 <- df[,2]

      normality_test <- shapiro.test(group2)
      p_value <- normality_test$p.value

      cat("Resultado de la prueba de Shapiro-Wilk:\n")
      print(normality_test)

      hypothesis <- "The null hypothesis is that your data follows a normal distribution."
      cat("Hipótesis:\n")
      cat(hypothesis, "\n\n")

      if (p_value > 0.05) {
        conclusion <- "Conclusion: The data follows a normal distribution (p-value > 0.05)."
      } else {
        conclusion <- "Conclusion: The data does not follow a normal distribution (p-value <= 0.05)."
      }

      cat(conclusion)

    })

    ##############################
    ##############################
    # test for difference in means

    output$ttest_result <- renderPrint({
        df <- data_info()$data
        group1_data <- df[,1]
        group2_data <- df[,2]

        # Realiza la prueba t
        t_test_result <- t.test(group1_data, group2_data, var.equal = FALSE)

        # Muestra los resultados
        cat("Resultado de la prueba t:\n")
        print(t_test_result)

        # Hipótesis nula
        cat("Nul Hypothesis: The means of the two groups are equal (H0: μ1 = μ2).\n")

        # Conclusión con el valor p
        p_value <- t_test_result$p.value
        if (p_value > 0.05) {
          conclusion <- "Conclusion: There is no evidence to reject the null hypothesis (p-value > 0.05)."
        } else {
          conclusion <- "Conclusion: The null hypothesis is rejected (p-value <= 0.05)."
        }
        cat(conclusion)

    })

    output$mann_whitney_result <- renderPrint({

      df <- data_info()$data
      group1_data <- df[,1]
      group2_data <- df[,2]

        # Realiza la prueba Mann-Whitney U
        mann_whitney_result <- wilcox.test(group1_data, group2_data, alternative = "two.sided", paired = F)

        # Muestra los resultados
        cat("Resultado de la prueba Mann-Whitney U:\n")
        print(mann_whitney_result)

        # Hipótesis nula
        cat("Null hypothesis: There is no difference (in terms of central tendency) between the two groups in the population.\n")

        # Conclusión con el valor p
        p_value <- mann_whitney_result$p.value
        if (p_value > 0.05) {
          conclusion <- "Conclusion: There is no evidence to reject the null hypothesis (p-valor > 0.05)."
        } else {
          conclusion <- "Conclusion: The null hypothesis is rejected (p-value <= 0.05)."
        }
        cat(conclusion)

    })






  })
}

## To be copied in the UI
# mod_Comparison_of_Two_Means_ui("Comparison_of_Two_Means_1")

## To be copied in the server
# mod_Comparison_of_Two_Means_server("Comparison_of_Two_Means_1")
