#' FunctionalANOVA UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_FunctionalANOVA_ui <- function(id){
  ns <- NS(id)
  tagList(
    sidebarLayout(
      sidebarPanel(width = 4,
                   radioButtons(
                     ns("groups"),
                     "Numer of groups:",
                     choices = list("Three Groups"=1,
                                    "Four Groups"=2,
                                    "Five Groups"=3
                     )
                   ),
                   conditionalPanel(
                     condition = "input.groups==1", ns=ns,
                     fileInput(ns("data_group1"),
                               accept = c('text/csv',
                                          'text/comma-separated-values,text/plain',
                                          '.csv'),
                               label = h5("Group1")),
                     fileInput(ns("data_group2"),
                               accept = c('text/csv',
                                          'text/comma-separated-values,text/plain',
                                          '.csv'),
                               label = h5("Group2")),
                     fileInput(ns("data_group3"),
                               accept = c('text/csv',
                                          'text/comma-separated-values,text/plain',
                                          '.csv'),
                               label = h5("Group3"))
                   ),
                   conditionalPanel(
                     condition = "input.groups==2", ns=ns,
                     fluidRow(
                       column(6,
                              fileInput(ns("data_group11"),
                                        accept = c('text/csv',
                                                   'text/comma-separated-values,text/plain',
                                                   '.csv'),
                                        label = h5("Group1")),
                              fileInput(ns("data_group22"),
                                        accept = c('text/csv',
                                                   'text/comma-separated-values,text/plain',
                                                   '.csv'),
                                        label = h5("Group2"))
                       ),
                       column(6,
                              fileInput(ns("data_group33"),
                                        accept = c('text/csv',
                                                   'text/comma-separated-values,text/plain',
                                                   '.csv'),
                                        label = h5("Group3")),
                              fileInput(ns("data_group44"),
                                        accept = c('text/csv',
                                                   'text/comma-separated-values,text/plain',
                                                   '.csv'),
                                        label = h5("Group4"))
                       )
                     )
                   ),

                   conditionalPanel(
                     condition = "input.groups==3", ns=ns,
                     fluidRow(
                       column(6,
                              fileInput(ns("data_group111"),
                                        accept = c('text/csv',
                                                   'text/comma-separated-values,text/plain',
                                                   '.csv'),
                                        label = h5("Group1")),
                              fileInput(ns("data_group222"),
                                        accept = c('text/csv',
                                                   'text/comma-separated-values,text/plain',
                                                   '.csv'),
                                        label = h5("Group2")),
                              fileInput(ns("data_group333"),
                                        accept = c('text/csv',
                                                   'text/comma-separated-values,text/plain',
                                                   '.csv'),
                                        label = h5("Group3"))
                       ),
                       column(6,
                              fileInput(ns("data_group444"),
                                        accept = c('text/csv',
                                                   'text/comma-separated-values,text/plain',
                                                   '.csv'),
                                        label = h5("Group4")),
                              fileInput(ns("data_group555"),
                                        accept = c('text/csv',
                                                   'text/comma-separated-values,text/plain',
                                                   '.csv'),
                                        label = h5("Group5"))
                       )
                     )

                   ),
                   checkboxInput(ns("checkbox1"), "Scalar effect", value = TRUE),
                   checkboxInput(ns("checkbox"), "Log transformation")



      ),
      mainPanel(
        tabsetPanel(
          type = "tabs",
          tabPanel("SummaryData",
                   conditionalPanel(
                     condition = "input.groups==1", ns=ns,
                     DT::DTOutput(ns("summary1")),
                     DT::DTOutput(ns("summary2")),
                     DT::DTOutput(ns("summary3"))
                   ),
                   conditionalPanel(
                     condition = "input.groups==2", ns=ns,
                     DT::DTOutput(ns("summary41")),
                     DT::DTOutput(ns("summary42")),
                     DT::DTOutput(ns("summary43")),
                     DT::DTOutput(ns("summary44"))
                   ),
                   conditionalPanel(
                     condition = "input.groups==3", ns=ns,
                     DT::DTOutput(ns("summary51")),
                     DT::DTOutput(ns("summary52")),
                     DT::DTOutput(ns("summary53")),
                     DT::DTOutput(ns("summary54")),
                     DT::DTOutput(ns("summary55"))
                   )
          ),
          tabPanel("Descriptive statistics",
                   tabsetPanel(
                     type = "tabs",
                     tabPanel("Plots",
                              conditionalPanel(
                                condition = "input.groups==1", ns=ns,
                                plotOutput(ns("graph_mean3"))
                              ),
                              conditionalPanel(
                                condition = "input.groups==2", ns=ns,
                                plotOutput(ns("graph_mean4"))
                              ),
                              conditionalPanel(
                                condition = "input.groups==3", ns=ns,
                                plotOutput(ns("graph_mean5"))
                              )
                     ),
                     tabPanel("Significance",
                              tags$h4("Base configuration",
                                      style = "color: gray; margin-top: 5px;"),
                              fluidRow(
                                column(6,
                                       textInput(ns("k1"), "k1:", value = "25")
                                ),
                                column(6,
                                       textInput(ns("k2"), "k2:", value = "25")
                                )
                              ),
                              conditionalPanel(
                                condition = "input.groups==1", ns=ns,
                                tabsetPanel(
                                  type = "tabs",
                                  tabPanel("Global effect",
                                           plotOutput(ns("graph_effect1"))
                                  ),
                                  tabPanel("Group1",
                                           plotOutput(ns("group_effect1"))
                                  ),
                                  tabPanel("Group2",
                                           plotOutput(ns("group_effect2"))
                                  ),
                                  tabPanel("Group3",
                                           plotOutput(ns("group_effect3"))
                                  ),
                                  tabPanel("Residual",
                                           plotOutput(ns("group_effect4"))
                                  ),
                                  tabPanel("Q plot",
                                           plotOutput(ns("group_effect5"))
                                  )
                                )),
                              conditionalPanel(
                                condition = "input.groups==2", ns=ns,
                                tabsetPanel(
                                  type = "tabs",
                                  tabPanel("Global effect",
                                           plotOutput(ns("graph_effect41"))
                                  ),
                                  tabPanel("Group1",
                                           plotOutput(ns("group_effect41"))
                                  ),
                                  tabPanel("Group2",
                                           plotOutput(ns("group_effect42"))
                                  ),
                                  tabPanel("Group3",
                                           plotOutput(ns("group_effect43"))
                                  ),
                                  tabPanel("Group4",
                                           plotOutput(ns("group_effect44"))
                                  ),
                                  tabPanel("Residual",
                                           plotOutput(ns("group_effect45"))
                                  ),
                                  tabPanel("Q plot",
                                           plotOutput(ns("group_effect46"))
                                  )
                                )),
                              conditionalPanel(
                                condition = "input.groups==3", ns=ns,
                                tabsetPanel(
                                  type = "tabs",
                                  tabPanel("Global effect",
                                           plotOutput(ns("graph_effect51"))
                                  ),
                                  tabPanel("Group1",
                                           plotOutput(ns("group_effect51"))
                                  ),
                                  tabPanel("Group2",
                                           plotOutput(ns("group_effect52"))
                                  ),
                                  tabPanel("Group3",
                                           plotOutput(ns("group_effect53"))
                                  ),
                                  tabPanel("Group4",
                                           plotOutput(ns("group_effect54"))
                                  ),
                                  tabPanel("Group5",
                                           plotOutput(ns("group_effect55"))
                                  ),
                                  tabPanel("Residual",
                                           plotOutput(ns("group_effect56"))
                                  ),
                                  tabPanel("Q plot",
                                           plotOutput(ns("group_effect57"))
                                  ),
                                  tabPanel("Comparations",
                                           selectInput(ns("Comparation"), "",
                                                       choices = c("Grupo 1 vs Grupo 2"=1,
                                                                   "Grupo 1 vs Grupo 3"=2,
                                                                   "Grupo 1 vs Grupo 4"=3,
                                                                   "Grupo 1 vs Grupo 5"=4,
                                                                   "Grupo 2 vs Grupo 3"=5,
                                                                   "Grupo 2 vs Grupo 4"=6,
                                                                   "Grupo 2 vs Grupo 5"=7,
                                                                   "Grupo 3 vs Grupo 4"=8,
                                                                   "Grupo 3 vs Grupo 5"=9,
                                                                   "Grupo 4 vs Grupo 5"=10)),
                                           conditionalPanel(
                                             condition = "input.Comparation==1", ns=ns,
                                             plotOutput(ns("Compartion1"))
                                           ),
                                           conditionalPanel(
                                             condition = "input.Comparation==2", ns=ns,
                                             plotOutput(ns("Compartion2"))
                                           ),
                                           conditionalPanel(
                                             condition = "input.Comparation==3", ns=ns,
                                             plotOutput(ns("Compartion3"))
                                           ),
                                           conditionalPanel(
                                             condition = "input.Comparation==4", ns=ns,
                                             plotOutput(ns("Compartion4"))
                                           ),
                                           conditionalPanel(
                                             condition = "input.Comparation==5", ns=ns,
                                             plotOutput(ns("Compartion5"))
                                           ),
                                           conditionalPanel(
                                             condition = "input.Comparation==6", ns=ns,
                                             plotOutput(ns("Compartion6"))
                                           ),
                                           conditionalPanel(
                                             condition = "input.Comparation==7", ns=ns,
                                             plotOutput(ns("Compartion7"))
                                           ),
                                           conditionalPanel(
                                             condition = "input.Comparation==8", ns=ns,
                                             plotOutput(ns("Compartion8"))
                                           ),
                                           conditionalPanel(
                                             condition = "input.Comparation==9", ns=ns,
                                             plotOutput(ns("Compartion9"))
                                           ),
                                           conditionalPanel(
                                             condition = "input.Comparation==10", ns=ns,
                                             plotOutput(ns("Compartion10"))
                                           )
                                  )
                                )),

                              conditionalPanel(
                                condition = "input.groups==1", ns=ns,
                                DT::DTOutput(ns("infotable"))
                              ),
                              conditionalPanel(
                                condition = "input.groups==2", ns=ns,
                                DT::DTOutput(ns("infotable4"))
                              ),
                              conditionalPanel(
                                condition = "input.groups==3", ns=ns,
                                DT::DTOutput(ns("infotable5"))
                              )


                     )
                   )
          )
        )
      )

    )

  )
}

#' FunctionalANOVA Server Functions
#'
#' @noRd
mod_FunctionalANOVA_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    ############ three groups #################
    ##########################################
    filedata1 <- reactive({
      req(input$data_group1)
      fileInput <- load_file(input$data_group1$name, input$data_group1$datapath)
      fileInput <- as.data.frame(fileInput)
      return(list(fileInput = fileInput))
    })
    filedata2 <- reactive({
      req(input$data_group2)
      fileInput <- load_file(input$data_group2$name, input$data_group2$datapath)
      fileInput <- as.data.frame(fileInput)
      return(list(fileInput = fileInput))
    })
    filedata3 <- reactive({
      req(input$data_group3)
      fileInput <- load_file(input$data_group3$name, input$data_group3$datapath)
      fileInput <- as.data.frame(fileInput)
      return(list(fileInput = fileInput))
    })
    output$summary1 <- DT::renderDataTable({
      df <- filedata1()$fileInput
      DT::datatable(df)
    })
    output$summary2 <- DT::renderDataTable({
      df <- filedata2()$fileInput
      DT::datatable(df)
    })
    output$summary3 <- DT::renderDataTable({
      df <- filedata3()$fileInput
      DT::datatable(df)
    })
    graph_group1 <- reactive({
      req(filedata1()$fileInput)
      df1 <- data.frame(filedata1()$fileInput)
      top1 <- reshape2::melt(df1, id=c(names(df1)[1]))
      g1 <- ggplot2::ggplot(data = top1, ggplot2::aes(x=Time, y=value,
                                                      group = factor(variable),
                                                      colour = factor(variable))) +
        ggplot2::geom_line(size=0.5) +
        ggplot2::theme(text = ggplot2::element_text(size = 13),
                       axis.text.x = ggplot2::element_text(angle=90)) +
        ggplot2::labs(y = "", x = "Time (s)") +
        ggplot2::theme(legend.position = "none") +
        ggplot2::ggtitle("Group 1")
      return(list(g1 = g1, top1 = top1))
    })
    graph_group2 <- reactive({
      req(filedata2()$fileInput)
      df1 <- data.frame(filedata2()$fileInput)
      top2 <- reshape2::melt(df1, id=c(names(df1)[1]))
      g2 <- ggplot2::ggplot(data = top2, ggplot2::aes(x=Time, y=value,
                                                      group = factor(variable),
                                                      colour = factor(variable))) +
        ggplot2::geom_line(size=0.5) +
        ggplot2::theme(text = ggplot2::element_text(size = 13),
                       axis.text.x = ggplot2::element_text(angle=90)) +
        ggplot2::labs(y = "", x = "Time (s)") +
        ggplot2::theme(legend.position = "none") +
        ggplot2::ggtitle("Group 2")
      return(list(g2 = g2, top2 = top2))
    })
    graph_group3 <- reactive({
      req(filedata3()$fileInput)
      df1 <- data.frame(filedata3()$fileInput)
      top3 <- reshape2::melt(df1, id=c(names(df1)[1]))
      g3 <- ggplot2::ggplot(data = top3, ggplot2::aes(x=Time, y=value,
                                                      group = factor(variable),
                                                      colour = factor(variable))) +
        ggplot2::geom_line(size=0.5) +
        ggplot2::theme(text = ggplot2::element_text(size = 13),
                       axis.text.x = ggplot2::element_text(angle=90)) +
        ggplot2::labs(y = "", x = "Time (s)") +
        ggplot2::theme(legend.position = "none") +
        ggplot2::ggtitle("Group 3")
      return(list(g3 = g3, top3 = top3))
    })
    means3 <- reactive({
      library(magrittr)
      req(filedata1()$fileInput)
      req(filedata2()$fileInput)
      req(filedata3()$fileInput)
      data1=rbind(graph_group1()$top1, graph_group2()$top2, graph_group3()$top3)
      df1 <- data.frame(filedata1()$fileInput)
      df2 <- data.frame(filedata2()$fileInput)
      df3 <- data.frame(filedata3()$fileInput)
      group <- c(rep('1', length(df1[,1])*ncol(df1[,-1])),     #df1[,1] es el tiempo
                 rep('2', length(df2[,1])*ncol(df2[,-1])),
                 rep('3', length(df3[,1])*ncol(df3[,-1]))
      )
      data2 = data.frame(data1, group=group)
      prom_global = aggregate(value~Time,data = data2, FUN = mean)
      mean4 <- data2%>%dplyr::group_by(group,Time)%>%dplyr::summarise(Mean = mean(value))%>%as.data.frame()
      prom1 = data.frame(group=c(rep("Global",length(df1[,1]))), Time=prom_global$Time, Mean=prom_global$value)
      prom3 = rbind(mean4, prom1)
      m4 = ggplot2::ggplot(prom3, ggplot2::aes(x=Time,y=Mean, group = group, colour = group)) +
        ggplot2::geom_line(size=1) +
        ggplot2::theme(text = ggplot2::element_text(size = 13), axis.text.x = ggplot2::element_text(angle=90))+
        ggplot2::labs (y  = "", x= "Time (s)") + ggplot2::ggtitle("Average curves")+
        ggplot2::scale_color_manual(values = c("red", "blue", "aquamarine2", "black")) +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
      return(list(m4 = m4))
    })
    output$graph_mean3 <- renderPlot({
      gridExtra::grid.arrange(graph_group1()$g1,
                              graph_group2()$g2,
                              graph_group3()$g3,
                              means3()$m4, ncol = 2)
    })
    Data_for_anova <- reactive({
      req(filedata1()$fileInput)
      req(filedata2()$fileInput)
      req(filedata3()$fileInput)
      df1 <- data.frame(filedata1()$fileInput)
      df2 <- data.frame(filedata2()$fileInput)
      df3 <- data.frame(filedata3()$fileInput)
      antigen1 = c(rep('1',ncol(df1[,-1])),
                   rep('2', ncol(df2[,-1])),
                   rep('3', ncol(df3[,-1])))
      data_functional = cbind(df1,
                              df2[,-1],
                              df3[,-1]
      )
      datafun = cbind(data_functional[,-1])
      id = seq(1:(as.numeric(dim(datafun)[2])))
      colnames(datafun ) <- c(id)
      data1 <- data.frame(antigen2=factor(antigen1))
      time = c(data_functional[,1])
      Ydata = matrix(t(datafun), as.numeric(dim(datafun)[2]), as.numeric(dim(datafun)[1]))
      return(list(Ydata = Ydata, data_functional = data_functional, data1 = data1, time = time))
    })

    fit <- reactive({
      minn <- abs(min(min(graph_group1()$top1[,3]), min(graph_group2()$top2[,3]), min(graph_group3()$top3[,3])))
      minn1 <- ifelse(minn %% 1 == 0, minn + 1, ceiling(minn))
      if (input$checkbox) {Ydata = log(Data_for_anova()$Ydata + minn1)}
      else{Ydata = Data_for_anova()$Ydata}
      data_functional = Data_for_anova()$data_functional
      data1 = Data_for_anova()$data1
      time <- Data_for_anova()$time
      if (input$checkbox1) {fit4 <- refund::pffr(Ydata ~ antigen2 + c(antigen2), data = data1, method = "REML",
                                                 bs.yindex=list(bs="ps", k = as.numeric(input$k1), m=c(2, 1)),
                                                 bs.int=list(bs="ps", k = as.numeric(input$k2), m=c(2,1)))}
      else {
        fit4 <- refund::pffr(Ydata ~ antigen2, data = data1,method = "REML",
                             bs.yindex=list(bs="ps", k = as.numeric(input$k1), m=c(2, 1)),
                             bs.int=list(bs="ps", k = as.numeric(input$k2), m=c(2,1)))
      }
      residual_fun = t(residuals(fit4))
      colnames(residual_fun) = colnames(data_functional)[-1]
      residual_fun = data.frame(time=time, residual_fun)
      residual_fun1 = reshape2::melt(residual_fun, id=c("time"))
      RMSE = sqrt(mean((residual_fun1$value)^2))
      AIC = AIC(fit4)
      BIC=BIC(fit4)
      R2_adjusted = summary(fit4)$r.sq*100
      infromation = data.frame(RMSE = RMSE, AIC = AIC , BIC = BIC, R2_adjusted = R2_adjusted)
      return(list(infromation = infromation, fit4 = fit4))
    })
    output$infotable <- DT::renderDataTable({
      df <- fit()$infromation
      DT::datatable(df, options = list(
        pagingType = 'simple',
        dom = 't'
      ), caption = tags$caption(tags$strong("Model Evaluation:")))
    })
    output$graph_effect1 <- renderPlot({
      FoSIntro::plot_1D(fit()$fit4, select = 1, ylab = "Estimate", xlab="Time(s)",main ="Global mean effect", base_size = 25) +
        ggplot2::scale_x_continuous(breaks=seq(0,820,100)) + ggplot2::theme(axis.text.x = ggplot2::element_text(angle=90))
    })
    output$group_effect1 <- renderPlot({
      FoSIntro::plot_1D(fit()$fit4, select = 2, ylab = "Estimate", xlab="Time(s)",main ="Mean effect group1", base_size = 25) +
        ggplot2::scale_x_continuous(breaks=seq(0,820,100)) + ggplot2::theme(axis.text.x = ggplot2::element_text(angle=90))
    })
    output$group_effect2 <- renderPlot({
      FoSIntro::plot_1D(fit()$fit4, select = 3, ylab = "Estimate", xlab="Time(s)",main ="Mean effect group2", base_size = 25) +
        ggplot2::scale_x_continuous(breaks=seq(0,820,100)) + ggplot2::theme(axis.text.x = ggplot2::element_text(angle=90))
    })
    output$group_effect3 <- renderPlot({
      FoSIntro::plot_1D(fit()$fit4, select = 4, ylab = "Estimate", xlab="Time(s)",main ="Mean effect group3", base_size = 25) +
        ggplot2::scale_x_continuous(breaks=seq(0,820,100)) + ggplot2::theme(axis.text.x = ggplot2::element_text(angle=90))
    })
    residual31 <- reactive({
      data_functional = Data_for_anova()$data_functional
      time <- Data_for_anova()$time
      residual_fun = t(residuals(fit()$fit4))
      colnames(residual_fun) = colnames(data_functional)[-1]
      residual_fun = data.frame(time=time, residual_fun)
      resid_fun = reshape2::melt(residual_fun, id=c("time"))
      fitted_1 = t(fitted(fit()$fit4))
      colnames(fitted_1) = colnames(data_functional)[-1]
      fitted_fun = data.frame(time=time, fitted_1)
      fitted_fun1 = reshape2::melt(fitted_fun, id=c("time"))
      resid1 = data.frame(fitted=fitted_fun1$value, residual = resid_fun$value)
      return(list(resid1 = resid1))
    })
    output$group_effect4 <- renderPlot({
      resid_fun2 <- residual31()$resid1
      ggplot2::ggplot(resid_fun2, ggplot2::aes(x = fitted, y = residual)) +  ggplot2::geom_point() +
        ggplot2::geom_hline(yintercept = 0) + ggplot2::labs (x="Fitted", y = "Residuals")+
        ggplot2::theme(axis.title.x = ggplot2::element_text(size = 25))+
        ggplot2::theme(axis.text.y = ggplot2::element_text(size=25))+
        ggplot2::theme(axis.text.x = ggplot2::element_text(size=25))+
        ggplot2::theme(axis.title.y = ggplot2::element_text(size = 30))+
        ggplot2::ggtitle("Analysis of Homoscedasticity of Variance")+
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))+
        ggplot2::theme(plot.title = ggplot2::element_text(size = 20, face = "bold"))
    })
    output$group_effect5 <- renderPlot({
      resid_fun2 <- residual31()$resid1
      ggplot2::ggplot(resid_fun2, ggplot2::aes(sample=residual))+ggplot2::stat_qq()+
        ggplot2::stat_qq_line()+ggplot2::labs (x="Theorical", y = "Sample quantiles")+
        ggplot2::theme(axis.title.x = ggplot2::element_text(size = 25))+
        ggplot2::theme(axis.text.y = ggplot2::element_text(size=25))+
        ggplot2::theme(axis.text.x = ggplot2::element_text(size=25))+
        ggplot2::theme(axis.title.y = ggplot2::element_text(size = 30))+
        ggplot2::ggtitle("Normal Q-Q plot")+
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))+
        ggplot2::theme(plot.title = ggplot2::element_text(size = 20, face = "bold"))
    })

    ############ four groups #################
    ##########################################

    filedata11 <- reactive({
      req(input$data_group11)
      fileInput <- load_file(input$data_group11$name, input$data_group11$datapath)
      fileInput <- as.data.frame(fileInput)
      return(list(fileInput = fileInput))
    })
    filedata22 <- reactive({
      req(input$data_group22)
      fileInput <- load_file(input$data_group22$name, input$data_group22$datapath)
      fileInput <- as.data.frame(fileInput)
      return(list(fileInput = fileInput))
    })
    filedata33 <- reactive({
      req(input$data_group33)
      fileInput <- load_file(input$data_group33$name, input$data_group33$datapath)
      fileInput <- as.data.frame(fileInput)
      return(list(fileInput = fileInput))
    })
    filedata44 <- reactive({
      req(input$data_group44)
      fileInput <- load_file(input$data_group44$name, input$data_group44$datapath)
      fileInput <- as.data.frame(fileInput)
      return(list(fileInput = fileInput))
    })

    output$summary41 <- DT::renderDataTable({
      df <- filedata11()$fileInput
      DT::datatable(df)
    })
    output$summary42 <- DT::renderDataTable({
      df <- filedata22()$fileInput
      DT::datatable(df)
    })
    output$summary43 <- DT::renderDataTable({
      df <- filedata33()$fileInput
      DT::datatable(df)
    })
    output$summary44 <- DT::renderDataTable({
      df <- filedata44()$fileInput
      DT::datatable(df)
    })

    graph_group41 <- reactive({
      req(filedata11()$fileInput)
      df1 <- data.frame(filedata11()$fileInput)
      top41 <- reshape2::melt(df1, id=c(names(df1)[1]))
      g41 <- ggplot2::ggplot(data = top41, ggplot2::aes(x=Time, y=value,
                                                        group = factor(variable),
                                                        colour = factor(variable))) +
        ggplot2::geom_line(size=0.5) +
        ggplot2::theme(text = ggplot2::element_text(size = 13),
                       axis.text.x = ggplot2::element_text(angle=90)) +
        ggplot2::labs(y = "", x = "Time (s)") +
        ggplot2::theme(legend.position = "none") +
        ggplot2::ggtitle("Group 1")
      return(list(g41 = g41, top41 = top41))
    })
    graph_group42 <- reactive({
      req(filedata22()$fileInput)
      df1 <- data.frame(filedata22()$fileInput)
      top42 <- reshape2::melt(df1, id=c(names(df1)[1]))
      g42 <- ggplot2::ggplot(data = top42, ggplot2::aes(x=Time, y=value,
                                                        group = factor(variable),
                                                        colour = factor(variable))) +
        ggplot2::geom_line(size=0.5) +
        ggplot2::theme(text = ggplot2::element_text(size = 13),
                       axis.text.x = ggplot2::element_text(angle=90)) +
        ggplot2::labs(y = "", x = "Time (s)") +
        ggplot2::theme(legend.position = "none") +
        ggplot2::ggtitle("Group 1")
      return(list(g42 = g42, top42 = top42))
    })
    graph_group43 <- reactive({
      req(filedata33()$fileInput)
      df1 <- data.frame(filedata33()$fileInput)
      top43 <- reshape2::melt(df1, id=c(names(df1)[1]))
      g43 <- ggplot2::ggplot(data = top43, ggplot2::aes(x=Time, y=value,
                                                        group = factor(variable),
                                                        colour = factor(variable))) +
        ggplot2::geom_line(size=0.5) +
        ggplot2::theme(text = ggplot2::element_text(size = 13),
                       axis.text.x = ggplot2::element_text(angle=90)) +
        ggplot2::labs(y = "", x = "Time (s)") +
        ggplot2::theme(legend.position = "none") +
        ggplot2::ggtitle("Group 1")
      return(list(g43 = g43, top43 = top43))
    })

    graph_group44 <- reactive({
      req(filedata44()$fileInput)
      df1 <- data.frame(filedata44()$fileInput)
      top44 <- reshape2::melt(df1, id=c(names(df1)[1]))
      g44 <- ggplot2::ggplot(data = top44, ggplot2::aes(x=Time, y=value,
                                                        group = factor(variable),
                                                        colour = factor(variable))) +
        ggplot2::geom_line(size=0.5) +
        ggplot2::theme(text = ggplot2::element_text(size = 13),
                       axis.text.x = ggplot2::element_text(angle=90)) +
        ggplot2::labs(y = "", x = "Time (s)") +
        ggplot2::theme(legend.position = "none") +
        ggplot2::ggtitle("Group 1")
      return(list(g44 = g44, top44 = top44))
    })

    means4 <- reactive({
      library(magrittr)
      req(filedata11()$fileInput)
      req(filedata22()$fileInput)
      req(filedata33()$fileInput)
      req(filedata44()$fileInput)
      data1=rbind(graph_group41()$top41, graph_group42()$top42,
                  graph_group43()$top43, graph_group44()$top44)
      df1 <- data.frame(filedata11()$fileInput)
      df2 <- data.frame(filedata22()$fileInput)
      df3 <- data.frame(filedata33()$fileInput)
      df4 <- data.frame(filedata44()$fileInput)
      group <- c(rep('1', length(df1[,1])*ncol(df1[,-1])),     #df1[,1] es el tiempo
                 rep('2', length(df2[,1])*ncol(df2[,-1])),
                 rep('3', length(df3[,1])*ncol(df3[,-1])),
                 rep('4', length(df4[,1])*ncol(df4[,-1]))
      )
      data2 = data.frame(data1, group=group)
      prom_global = aggregate(value~Time,data = data2, FUN = mean)
      mean4 <- data2%>%dplyr::group_by(group,Time)%>%dplyr::summarise(Mean = mean(value))%>%as.data.frame()
      prom1 = data.frame(group=c(rep("Global",length(df1[,1]))), Time=prom_global$Time, Mean=prom_global$value)
      prom3 = rbind(mean4, prom1)
      m4 = ggplot2::ggplot(prom3, ggplot2::aes(x=Time,y=Mean, group = group, colour = group)) +
        ggplot2::geom_line(size=1) +
        ggplot2::theme(text = ggplot2::element_text(size = 13), axis.text.x = ggplot2::element_text(angle=90))+
        ggplot2::labs (y  = "", x= "Time (s)") + ggplot2::ggtitle("Average curves")+
        ggplot2::scale_color_manual(values = c("red", "blue", "aquamarine2", "green", "black")) +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
      return(list(m4 = m4))
    })

    output$graph_mean4 <- renderPlot({
      gridExtra::grid.arrange(graph_group41()$g41,
                              graph_group42()$g42,
                              graph_group43()$g43,
                              graph_group44()$g44,
                              means4()$m4, ncol = 2)
    })

    Data_for_anova4 <- reactive({
      req(filedata11()$fileInput)
      req(filedata22()$fileInput)
      req(filedata33()$fileInput)
      req(filedata44()$fileInput)
      df1 <- data.frame(filedata11()$fileInput)
      df2 <- data.frame(filedata22()$fileInput)
      df3 <- data.frame(filedata33()$fileInput)
      df4 <- data.frame(filedata44()$fileInput)

      antigen1 = c(rep('1',ncol(df1[,-1])),
                   rep('2', ncol(df2[,-1])),
                   rep('3', ncol(df3[,-1])),
                   rep('4', ncol(df4[,-1]))
      )
      data_functional = cbind(df1,
                              df2[,-1],
                              df3[,-1],
                              df4[,-1]
      )
      datafun = cbind(data_functional[,-1])
      id = seq(1:(as.numeric(dim(datafun)[2])))
      colnames(datafun) <- c(id)
      data1 <- data.frame(antigen2=factor(antigen1))
      time = c(data_functional[,1])
      Ydata = matrix(t(datafun), as.numeric(dim(datafun)[2]), as.numeric(dim(datafun)[1]))
      return(list(Ydata = Ydata, data_functional = data_functional, data1 = data1, time = time))
    })

    fit_4 <- reactive({
      minn <- abs(min(min(graph_group41()$top41[,3]), min(graph_group42()$top42[,3]),
                      min(graph_group43()$top43[,3]), min(graph_group44()$top44[,3])))
      minn1 <- ifelse(minn %% 1 == 0, minn + 1, ceiling(minn))
      if (input$checkbox) {Ydata = log(Data_for_anova4()$Ydata + minn1)}
      else{Ydata = Data_for_anova4()$Ydata}
      data_functional = Data_for_anova4()$data_functional
      data1 = Data_for_anova4()$data1
      time <- Data_for_anova4()$time
      if (input$checkbox1) {
        fit41 <- refund::pffr(Ydata ~ antigen2 + c(antigen2), data = data1, method = "REML",
                              bs.yindex=list(bs="ps", k = as.numeric(input$k1), m=c(2, 1)),
                              bs.int=list(bs="ps", k = as.numeric(input$k2), m=c(2,1)))
      }
      else {
        fit41 <- refund::pffr(Ydata ~ antigen2, data = data1,method = "REML",
                              bs.yindex=list(bs="ps", k = as.numeric(input$k1), m=c(2, 1)),
                              bs.int=list(bs="ps", k = as.numeric(input$k2), m=c(2,1)))
      }
      residual_fun = t(residuals(fit41))
      colnames(residual_fun) = colnames(data_functional)[-1]
      residual_fun = data.frame(time=time, residual_fun)
      residual_fun1 = reshape2::melt(residual_fun, id=c("time"))
      RMSE = sqrt(mean((residual_fun1$value)^2))
      AIC = AIC(fit41)
      BIC=BIC(fit41)
      R2_adjusted = summary(fit41)$r.sq*100
      infromation = data.frame(RMSE = RMSE, AIC = AIC , BIC = BIC, R2_adjusted = R2_adjusted)
      return(list(infromation = infromation, fit41 = fit41))
    })

    output$infotable4 <- DT::renderDataTable({
      df <- fit_4()$infromation
      DT::datatable(df, options = list(
        pagingType = 'simple',
        dom = 't'
      ), caption = tags$caption(tags$strong("Model Evaluation:")))
    })

    output$graph_effect41 <- renderPlot({
      FoSIntro::plot_1D(fit_4()$fit41, select = 1, ylab = "Estimate", xlab="Time(s)",main ="Global mean effect", base_size = 25) +
        ggplot2::scale_x_continuous(breaks=seq(0,820,100)) + ggplot2::theme(axis.text.x = ggplot2::element_text(angle=90))
    })
    output$group_effect41 <- renderPlot({
      FoSIntro::plot_1D(fit_4()$fit41, select = 2, ylab = "Estimate", xlab="Time(s)",main ="Mean effect group1", base_size = 25) +
        ggplot2::scale_x_continuous(breaks=seq(0,820,100)) + ggplot2::theme(axis.text.x = ggplot2::element_text(angle=90))
    })
    output$group_effect42 <- renderPlot({
      FoSIntro::plot_1D(fit_4()$fit41, select = 3, ylab = "Estimate", xlab="Time(s)",main ="Mean effect group2", base_size = 25) +
        ggplot2::scale_x_continuous(breaks=seq(0,820,100)) + ggplot2::theme(axis.text.x = ggplot2::element_text(angle=90))
    })
    output$group_effect43 <- renderPlot({
      FoSIntro::plot_1D(fit_4()$fit41, select = 4, ylab = "Estimate", xlab="Time(s)",main ="Mean effect group3", base_size = 25) +
        ggplot2::scale_x_continuous(breaks=seq(0,820,100)) + ggplot2::theme(axis.text.x = ggplot2::element_text(angle=90))
    })
    output$group_effect44 <- renderPlot({
      FoSIntro::plot_1D(fit_4()$fit41, select = 5, ylab = "Estimate", xlab="Time(s)",main ="Mean effect group3", base_size = 25) +
        ggplot2::scale_x_continuous(breaks=seq(0,820,100)) + ggplot2::theme(axis.text.x = ggplot2::element_text(angle=90))
    })

    residual41 <- reactive({
      data_functional = Data_for_anova4()$data_functional
      time <- Data_for_anova4()$time
      residual_fun = t(residuals(fit_4()$fit41))
      colnames(residual_fun) = colnames(data_functional)[-1]
      residual_fun = data.frame(time=time, residual_fun)
      resid_fun = reshape2::melt(residual_fun, id=c("time"))
      fitted_1 = t(fitted(fit_4()$fit41))
      colnames(fitted_1) = colnames(data_functional)[-1]
      fitted_fun = data.frame(time=time, fitted_1)
      fitted_fun1 = reshape2::melt(fitted_fun, id=c("time"))
      resid1 = data.frame(fitted=fitted_fun1$value, residual = resid_fun$value)
      return(list(resid1 = resid1))
    })
    output$group_effect45 <- renderPlot({
      resid_fun2 <- residual41()$resid1
      ggplot2::ggplot(resid_fun2, ggplot2::aes(x = fitted, y = residual)) +  ggplot2::geom_point() +
        ggplot2::geom_hline(yintercept = 0) + ggplot2::labs (x="Fitted", y = "Residuals")+
        ggplot2::theme(axis.title.x = ggplot2::element_text(size = 25))+
        ggplot2::theme(axis.text.y = ggplot2::element_text(size=25))+
        ggplot2::theme(axis.text.x = ggplot2::element_text(size=25))+
        ggplot2::theme(axis.title.y = ggplot2::element_text(size = 30))+
        ggplot2::ggtitle("Analysis of Homoscedasticity of Variance")+
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))+
        ggplot2::theme(plot.title = ggplot2::element_text(size = 20, face = "bold"))
    })
    output$group_effect46 <- renderPlot({
      resid_fun2 <- residual41()$resid1
      ggplot2::ggplot(resid_fun2, ggplot2::aes(sample=residual))+ggplot2::stat_qq()+
        ggplot2::stat_qq_line()+ggplot2::labs (x="Theorical", y = "Sample quantiles")+
        ggplot2::theme(axis.title.x = ggplot2::element_text(size = 25))+
        ggplot2::theme(axis.text.y = ggplot2::element_text(size=25))+
        ggplot2::theme(axis.text.x = ggplot2::element_text(size=25))+
        ggplot2::theme(axis.title.y = ggplot2::element_text(size = 30))+
        ggplot2::ggtitle("Normal Q-Q plot")+
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))+
        ggplot2::theme(plot.title = ggplot2::element_text(size = 20, face = "bold"))
    })

    ############ five groups #################
    ##########################################

    filedata111 <- reactive({
      req(input$data_group111)
      fileInput <- load_file(input$data_group111$name, input$data_group111$datapath)
      fileInput <- as.data.frame(fileInput)
      return(list(fileInput = fileInput))
    })
    filedata222 <- reactive({
      req(input$data_group222)
      fileInput <- load_file(input$data_group222$name, input$data_group222$datapath)
      fileInput <- as.data.frame(fileInput)
      return(list(fileInput = fileInput))
    })
    filedata333 <- reactive({
      req(input$data_group333)
      fileInput <- load_file(input$data_group333$name, input$data_group333$datapath)
      fileInput <- as.data.frame(fileInput)
      return(list(fileInput = fileInput))
    })
    filedata444 <- reactive({
      req(input$data_group444)
      fileInput <- load_file(input$data_group444$name, input$data_group444$datapath)
      fileInput <- as.data.frame(fileInput)
      return(list(fileInput = fileInput))
    })
    filedata555 <- reactive({
      req(input$data_group555)
      fileInput <- load_file(input$data_group555$name, input$data_group555$datapath)
      fileInput <- as.data.frame(fileInput)
      return(list(fileInput = fileInput))
    })

    output$summary51 <- DT::renderDataTable({
      df <- filedata111()$fileInput
      DT::datatable(df)
    })
    output$summary52 <- DT::renderDataTable({
      df <- filedata222()$fileInput
      DT::datatable(df)
    })
    output$summary53 <- DT::renderDataTable({
      df <- filedata333()$fileInput
      DT::datatable(df)
    })
    output$summary54 <- DT::renderDataTable({
      df <- filedata444()$fileInput
      DT::datatable(df)
    })
    output$summary55 <- DT::renderDataTable({
      df <- filedata555()$fileInput
      DT::datatable(df)
    })

    graph_group51 <- reactive({
      req(filedata111()$fileInput)
      df1 <- data.frame(filedata111()$fileInput)
      top51 <- reshape2::melt(df1, id=c(names(df1)[1]))
      g51 <- ggplot2::ggplot(data = top51, ggplot2::aes(x=Time, y=value,
                                                        group = factor(variable),
                                                        colour = factor(variable))) +
        ggplot2::geom_line(size=0.5) +
        ggplot2::theme(text = ggplot2::element_text(size = 13),
                       axis.text.x = ggplot2::element_text(angle=90),
                       panel.background = ggplot2::element_blank(),  # Elimina el fondo gris
                       plot.background = ggplot2::element_blank()) +  # Elimina el fondo del área de trazado
        ggplot2::labs(y = "", x = "Time (s)") +
        ggplot2::theme(legend.position = "none") +
        ggplot2::ggtitle("Group 1")


      return(list(g51 = g51, top51 = top51))
    })

    graph_group52 <- reactive({
      req(filedata222()$fileInput)
      df1 <- data.frame(filedata222()$fileInput)
      top52 <- reshape2::melt(df1, id=c(names(df1)[1]))
      g52 <- ggplot2::ggplot(data = top52, ggplot2::aes(x=Time, y=value,
                                                        group = factor(variable),
                                                        colour = factor(variable))) +
        ggplot2::geom_line(size=0.5) +
        ggplot2::theme(text = ggplot2::element_text(size = 13),
                       axis.text.x = ggplot2::element_text(angle=90),
                       panel.background = ggplot2::element_blank(),  # Elimina el fondo gris
                       plot.background = ggplot2::element_blank()) +  # Elimina el fondo del área de trazado
        ggplot2::labs(y = "", x = "Time (s)") +
        ggplot2::theme(legend.position = "none") +
        ggplot2::ggtitle("Group 2")

      return(list(g52 = g52, top52 = top52))
    })

    graph_group53 <- reactive({
      req(filedata333()$fileInput)
      df1 <- data.frame(filedata333()$fileInput)
      top53 <- reshape2::melt(df1, id=c(names(df1)[1]))
      g53 <- ggplot2::ggplot(data = top53, ggplot2::aes(x=Time, y=value,
                                                        group = factor(variable),
                                                        colour = factor(variable))) +
        ggplot2::geom_line(size=0.5) +
        ggplot2::theme(text = ggplot2::element_text(size = 13),
                       axis.text.x = ggplot2::element_text(angle=90),
                       panel.background = ggplot2::element_blank(),  # Elimina el fondo gris
                       plot.background = ggplot2::element_blank()) +  # Elimina el fondo del área de trazado
        ggplot2::labs(y = "", x = "Time (s)") +
        ggplot2::theme(legend.position = "none") +
        ggplot2::ggtitle("Group 3")

      return(list(g53 = g53, top53 = top53))
    })

    graph_group54 <- reactive({
      req(filedata444()$fileInput)
      df1 <- data.frame(filedata444()$fileInput)
      top54 <- reshape2::melt(df1, id=c(names(df1)[1]))
      g54 <- ggplot2::ggplot(data = top54, ggplot2::aes(x=Time, y=value,
                                                        group = factor(variable),
                                                        colour = factor(variable))) +
        ggplot2::geom_line(size=0.5) +
        ggplot2::theme(text = ggplot2::element_text(size = 13),
                       axis.text.x = ggplot2::element_text(angle=90),
                       panel.background = ggplot2::element_blank(),  # Elimina el fondo gris
                       plot.background = ggplot2::element_blank()) +  # Elimina el fondo del área de trazado
        ggplot2::labs(y = "", x = "Time (s)") +
        ggplot2::theme(legend.position = "none") +
        ggplot2::ggtitle("Group 4")

      return(list(g54 = g54, top54 = top54))
    })

    graph_group55 <- reactive({
      req(filedata555()$fileInput)
      df1 <- data.frame(filedata555()$fileInput)
      top55 <- reshape2::melt(df1, id=c(names(df1)[1]))
      g55 <- ggplot2::ggplot(data = top55, ggplot2::aes(x=Time, y=value,
                                                        group = factor(variable),
                                                        colour = factor(variable))) +
        ggplot2::geom_line(size=0.5) +
        ggplot2::theme(text = ggplot2::element_text(size = 13),
                       axis.text.x = ggplot2::element_text(angle=90),
                       panel.background = ggplot2::element_blank(),  # Elimina el fondo gris
                       plot.background = ggplot2::element_blank()) +  # Elimina el fondo del área de trazado
        ggplot2::labs(y = "", x = "Time (s)") +
        ggplot2::theme(legend.position = "none") +
        ggplot2::ggtitle("Group 5")

      return(list(g55 = g55, top55 = top55))
    })

    means5 <- reactive({
      library(magrittr)
      req(filedata111()$fileInput)
      req(filedata222()$fileInput)
      req(filedata333()$fileInput)
      req(filedata444()$fileInput)
      req(filedata555()$fileInput)
      data1=rbind(graph_group51()$top51, graph_group52()$top52,
                  graph_group53()$top53, graph_group54()$top54,
                  graph_group55()$top55)
      df1 <- data.frame(filedata111()$fileInput)
      df2 <- data.frame(filedata222()$fileInput)
      df3 <- data.frame(filedata333()$fileInput)
      df4 <- data.frame(filedata444()$fileInput)
      df5 <- data.frame(filedata555()$fileInput)
      group <- c(rep('1', length(df1[,1])*ncol(df1[,-1])),     #df1[,1] es el tiempo
                 rep('2', length(df2[,1])*ncol(df2[,-1])),
                 rep('3', length(df3[,1])*ncol(df3[,-1])),
                 rep('4', length(df4[,1])*ncol(df4[,-1])),
                 rep('5', length(df5[,1])*ncol(df5[,-1]))
      )
      data2 = data.frame(data1, group=group)
      prom_global = aggregate(value~Time,data = data2, FUN = mean)
      mean4 <- data2%>%dplyr::group_by(group,Time)%>%dplyr::summarise(Mean = mean(value))%>%as.data.frame()
      prom1 = data.frame(group=c(rep("Global",length(df1[,1]))), Time=prom_global$Time, Mean=prom_global$value)
      prom3 = rbind(mean4, prom1)
      m5 = ggplot2::ggplot(prom3, ggplot2::aes(x=Time, y=Mean, group = group, colour = group)) +
        ggplot2::geom_line(size=1) +
        ggplot2::theme(text = ggplot2::element_text(size = 13), axis.text.x = ggplot2::element_text(angle=90),
                       panel.background = ggplot2::element_blank(),  # Elimina el fondo gris
                       plot.background = ggplot2::element_blank()) +  # Elimina el fondo del área de trazado
        ggplot2::labs(y  = "", x= "Time (s)") +
        ggplot2::ggtitle("Average curves")+
        ggplot2::scale_color_manual(values = c("red", "blue", "aquamarine2", "green", "orange", "black")) +
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
      return(list(m5 = m5))
    })

    output$graph_mean5 <- renderPlot({
      gridExtra::grid.arrange(graph_group51()$g51,
                              graph_group52()$g52,
                              graph_group53()$g53,
                              graph_group54()$g54,
                              graph_group55()$g55,
                              means5()$m5, ncol = 2)
    })

    Data_for_anova5 <- reactive({
      req(filedata111()$fileInput)
      req(filedata222()$fileInput)
      req(filedata333()$fileInput)
      req(filedata444()$fileInput)
      req(filedata555()$fileInput)
      df1 <- data.frame(filedata111()$fileInput)
      df2 <- data.frame(filedata222()$fileInput)
      df3 <- data.frame(filedata333()$fileInput)
      df4 <- data.frame(filedata444()$fileInput)
      df5 <- data.frame(filedata555()$fileInput)

      antigen1 = c(rep('1',ncol(df1[,-1])),
                   rep('2', ncol(df2[,-1])),
                   rep('3', ncol(df3[,-1])),
                   rep('4', ncol(df4[,-1])),
                   rep('5', ncol(df5[,-1]))
      )
      data_functional = cbind(df1,
                              df2[,-1],
                              df3[,-1],
                              df4[,-1],
                              df5[,-1]
      )
      datafun = cbind(data_functional[,-1])
      id = seq(1:(as.numeric(dim(datafun)[2])))
      colnames(datafun) <- c(id)
      data1 <- data.frame(antigen2=factor(antigen1))
      time = c(data_functional[,1])
      Ydata = matrix(t(datafun), as.numeric(dim(datafun)[2]), as.numeric(dim(datafun)[1]))
      return(list(Ydata = Ydata, data_functional = data_functional, data1 = data1, time = time))
    })


    fit_5 <- reactive({
      minn <- abs(min(min(graph_group51()$top51[,3]), min(graph_group52()$top52[,3]),
                      min(graph_group53()$top53[,3]), min(graph_group54()$top54[,3]),
                      min(graph_group55()$top55[,3])))
      minn1 <- ifelse(minn %% 1 == 0, minn + 1, ceiling(minn))
      if (input$checkbox) {Ydata = log(Data_for_anova5()$Ydata + minn1)}
      else{Ydata = Data_for_anova5()$Ydata}
      data_functional = Data_for_anova5()$data_functional
      data1 = Data_for_anova5()$data1
      time <- Data_for_anova5()$time
      if (input$checkbox1) {
        fit51 <- refund::pffr(Ydata ~ antigen2 + c(antigen2), yind = time, data = data1, method = "REML",
                              bs.yindex=list(bs="ps", k = as.numeric(input$k1), m=c(2, 1)),
                              bs.int=list(bs="ps", k = as.numeric(input$k2), m=c(2,1)))
      }
      else {
        fit51 <- refund::pffr(Ydata ~ antigen2, yind = time, data = data1, method = "REML",
                              bs.yindex=list(bs="ps", k = as.numeric(input$k1), m=c(2, 1)),
                              bs.int=list(bs="ps", k = as.numeric(input$k2), m=c(2,1)))
      }
      residual_fun = t(residuals(fit51))
      colnames(residual_fun) = colnames(data_functional)[-1]
      residual_fun = data.frame(time=time, residual_fun)
      residual_fun1 = reshape2::melt(residual_fun, id=c("time"))
      RMSE = sqrt(mean((residual_fun1$value)^2))
      AIC = AIC(fit51)
      BIC=BIC(fit51)
      R2_adjusted = summary(fit51)$r.sq*100
      infromation = data.frame(RMSE = RMSE, AIC = AIC , BIC = BIC, R2_adjusted = R2_adjusted)
      return(list(infromation = infromation, fit51 = fit51))
    })

    output$infotable5 <- DT::renderDataTable({
      df <- fit_5()$infromation
      DT::datatable(df, options = list(
        pagingType = 'simple',
        dom = 't'
      ), caption = tags$caption(tags$strong("Model Evaluation:")))
    })

    output$graph_effect51 <- renderPlot({
      FoSIntro::plot_1D(fit_5()$fit51, select = 1, ylab = "Estimate", xlab="Time(s)",main ="Global mean effect", base_size = 25) +
        ggplot2::scale_x_continuous(breaks=seq(0,820,100)) + ggplot2::theme(axis.text.x = ggplot2::element_text(angle=90))
    })
    output$group_effect51 <- renderPlot({
      FoSIntro::plot_1D(fit_5()$fit51, select = 2, ylab = "Estimate", xlab="Time(s)",main ="Mean effect group1", base_size = 25) +
        ggplot2::scale_x_continuous(breaks=seq(0,820,100)) + ggplot2::theme(axis.text.x = ggplot2::element_text(angle=90))
    })
    output$group_effect52 <- renderPlot({
      FoSIntro::plot_1D(fit_5()$fit51, select = 3, ylab = "Estimate", xlab="Time(s)",main ="Mean effect group2", base_size = 25) +
        ggplot2::scale_x_continuous(breaks=seq(0,820,100)) + ggplot2::theme(axis.text.x = ggplot2::element_text(angle=90))
    })
    output$group_effect53 <- renderPlot({
      FoSIntro::plot_1D(fit_5()$fit51, select = 4, ylab = "Estimate", xlab="Time(s)",main ="Mean effect group3", base_size = 25) +
        ggplot2::scale_x_continuous(breaks=seq(0,820,100)) + ggplot2::theme(axis.text.x = ggplot2::element_text(angle=90))
    })
    output$group_effect54 <- renderPlot({
      FoSIntro::plot_1D(fit_5()$fit51, select = 5, ylab = "Estimate", xlab="Time(s)",main ="Mean effect group3", base_size = 25) +
        ggplot2::scale_x_continuous(breaks=seq(0,820,100)) + ggplot2::theme(axis.text.x = ggplot2::element_text(angle=90))
    })
    output$group_effect55 <- renderPlot({
      FoSIntro::plot_1D(fit_5()$fit51, select = 6, ylab = "Estimate", xlab="Time(s)",main ="Mean effect group3", base_size = 25) +
        ggplot2::scale_x_continuous(breaks=seq(0,820,100)) + ggplot2::theme(axis.text.x = ggplot2::element_text(angle=90))
    })

    residual51 <- reactive({
      data_functional = Data_for_anova5()$data_functional
      time <- Data_for_anova5()$time
      residual_fun = t(residuals(fit_5()$fit51))
      colnames(residual_fun) = colnames(data_functional)[-1]
      residual_fun = data.frame(time=time, residual_fun)
      resid_fun = reshape2::melt(residual_fun, id=c("time"))
      fitted_1 = t(fitted(fit_5()$fit51))
      colnames(fitted_1) = colnames(data_functional)[-1]
      fitted_fun = data.frame(time=time, fitted_1)
      fitted_fun1 = reshape2::melt(fitted_fun, id=c("time"))
      resid1 = data.frame(fitted=fitted_fun1$value, residual = resid_fun$value)
      return(list(resid1 = resid1))
    })
    output$group_effect56 <- renderPlot({
      resid_fun2 <- residual51()$resid1
      ggplot2::ggplot(resid_fun2, ggplot2::aes(x = fitted, y = residual)) +  ggplot2::geom_point() +
        ggplot2::geom_hline(yintercept = 0) + ggplot2::labs (x="Fitted", y = "Residuals")+
        ggplot2::theme(axis.title.x = ggplot2::element_text(size = 25))+
        ggplot2::theme(axis.text.y = ggplot2::element_text(size=25))+
        ggplot2::theme(axis.text.x = ggplot2::element_text(size=25))+
        ggplot2::theme(axis.title.y = ggplot2::element_text(size = 30))+
        ggplot2::ggtitle("Analysis of Homoscedasticity of Variance")+
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))+
        ggplot2::theme(plot.title = ggplot2::element_text(size = 20, face = "bold"))
    })
    output$group_effect57 <- renderPlot({
      resid_fun2 <- residual51()$resid1
      ggplot2::ggplot(resid_fun2, ggplot2::aes(sample=residual))+ggplot2::stat_qq()+
        ggplot2::stat_qq_line()+ggplot2::labs (x="Theorical", y = "Sample quantiles")+
        ggplot2::theme(axis.title.x = ggplot2::element_text(size = 25))+
        ggplot2::theme(axis.text.y = ggplot2::element_text(size=25))+
        ggplot2::theme(axis.text.x = ggplot2::element_text(size=25))+
        ggplot2::theme(axis.title.y = ggplot2::element_text(size = 30))+
        ggplot2::ggtitle("Normal Q-Q plot")+
        ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))+
        ggplot2::theme(plot.title = ggplot2::element_text(size = 20, face = "bold"))
    })

    ### groups comparations for 5 levels
    ########################
    comparations <- reactive({

      minn <- abs(min(min(graph_group51()$top51[,3]), min(graph_group52()$top52[,3]),
                      min(graph_group53()$top53[,3]), min(graph_group54()$top54[,3]),
                      min(graph_group55()$top55[,3])))
      minn1 <- ifelse(minn %% 1 == 0, minn + 1, ceiling(minn))

      if (input$checkbox) {Ydata = log(Data_for_anova5()$Ydata + minn1)}
      else{Ydata = Data_for_anova5()$Ydata}
      data_functional = Data_for_anova5()$data_functional
      data1 = Data_for_anova5()$data1
      time <- Data_for_anova5()$time

      fit51 <- fit_5()$fit51

      coefi <- coef(fit51)

      antigen21 = data.frame(coefi$smterms[2][1])[,c(1, 4, 8)]
      colnames(antigen21) <- c("Time1","coef_A1_value", "coef_A1_se")
      coef_A1_se <- antigen21$coef_A1_se

      antigen22 = data.frame(coefi$smterms[3][1])[,c(1, 4, 8)]
      colnames(antigen22) <- c("Time1","coef_A2_value", "coef_A2_se")
      coef_A2_se <- antigen22$coef_A2_se

      antigen23 = data.frame(coefi$smterms[4][1])[,c(1, 4, 8)]
      colnames(antigen23) <- c("Time1","coef_A3_value", "coef_A3_se")
      coef_A3_se <- antigen23$coef_A3_se

      antigen24 = data.frame(coefi$smterms[5][1])[,c(1, 4, 8)]
      colnames(antigen24) <- c("Time1","coef_A4_value", "coef_A4_se")
      coef_A4_se <- antigen24$coef_A4_se

      antigen25 = data.frame(coefi$smterms[6][1])[,c(1, 4, 8)]
      colnames(antigen25) <- c("Time1","coef_A5_value", "coef_A5_se")
      coef_A5_se <- antigen25$coef_A5_se

      dif_2_1 = antigen22$coef_A2_value - antigen21$coef_A1_value


      predframe2_1 <- data.frame(Time1 = antigen21$Time1, dif_2_1,
                                 CI_lwr=dif_2_1-1.96*sqrt(coef_A1_se^2+coef_A2_se^2),
                                 CI_upr = dif_2_1+1.96*sqrt(coef_A1_se^2+coef_A2_se^2))

      dif_3_1 = antigen23$coef_A3_value - antigen21$coef_A1_value

      predframe3_1 <- data.frame(Time1 = antigen21$Time1, dif_3_1,
                                 CI_lwr=dif_3_1-1.96*sqrt(coef_A1_se^2+coef_A3_se^2),
                                 CI_upr = dif_3_1+1.96*sqrt(coef_A1_se^2+coef_A3_se^2))

      dif_4_1 = antigen24$coef_A4_value - antigen21$coef_A1_value

      predframe4_1 <- data.frame(Time1 = antigen21$Time1, dif_4_1,
                                 CI_lwr=dif_4_1-1.96*sqrt(coef_A1_se^2+coef_A4_se^2),
                                 CI_upr = dif_4_1+1.96*sqrt(coef_A1_se^2+coef_A4_se^2))

      dif_5_1 = antigen25$coef_A5_value - antigen21$coef_A1_value

      predframe5_1 <- data.frame(Time1 = antigen21$Time1, dif_5_1,
                                 CI_lwr=dif_5_1-1.96*sqrt(coef_A1_se^2+coef_A5_se^2),
                                 CI_upr = dif_5_1+1.96*sqrt(coef_A1_se^2+coef_A5_se^2))

      dif_2_3 = antigen22$coef_A2_value - antigen23$coef_A3_value

      predframe2_3 <- data.frame(Time1 = antigen22$Time1, dif_2_3,
                                 CI_lwr=dif_2_3-1.96*sqrt(coef_A2_se^2+coef_A3_se^2),
                                 CI_upr = dif_2_3+1.96*sqrt(coef_A2_se^2+coef_A3_se^2))

      dif_2_4 = antigen22$coef_A2_value - antigen24$coef_A4_value

      predframe2_4 <- data.frame(Time1 = antigen22$Time1, dif_2_4,
                                 CI_lwr=dif_2_4-1.96*sqrt(coef_A2_se^2+coef_A4_se^2),
                                 CI_upr = dif_2_4+1.96*sqrt(coef_A2_se^2+coef_A4_se^2))

      dif_2_5 = antigen22$coef_A2_value - antigen25$coef_A5_value

      predframe2_5 <- data.frame(Time1 = antigen22$Time1, dif_2_5,
                                 CI_lwr=dif_2_5-1.96*sqrt(coef_A2_se^2+coef_A5_se^2),
                                 CI_upr = dif_2_5+1.96*sqrt(coef_A2_se^2+coef_A5_se^2))

      dif_3_4 = antigen23$coef_A3_value - antigen24$coef_A4_value

      predframe3_4 <- data.frame(Time1 = antigen23$Time1, dif_3_4,
                                 CI_lwr=dif_3_4-1.96*sqrt(coef_A3_se^2+coef_A4_se^2),
                                 CI_upr = dif_3_4+1.96*sqrt(coef_A3_se^2+coef_A4_se^2))

      dif_3_5 = antigen23$coef_A3_value - antigen25$coef_A5_value

      predframe3_5 <- data.frame(Time1 = antigen23$Time1, dif_3_5,
                                 CI_lwr=dif_3_5-1.96*sqrt(coef_A3_se^2+coef_A5_se^2),
                                 CI_upr = dif_3_5+1.96*sqrt(coef_A3_se^2+coef_A5_se^2))

      dif_4_5 = antigen24$coef_A4_value - antigen25$coef_A5_value

      predframe4_5 <- data.frame(Time1 = antigen24$Time1, dif_4_5,
                                 CI_lwr=dif_4_5-1.96*sqrt(coef_A4_se^2+coef_A5_se^2),
                                 CI_upr = dif_4_5+1.96*sqrt(coef_A4_se^2+coef_A5_se^2))

      return(list(predframe2_1 = predframe2_1, predframe3_1 = predframe3_1,
                  predframe4_1 = predframe4_1, predframe5_1 = predframe5_1,
                  predframe2_3 = predframe2_3, predframe2_4 = predframe2_4,
                  predframe2_5 = predframe2_5, predframe3_4 = predframe3_4,
                  predframe3_5 = predframe3_5, predframe4_5 = predframe4_5))
    })


    output$Compartion1 <- renderPlot({
      predframe = comparations()$predframe2_1
      ggplot2::ggplot(predframe, ggplot2::aes(Time1,dif_2_1)) + ggplot2::geom_line()+
        ggplot2::geom_ribbon(data=predframe,ggplot2::aes(ymin=CI_lwr,ymax=CI_upr),alpha=0.3)+
        ggplot2::labs(x = "Time(s)", y= "Estimated effect difference",
                      title = "Group 2 and Group 1" ) +
        ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white", color = "black"))+
        ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "black")+
        #ggplot2::scale_x_continuous(breaks=seq(0,820,50))+
        ggplot2::theme(plot.title = ggplot2::element_text(size = 17, hjust = 0.5),
                       axis.text.x = ggplot2::element_text(angle=90),
                       axis.text = ggplot2::element_text(size = 17),
                       axis.title = ggplot2::element_text(size = 17)) #+
      #ggplot2::scale_y_continuous(limits = c(-0.25, 0.25))
    })

    output$Compartion2 <- renderPlot({
      predframe = comparations()$predframe3_1
      ggplot2::ggplot(predframe, ggplot2::aes(Time1,dif_3_1)) + ggplot2::geom_line()+
        ggplot2::geom_ribbon(data=predframe,ggplot2::aes(ymin=CI_lwr,ymax=CI_upr),alpha=0.3)+
        ggplot2::labs(x = "Time(s)", y= "Estimated effect difference",
                      title = "Group 3 and Group 1" ) +
        ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white", color = "black"))+
        ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "black")+
        #ggplot2::scale_x_continuous(breaks=seq(0,820,50))+
        ggplot2::theme(plot.title = ggplot2::element_text(size = 17, hjust = 0.5),
                       axis.text.x = ggplot2::element_text(angle=90),
                       axis.text = ggplot2::element_text(size = 17),
                       axis.title = ggplot2::element_text(size = 17)) #+
      #ggplot2::scale_y_continuous(limits = c(-0.25, 0.25))
    })

    output$Compartion3 <- renderPlot({
      predframe = comparations()$predframe4_1
      ggplot2::ggplot(predframe, ggplot2::aes(Time1,dif_4_1)) + ggplot2::geom_line()+
        ggplot2::geom_ribbon(data=predframe,ggplot2::aes(ymin=CI_lwr,ymax=CI_upr),alpha=0.3)+
        ggplot2::labs(x = "Time(s)", y= "Estimated effect difference",
                      title = "Group 4 and Group 1" ) +
        ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white", color = "black"))+
        ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "black")+
        #ggplot2::scale_x_continuous(breaks=seq(0,820,50))+
        ggplot2::theme(plot.title = ggplot2::element_text(size = 17, hjust = 0.5),
                       axis.text.x = ggplot2::element_text(angle=90),
                       axis.text = ggplot2::element_text(size = 17),
                       axis.title = ggplot2::element_text(size = 17)) #+
      #ggplot2::scale_y_continuous(limits = c(-0.25, 0.25))
    })

    output$Compartion4 <- renderPlot({
      predframe = comparations()$predframe5_1
      ggplot2::ggplot(predframe, ggplot2::aes(Time1,dif_5_1)) + ggplot2::geom_line()+
        ggplot2::geom_ribbon(data=predframe,ggplot2::aes(ymin=CI_lwr,ymax=CI_upr),alpha=0.3)+
        ggplot2::labs(x = "Time(s)", y= "Estimated effect difference",
                      title = "Group 5 and Group 1" ) +
        ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white", color = "black"))+
        ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "black")+
        #ggplot2::scale_x_continuous(breaks=seq(0,820,50))+
        ggplot2::theme(plot.title = ggplot2::element_text(size = 17, hjust = 0.5),
                       axis.text.x = ggplot2::element_text(angle=90),
                       axis.text = ggplot2::element_text(size = 17),
                       axis.title = ggplot2::element_text(size = 17)) #+
      #ggplot2::scale_y_continuous(limits = c(-0.25, 0.25))
    })

    output$Compartion5 <- renderPlot({
      predframe = comparations()$predframe2_3
      ggplot2::ggplot(predframe, ggplot2::aes(Time1,dif_2_3)) + ggplot2::geom_line()+
        ggplot2::geom_ribbon(data=predframe,ggplot2::aes(ymin=CI_lwr,ymax=CI_upr),alpha=0.3)+
        ggplot2::labs(x = "Time(s)", y= "Estimated effect difference",
                      title = "Group 2 and Group 3" ) +
        ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white", color = "black"))+
        ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "black")+
        #ggplot2::scale_x_continuous(breaks=seq(0,820,50))+
        ggplot2::theme(plot.title = ggplot2::element_text(size = 17, hjust = 0.5),
                       axis.text.x = ggplot2::element_text(angle=90),
                       axis.text = ggplot2::element_text(size = 17),
                       axis.title = ggplot2::element_text(size = 17)) #+
      #ggplot2::scale_y_continuous(limits = c(-0.25, 0.25))
    })

    output$Compartion6 <- renderPlot({
      predframe = comparations()$predframe2_4
      ggplot2::ggplot(predframe, ggplot2::aes(Time1,dif_2_4)) + ggplot2::geom_line()+
        ggplot2::geom_ribbon(data=predframe,ggplot2::aes(ymin=CI_lwr,ymax=CI_upr),alpha=0.3)+
        ggplot2::labs(x = "Time(s)", y= "Estimated effect difference",
                      title = "Group 2 and Group 4" ) +
        ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white", color = "black"))+
        ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "black")+
        #ggplot2::scale_x_continuous(breaks=seq(0,820,50))+
        ggplot2::theme(plot.title = ggplot2::element_text(size = 17, hjust = 0.5),
                       axis.text.x = ggplot2::element_text(angle=90),
                       axis.text = ggplot2::element_text(size = 17),
                       axis.title = ggplot2::element_text(size = 17)) #+
      #ggplot2::scale_y_continuous(limits = c(-0.25, 0.25))
    })

    output$Compartion7 <- renderPlot({
      predframe = comparations()$predframe2_5
      ggplot2::ggplot(predframe, ggplot2::aes(Time1,dif_2_5)) + ggplot2::geom_line()+
        ggplot2::geom_ribbon(data=predframe,ggplot2::aes(ymin=CI_lwr,ymax=CI_upr),alpha=0.3)+
        ggplot2::labs(x = "Time(s)", y= "Estimated effect difference",
                      title = "Group 2 and Group 5" ) +
        ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white", color = "black"))+
        ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "black")+
        #ggplot2::scale_x_continuous(breaks=seq(0,820,50))+
        ggplot2::theme(plot.title = ggplot2::element_text(size = 17, hjust = 0.5),
                       axis.text.x = ggplot2::element_text(angle=90),
                       axis.text = ggplot2::element_text(size = 17),
                       axis.title = ggplot2::element_text(size = 17)) #+
      #ggplot2::scale_y_continuous(limits = c(-0.25, 0.25))
    })

    output$Compartion8 <- renderPlot({
      predframe = comparations()$predframe3_4
      ggplot2::ggplot(predframe, ggplot2::aes(Time1,dif_3_4)) + ggplot2::geom_line()+
        ggplot2::geom_ribbon(data=predframe,ggplot2::aes(ymin=CI_lwr,ymax=CI_upr),alpha=0.3)+
        ggplot2::labs(x = "Time(s)", y= "Estimated effect difference",
                      title = "Group 3 and Group 4" ) +
        ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white", color = "black"))+
        ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "black")+
        #ggplot2::scale_x_continuous(breaks=seq(0,820,50))+
        ggplot2::theme(plot.title = ggplot2::element_text(size = 17, hjust = 0.5),
                       axis.text.x = ggplot2::element_text(angle=90),
                       axis.text = ggplot2::element_text(size = 17),
                       axis.title = ggplot2::element_text(size = 17)) #+
      #ggplot2::scale_y_continuous(limits = c(-0.25, 0.25))
    })

    output$Compartion9 <- renderPlot({
      predframe = comparations()$predframe3_5
      ggplot2::ggplot(predframe, ggplot2::aes(Time1,dif_3_5)) + ggplot2::geom_line()+
        ggplot2::geom_ribbon(data=predframe,ggplot2::aes(ymin=CI_lwr,ymax=CI_upr),alpha=0.3)+
        ggplot2::labs(x = "Time(s)", y= "Estimated effect difference",
                      title = "Group 3 and Group 5" ) +
        ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white", color = "black"))+
        ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "black")+
        #ggplot2::scale_x_continuous(breaks=seq(0,820,50))+
        ggplot2::theme(plot.title = ggplot2::element_text(size = 17, hjust = 0.5),
                       axis.text.x = ggplot2::element_text(angle=90),
                       axis.text = ggplot2::element_text(size = 17),
                       axis.title = ggplot2::element_text(size = 17)) #+
      #ggplot2::scale_y_continuous(limits = c(-0.25, 0.25))
    })

    output$Compartion10 <- renderPlot({
      predframe = comparations()$predframe4_5
      ggplot2::ggplot(predframe, ggplot2::aes(Time1,dif_4_5)) + ggplot2::geom_line()+
        ggplot2::geom_ribbon(data=predframe,ggplot2::aes(ymin=CI_lwr,ymax=CI_upr),alpha=0.3)+
        ggplot2::labs(x = "Time(s)", y= "Estimated effect difference",
                      title = "Group 4 and Group 5" ) +
        ggplot2::theme(panel.background = ggplot2::element_rect(fill = "white", color = "black"))+
        ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "black")+
        #ggplot2::scale_x_continuous(breaks=seq(0,820,50))+
        ggplot2::theme(plot.title = ggplot2::element_text(size = 17, hjust = 0.5),
                       axis.text.x = ggplot2::element_text(angle=90),
                       axis.text = ggplot2::element_text(size = 17),
                       axis.title = ggplot2::element_text(size = 17)) #+
      #ggplot2::scale_y_continuous(limits = c(-0.25, 0.25))
    })



  })
}

## To be copied in the UI
# mod_FunctionalANOVA_ui("FunctionalANOVA_1")

## To be copied in the server
# mod_FunctionalANOVA_server("FunctionalANOVA_1")
