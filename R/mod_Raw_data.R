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
                   shinyjs::useShinyjs(),
                   tags$style(HTML(".param-label {display: flex; align-items: flex-start;}.small-button { font-size: 10px; padding: 2px 2px; }")),

                    actionButton(ns("param_info_button11"), "Help",
                                class = "btn-sm",
                                style = "position: absolute; top: 0; right: 15px; margin: 5px;"),

                       fileInput(ns("fileBcsv2"),
                                 accept = c('text/csv',
                                            'text/comma-separated-values,text/plain',
                                            '.csv'),
                                 label = h5("Dataset")),

                   # radioButtons(ns("data_simulate"), "Data simulate",  #data example
                   #                    choices = c("Yes"=1, "No"=0),
                   #                    selected = 0),

                   div(style = "border-top: 1px solid #ccc; margin-top: 10px; margin-bottom: 10px;"),

                   numericInput(inputId = ns("Cell2"),
                                label = "Region of Interest (ROI):",
                                value = 1, min = 1),
                   # numericInput(inputId = ns("span"),
                   #              label = "Smoothness Control:",
                   #              value = 0.05, min = 0, max = 1,step = 0.01),

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
                                           "Your baseline"=4,
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
                   radioButtons(
                     inputId = ns("FWHM"),
                     label = "Full Width at Half Maximum:",
                     choices = c("No" = 1, "Yes" = 2), selected = 1
                   ),
                   downloadButton(ns("descargarP"), "Trace Metrics"),
                   downloadButton(ns("descargar"), "Transient Metrics"),
                   downloadButton(ns("Calcium_Trance_Graph"), "Calcium Trace Graph")
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
                              actionButton(ns("legends"), "Legends",
                                           class = "btn-sm",
                                           style = "position: absolute; top: 80px; right: 160px; margin: 5px;"),
                              plotOutput(ns("plot_peak3")),
                              plotOutput(ns("derivative")),
                              #plotOutput(ns("plot_raw_smoothed"))
                     ),
                     # tabPanel("Components",
                     #          #plotOutput(ns("plot_component")),
                     #          plotOutput(ns("panel")),
                     #          verbatimTextOutput(ns("outputList")),
                     #          plotOutput(ns("plot_ls")),
                     #          plotOutput(ns("plot_box_ls")),
                     #          plotOutput(ns("plot_ls1")),
                     #          plotOutput(ns("plot_box_ls1")),
                     #          plotOutput(ns("plot_ls2")),
                     #          plotOutput(ns("plot_box_ls2")),
                     # )
                   )
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

    #########Help #########

    observeEvent(input$param_info_button11, {
      showModal(modalDialog(
        title = "Help",
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
        <strong>Region of Interest (ROI):</strong> Choose the cell you want to analyze, corresponding to the columns in the loaded dataset.
      </p>
    "),
        HTML("
      <p style='text-align: justify;'>
        <strong>Peak Height (min):</strong> This is the minimum (absolute) height required for a peak to be recognized.
      </p>
    "),
        HTML("
      <p style='text-align: justify;'>
        <strong>Peak Ascent:</strong> This is the minimum number of increasing steps required before a peak is reached.
      </p>
    "),
        HTML("
      <p style='text-align: justify;'>
        <strong>Peak Descent:</strong> This is the minimum number of decreasing steps required after a peak.
      </p>
    "),
        HTML("
      <p style='text-align: justify;'>
        <strong>Min Peak Distance:</strong> This is the minimum distance (in indices) that peaks must have to be counted.
      </p>
    "),
        HTML("
      <p style='text-align: justify;'>
        <strong>FWHP (min):</strong> The minimum Full Width at Half-Prominence considered for a peak to be identified as such.
      </p>
    "),
        HTML("
      <p style='text-align: justify;'>
        <strong>Prominence (min):</strong> This is the prominence distance that peaks must have to be counted.
      </p>
    "),
        HTML("
      <p style='text-align: justify;'>
        <strong>Area Under the Curve (AUC):</strong> You can choose to calculate the area under the curve or not.
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
        <strong>Baseline:</strong> You can select a Baseline.
      </p>
    "),
        HTML("
      <p style='text-align: justify;'>
        - 'Reference Level 0': The baseline takes the line y = 0.
      </p>
      <p style='text-align: justify;'>
        - 'Standard definition': The baseline is the average fluorescence from the beginning of the signal up to the Time Onset.
      </p>
      <p style='text-align: justify;'>
        - 'Interval': Here you can define a time interval of your preference where you can calculate the average fluorescence of the signal, and this average will be used as the baseline.
      </p>
      <p style='text-align: justify;'>
        - 'Your baseline': You can define your own baseline value.
      </p>
       <p style='text-align: justify;'>
        - 'min': Baseline is defined as the minimum fluorescence value of the signal.
      </p>
    "),
        HTML("
      <p style='text-align: justify;'>
        <strong> You can choose whether to display or not the Full Width at Half Maximum (FWHM) on the Calcium Trace graph.</strong>
      </p>
    "),
        HTML("
      <p style='text-align: justify;'>
        - 'No': If you do not wish to display the Full Width at Half Maximum (FWHM) on the Calcium Trace graph.
      </p>
      <p style='text-align: justify;'>
        - 'Yes': If you wish to display the Full Width at Half Maximum (FWHM) on the Calcium Trace graph.
      </p>
    "),


        HTML("
  <div class='container'>
    <h2>Metrics Definitions</h2>
    <div style='overflow-x: auto;'> <!-- Agregamos un contenedor con desplazamiento horizontal -->
     <table class='table table-striped table-bordered custom-width' style='max-width: 60%;'>
  <colgroup>
    <col style='width: 30%;'>
    <col style='width: 50%;'>
    <col style='width: 20%;'>
  </colgroup>
  <thead>
    <tr>
      <th>Metric</th>
      <th>Description</th>
      <th>Reference</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td>Peak</td>
      <td>The peak is the maximum fluorescence ratio during the transient.</td>
      <td>[1]</td>
    </tr>
    <tr>
      <td>Amplitude or Peak Height</td>
      <td>Peak height is the difference between the baseline and peak, i.e., the amplitude of the transient, constructed using the following formula:
      Amplitude = Peak Calcium Level - Baseline Calcium</td>
      <td>[1,2]</td>
    </tr>
    <tr>
      <td>Full Width at Half-Maximum (FWHM)</td>
      <td>The maximum distance a wave travels from its resting or equilibrium position, usually reported as the spatial spread. Response duration, which is the time between half amplitude on the ascent and descent of the transient.</td>
      <td>[3,4,5]</td>
    </tr>
    <tr>
      <td>Prominence</td>
      <td>Prominence refers to the distinctiveness of a peak in a calcium signal. It is a measure of how much a specific peak stands out from the surrounding fluctuations in calcium concentration. Prominence takes into account both the height of the peak and its relative position compared to neighboring events.</td>
      <td>[6]</td>
    </tr>
    <tr>
      <td>Full Width at Half-Prominence (FWHP)</td>
      <td>It represents the width of a signal, typically a peak or an event, at the level where its prominence is equal to half of its maximum prominence. It measures the width of a feature in a signal at the point where its intensity has decreased by half from the peak value. This parameter is also referred to as transient duration 50 (TD50).</td>
      <td>[6,7]</td>
    </tr>
    <tr>
      <td>Peak Occurrence Time</td>
      <td>Refers to the specific moment or point in time when a calcium signal reaches its maximum amplitude or concentration during a transient event.</td>
      <td>[8]</td>
    </tr>
    <tr>
      <td>Time Onset</td>
      <td>It refers to the moment when the increase in calcium concentration within a cell begins. It marks the initiation or starting point of the transient event.</td>
      <td>[5,9]</td>
    </tr>
    <tr>
      <td>Transient Occurrence Time</td>
      <td>It alludes to the specific moment or time at which a transient event, characterized by a temporary increase in calcium concentration within a cell, takes place. It signifies the initiation or onset of the transient event and is typically measured from the beginning of the recording or a specific reference point.</td>
      <td></td>
    </tr>
    <tr>
      <td>Peak Rise Time</td>
      <td>Amount of time that a calcium transient takes to reach its peak from the baseline.</td>
      <td>[10]</td>
    </tr>
    <tr>
      <td>Baseline</td>
      <td>The baseline is defined as the fluorescence ratio at the beginning of the transient. The base or starting level of calcium before a signal occurs. Resting calcium concentration in the cytosol of a cell.</td>
      <td>[1,3]</td>
    </tr>
    <tr>
      <td>Area Under the Curve (AUC)</td>
      <td>A statistical technique with mathematical integration to quantify the exposure of a calcium signal in a specific period.</td>
      <td>[11]</td>
    </tr>
    <tr>
      <td>Number of Peaks</td>
      <td>The number of calcium transients or spikes in a signal denotes the number of peaks. Calculated from discerning the time between changes in calcium concentrations.</td>
      <td>[12–14]</td>
    </tr>
    <tr>
      <td>Frequency</td>
      <td>How often an event repeats over a period of time represents frequency. Regarding calcium, it refers to the oscillatory feature of a calcium signal. The duration of each change in calcium concentration.</td>
      <td>[15]</td>
    </tr>
    <tr>
      <td>Rise Rate</td>
      <td>The rate of change of the average fluorescence intensity with respect to time, specifically,
      is the change in average fluorescence relative to its initial value per second.
      This expression can be used to quantify how quickly the average frequency is increasing in relation to time, and the maximal velocity is usually reported.
      </td>
      <td>[16,17]</td>
    </tr>
  </tbody>
</table>
</div>
  </div>
"),
 HTML("
<p style='text-align: justify;'>
  <strong>Reference Levels:</strong>
  <ol>
    <li>Knyrim M, Rabe S, Grossmann C, Gekle M, Schreier B. Influence of miR-221/222 on cardiomyocyte calcium handling and function. Cell Biosci. 2021 Dec;11(1):160.</li>
    <li>Ríos E, Shirokova N, Kirsch WG, Pizarro G, Stern MD, Cheng H, et al. A Preferred Amplitude of Calcium Sparks in Skeletal Muscle. Biophys J. 2001 Jan;80(1):169–83.</li>
    <li>Smith IF, Wiltgen SM, Parker I. Localization of puff sites adjacent to the plasma membrane: Functional and spatial characterization of Ca2+ signaling in SH-SY5Y cells utilizing membrane-permeant caged IP3. Cell Calcium. 2009 Jan;45(1):65–76.</li>
    <li>Smedler E, Uhlén P. Frequency decoding of calcium oscillations. Biochim Biophys Acta BBA - Gen Subj. 2014 Mar;1840(3):964–9.</li>
    <li>Mackay L, Mikolajewicz N, Komarova SV, Khadra A. Systematic Characterization of Dynamic Parameters of Intracellular Calcium Signals. Front Physiol [Internet]. 2016 Nov 10 [cited 2024 Jan 10];7. Available from: http://journal.frontiersin.org/article/10.3389/fphys.2016.00525/full</li>
    <li>Yang H, Stebbeds W, Francis J, Pointon A, Obrezanova O, Beattie KA, et al. Deriving waveform parameters from calcium transients in human iPSC-derived cardiomyocytes to predict cardiac activity with machine learning. Stem Cell Rep. 2022 Mar;17(3):556–68.</li>
    <li>Burridge PW, Diecke S, Matsa E, Sharma A, Wu H, Wu JC. Modeling Cardiovascular Diseases with Patient-Specific Human Pluripotent Stem Cell-Derived Cardiomyocytes. In: Nagy A, Turksen K, editors. Patient-Specific Induced Pluripotent Stem Cell Models [Internet]. New York, NY: Springer New York; 2015 [cited 2024 Jan 11]. p. 119–30. (Methods in Molecular Biology; vol. 1353). Available from: https://link.springer.com/10.1007/7651_2015_196</li>
    <li>Brancaccio M, Maywood ES, Chesham JE, Loudon ASI, Hastings MH. A Gq-Ca2+ Axis Controls Circuit-Level Encoding of Circadian Time in the Suprachiasmatic Nucleus. Neuron. 2013 May;78(4):714–28.</li>
    <li>Zhu MH, Jang J, Milosevic MM, Antic SD. Population imaging discrepancies between a genetically-encoded calcium indicator (GECI) versus a genetically-encoded voltage indicator (GEVI). Sci Rep. 2021 Mar 5;11(1):5295.</li>
    <li>Gu X, Olson E, Spitzer N. Spontaneous neuronal calcium spikes and waves during early differentiation. J Neurosci. 1994 Nov 1;14(11):6325–35.</li>
    <li>Heaney RP, Dowell MS, Hale CA, Bendich A. Calcium Absorption Varies within the Reference Range for Serum 25-Hydroxyvitamin D. J Am Coll Nutr. 2003 Apr;22(2):142–6.</li>
    <li>Sorensen J, Wiklendt L, Hibberd T, Costa M, Spencer NJ. Techniques to identify and temporally correlate calcium transients between multiple regions of interest in vertebrate neural circuits. J Neurophysiol. 2017 Mar 1;117(3):885–902.</li>
    <li>Gerstein GL, Perkel DH. Simultaneously Recorded Trains of Action Potentials: Analysis and Functional Interpretation. Science. 1969 May 16;164(3881):828–30.</li>
    <li>Windhorst U, Johansson H, editors. Modern Techniques in Neuroscience Research [Internet]. Berlin, Heidelberg: Springer Berlin Heidelberg; 1999 [cited 2024 Jan 12]. Available from: https://link.springer.com/10.1007/978-3-642-58552-4</li>
    <li>Smedler E, Uhlén P. Frequency decoding of calcium oscillations. Biochim Biophys Acta BBA - Gen Subj. 2014 Mar;1840(3):964–9.</li>
    <li>Dickinson GD, Parker I. Temperature Dependence of IP3-Mediated Local and Global Ca2+ Signals. Biophys J. 2013 Jan;104(2):386–95.</li>
    <li>Gómez-Viquez NL, Guerrero-Serna G, Arvizu F, García U, Guerrero-Hernández A. Inhibition of SERCA pumps induces desynchronized RyR activation in overloaded internal Ca 2+ stores in smooth muscle cells. Am J Physiol-Cell Physiol. 2010 May;298(5):C1038–46.</li>
  </ol>
</p>

"),
footer = modalButton("Close")
      ))
    })

##### legends of graph
observeEvent(input$legends, {
  showModal(modalDialog(
    title = "Legends",
    size = "l",
    img(src = system.file("app", "www", "images", "legend.png",
                          package = "CalciumInsights"), width = "100%"),
    footer = modalButton("Close")
  ))
})





    #########load Data #########


    filedata <- reactive({

      # if (input$data_simulate > 0) {
      #   # Cargar los datos de ejemplo
      #   data_example <- readRDS(system.file("data", "data_Simulate_calcium.rds", package = "CalciumInsights"))
      #   fileInput <- data_example
      #   fileInput2 <- NULL  # O ajusta según tus necesidades
      # }

      #else{
      req(input$fileBcsv2)

      ext <- tools::file_ext(input$fileBcsv2$name)
      fileInput1 <- load_file(input$fileBcsv2$name,
                              input$fileBcsv2$datapath,
                              ext)

      if (ext %in% c("csv", "tsv")) {
        fileInput <- as.data.frame(fileInput1)
        fileInput2 <- NULL
      } else if (ext == "json") {
        fileInput2 <- fileInput1
        comp <- fileInput2$components
        com <- t(fileInput2$components)
        time <- seq(0, fileInput2$image_data[2]-1, by = 1) * fileInput2$image_data[1]
        com <- cbind(time, com)
        fileInput <- com
      }
      #}



      return(list(fileInput = fileInput, fileInput2 = fileInput2))
    })


    #########Summary of Data #########

    data_info <- reactive({
      req(filedata()$fileInput)
      Nobservations <- nrow(filedata()$fileInput)
      Ncells <- ncol(filedata()$fileInput)-1
      SummaryData <- data.frame(list(Number = c(Ncells, Nobservations)))
      rownames(SummaryData) <- c("Region of Interest (ROI)", "Time observations")
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
      #smoothed <- loess(signal ~ Time, data = data_raw , span = input$span)
      #predictions <- predict(smoothed)
      #df_smoothed1 <- data.frame(Time = data_raw$Time, signal = predictions)

      df_smoothed <- data_raw
      #####

      peaks_found <- peaks(data = df_smoothed, nups=input$nups2,
                           ndowns = input$ndowns2, minpeakheight = input$minpeakheight2,
                           minpeakdistance = input$minpeakdistance2)

      table_peak <- peaks_found$p_eak
      table_positions_peaks <- peaks_found$peak

      return(list(table_peak = table_peak,
                  table_positions_peaks  = table_positions_peaks,
                  data_raw = data_raw, df_smoothed = df_smoothed,
                  data = data))
    })

    ######### Extraction of metrics #########

    peaks_plot <- reactive({
      table_peak = peaks_df()$table_peak  #tabla que muestra los piko
      table_positions_peaks = peaks_df()$table_positions_peaks # tabla de las posiciones de los piko
      data_raw = peaks_df()$data_raw  #data con la celula analizada
      data_smoothed = peaks_df()$df_smoothed   # data raw
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
      return(list(table_peak = table_peak, tabla_AUC = tabla_AUC,
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
                      y = latex2exp::TeX("$\\textbf{\\Delta F/F_0\\cdot s^{-1}}$")) +

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


    Trance_Graph <- reactive({

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
                      y = latex2exp::TeX("$\\textbf{\\Delta F/F_0}$")) +

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
      return(list(gg3 = gg3))


    })

    output$plot_peak3 <- renderPlot({
      Trance_Graph()$gg3
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


    # output$plot_raw_smoothed <- renderPlot({
    #   peaks_plot()$gg2
    # })

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
        # smoothed <- loess(signal ~ Time, data = data_raw ,
        #                   span = input$span)
        # predictions <- predict(smoothed)
        df_smoothed <- data_raw
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
        paste("Transient_Metrics_Raw", ".csv", sep = "")

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
          ransient_Metrics_Raw <- cbind(df_p2, peaks_plot()$tabla_AUC)
        }
        else {ransient_Metrics_Raw <- df_p2}

        write.csv( ransient_Metrics_Raw, file, row.names = FALSE)
      }
    )



    #download Trace_Metrics
    output$descargarP <- downloadHandler(
      filename = function() {
        paste("Trace_Metrics_Raw", ".csv", sep = "")

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

    output$Calcium_Trance_Graph <- downloadHandler(
      filename = function() {
        paste("calcium_trace", Sys.Date(), ".png", sep = "")
      },
      content = function(file) {
        # Get the ggplot object from the reactive
        gg_plot <- Trance_Graph()$gg3

        # Save the ggplot object as a PNG file
        ggplot2::ggsave(file, plot = gg_plot, dpi = 300)
      }
    )








  })
}

## To be copied in the UI
# mod_Raw_data_ui("Raw_data_1")

## To be copied in the server
# mod_Raw_data_server("Raw_data_1")
