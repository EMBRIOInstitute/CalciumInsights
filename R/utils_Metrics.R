Metrics <- function(fileInput_1, nups3, ndowns3, minpeakheight3,
                     minpeakdistance3, span){

  data = fileInput_1
  data = t(data)
  colnames(data) = data[1,]
  data = data[-1,]


  ls <- list()
  ls1 <- list()
  ls2 <- list()
  ls3 <- list()
  ls4 <- list()
  ls5 <- list()
  ls6 <- list()
  ls7 <- list()
  ls8 <- list()
  ls9 <- list()
  ls10 <- list()
  for (i in 1:dim(data)[1]) {

    cell = i

    data_raw = data.frame(Time = as.numeric(colnames(data)),
                          signal = as.numeric(data[cell,]))

    ##### function loess for smoothed
    smoothed <- loess(signal ~ Time, data = data_raw , span = span)
    predictions <- predict(smoothed)
    df_smoothed <- data.frame(Time = data_raw$Time, signal = predictions)
    #####

    peaks_found <- peaks(data = df_smoothed, nups = nups3,
                         ndowns = ndowns3, minpeakheight = minpeakheight3,
                         minpeakdistance = minpeakdistance3)

    table_peak <- peaks_found$p_eak
    table_positions_peaks <- peaks_found$peak




    table_peak = table_peak  #tabla que muestra los piko
    table_positions_peaks = table_positions_peaks # tabla de las posiciones de los piko
    data_raw = data_raw  #data con la celula analizada
    data_smoothed = df_smoothed   # data suavizada

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

    Puntos_medios <- FWHP2(peaks = data_smoothed[,1][peaks], df_peaks_parcia = df_peaks_parcia)$Puntos_medios  # puntos medios de los prominances

    table_peak$prominence <- round(prominens2(data = data_smoothed, peak = table_positions_peaks, MSCPFP = MSCPFP)$prominens_amplitud,3)  # valor de los prominens
    table_peak$Prominence_Midpoint <- Puntos_medios$p_eak_mediun # valor medio de las promineces

    first_time <- as.data.frame(response_time(data = data_smoothed, peak = table_positions_peaks,
                                              Puntos_medios = Puntos_medios)$first_time )  #primer tiempo
    second_time <- as.data.frame(response_time(data = data_smoothed, peak = table_positions_peaks,
                                               Puntos_medios = Puntos_medios)$second_time)  #segundo tiempo
    Tiempo_respose <- response_time(data = data_smoothed, peak = table_positions_peaks,
                                    Puntos_medios = Puntos_medios)$Tiempo_respose #tiempo de respuesta

    data_segmento_tiempo <- data.frame(x1 = first_time[1,1], x2 = second_time[1,1])

    right_left_FWHM <- right_left_FWHP(data1=data_smoothed, peak = table_positions_peaks,
                                       P_M = Puntos_medios)
    left_FWHM <- right_left_FWHM$df
    right_FWHM <- right_left_FWHM$df2

    table_peak$Time_left_FWHM <- left_FWHM$Time_left_FWHM
    table_peak$Time_right_FWHM <- right_FWHM$Time_right_FWHM

    table_peak$FWHM <- right_FWHM$Time_right_FWHM -left_FWHM$Time_left_FWHM

    table_peak$Time_to_peak <- table_peak$posision_peak - time_start_increasin_peak$Time

    table_peak$puntominimo_y <- prominens2(data = data_smoothed,
                                           peak = table_positions_peaks,
                                           MSCPFP = MSCPFP)$df_peaks_parcia$p_fin1

    table_FWHM <- data.frame(t1 = left_FWHM$Time_left_FWHM, t2 = right_FWHM$Time_right_FWHM,
                             y_FWHM = Puntos_medios$p_eak_mediun)
    table_peak$Transient_Ocurrence_Time <- time_start_increasin_peak$Time

    ######## Funcion de rise #####
    data_minimos_crecientes <- data.frame(x1 = time_start_increasin_peak$Time, y1 = data_min$y,
                                          x2 = table_peak$posision_peaks, y2 = table_peak$absolute_amplitude)

    primera_derivada <- Savitzky_Golay(data = data, p = 2, w = 5, Cell = cell)$data.1nd_P

    primera_derivada1 <- data.frame(Time = primera_derivada$Time,
                                    deri1 = prospectr::savitzkyGolay(X=data_smoothed$signal, m=1, p = 2, w = 5))

    slope <- c()
    #times_predition <- list()
    for (j in 1:length(data_minimos_crecientes$x1)) {
      resultados_filtrados <- subset(primera_derivada1,
                                     Time %in% data_minimos_crecientes$x1[j]:data_minimos_crecientes$x2[j])
      slope[j] <- max(resultados_filtrados$deri1)
    }
    ##################################
    table_peak$Rise <- slope



    df_p1 <- table_peak

    colnames(df_p1) <- c("Absolute_Amplitude", "Peak_Time", "L_inf", "L_sup",
                         "Amplitude", "Prominence_Midpoint", "Time_left_FWHM",
                         "Time_right_FWHM", "FWHM", "Time_to_peak", "Baseline",
                         "Transient_Ocurrence_Time", "Rise")

    time1 <- min(data_raw$Time)
    time2 <- max(data_raw$Time)
    Time_OnSet <- df_p1[,12][1]
    Frequency <- length(df_p1[,1])/(time2-time1)


    ls[[i]] <- df_p1$Peak_Time
    ls1[[i]] <- df_p1$Time_to_peak
    ls2[[i]] <- df_p1$FWHM
    ls3[[i]] <- df_p1$Transient_Ocurrence_Time


    ls4[[i]] <- Time_OnSet
    ls5[[i]] <- Frequency
    ls6[[i]] <- df_p1$Baseline

    Integration_Reference <- 0    #input$Integration_Reference1
    AUC <- AUC2(datos = data_smoothed, Integration_Reference = Integration_Reference)
    ls7[[i]] <- AUC$area

    ls8[[i]] <- df_p1$Amplitude
    ls9[[i]] <- length(df_p1$Absolute_Amplitude)
    ls10[[i]] <- df_p1$Rise




  }

  return(list(ls = ls, ls1 = ls1, ls2 = ls2,
              ls3 = ls3, ls4 = ls4, ls5 = ls5, ls6 = ls6, ls7 = ls7, ls8 = ls8, ls9 = ls9, ls10 = ls10))
}






