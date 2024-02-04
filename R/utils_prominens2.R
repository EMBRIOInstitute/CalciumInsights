prominens2 <- function(data, peak, MSCPFP) {

  leng <- length(peak[, 2])

  data1 = data
  colnames(data1) <- c("Time", "sing")
  # Derivative <- prospectr::savitzkyGolay(X=data1$sing,
  #                                        m=1,
  #                                        p = 2,
  #                                        w = 5)
  Derivative <- c(0,diff(data1$sing) / diff(data1$Time))
  cambios_signo <- which(diff(sign(Derivative)) > 0)
  cambios_menor_pfp <- c()
  for (i in 1:leng) {
    pfp <- peak[i,2]
    cambios_menor_pfp[i] <- max(cambios_signo[cambios_signo < pfp]) #posicion donde empieza el pico a crecer
  }
  minimos <- data[,2][cambios_menor_pfp]

  minimos <- c(minimos)

  data_min <- data.frame(x = data[,1][peak[, 2]], y = minimos)

  punto_de_corte <- minimos #punto abajo en y
  ampl2 <- peak[, 1] #punto de arriba en y

  df_peaks_parcia <- data.frame(p_ini1=data[,1][peak[, 2]], p_fin1=punto_de_corte,
                                p_ini2=data[,1][peak[, 2]], p_fin2=ampl2 )

  prominens_amplitud <-c(ampl2-punto_de_corte)


  ### tiempos dinde la curva empieza a crecer para el pico

  # min_tiempo <- c()
  # for (i in 1:leng) {
  #   found1 <- peak[i:(i + 1), 2]
  #   data2 <- data[found1[1]:found1[2],]
  #   min_index1 <- which.min(data2[,2])
  #   min_tiempo[i] <- data2[,1][min_index1]
  # }


  time_start_increasin_peak1 <- data[,1][cambios_menor_pfp]
  time_start_increasin_peak <- data.frame(Time = time_start_increasin_peak1,
                                          y = rep(0,length(time_start_increasin_peak1)) )




  return(list(data_min = data_min, df_peaks_parcia = df_peaks_parcia,
              prominens_amplitud = prominens_amplitud,
              time_start_increasin_peak = time_start_increasin_peak))
}
