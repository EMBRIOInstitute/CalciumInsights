prominens2 <- function(data, peak, MSCPFP) {

  leng <- length(peak[, 2])
  minimos <- sapply(1:(leng - 1), function(i) {
    found <- peak[i:(i + 1), 2]
    min(data[,2][found[1]:found[2]])
  })


  minimos <- c(data[MSCPFP,][,2], minimos)

  data_min <- data.frame(x = data[,1][peak[, 2]], y = minimos)

  punto_de_corte <- minimos #punto abajo
  ampl2 <- peak[, 1] #punto de arriba

  df_peaks_parcia <- data.frame(p_ini1=data[,1][peak[, 2]], p_fin1=punto_de_corte,
                                p_ini2=data[,1][peak[, 2]], p_fin2=ampl2 )

  prominens_amplitud <-c(ampl2-punto_de_corte)


  ### tiempos dinde la curva empieza a crecer para el pico

  min_tiempo <- c()
  for (i in 1:(leng - 1)) {
    found1 <- peak[i:(i + 1), 2]
    data2 <- data[found1[1]:found1[2],]
    min_index1 <- which.min(data2[,2])
    min_tiempo[i] <- data2[,1][min_index1]
  }


time_start_increasin_peak1 <- c(data[MSCPFP,][,1], min_tiempo)
time_start_increasin_peak <- data.frame(Time = time_start_increasin_peak1,
                                        y = rep(0,length(time_start_increasin_peak1)) )




  return(list(data_min = data_min, df_peaks_parcia = df_peaks_parcia,
              prominens_amplitud = prominens_amplitud,
              time_start_increasin_peak = time_start_increasin_peak))
}
