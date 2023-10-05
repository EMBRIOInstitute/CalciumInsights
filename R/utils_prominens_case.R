prominens_case <- function(data, peak) {

  minimos <- min(data[,2][data[1,1]:peak[, 2]])


  data_min <- data.frame(x = data[,1][peak[, 2]], y = minimos)

  punto_de_corte <- minimos #punto abajo
  ampl2 <- peak[, 1] #punto de arriba

  df_peaks_parcia <- data.frame(p_ini1=data[,1][peak[, 2]], p_fin1=punto_de_corte,
                                p_ini2=data[,1][peak[, 2]], p_fin2=ampl2 )

  prominens_amplitud <-c(ampl2-punto_de_corte)


  return(list(data_min = data_min, df_peaks_parcia = df_peaks_parcia, prominens_amplitud = prominens_amplitud ))
}
