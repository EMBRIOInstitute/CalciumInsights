FWHM2 <- function(peaks, df_peaks_parcia ){
  p_eak_mediun <- c((df_peaks_parcia$p_fin1+df_peaks_parcia$p_fin2)/2)  #absolute_amplitude
  Puntos_medios <- data.frame(posiscion_medio = peaks,
                              p_eak_mediun = p_eak_mediun  )
  return(list(Puntos_medios=Puntos_medios))
}
