line_half_prominence <- function(data, peak, Puntos_medios){
  library(dplyr)

  peak = as.numeric(peak)


  filtered_firts <- data %>%
    filter(data[,1] <= data[,1][peak[2]], data[,2]>= round(Puntos_medios-1,0), peak[1] >= data[,2])

  filtered_second <- data %>%
    filter(data[,1] >= data[,1][peak[2]], data[,2]>= round(Puntos_medios-1,0), peak[1] >= data[,2])

  posicion_min <- which.min(filtered_firts[,2])
  posicion_max <- which.min(filtered_second[,2])

  posicion_izq <- filtered_firts[posicion_min,]
  posicion_dere <- filtered_second[posicion_max,]
  y = min(posicion_izq$signal, posicion_dere$signal)

  half_prominence <- data.frame(x1 = posicion_izq$Time, x2 = posicion_dere$Time, y = y)

  return(list(half_prominence = half_prominence))
}
