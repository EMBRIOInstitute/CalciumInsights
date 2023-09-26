response_time <- function(data, peak, Puntos_medios){
  library(dplyr)

  filtered_firts <- data %>%
    filter(data[,1] <= data[,1][peak[1,2]], data[,2]>= Puntos_medios[1,2])

  ultimo_peak <- length(peak[,1])

  filtered_second <- data %>%
    filter(data[,1] >= data[,1][peak[ultimo_peak,2]], data[,2] >= Puntos_medios[ultimo_peak,2])

  first_time <- filtered_firts[which.min(filtered_firts[,2]),]
  second_time <- filtered_second[which.min(filtered_second[,2]),]
  Tiempo_respose <- second_time[,1] - first_time[,1]

  return(list(first_time = first_time, second_time = second_time,  Tiempo_respose = Tiempo_respose))
}
