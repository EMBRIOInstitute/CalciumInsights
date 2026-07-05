Time_of_the_first_peak <- function(data1, peak){
  colnames(data1) <- c("Time", "sing")
  Derivative <- c(0,diff(data1$sing) / diff(data1$Time))
  cambios_signo <- which(diff(sign(Derivative)) > 0)
  pfp <- peak[1,2]
  cambios_menor_que_pfp <- max(cambios_signo[cambios_signo < pfp]) #posicion donde empieza el pico
  return(list(cambios_menor_que_pfp = cambios_menor_que_pfp))
}
