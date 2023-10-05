AUC_case <- function(datos){

  df <- data.frame(tiempo = datos[,1], sing = datos[,2])
  P_min <- min(df$tiempo)
  P_max <- max(df$tiempo)

  your_function <- function(x) {
    approx(df$tiempo, df$sing, xout = x)$y
  }

  area <- integrate(your_function, P_min, P_max, subdivisions = 10000, rel.tol = 0.01, abs.tol = 0.01)

  area1 <- area$value

  hight <- min(df$sing)
  base <- length(df$tiempo)
  area2 <- base*hight

  area_under <- area1 - area2


  with_absolute_error <- area$abs.error


  return(list(area = area_under, with_absolute_error = with_absolute_error))

}
