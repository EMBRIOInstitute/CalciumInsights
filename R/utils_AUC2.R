AUC2 <- function(datos, P_min , P_max){

  df <- data.frame(tiempo = datos[,1], sing = datos[,2])


  your_function <- function(x) {
    approx(df$tiempo, df$sing, xout = x)$y
  }

  area <- integrate(your_function, P_min, P_max, subdivisions = 10000, rel.tol = 0.01, abs.tol = 0.01)

  area1 <- area$value

  with_absolute_error <- area$abs.error


  return(list(area = area1, with_absolute_error = with_absolute_error))

}

