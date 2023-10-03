AUC <- function(datos, P_min , P_max){


  colnames(datos) <- c("Time","sing")
  smoothed <- loess(sing ~ Time, data = datos, span = 0.1)
  predictions <- predict(smoothed)


  df <- data.frame(tiempo = datos$Time, sing = predictions)


  your_function <- function(x) {
    approx(df$tiempo, df$sing, xout = x)$y
  }

area <- integrate(your_function, P_min, P_max, subdivisions = 10000, rel.tol = 0.01, abs.tol = 0.01)

area1 <- area$value

with_absolute_error <- area$abs.error


return(list(area = area1, with_absolute_error = with_absolute_error))

}
