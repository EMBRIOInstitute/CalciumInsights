AUC <- function(datos, Integration_Reference){

  colnames(datos) <- c("Time","sing")
  smoothed <- loess(sing ~ Time, data = datos, span = 0.1)
  predictions <- predict(smoothed)
  df <- data.frame(tiempo = datos$Time, sing = predictions) #toda la data
  df2 <- data.frame(tiempo = datos$Time, sing = predictions)  #data restringida al nivel de referencia de integracion
  df2$sing[df2$sing > Integration_Reference] <- Integration_Reference
  df3 = df[df$sing >= Integration_Reference,]  #todos los datos por ensima del eje de referencia

  #integral total
  P_min <- min(df$tiempo)
  P_max <- max(df$tiempo)


  your_function <- function(x) {
    approx(df$tiempo, df$sing, xout = x)$y
  }

  area1 <- integrate(your_function, P_min, P_max, subdivisions = 10000, rel.tol = 0.01, abs.tol = 0.01)

  #####
  ##integral restringida

  your_function2 <- function(x) {
    approx(df2$tiempo, df2$sing, xout = x)$y
  }

  area2 <- integrate(your_function2, P_min, P_max, subdivisions = 10000, rel.tol = 0.01, abs.tol = 0.01)





  area <- area1$value - area2$value

  with_absolute_error <- area1$abs.error-area2$abs.error


  return(list(area = area, with_absolute_error = with_absolute_error, P_min = min(df3$tiempo), P_max = max(df3$tiempo)))

}








