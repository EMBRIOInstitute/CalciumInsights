AUC <- function(datos, Integration_Reference){


  colnames(datos) <- c("Time","sing")
  smoothed <- loess(sing ~ Time, data = datos, span = 0.1)
  predictions <- predict(smoothed)
 df <- data.frame(tiempo = datos$Time, sing = predictions)


 df2 = df[df$sing >= Integration_Reference,]

 P_min <- min(df2$tiempo)
 P_max <- max(df2$tiempo)


  your_function <- function(x) {
    approx(df2$tiempo, df2$sing, xout = x)$y
  }

area <- integrate(your_function, P_min, P_max, subdivisions = 10000, rel.tol = 0.01, abs.tol = 0.01)

area1 <- area$value

with_absolute_error <- area$abs.error


return(list(area = area1, with_absolute_error = with_absolute_error, P_min = P_min, P_max = P_max))

}
