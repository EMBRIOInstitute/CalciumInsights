Rise <- function(data_minimos_crecientes) {
slope <- c()
predicciones <- list()
times_predition <- list()
for (i in 1:length(data_minimos_crecientes$x1)) {
  resultados_filtrados <- subset(data_smoothed,
                                 Time %in% data_minimos_crecientes$x1[i]:data_minimos_crecientes$x2[i])
  times_predition[[i]] <- resultados_filtrados$Time
  ml <- lm(signal ~ Time, data = resultados_filtrados)
  predicciones[[i]] <- predict(ml)
  slope[i] <- coef(ml)[2]
}
return(list(slope = slope))
}
