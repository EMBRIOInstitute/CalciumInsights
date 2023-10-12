line_half_prominence <- function(data1, peak, P_M){
  library(dplyr)

  peak = as.numeric(peak)

  izq_FWHM <- function(data1, peak, P_M){
  colnames(data1) <- c("Time", "sing")
  v_ps <- peak[2]  # posicion de los picos
  v_p <- peak[1]   # los picos
  P_M1= P_M   # el valor medio

  v_ps1 = v_ps
  v_p1 = v_p
  P_M11 = P_M1

  datos <- data1[,2]
  # Inicializar una variable para seguir la posición
  posicion <- NULL

  # Iterar a través del vector
  for (j in rev(1:v_ps1)) {
    # Verificar si el elemento actual es menor que 9
    if (datos[j] < v_p1) {
      # Si es menor que 4, almacenar la posición y salir del bucle
      if (datos[j] < P_M11) {
        posicion <- j
        break
      }
    }
  }
  data1_HFWM = data1[posicion:v_ps1,]
  # Datos de entrada
  x <- data1_HFWM[,1]
  y <- data1_HFWM[,2]
  # Realizar un ajuste de regresión polinómica de grado 10
  degree <- 20
  fit <- lm(y ~ poly(x, degree, raw = TRUE))
  # Valores x para el gráfico
  x_values <- seq(min(x), max(x), length.out = 500)
  # Predecir valores y con el modelo ajustado
  y_pred <- predict(fit, data.frame(x = x_values))
  df_interpolation = data.frame(Time = x_values, sing = round(y_pred,2))
  posicion <- which.min(abs(df_interpolation$sing-P_M11))

  as.numeric(unname(df_interpolation[posicion,][1,]))
  }

  ls = list()
  ls[[1]] = izq_FWHM(data1 = data1, peak = peak, P_M = P_M)

  df= data.frame(ls)
  colnames(df) <- NULL
  df <- t(df)
  colnames(df) <- c("Time_left_FWHM", "y")
  df <- as.data.frame(df)


  der_FWHM <- function(data1, peak, P_M ,i ){
    colnames(data1) <- c("Time", "sing")
    v_ps <- peak[2]  # posicion de los picos
    v_p <- peak[1]   # los picos
    P_M1= P_M    # el valor medio

    v_ps1 = v_ps
    v_p1 = v_p
    P_M11 = P_M1

    datos <- data1[,2]
    # Inicializar una variable para seguir la posición
    posicion <- NULL

    # Iterar a través del vector
    for (j in v_ps1: length(datos)) {
      # Verificar si el elemento actual es menor que 9
      if (datos[j] < v_p1) {
        # Si es menor que 4, almacenar la posición y salir del bucle
        if (datos[j] < P_M11) {
          posicion <- j
          break
        }
      }
    }

    data1_HFWM = data1[v_ps1:posicion,]
    # Datos de entrada
    x <- data1_HFWM[,1]
    y <- data1_HFWM[,2]
    # Realizar un ajuste de regresión polinómica de grado 10
    degree <- 20
    fit <- lm(y ~ poly(x, degree, raw = TRUE))
    # Valores x para el gráfico
    x_values <- seq(min(x), max(x), length.out = 500)
    # Predecir valores y con el modelo ajustado
    y_pred <- predict(fit, data.frame(x = x_values))
    df_interpolation = data.frame(Time = x_values, sing = round(y_pred,2))
    posicion <- which.min(abs(df_interpolation$sing-P_M11))

    as.numeric(unname(df_interpolation[posicion,][1,]))
  }

  ls2 = list()
  ls2[[1]] = der_FWHM(data1 = data1, peak = peak, P_M = P_M, i=i)
  df2= data.frame(ls2)
  colnames(df2) <- NULL
  df2 <- t(df2)
  colnames(df2) <- c("Time_right_FWHM", "y")
  df2 <- as.data.frame(df2)


  return(list(df = df, df2 = df2))
}
