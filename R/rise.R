# data = read_csv("~/Documents/Metric/Json/cell_4_plantas copy.csv")
# data = t(data)
# colnames(data) = data[1,]
# data = data[-1,]
# cell = 1
# data_raw = data.frame(Time = as.numeric(colnames(data)),
#                       signal = as.numeric(data[cell,]))
# data_raw  = data_raw[data_raw$Time>=0,]
#
# ##### function loess for smoothed
# smoothed <- loess(signal ~ Time, data = data_raw , span = 0.05)
# predictions <- predict(smoothed)
# data_smoothed <- data.frame(Time = data_raw$Time, signal = predictions)
# #####
# peaks <- function(data, nups, ndowns, minpeakheight, minpeakdistance) {
#   peak = pracma::findpeaks(x = as.numeric(data[,2]), nups = nups, ndowns = ndowns ,
#                            minpeakheight = minpeakheight,
#                            minpeakdistance = minpeakdistance, sortstr=FALSE) # Busquedad de picos
#   peak <- peak[order(peak[, 2]),]
#   posision_peak = data[,1][peak[,2]]
#   l_inf = data[,1][peak[,3]]
#   l_sup = data[,1][peak[,4]]
#   p_eak = data.frame(absolute_amplitude =round(peak[,1],3),
#                      posision_peaks = posision_peak,  l_inf = l_inf,
#                      l_sup = l_sup )
#   return(list(p_eak = p_eak, peak = peak))
# }
# Savitzky_Golay <- function(data, p, w, Cell){
#
#   if (p==1){m=1}
#   else{m=2}
#   data.snv = data
#   data.suav=prospectr::savitzkyGolay(X=data.snv,m=0,p,w) # data suvizada
#   data_1nd=prospectr::savitzkyGolay(X=data.snv,m=1,p,w)  # data con 1nd derivada
#   data_2nd=prospectr::savitzkyGolay(X=data.snv,m=m,p,w)  # data con 2nd derivada
#
#   data.suav_P = data.frame(Time = as.numeric(colnames(data.suav)), data.suav = data.suav[Cell,])
#   data.1nd_P = data.frame(Time = as.numeric(colnames(data_1nd)), data_1nd = data_1nd[Cell,] )
#   data.2nd_P = data.frame(Time = as.numeric(colnames(data_2nd)), data_2nd = data_2nd[Cell,] )
#   return(list(data.suav_P = data.suav_P, data.1nd_P = data.1nd_P, data.2nd_P = data.2nd_P  ))
# }
#
# peaks_found <- peaks(data = data_smoothed, nups=1,
#                      ndowns = 1, minpeakheight = 0.5,
#                      minpeakdistance = 0)
#
# table_peak <- peaks_found$p_eak
# table_positions_peaks <- peaks_found$peak
#
# Time_of_the_first_peak <- function(data1, peak){
#   colnames(data1) <- c("Time", "sing")
#   Derivative <- c(0,diff(data1$sing) / diff(data1$Time))
#   cambios_signo <- which(diff(sign(Derivative)) > 0)
#   pfp <- peak[1,2]
#   cambios_menor_que_pfp <- max(cambios_signo[cambios_signo < pfp]) #posicion donde empieza el pico
#   return(list(cambios_menor_que_pfp = cambios_menor_que_pfp))
# }
#
# MSCPFP = Time_of_the_first_peak(data1 = data_smoothed, peak = table_positions_peaks)$cambios_menor_que_pfp # posicion donde hay un cambio en la primera dericada
# # para el primer pico
#
# prominens2 <- function(data, peak, MSCPFP) {
#
#   leng <- length(peak[, 2])
#   minimos <- sapply(1:(leng - 1), function(i) {
#     found <- peak[i:(i + 1), 2]
#     min(data[,2][found[1]:found[2]])
#   })
#
#
#   minimos <- c(data[MSCPFP,][,2], minimos)
#
#   data_min <- data.frame(x = data[,1][peak[, 2]], y = minimos)
#
#   punto_de_corte <- minimos #punto abajo
#   ampl2 <- peak[, 1] #punto de arriba
#
#   df_peaks_parcia <- data.frame(p_ini1=data[,1][peak[, 2]], p_fin1=punto_de_corte,
#                                 p_ini2=data[,1][peak[, 2]], p_fin2=ampl2 )
#
#   prominens_amplitud <-c(ampl2-punto_de_corte)
#
#
#   ### tiempos dinde la curva empieza a crecer para el pico
#
#   min_tiempo <- c()
#   for (i in 1:(leng - 1)) {
#     found1 <- peak[i:(i + 1), 2]
#     data2 <- data[found1[1]:found1[2],]
#     min_index1 <- which.min(data2[,2])
#     min_tiempo[i] <- data2[,1][min_index1]
#   }
#
#
#   time_start_increasin_peak1 <- c(data[MSCPFP,][,1], min_tiempo)
#   time_start_increasin_peak <- data.frame(Time = time_start_increasin_peak1,
#                                           y = rep(0,length(time_start_increasin_peak1)) )
#
#
#
#
#   return(list(data_min = data_min, df_peaks_parcia = df_peaks_parcia,
#               prominens_amplitud = prominens_amplitud,
#               time_start_increasin_peak = time_start_increasin_peak))
# }
#
#
# data_min <- prominens2(data = data_smoothed, peak = table_positions_peaks, MSCPFP = MSCPFP)$data_min # puntos minimos donde empiezan los prominents
#
# time_start_increasin_peak <- prominens2(data = data_smoothed, peak = table_positions_peaks, MSCPFP = MSCPFP)$time_start_increasin_peak # puntos minimos donde empiezan los prominents
#
#
# data_minimos_crecientes <- data.frame(x1 = time_start_increasin_peak$Time, y1 = data_min$y,
#                                       x2 = table_peak$posision_peaks, y2 = table_peak$absolute_amplitude )
#
#
#
#
#
# primera_derivada <- Savitzky_Golay(data = data, p = 2, w = 5, Cell = cell)$data.1nd_P
#
# primera_derivada1 <- data.frame(Time = primera_derivada$Time,
#                                 deri1 = prospectr::savitzkyGolay(X=data_smoothed$signal,m=1,p = 2,w = 5))
#
# slope <- c()
# times_predition <- list()
# for (i in 1:length(data_minimos_crecientes$x1)) {
# resultados_filtrados <- subset(primera_derivada1,
#                                Time %in% data_minimos_crecientes$x1[1]:data_minimos_crecientes$x2[1])
# slope[i] <- max(resultados_filtrados$deri1)
# }
#
#
#
# # Suponiendo que 'data_smoothed' contiene tus datos suavizados y 'primera_derivada1' tiene los resultados de la primera derivada
#
# # Graficar la señal suavizada
# plot(data_smoothed$Time, data_smoothed$signal, xlab = "Tiempo", ylab = "Señal", main = "Señal suavizada")
# points(data_minimos_crecientes$x1, data_minimos_crecientes$y1, col = "red", pch = 19)
# points(data_minimos_crecientes$x2, data_minimos_crecientes$y2, col = "blue", pch = 19)
#
# # Calcular la primera derivada
# primera_derivada <- prospectr::savitzkyGolay(X = data_smoothed$signal, m = 0, p = 2, w = 5)
# ti <- primera_derivada1$Time
#
# # Agregar la primera derivada al gráfico
# lines(ti, primera_derivada, col = "pink", type = "l")










