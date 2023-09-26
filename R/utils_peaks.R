peaks <- function(data, nups, ndowns, minpeakheight, minpeakdistance) {
  peak = pracma::findpeaks(x = as.numeric(data[,2]), nups = nups, ndowns = ndowns ,
                           minpeakheight = minpeakheight,
                   minpeakdistance = minpeakdistance, sortstr=FALSE) # Busquedad de picos
  peak <- peak[order(peak[, 2]),]
  posision_peak = data[,1][peak[,2]]
  l_inf = data[,1][peak[,3]]
  l_sup = data[,1][peak[,4]]
  p_eak = data.frame(absolute_amplitude =round(peak[,1],3),
                     posision_peaks = posision_peak,  l_inf = l_inf,
                     l_sup = l_sup )
  return(list(p_eak = p_eak, peak = peak))
}
