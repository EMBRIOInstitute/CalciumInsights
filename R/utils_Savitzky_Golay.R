Savitzky_Golay <- function(data, p, w, Cell){

  if (p==1){m=1}
  else{m=2}
  data.snv = data
  data.suav=prospectr::savitzkyGolay(X=data.snv,m=0,p,w) # data suvizada
  data_1nd=prospectr::savitzkyGolay(X=data.snv,m=1,p,w) # data con 1nd derivada
  data_2nd=prospectr::savitzkyGolay(X=data.snv,m=m,p,w) # data con 2nd derivada

  data.suav_P = data.frame(Time = as.numeric(colnames(data.suav)), data.suav = data.suav[Cell,])
  data.1nd_P = data.frame(Time = as.numeric(colnames(data_1nd)), data_1nd = data_1nd[Cell,] )
  data.2nd_P = data.frame(Time = as.numeric(colnames(data_2nd)), data_2nd = data_2nd[Cell,] )
  return(list(data.suav_P = data.suav_P, data.1nd_P = data.1nd_P, data.2nd_P = data.2nd_P  ))
}
