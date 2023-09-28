model_functional <- function(data, fac, niv){

library(dplyr)
library(ggplot2)

  datafunctional2 <- data
  datafunctional4 = subset(datafunctional2, Time>= 0)
  y=c(as.numeric(unlist(strsplit(niv,","))))
  lentime = length(datafunctional4$Time)
  time = c(datafunctional4[,1])
  cell = y[1]+y[2]+y[3]
  datafun = cbind(datafunctional4[,2:(cell+1)])
  id = seq(1:(cell))
  colnames(datafun ) <- c(id)
  Ydata = matrix(t(datafun), cell, lentime)
  if(fac==2){x1=c(rep(0,y[1]), rep(1,y[2]))}
  if(fac==3){x1=c(rep(0,y[1]), rep(1,y[2]),rep(2,y[3]))}
  layers = x1
  datal <- data.frame(layers=factor(layers))
  #escalar
  #fit<-refund::pffr(Ydata ~ layers, yind= time, data = datal,method = "REML",
            #bs.yindex=list(bs="ps", k=20, m=c(2, 1)),bs.int=list(bs="ps", k=30, m=c(2,1)))

  #summary_fun = summary(fit)

  top1 = reshape2::melt(datafunctional4, id=c("Time"))

  if(fac==2){x=c(rep(0,y[1]*lentime), rep(1,y[2]*lentime))}
  if(fac==3){x=c(rep(0,y[1]*lentime), rep(1,y[2]*lentime), rep(2,y[3]*lentime))}
  top2 = data.frame(Time = top1$Time, Single_cell = top1$variable, Layers = x ,calcium = top1$value)


  top2$Layers = factor(top2$Layers)
  mean4<- top2%>%group_by(Layers,Time)%>%summarise(Mean = mean(calcium))%>%as.data.frame()

  m4 = ggplot(mean4, aes(x=Time,y=Mean, group = Layers, colour = Layers))+ geom_line(size=3)+
    theme(text = element_text(size = 30), axis.text.x = element_text(angle=90))+
    labs (y  = expression((F-F[0])/F[0]), x= "Time (s)")+
    scale_x_continuous(breaks=seq(0,1800,100))+
    scale_y_continuous(breaks=seq(-1,4.5,0.3))+ggtitle("Average curves")+
    scale_color_manual(values = c("red", "blue","aquamarine2"))+theme(plot.title = element_text(hjust = 0.5))

  g5 = ggplot(top2, aes(x=Time,y=calcium, group = Single_cell, colour = Single_cell))+
    geom_line(size=2)+theme(text = element_text(size = 20), axis.text.x = element_text(angle=90))+
    labs (y  = expression((F-F[0])/F[0]), x= "Time (s)")+scale_x_continuous(breaks=seq(0,1800,100))+
    scale_y_continuous(breaks=seq(-1,4.5,0.3))+scale_color_manual(values = c("red", "red","red", "blue",
                                                                             "blue","blue","blue","blue",
                                                                             "aquamarine2","aquamarine2",
                                                                             "aquamarine2","aquamarine2","aquamarine2"))
  return(list(m4 = m4, g5 = g5))

}
