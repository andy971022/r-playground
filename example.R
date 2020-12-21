library(openxlsx)
library(ggplot2)
data = read.xlsx('matrix_acidizing_data.xlsx',sheet=1)

parameters = data[c(3,4,8,9,13,14)]
#"X3","Sample","Vi(cm/min)","PVbt","Vi-opt","PVbt-opt"

IL = parameters[1:6,]
EW = parameters[7:10,]
AC = parameters[12:15,]
L_IL = parameters[16:20,]
L_EW = parameters[21:24,]
L_AC = parameters[25:29,]

BG <- function(corename){
  name = unlist(corename[c(1)])[1]
  Viopt = as.numeric(unlist(corename[c(5)]))[1]
  PVbtopt = as.numeric(unlist(corename[c(6)]))[1]
  x = as.numeric(unlist(corename[c(3)]))/Viopt
  y = as.numeric(unlist(corename[c(4)]))
  eq <- function(x){PVbtopt*x**(1/3)/(1-exp(1)**(-4*x**2))**2}
  points = data.frame(x=x*Viopt, y=y)
  y_hat = eq(x)
  error = abs(y-y_hat)**2
  p = ggplot()  + geom_point(data = points, mapping=aes(x=x,y=y)) + geom_function(fun=eq) + scale_x_log10(limits=c(0.1,10)) + scale_y_log10(limits=c(0.1,10))+xlab("vi")+ylab("PVbt")
  ggsave(filename=paste(name,".png",sep=""),p,"png")
  show(p)
  print(name)
  print(c("PVbt exp:",y))
  print(c("PVbt theor:",y_hat))
  print(c("error:",error))
  return(c(y_hat,error))
}
  
cores <- list(IL,EW,AC,L_IL,L_EW,L_AC)

for ( core in cores ) {
  BG_result = BG(core)
}

