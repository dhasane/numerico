#install.packages("lmtest")   #Instalación del paquete

library(lmtest)  #Carga del paquete

#Error
error<- function(errorActual, errorMin){
  if(errorActual<errorMin){
    return (errorActual)
  }
  return (errorMin)
}

funcionError<- function(errorActual, errorMin, funcionActual, funcionErrorMin){
  if(errorActual<errorMin){
    return (funcionActual)
  }
  return (funcionErrorMin)
}

#principal
principal<- function(x,y)
{
  errorActual=0
  errorMin=0
  n=0
  for(n in 1:5)
  {
    if(n==0){
      #lineal
      funcionActual=lm(x ~ y)
      funcionErrorMin=funcionActual
      errorActual=summary(lm(x~ y))$coefficients[,1:2]
      errorMin=errorActual
    }else if(n==1){
      funcionActual<-nls(x~ y) 
      errorActual=summary(funcionActual)$parameters[,1:2] 
    }else if(n==2){
      #Grado euler
    }else if(n==3){
      #Funcion sin
    }else if(n==4){
      #Funcion cos
    }
    errorMin<-error( errorActual , errorMin )
    funcionErrorMin<-funcionError( errorActual, errorMin, funcionActual, funcionErrorMin )
    n = n + 1
  }
  
  if(error<0.05){
    print("Al tomar un error menor a 0.05 se encontro que la función que mejor se adapta a los puntos es la siguiente")
    summary(funcionErrorMin)
    plot(x,y)
    abline(funcionErrorMin) 
    plot(funcionErrorMin)
  }else{
    #llamar funcion Hamilton
  }
  
  x <- c(56, 42, 72, 36, 63, 47, 55, 47, 38, 42)
  y <- c(148, 126, 159, 118, 149, 130, 151, 142, 114, 141)
  
  principal(x,y)
  
}