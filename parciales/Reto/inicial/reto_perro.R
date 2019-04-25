


rm(list=ls())

hermite = function(x,y){
  
  v = 1:(length(x)-1)
  print("mas pruebaaas ")
  print(x)
  print(y)
  print("pruebaaa")
  for (i in 1:(length(x)-1))
  {
    print((y[i+1]-y[i]))
    print((x[i+1]-x[i]))
    
    print((y[i+1]-y[i])/(x[i+1]-x[i]))
    v[i] = (y[i+1]-y[i])/(x[i+1]-x[i])
    
  }
  print("pruebaaa")
  
  M = cbind(v)
  
  for(i in 2:(length(x)-1)){
    
    v1 = herHelper(x,v,i)
    
    M = cbind(M,v1)
    
    v = v1
    
  }
  
  return(M)
  
}


herHelper = function(x,z,paso){
  
  v = rep(0, times=length(x)-1)
  
  ii = 1
  
  for(i in paso:length(z)){
    
    v[i] = (z[i]-z[i-1])/(x[ii+paso]-x[ii])
    
    ii = ii+1
    
  }
  
  return(v)
  
}


obtenertAn = function(M,y){
  
  A = 1:length(y)
  
  A[1] = y[1]
  
  for (i in 1:(length(y)-1))
  {
    
    A[i+1] =
      M[i,i]
    
  }
  
  return(A)
  
}


calcularPolinomio = function(A,x,z){
  
  r = A[1]
  
  p = 1
  
  for (i in 2:length(A))
  {
    
    p = (z-x[i-1])*p
    
    r = r+(A[i]*p)
    
  }
  
  return(r)
  
}


graficarHermite = function(A,x,y){
  
  z = seq(x[1],x[length(x)],length=100
  )
  
  z1 = rep(0,times=length(z))
  
  for (i in 1:length(z))
  {
    
    z1[i] = calcularPolinomio(A,x,z[i])
    
  }
  
  plot(z,z1,type="l",asp=1)
  
  points(x, y, pch=19,
         cex=1, col =
           "red", asp=1,xlab="X",
         ylab="Y",
         main="interpolar")
  
}


invertirV = function(v,m){
  
  for (i in 1:length(v))
  {
    
    v[i] = m-v[i]
    
  }
  
  return(v)
  
}


limitarV = function(v,i,f){
  
  return(v[i:f])
  
}


generarZ = function(x,cant){
  
  z = seq(x[1],x[length(x)],length=cant
  )
  
  return(z)
  
}


generarZ1 = function(A,x,z){
  
  z1 = rep(0,times=length(z))
  
  for (i in 1:length(z))
  {
    
    z1[i] = calcularPolinomio(A,x,z[i])
    
  }
  
  return(z1)
  
}


calcularZs = function(cordsx,cordsy,cant,tam){
  
  zf = c()
  
  zff = c()
  
  print(cordsx[1])
  print(cordsy)
  
  for (i in 1:tam -1 )
  {
    
    M = hermite(cordsx[i,],cordsy[i,])
    
    A = obtenertAn(M,cordsy[i,])
    
    z = generarZ(cordsx[i,],100)
    
    zi = generarZ1(A,cordsx[i,],z)
    
    zf = c(zf,z)
    
    zff = c(zff,zi)
    
  }
  
  zetas = rbind(zf,zff)
  
  return(zetas)
  
}

binde = function(arr)
{
  #ret = rbind(arr[0],arr[1])
  #for (i in 1:length(arr))
  #{
  #  ret = rbind(ret[0][i],x)  
  #}
  
  ret = arr
  
  print("----------------ret ")
  print(arr)
  print("----------------ret ")
  print(ret)
  print("----------------ret ")
  
  return(ret)
  
}

ventana = function(x,y)
{
  for (i in 2:(length(x)/2 ) )
  {
    if(length(x)%%i == 0)
    {
      cat("valor actual: ",i,"\n")
      
      ret  = matrix(c(x), nrow=i, byrow=TRUE)
      ret2 = matrix(c(y), nrow=i, byrow=TRUE)
      print(ret)
      print(ret2)
      
      cat ("tamaño : ",length(ret),"\n --------------------------\n")
      
      
      zetas = calcularZs(ret,ret2,100,12)
      cat("-------------\n")
      xf = c(x)
      yf = c(y)
      plot(zetas[1,],zetas[2,],type="l",asp=1)
      points(xf, yf, pch=19, cex=1, col ="red", asp=1,xlab="X",ylab="Y",main="interpolar")
    }
  }
}


eval = function()
{
  
}

#y=c(3,3.7,3.9)
#y2 =c(4.5,5.7,6.69,7.12,6.7,4.45,7,6.1,5.6,5.87,5.15,4.1,4.3,4.1,3)                                                                                                       
#x=c(1,2,5)
#x2=c(6,7.5,8.1,10,13,17.6,20,23.5,24.5,25,26.5,27.5,28,29,30)   

y=c(3,3.7,3.9,4.5,5.7,6.69,7.12,6.7,4.45,7,6.1,5.6,5.87,5.15,4.1,4.3,4.1,3)                                                                                                       
x=c(1,2,5,6,7.5,8.1,10,13,17.6,20,23.5,24.5,25,26.5,27.5,28,29,30)   


print(x)

ret = ventana(x,y)

#length(x)
#length(y)
#



