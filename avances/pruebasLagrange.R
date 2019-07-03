install.packages("rSymPy")   #Instalación del paquete

library(rSymPy)

install.packages("rJava")   #Instalación del paquete
library(rJava)

lagrange.poly <- function(x, y) {
  
  l <- list() # List to store Lagrangian polynomials L_{1,2,3,4}
  k <- 1
  
  for (i in x) {
    # Set the numerator and denominator of the Lagrangian polynomials to 1 and build them up
    num <- 1
    denom <- 1
    
    # Remove the current x value from the iterated list
    p <- x[! x %in% i]
    
    # For the remaining points, construct the Lagrangian polynomial by successively 
    # appending each x value
    for (j in p) {
      num <- paste(num, "*", "(", 'x', " - ", as.character(j), ")", sep = ", collapse = ")
      denom <- paste(denom, "*", "(", as.character(i)," - ", as.character(j), ")", sep = ", collapse = ")
    }
    
    # Set each Lagrangian polynomial in rSymPy to simplify later.
    l[k] <- paste("(", num, ")", "/", "(", denom, ")", sep = ", collapse = ")
    k <- k + 1
  }
  
  # Similar to before, we construct the final Lagrangian polynomial by successively building 
  # up the equation by iterating through the polynomials L_{1,2,3,4} and the y values 
  # corresponding to the x values.
  eq <- 0
  
  for (i in 1:length(y)) {
    eq <- paste(eq, '+', as.character(y[i]), "*", l[[i]], sep = ", collapse = ")
  }
  
  # Define x variable for rSymPy to simplify
  #x <- Var('x')
  
  # Simplify the result with rSymPy and return the polynomial
  return(sympy(paste("simplify(", eq, ")")))
}


ordenar <- function( l1, l2 )
{
  ret = c()
  ret2 = c()
  while( length(l1) > 0 )
  {
    min <- Inf 
    pos <- 0 
    for ( i in length(l1) )
    {
      if ( l1[i] < min )
      {
        min <- l1[i]
        pos <- i
      }
    }
    ret = c(ret, l1[i] )
    ret2= c(ret2, l2[i])
    
    l1 = l1[-i]
    l2 = l2[-i]
  }
  return( ret2 )
  
}

install.packages("polynom")   #Instalación del paquete
install.packages("ggplot2")   #Instalación del paquete
library(polynom)
library(ggplot2)

x <- c(56, 41, 72, 36, 63, 48, 55, 47, 38, 42)
y <- c(148, 126, 159, 118, 149, 130, 151, 142, 114, 141)

y = ordenar(x,y) 
x = sort(x)


plot(x,y,type="l",asp=1,main="Mano")



dat <- data.frame(cbind(x, y))

lagrange.poly(x,y)

#ggplot(dat, aes(x=x, y=y)) + geom_point(size=5, col='blue')

ff <- poly.calc(x, y)

print(ff)

poly <- function(x) -534078600 + 101300500*x - 8488987*x^2 + 412469.8*x^3 - 12804.82*x^4 + 263.3596*x^5 - 3.588186*x^6 + 0.03122567*x^7 - 0.0001574767*x^8 +  
  3.506253e-07*x^9 

ggplot(dat, aes(x=x, y=y)) + 
  geom_point(size=5, col='blue') + 
  stat_function(fun = poly, size=1.25, alpha=0.4)















x <- c(0, 2, 3, 4)
y <- c(7, 11, 28, 63)

x <- c(56, 41, 72, 36, 63, 48, 55, 47, 38, 42)
y <- c(148, 126, 159, 118, 149, 130, 151, 142, 114, 141)
y = ordenar(x,y) 
x = sort(x)

dat <- data.frame(cbind(x, y))

#ggplot(dat, aes(x=x, y=y)) + 
#  geom_point(size=5, col='blue')


f <- function(x) {
  return(-534078600 + 101300500*x - 8488987*x^2 + 412469.8*x^3 - 12804.82*x^4 + 263.3596*x^5 - 3.588186*x^6 + 0.03122567*x^7 - 0.0001574767*x^8 +  
           3.506253e-07*x^9 )
}

ggplot(dat, aes(x=x, y=y)) + 
  geom_point(size=5, col='blue') + 
  stat_function(fun = poly.calc(x, y) , size=1.25, alpha=0.4)


