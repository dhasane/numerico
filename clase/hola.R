

setwd("./home/dhas/uni/numerico/parciales/parcial1")

A = matrix(c(1,  1,
             1, -1,
             3, -1), nrow=3, byrow=TRUE)
b = c(2,0.1,2)
qr.solve(A,b)

