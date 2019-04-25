

AreaBajoCurva <- function(f,inicio, fin,cortes)
{
  paso <- ( fin - inicio )/cortes
  for( i in 1:tam )
  {
    f(i*paso)
  }
}


simpson <- function()
{
  
}