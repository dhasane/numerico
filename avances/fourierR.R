intAn<- function(bin , numero , frecuencia )
{
  tam = length(bin)
  x = 0 
  pin = pi * numero *2 * frecuencia
  dat = 0
  for( nbin in bin ){
    print( nbin )
    
    if (nbin != '0')
    {
      A = x
      B = x + 1
      dat = dat + (cos(pin*A) - cos(pin*B))
    }
    x = x + 1
  }
  return (2/(periodo*pin))* dat
}

intBn <- function(bin , numero , frecuencia ){
  tam = length(bin)
  x = 0 
  pin = pi * numero * 2 * frecuencia 
  dat = 0
  for (nbin in bin)
  {
    if (nbin != '0'){
      A = x
      B = x+1
      dat = dat + (sin(pin*B) - sin(pin*A))
    }
    
    x = x + 1
  }
  return (2/(periodo*pin))* dat
}
  
  bin = 10100011
  armonicos = 10
  frecuencia = 5
  periodo = 3
  an = c()
  bn = c()
  cn = c()
  teta = c()
  
  x = 0
  for (arm in armonicos){
    x = x + 1
    num = x - 1
    
    van = intAn(bin , x , frecuencia)
    vbn = intBn(bin , x , frecuencia)
    vcn = sqrt( van**2 + vbn**2 )
    vteta = atan2( vbn , van ) 
    
    an= c(an, van) 
    bn= c(bn, vbn)
    cn= c(cn, vcn) 
    teta= c(teta,  vteta )
  }
    
  
  