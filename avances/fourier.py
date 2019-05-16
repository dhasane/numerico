

# estas son las formulas esas horribles que hacen todo esto funcionar :v 
def intAn(bin , numero , frecuencia ):
	tam = len(bin)
	x = 0 
	pin = math.pi * numero *2 * frecuencia
	dat = 0
	for nbin in bin:
            print( nbin )
            if (nbin != '0'):
                A = x
                B = x+1
                dat += (math.cos(pin*A) - math.cos(pin*B))
            x += 1
	return (2/(periodo*pin))* dat

def intBn(bin , numero , frecuencia ):
	tam = len(bin)
	x = 0 
	pin = math.pi * numero *2 * frecuencia 
	dat = 0
	for nbin in bin:
	
		if (nbin != '0'):
			A = x
			B = x+1
			dat += (math.sin(pin*B) - math.sin(pin*A))
		x += 1
	return (2/(periodo*pin))* dat
# -----------------------------------------------------------------------

# puntos en un espacio tridimensional
def scatterplot(deg):
	from matplotlib import pyplot
	from mpl_toolkits.mplot3d import Axes3D
	import random
	fig = pyplot.figure()
	ax = Axes3D(fig)

	sequence_containing_x_vals = range(0,80)
	sequence_containing_y_vals = deg
	sequence_containing_z_vals = 5
	ax.scatter(sequence_containing_x_vals, sequence_containing_y_vals, sequence_containing_z_vals)
	pyplot.show()

# lineas en un espacio tridimensional
def lineplot(deg):
	import matplotlib.pyplot as plt
	from mpl_toolkits.mplot3d import Axes3D
	import numpy as np
	fig = plt.figure()
	ax = fig.add_subplot(111, projection='3d')
	
	arr = np.array(deg)
	#for n in range(5):
	#	for val in deg:
	ax.plot( range(0,len(arr)), arr, 0 )
		
	plt.show()
	Axes3D.plot()


import numpy as np
import math
import random as rand

frecuencia = 1/8 
periodo = 1/frecuencia 
corriente = 3/8 


#letra = input('ingresar letra : ')
anban = int(input('ingresar ancho de banda : '))
bitsps = int(input('ingresar bit por segundo : '))

armonicos = int((anban/bitsps) * 8)
print(armonicos)

# encuentra en binario del texto dado
#bin = ' '.join(format(ord(x), 'b') for x in letra)
#'1101000 1100101 1101100 1101100 1101111 100000 1110111 1101111 1110010 1101100 1100100'


#bin = '01000110'

#bin = '0' + bin

#print (bin)
print('---------------------')

#bin = list(bin)

#exit()

#bin = [0.45,0.9]
bin = [ rand.random() for i in range( 10 ) ]

print(bin)

listanum = list(range(armonicos))

an = []
bn = []
cn = []
teta = []

x = 0
for arm in range(armonicos):
	x = x + 1
	num = x - 1
	
	van = intAn(bin , x , frecuencia)
	vbn = intBn(bin , x , frecuencia)
	vcn = math.sqrt( van**2 + vbn**2 )
	vteta = math.atan2( vbn , van ) 
	
	an.insert(x,van)
	bn.insert(x,vbn)
	cn.insert(x,vcn)
	teta.insert(x, vteta )
	

for i in teta:
	print(i, "\n\n\n")

vt = range(8*10)
# esto tengo que aclarar que es el 8 bits

gts = []
for arm in range(armonicos): # supongo que esto es el numero de armonicos
	
	pin = 2 * math.pi * (arm + 1) * frecuencia
	
	cnn = cn[arm]
	
	tetan = teta[arm]
	
	gt = []
	
	for lin in vt:
		ll = lin / 10
		gt.insert(lin , cnn * ( math.sin( pin * ll + tetan )  ) ) 
		
	gts.insert(x,gt)

deg = []
for nvt in vt:
	degt = corriente
	for i in range(armonicos):
		degt += gts[i][nvt]
	
	deg.insert(nvt , degt)
	
	# hace la graficacion
#scatterplot(deg)

lineplot(deg)


