#Realiza un proceso de limpieza de datos aplicando nsigma
#X es una muestra de una variable
#n es el par�metro que se le pasar� a nsigma
#meanTol es una tolerancia (en tanto por uno) para la diferencia de las medias en cada iteraci�n (se recomienda 0.1)
#sdTol es una tolerancia (en tanto por uno) para el ratio de las desviaciones t�picas en cada iteraci�n (se recomienda 0.1)
#deletedTol es el ratio m�ximo (en tanto por uno) de datos que se permiten eliminar
#Falta determinar con m�s exactitud, pero 0.27-0.30 han dado buenos resultados
sigmaiter = function(X, n, meanTol, sdTol, deletedTol)
	{
	oldData = X
	startingDays = length(X) # Numero de dias (tama�o muestral) inicial
	# Aplica nsigma a los datos
	newData = nsigma(oldData, n)
	# Calcula los d�as eliminados (totales)
	deletedDays = startingDays - length(newData)
	meandif = abs(mean(oldData) - mean(newData))
	# Calcula la diferencia de medias con respecto de la media anterior (tanto por uno)
	if (mean(oldData) != 0)
		{
		meandif = meandif/mean(oldData)
		}
	# Calcula el ratio entre las desviaciones t�picas en cada iteraci�n
	sdRatio = sd(newData)/sd(oldData)
	iter = 0
	# Las reglas de parada para el algoritmo son las siguientes:
	# La el ratio de dias eliminados no debe exceder el ratio m�ximo
	# La media de los conjuntos de datos antes y despu�s de la iteraci�n debe ser similar
	# El ratio de desviaciones t�picas debe de estar cerca de 1
	while (((meandif > meanTol) || (sdRatio < 1 - sdTol)) && (deletedDays/startingDays < deletedTol))
		{
			oldData = newData
			# Aplica nsigma
			newData = nsigma(oldData, n)
			# Calcula el numero de d�as eliminados
			deletedDays = startingDays - length(newData)
			# Calcula la diferencia de medias con respecto de la media anterior (tanto por uno)
			meandif = abs(mean(oldData) - mean(newData))
				if (mean(oldData) != 0)
				{
				meandif = meandif/mean(oldData)
				}
			# Calcula el ratio entre las desviaciones t�picas en cada iteraci�n
			sdRatio = sd(newData)/sd(oldData)
			iter = iter + 1
			###########################DEBUG##########################
			print("Iteraci�n:")
			print(iter)
			#print("Datos:")
			#print(newData)
			print("Dias iniciales - dias eliminados - ratio")
			print(startingDays)
			print(deletedDays)
			print("Diferencia de medias - RatioSD - RatioEliminados")
			print(meandif)
			print(sdRatio)
			print(deletedDays/startingDays)
			print("--------------------")
			###########################DEBUG##########################
		}
	print("Iter:")
	print(iter)
	return(oldData)
	}
