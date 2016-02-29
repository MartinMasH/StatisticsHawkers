#Realiza un proceso de limpieza de datos aplicando nsigma
#X es una muestra de una variable
#n es el parámetro que se le pasará a nsigma
#meanTol es una tolerancia (en tanto por uno) para la diferencia de las medias en cada iteración (se recomienda 0.1)
#sdTol es una tolerancia (en tanto por uno) para el ratio de las desviaciones típicas en cada iteración (se recomienda 0.1)
#deletedTol es el ratio máximo (en tanto por uno) de datos que se permiten eliminar
#Falta determinar con más exactitud, pero 0.27-0.30 han dado buenos resultados
sigmaiter = function(X, n, meanTol, sdTol, deletedTol)
	{
	oldData = X
	startingDays = length(X) # Numero de dias (tamaño muestral) inicial
	# Aplica nsigma a los datos
	newData = nsigma(oldData, n)
	# Calcula los días eliminados (totales)
	deletedDays = startingDays - length(newData)
	meandif = abs(mean(oldData) - mean(newData))
	# Calcula la diferencia de medias con respecto de la media anterior (tanto por uno)
	if (mean(oldData) != 0)
		{
		meandif = meandif/mean(oldData)
		}
	# Calcula el ratio entre las desviaciones típicas en cada iteración
	sdRatio = sd(newData)/sd(oldData)
	iter = 0
	# Las reglas de parada para el algoritmo son las siguientes:
	# La el ratio de dias eliminados no debe exceder el ratio máximo
	# La media de los conjuntos de datos antes y después de la iteración debe ser similar
	# El ratio de desviaciones típicas debe de estar cerca de 1
	while (((meandif > meanTol) || (sdRatio < 1 - sdTol)) && (deletedDays/startingDays < deletedTol))
		{
			oldData = newData
			# Aplica nsigma
			newData = nsigma(oldData, n)
			# Calcula el numero de días eliminados
			deletedDays = startingDays - length(newData)
			# Calcula la diferencia de medias con respecto de la media anterior (tanto por uno)
			meandif = abs(mean(oldData) - mean(newData))
				if (mean(oldData) != 0)
				{
				meandif = meandif/mean(oldData)
				}
			# Calcula el ratio entre las desviaciones típicas en cada iteración
			sdRatio = sd(newData)/sd(oldData)
			iter = iter + 1
			###########################DEBUG##########################
			print("Iteración:")
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
