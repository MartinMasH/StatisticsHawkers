# Calcula el intervalo de confianza de longitud m�nima para la muestra
confmin = function(X, alpha)
	{
	
	nalpha = ceiling(length(X) * (1 - alpha))
	print("pre-ceiling:")
	print(length(X) * (1 - alpha))
	data = sort(X)
	print(data)
	# longInterv contendr� la longitud de los intervalos posibles
	longInterv = c()
	for (i in 1:(length(data) - nalpha))
		{
		longInterv = c(longInterv, data[nalpha + i] - data[i])
		} 
	print(longInterv)
	# Toma la longitud m�nima de intervalo
	interMin = min(longInterv)
	print("intermin:")
	print(interMin)
	# Toma los extremos del intervalo de longitud m�nima
	a = data[which.min(longInterv)]
	print("a:")
	print(a)
	b = a + interMin
	print("b")
	print(b)
	return(c(a, b))
	}
