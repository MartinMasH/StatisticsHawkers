#Dada una muestra, elimina los datos que distan 3 desviaciones t�picas de la media (3 hacia cada lado)
seissigma=function(X)
	{
	media=mean(X)
	desv=sd(X)
	Xnew=X[abs(X-media)<3*desv]
	return(Xnew)
	}