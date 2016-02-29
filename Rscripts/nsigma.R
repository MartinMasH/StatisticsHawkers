#Dada una muestra, elimina los datos que distan n desviaciones de la media (hacia un lado)
#Es decir, 2n*dt es el intervalo centrado en la media con el cual se queda
nsigma=function(X, n)
	{
	media=mean(X)
	desv=sd(X)
	Xnew=X[abs(X-media)<(n/2)*desv]
	return(Xnew)
	}