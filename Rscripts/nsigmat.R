nsigmat=function(X, n, t)
	{
	media=mean(X[(length(X) - t + 1):length(X)])
	desv=sd(X)
	Xnew=X[abs(X-media)<(n/2)*desv]
	return(Xnew)
	}