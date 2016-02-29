confcent=function(X,alpha,tol)
	{
	x0=percentil(X,alpha/2,tol)
	x1=percentil(X,1-alpha/2,tol)
	return(c(x0,x1))
	}