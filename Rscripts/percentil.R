percentil=function(X,alpha,tol)
	{
	x0=min(X)
	x1=max(X)
	distri=ecdf(X)
	error=abs(alpha-distri(x0))
	while (error>tol)
		{
		x2=(x0+x1)/2
		if (distri(x2)<alpha)
			{
			x0=x2
			}
		else
			{
			x1=x2
			}
		error=abs(alpha-distri(x2))
		}
	return(x0)
	}