## code for makeCacheMatrix function
makeCacheMatrix <- function(x = matrix())
{
	## set function, save the inverse to global variable
	set <- function(y)
	{
		x <<- solve(y)
	}

	## get function, returns the global variable 
	get <- function()
	{
		x 
	}
}

## code for cacheSolve
cacheSolve <- function(x)
{
	## retrieve the existing inverse variable
	m <- x$get()

	## check if already computed the inverseand is same
	if(!is.null(m) && m == solve(x))
	{
	message("getting cached data")
	return(m)
	}

	## didn't computre the inverse yet or not same
	## set the globale variable to our newly inverse matrix and write out the asnwer	
	m <- solve(x$get())
	x$set(m)
	return(m)

}
