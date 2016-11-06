## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) 
{
	inv <- NULL # Defines the inverse as the NULL matrix
	
	set <- function(m) # set the matrix for which inverse has to be computed
	
	{
		x<<-m
		inv <<- NULL
	}
	
	get <- function() x	# function to get matrix x
	
	setInv <- function(inverse) inv <<- inverse # function which sets the inverse
	
	getInv <- function() inv # returns the inverse

	list(set =set,get = get, setInverse = setInv, getInverse = getInv) # Returns the list 

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) 

{
        ## Return a matrix that is the inverse of 'x'

inv <- x$getInv() # gets the inverse from the cache function
	
	if(!is.null(inv)) # check if the inverse already exists
	
	{
		message("getting cached data")
		return(inv)
	}

	data <-x$get() # call for the matrix
	
	inv <- solve(data) # compute inverse using solve function
	
	x$setInv(inv) # sets inverse for future purposes
	
	inv # returns the inverse

}
