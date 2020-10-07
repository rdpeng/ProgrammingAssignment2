## A combination of two functions that cache the inverse of a matrix.


## The first function (shown below) creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) 
	{
  		inv <- NULL
  		set <- function(y)
			{
    				x <<- y
    				inv <<- NULL 
              		}
 		get <- function() x
  		setInverse <- function(inverse) inv <<- inverse
  		getInverse <- function() inv
  		list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
	}
  	

## The second function (shown below) computes the inverse of the special "matrix" returned by makeCacheMatrix function above.

cacheSolve <- function(x, ...) 
	{
  		inv <- x$getInverse()
  		if(!is.null(inv))
			{
    				message("getting cached data")
    				return(inv)
  			}
  		m <- x$get()
  		inv <- solve(m,...)
  		x$setInverse(inv)
  		inv      
	}