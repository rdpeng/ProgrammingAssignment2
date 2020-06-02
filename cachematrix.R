## Below are two functions that are used to create a special "matrix" object and caches its inverse.


## The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to

## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
	a<-NULL
	set <- function(y)
	{
		x<<- y
		a<<- NULL
	}

	get <- function() x

	setinverse <- function(inverse) a<<- inverse
	getinverse <- function() a

	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated, then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        a<-x$getinverse()
        if(!is.null(a)){
        	message("We are getting the cached data")
        	return (a)
        }

        data <- x$get()
        a<- solve(data, ...)
        x$setinverse(a)
        a
}
