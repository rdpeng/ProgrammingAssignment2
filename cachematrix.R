## The makeChacheMatrix function is able to create a special "matrix" object that cache the inverse
## a través de la función solve

##It set and get the value of the matrix. And set and get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	n <- NULL
	set <- function(y) {
		x <<- y
		n <<- NULL
	}
	get <- function() x
	setinverse <- function(solve) n <<-solve
	getinverse <- function() n
	list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## The function below is able to calculate the inverse of the matrix returned by the funcion above.
## If the inverse has been calculated before, the cacheSolve function returns it from the cache.

cacheSolve <- function(x, ...) {
	n <- x$getinverse()
	if (!is.null(n)) {
		message("getting cached data")
		return(n)
	}
	dato <- x$get()
	n <- solve(dato, ...)
	x$setinverse(n)
	n
}