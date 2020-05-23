## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## this function return the inverse of a matrix that take a matrix variable as parameter and then calculate the inverse
makeCacheMatrix <- function(x = matrix()) {
		Inverse<-NULL
		set <- function(y) {
                x <<- y
                Inverse <<- NULL
        }
		get <- function() x
        setinverse <- function(solv) Inverse <<- solv
        getinverse <- function() Inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)		
}


## Write a short comment describing this function
## this function return the cache inverse if existed, if else it returns the demanded inverse of your special matrix
cacheSolve <- function(x, ...) {
        Inverse <- x$getinverse()
        if(!is.null(Inverse)) {
                message("getting cached data")
                return(Inverse)
        }
        data <- x$get()
        Inverse <- solve(data)
        x$setinverse(Inverse)
        Inverse
}

