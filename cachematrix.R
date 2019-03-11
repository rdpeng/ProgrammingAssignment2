## To cache the inverse of given matrix and retrieve it in further computations

## Function to create a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
	  inv <- NULL
	  ## set the value of the matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
	  ## get the value of the matrix
        get <- function() x
	  ## set the inverse of the matrix
        setinverse <- function(inverse) inv <<- inverse
	  ## get the inverse of the matrix
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Function to compute inverse of the special matrix returned by makeCacheMatrix

## If inverse already calculated, retrieve from cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	  inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
