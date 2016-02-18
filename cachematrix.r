
## Put comments here that give an overall description of what your
## functions do


##this function creates a special matrix in memory
#input matrix object

makeCacheMatrix <- function(x = matrix) {	        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) m <<- inverse
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


##input - object of matrix type
## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
        m <- x$getInverse()		#check if we already have data in cache
        if(!is.null(m)) {			#by calling gtefunction of makeCacheMatrix
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setInverse(m)
        m
}
