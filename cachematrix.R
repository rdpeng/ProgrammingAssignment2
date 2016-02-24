## The purpose of this fuction is to return the inverse of a given matrix. To make the process
## more efficient, if the inverse of the matrix had already been calculated, the output will be cached
## and retreived if the process is to be completed again without the input matrix having changed.
        
        
## This function creates a special "matrix" that is a list containing the set, get, setinv and getinv functions. This
## is done so that the following fuction is able to use the data correctly.

makeCacheMatrix <- function(x = matrix()) {
i <- NULL
        set <- function(y){
                x <<- y
                i <<- NULL
        }
        
        get <- function() x
        setinv <- function(inverse) i <<- inverse
        getinv <- function() i
        
        list(set = set, get = get, setinv = setinv, getinv = getinv)

}


## This function then uses the special "matrix" and calculates it's inverse. Firstly it retreives the matrix 
## and checks if it's inverse has already been calculated. If it has, the program returns the cached value, 
## and if not it it calculates the inverse and stores it.

cacheSolve <- function(x, ...) {
        i <- x$getinv()
        
        if(!is.null(i)){
                message("getting cached data")
                return(i)
        }
        
        data <- x$get()
        i <- solve(data)
        x$setinv(i)
        i
}
