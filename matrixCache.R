## First attempt to collaboration on Git in a Coursera work
## Funtions to calculate Matrix Inverse of a given Matrix (integer, numeric, complex)

## Write a short comment describing this function
##Function for the creation of a matrix object with input matrix 
##In addition having set, get setInverse and getInverse Methods
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        ##Returns the methods added to Matrix to build
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function
##Funcion that obtains the Inverse of the given matrix
##The main feature is that this Inverse is computed just once
##When invoked this function again a cached inverse is returned (saving time!)
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("regresa matriz inversa en cache")
                return(i)
        }
       ## Possible message("Computing and Returning Inverse Matrix")
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i

}
