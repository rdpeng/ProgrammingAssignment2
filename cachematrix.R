## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        ##initialize the inverse to NULL
        inv <- NULL
        
        ##define x and inv in the enclosing enviroment
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        ##return x
        get <- function() x
        
        ##set the inverse of the Matrix x, called by cacheSolve
        setinverse <- function(inverse) inv <<- inverse
        
        ##return inverse
        getinverse <- function() inv
        
        ##return of the makeCacheMatrix is a list
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ##get the inverse of the matrix defined inside x
        inv <- x$getinverse()
        
        ##if we've computed inverse and stored it in inv,
        #then stop computing and return stored result
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        
        ## calculate the inverse of the matrix, pass to inv
        inv <- solve(data, ...)
        x$setinverse(inv)     
        inv        ## Return a matrix that is the inverse of 'x'
}