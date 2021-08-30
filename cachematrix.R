@@ -1,15 +1,54 @@
## Put comments here that give an overall description of what your
## functions do
## Week 3 / Programming Assignment 2: Lexical Scoping

## Write a short comment describing this function
## 2)makeCacheMatrix: creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
makeCacheMatrix <- function(mat = matrix()) {
        inv <- NULL
        set <- function(y) {
                
                ## assign values in parent environment
                mat <<- matrix
                inv <<- NULL
        }
        get <- function() x 

}
        ## assigns value of inv in parent environment
        setinverse <- function(inverse) {
                inv <<- inverse
        }
        getinverse <- function() {
                inv
        }
        
        ## return a list
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}

        
## Write a short comment describing this function
## 3)cascheSolve: computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##   If the inverse has already been calculated (and the matrix has not changed),
##   then the cachesolve should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
        
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
