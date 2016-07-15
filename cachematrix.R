## Functions makeCacheMatrix and cacheSolve work together to 
## enable the cacheing of inverse matrices, in order to
## efficiently store these data objects for future reuse, 
## minimizing use of processing resources.

## makeCacheMatrix a form of the matrix x, that can be cached
## Function setInv() saves the inverse of the matrix in the parent environment
## Function getInv() returns the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        invm <- NULL
        set <- function(y){
                x <<- y
                invm <<- NULL
                
        }
        get <- function() x
        setInv <- function(inverse) invm <<- inverse
        getInv <- function() invm
        list(set = set, get = get, setInv = setInv, 
             getInv = getInv)
        
}


## cacheSolve takes the cacheable form of the matrix 'x', checks if its inverse 
## 'inv.x' has been cached, and returns the inverse 'inv.x';  if not cached, generate
## 'inv.x', caches it and returns it.
## This function assumes that x can be inverted

cacheSolve <- function(x, ...) {
        ## Return a matrix 'inv.x' that is the inverse of 'x'
        inv.x <- x$getInv() 
        if(!is.null(inv.x)) {
                message("getting cached data")
                return(inv.x)
        }        
        data <- x$get()
        inv.x <- solve(data, ...)
        x$setInv(inv.x)
        inv.x           
}
