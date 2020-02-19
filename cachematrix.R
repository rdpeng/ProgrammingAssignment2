## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly, especially if it is going be used in a for loop and if
## we're sure we are going to re-use it somewhere else in the code. Caching can be a very efficient practice when we have a 'layered code', in the sense
## that we are computing some objects in the first layers, to be then re-used, rearranged in other, more sophisticated operations. 
## Below are a pair of functions that are used to create a special object that 
## stores a matrix in a list and caches its inverse.
## Write a short comment describing this function

## This function creates a special "matrix" object that can cache its inverse. It takes a matrix as argument; then, it clears possible past computations. By using set () and get (), the
## function mutates and retrieves the data needed/called in the cacheSolve function. Finally, it prints a list containing all the functions that we need in order to cache, compute
## and re-set matrix inversion more efficiently. makeChaceMatrix is like a big 'container' where we can find what we need.

makeCacheMatrix <- function(x = matrix()) { 

inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)

}

## This function computes the inverse of the special 'matrix' created by 
## makeCacheMatrix above. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache--> this is equivalent to retrieving an element (via x$getInverse(), where x is our argument of the list function)
## If the 'cache is empty', i.e. if it is the first time that we're calling the couple of functions or it have re-set x, we need to compute again the Inverse (when the if condition is FALSE)
## In order to do this, we first get (x$get()) the original matrix, and we assign it to the newly opened variable 'mat'. Then, we invert 'mat'; finally, we store it in makeCacheMatrix via the  x$setInverse (inv) operator


cacheSolve <- function(x, ...) { 
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}
