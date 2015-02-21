## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly. This pair of
## functions cache the inverse of a matrix.


## makeCacheMatrix creates a list, which contains a function to do the following:
## 1) set the value of the matrix x
## 2) get the value of the matrix x
## 3) set the value of the inverse of x
## 4) get the value of the inverse of x

makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = getinverse,
             getinverse = getinverse)
}


## cacheSolve returns the inverse of the matrix x.
## First, it checks whether the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## If not, it calculates the inverse of the matrix, and sets the value 
## of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
