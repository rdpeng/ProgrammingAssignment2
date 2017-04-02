## Put comments here that give an overall description of what your
## functions do

## The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to 
## 1 


makeCacheMatrix <- function(x = matrix()) {
                i <- NULL
                set <- function(y) {
                        x <<- y
                        i <<- NULL
                }
                get <- function() x
                setinverse <- function(inverse) i <<- inverse
                getinverse <- function() m
                list(set = set, get = get,
                     setinverse = setinverse,
                     getinverse = getinverse)
}                      


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
