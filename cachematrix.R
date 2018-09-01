## Put comments here that give an overall description of what your
## functions do

## This function creates a list of 4 other functions 
## that will be used to cache the inverse of a matrix
## in the cacheSolve function.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
                
        }
        get <- function()x
        setInverse <- function(inverse) m <<- inverse
        getInverse <- function() m
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
        
}


## This function takes as input a list similar to the 
## return value of makeCacheMatrix and checks if the Inverse
## has been calculated. If so, it retrieves it from the getInverse function
##  otherwise it is calculated and cached. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}

