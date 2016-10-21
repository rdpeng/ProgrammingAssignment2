## Put comments here that give an overall description of what your
## functions do
## 1. Starting with the example of cache form the assignment,
##    I change the mean computation to solve.
## 2. Utilizing the !is.null in the cacheSolve, it will pull the
##    data ONLY when m is null, so in essence if I run:
##    
##    solve(matrix(1)) 
##    
##    It will take longer than if I run
##
##    cacheSolve(makeCacheMatrix(1))
##    
##    


## Write a short comment describing this function

## Upon declaring and running the makeCacheMatrix, the data in m
## is stored into variable. 
## The cacheSolve take advantage of the lexical scoping by checking
## the enviroment for m. Eventhough m is not part of the cacheSolve, 
## it is available for reading in the enviroment.


makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        get <- function () x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set=set, get=get,
             setsolve = setsolve,
             getsolve = getsolve)
                
}

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}

## Additional line added for Github Testing