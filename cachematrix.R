## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL                   #initialize cache and clear it  
        set <- function(y) {
                x <<- y             # store the value the function was called with using superassignment operator 
                                    # so x will be available in the global environment
                m <<- NULL          # clear cache
        }
        get <- function() x         # retrieves the matrix from the cache 
        setinverse <- function(inverse) m <<- inverse # writes the inverse matrix into the cache 
        getinverse <- function() m  # gets inverse matrix  from the  cache
        list(set = set, get = get,  # returns a list with all four the defined functions 
                                    # to make them available from wherever makeCachematrix is called
             setinverse = setinverse,
             getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()         # read chached object
        if(!is.null(m)) {           # check whether cache already contains the previously calculated inverse 
                message("getting cached data")  
                return(m)           # return cached object
        }
        data <- x$get()             # read matrix 
        m <- solve(data, ...)       # calculate inverse of matrix
        x$setinverse(m)             # write inverse into cache before returning
        m                           # returning inverse
}

                                    # trying function in R

> source("makeCache.R")             # read functions 
> u <- matrix(c(1, 2, 3,4),2,2)     # u is defined as a invertible matrix 
                                    # (must be square & determinant ad-bc<>0)
> v  <- makeCacheMatrix(u)          # define v as u, using the makeCacheMatrix function   
> cacheSolve(v)                     # first time cacheSolve is called, it gives the inverse as expected 
[,1] [,2]                      
[1,]   -2  1.5
[2,]    1 -0.5
> cacheSolve(v)                     # the second time cacheSolve is called, it simply reads from the cache 
                                    # that has now been written by the makeCacheMatrix function 
[,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
> 






