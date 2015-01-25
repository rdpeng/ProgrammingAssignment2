## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix is a function that returns a list of functions
## This function stores a matrix and cached value of its inverse 
## The following functions has been used 
## setMtx      Matrix value will be set in this function 
## getMtx      Matrix value will be retrived
## cacheInv    Inverse value is stored in the cached
## getInv      retrive the inverse value stored in the cached

makeCacheMatrix <- function(x = matrix()) {
        cache <- NULL
        
        setMtx <- function(nv) {
                x <<- nv
                # clear the cache
                cache <<- NULL
        }

        getMtx <- function() {
                x
        }

        cacheInv <- function(solve) {
                cache <<- solve
        }

        getInv <- function() {
                cache
        }
        
        list(setMtx = setMtx, getMtx = getMtx, cacheInv = cacheInv, getInv = getInv)
}


## Write a short comment describing this function
# Inverse of the matrix created from the above function "makeCacheMatrix" will be created from this function
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        # retrive cached data
        Inv <- x$getInv()
        # return the cached data if it has
        if(!is.null(Inv)) {
                message("retriving cached date")
                return(Inv)
        }
        # else, get the data for matrix and calculate the inverse and store in the cache
        data <- x$getMat()
        Inv <- solve(data)
        x$cacheInv(Inv)
        # return the Inv
        Inv        
}
