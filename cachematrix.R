## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Similar to the example given, we want to: 
##      1) set the value of the matrix
##      2) get the value of the matrix
##      3) set the value of the inverse of the matrix
##      4) return the value of the inverse of the matrix

## We will use the following makeCacheMatrix function in the 
## cacheSolve function to create the inverse of a matrix and 
## cache it to speed up R's calculations.

makeCacheMatrix <- function(x = matrix()) {
        set <- function(y) {
                ## like the example, we will assign x to y and the current inverse to be null
                x <<- y
                invs <<- NULL
        }
        ## the rest of the function is the same as the example, but with inverse instead of mean
        get <- function() x
        setinvs <- function(inverse) invs <<- inverse
        getinvs <- function () invs
        list(set = set, get = get, setinvs = setinvs, getinvs = getinvs)
}


## Write a short comment describing this function

## This function will take the above makeCacheMatrix function
## and return the inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        invs <- x$getinvs()
        if(!is.null(invs)){  ## if the inverse has already been done, skip it
                message("getting cached data")
                return(invs)
        }
        
        ## if the inverse has not been done, do it now
        invs.data <- x$get()
        invs <- solve(invs.data, ...)
        
        ## now set the value of the inverse matrix
        
        x$setinvs(invs)
        
        return(invs)
}
