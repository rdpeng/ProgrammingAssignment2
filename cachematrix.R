## Define a pair of functions that cache the inverse of a matrix rather than computing.
## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## create a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        ## set inverse as null
        inv <- NULL
        ## Define a function that assigns matrix x to a new matrix y and resets the inverse as NULL
        set <- function(y) {
               x <<- y
               inv <<- NULL
        }
        ## define a funtion to return x
        get <- function() x
        ## define a function using function solve to get the inverse of matrix 'x' and store into 'inv'
        setinverse <- function(solve) inv <<- solve
        ## define a funtion to return the inverse of matrix 'x'
        getinverse <- function() inv
        ## return a list of functions defined
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## Return inverse of the special "matrix" if its inverse is found from the above result.
## Otherwise, computes the inverse.  
## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## call the function to get the inverse of matrix 'x'
        inv <- x$getinverse()
        ## test whether the result exits
        if(!is.null(inv)) {
             message("getting cached data")
              return(inv)
        }
        ## get the original matrix and compute the inverse
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
