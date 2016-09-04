## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Creates an R equivalent of (POJO - business logic) for 
## a matrix and its inverse. 
## The last statement creates a list of functions defined in this 
## function. As the last statement in function is returned by default
## so this function returns a list of all the functions defined within it.
## These functions can be accessed like
## mcm <- makeCacheMatrix(matrix(1:4, 2, 2))
## mcm$get() ## returns the matrix as is
## mcm$set(matrix(round(rnorm(9, 10, 4)), 3, 3)) ## changes the stored matrix, and hence 
## sets the computed inverse (if calculated) to NULL.
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        
        get <- function() x
        
        setInverse <- function(matrInv) inv <<- matrInv
        
        getInverse <- function() inv
        
        list(set = set, get = get, 
             setInverse = setInverse, getInverse = getInverse)

}


## Write a short comment describing this function
## Should always be called with a variable containing the makeCacheMatrix
## For example,
##
## mcm <- makeCacheMatrix(x)
## cacheSolve(mcm)
##
## Checks if the matrix already has an inverse computed and cached.
## If cached result exists, it is returned without computing again,
## otherwise, the inverse is computed and cached. 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        if(!class(x)=="list" || is.null(x[["getInverse"]]) || is.null(x[["setInverse"]])){
                msg <- paste("The value passed is not an inverse-cachable matrix!",
                             "Sample Usage: \n", 
                             "m <- matrix(1:4, 2, 2)\n",
                             "mcm <- makeCacheMatrix(m)\n", 
                             "cacheSolve(mcm)")
                stop(msg)
        }
        inv <- x$getInverse()
        if(!is.null(inv)){
                message("Getting cached inverse")
                return(inv)
        }
        matr <- x$get()
        inv <- tryCatch(solve(matr), error = function(e)
                 {
                         msg <- paste("The matrix supplied is not inversible!",
                                      "Try another matrix like \n",
                                      "matrix(1:4, 2,2), or\n",
                                      "matrix(round(runif(9,20, 30)), 3, 3), or\n",
                                      "matrix(rnorm(1000000, 50000, 5000), 1000, 1000)")
                         stop(msg)
                 }
        )
        x$setInverse(inv)
        inv
}
