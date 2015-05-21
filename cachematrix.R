        ##   makeCacheMatrix is a function that stores a list of functions. 
        ##  makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
        ##  makeCacheMatrix contains 4 functions: set, get, setmean, getmean.

makeCacheMatrix <- function(x = matrix()) {

        
        inv <- NULL                             ##   restores to null the value of the inverse inv
        
        
        set <- function(y) {                    ##  set is a function that changes the vector stored in                                                              ##  the main function.
                x <<- y  
                inv <<- NULL
        }
        
        get <- function() {                     ##  get is a function that extracts and returns the vector x                                                               ##  stored in the main function. Doesn't require any input.
                x
        }         
        
        setinv <- function(inverse) {           ##   stores inverse into inv in the main function makeCacheMatrix
                inv <<- inverse                                               
        }
        
        getinv <- function() {                  ##   gets the value of inv in the main function makeCacheMatrix
                inv
        }
        
  list(set = set, get = get, setinv = setinv, getinv = getinv)         ## stores the 4 functions in the function makeCacheMatrix
                                                                       ## when we assign makeVector to an object, the object has all the 4 functions.
}


        ## BEGINNING OF cachemean
        ##  cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
        ##  If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should 
        ##  retrieve the inverse from the cache.
        ## Input of cachemean is the object where makeVector is stored.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()                                ##  stores the value of inv in the function makeCacheMatrix
        
        if(!is.null(inv)) {                              ##  verifies the value inv
                message("getting cached data")
                return(inv)
                
## The portion below serves as the else statement if the if-statement above is not satisfied
        
        data <- x$get()                                 ##  extracts and returns the matrix x in the function makeCacheMatrix
        inv1 <- solve(data, ...)                        ##  computes the inverse of a square invertible matrix
        x$setinv(inv1)                                               
        inv <- inv1                                     ##  stores inv1 into inv in the main function makeCacheMatrix
        
        inv1                                            ##  outputs inv1    
}
