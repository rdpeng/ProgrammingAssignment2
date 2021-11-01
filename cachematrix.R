## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL                                ## "inv" is the variable for the inverse matrix
        
        set <- function(y) {                       ## the function sets the value of the matrix,                            
                x <<- y                            ## of which the inverse will be calculated
                inv <<- NULL
        }
        get <- function() x                        ## the function returns the value of the matrix  
        setinv <- function(solve) inv <<- solve    ## calculating the inverse of the matrix, storing the
                                                   ## inverse matrix in the variable "inv"
        
        getinv <- function() inv                   ## return the value of the inverse matrix
        list(set = set, get = get,                 ## the created vector (list) which contains the functions
             setinv = setinv,
             getinv = getinv)

}


## This functions calculates the inverse of a matrix created with the above function.
## It first checks to see if the inverse matrix has already been calculated. 
## If so, it geit's the inverse matrix from the cache and skips the computation.
## Otherwise, it calculates the inverse of the matrix and sets the value of the 
## inverse matrix in the cache via the 'setinv' function. 

## The function returns a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
        inv <- x$getinv()                         ## getting the inverse matrix out of the cache
        if(!is.null(inv)) {                       ## checking if the inverse matrix is stored in the cache
                message("getting cached data")
                return(inv)                       ## returning the inverse matrix
        }
        data <- x$get()                           ## getting the matrix to calclulate the inverse if it wasn't 
        inv <- solve(data, ...)                   ## stored in the cache
        x$setinv(inv)                             ## calling the setinv function to store the inverse matrix in the cache
        inv                                       ## Return a matrix that is the inverse of 'x'       
}
