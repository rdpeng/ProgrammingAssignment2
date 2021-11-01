## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        
        ## "inv" is the variable for the inverse matrix
        inv <- NULL                                
        
        ## the function sets the value of the matrix, of which the inverse will be calculated 
        set <- function(y) {                                               
                x <<- y                           
                inv <<- NULL
        }
        
        ## the function returns the value of the matrix
        get <- function() x     
        
        ## calculating the inverse of the matrix, storing the inverse matrix in the variable "inv"
        setinv <- function(solve) inv <<- solve    
                                                   
        ## return the value of the inverse matrix
        getinv <- function() inv                   
        list(set = set, get = get,                
             setinv = setinv,
             getinv = getinv)

}

####
# This functions calculates the inverse of a matrix created with the above function.
# It first checks to see if the inverse matrix has already been calculated. If so, it geit's the inverse matrix from the cache and skips the computation. 
# Otherwise, it calculates the inverse of the matrix and sets the value of the inverse matrix in the cache via the 'setinv' function. 
# The function returns a matrix that is the inverse of 'x'
####

cacheSolve <- function(x, ...) {
        ## getting the inverse matrix out of the cache
        inv <- x$getinv()     
        
        ## checking if the inverse matrix is stored in the cache
        if(!is.null(inv)) {                       
                message("getting cached data")
                
                ## returning the inverse matrix
                return(inv)                       
        }
        
        data <- x$get()                           ## calclulate the inverse if it wasn't 
        inv <- solve(data, ...)                   ## stored in the cache
        x$setinv(inv)                             ## calling the setinv function to store the inverse matrix in the cache
        inv                                       ## Return the inverse of 'x'       
}
