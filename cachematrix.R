## This program is to compute and cache the inverse of a matrix
## to avoid computing it repeatedly

## This function creates a special matrix object 
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL ## Initializing NULL
    set <- function(y)  { ## Defining set function
        x <<- y
        m <<- NULL
    }
    get <- function() x  ## Returns x
    setinverse <- function(solve) m <<- solve ## Function to calculate inverse
    getinverse <- function() m     ## Function to return value of m
    list(set = set, get = get,     ## Sets the list 
    setinverse = setinverse,
    getinverse = getinverse)
    

}


## This function computes the inverse of special matrix returned by
## makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getinverse() ## Calls getinverse function from makeCacheMatrix and sets it to m
        if(!is.null(m)) {   ## Code snippet to return m if its cached
            message("getting cached data")
            return(m)
        }
        data <- x$get()   ## Calls get() function and set the value to variable "data" 
        m <- solve(data, ...)
        x$setinverse(m) ## Sets the inverse of matrix m
        m               ## Returns the inverse matrix m
        
}
