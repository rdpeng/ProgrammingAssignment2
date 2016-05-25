## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        # x is a square invertible matrix
        # returns a list containing functions to:
        # 1. set the matrix
        # 2. get the matrix
        # 3. set the inverse
        # 4. get the inverse
        # this list is used as the input to cacheSolve()
        
        # set the matrix
        inv_m <- NULL
        set <- function(y) {
        
        # `<<-` assigns a value to an object in an environment different from current environment
                x <<- y 
                inv_m <<- NULL
        }
        
        # get the matrix
        get <- function() x
        
        # set the inverse
        setinv <- function(inverse) inv_m <<- inverse
        
        # get the inverse
        getinv <- function() inv_m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}




## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        # output of makeCacheMatrix()
        # returns the inverse of the original matrix input to makeCacheMatrix()
        
        inv_m <- x$getinv()
        
        # write if loop for 2 conditions 1. inverse already calculated 2. get inverse from cache
        # if the inverse has already been calculated
        
        if(!is.null(inv_m)) {
                # get it from the cache and skip computation
                message("getting cached inverse matrix data")
                return(inv_m)
        }
        # otherwise, calculate the inverse 
        else {
                data <- x$get()
                inv_m <- solve(data, ...)
                
                #inv_m <- solve(x$get())
                
                # set the value of the inverse in the cache via the setinv function.
                x$setinv(inv_m)
                return(inv_m)
        }
}   
