## Coursera: R, week 3, Programming Assignment 2: Lexical Scoping

## npommer, 10.03.2017

## We do this because sometimes vectors can be very large and it takes a lot of time to compute them every time

## The first function "makeCacheMatrix" returns a list of functions

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        
        ## We store the matrix here
        set <- function(y){
                x <<- y 
                i <<- NULL  
        }
        ## Returns the stored matrix
        get <- function() x
        
        ## Caches the argument
        setinv <- function(inverse) 
        i <<- inverse
        
        ## Gets the cached argument
        getinv <- function() i
        
        ## Returns a list
        list(set = set,
             get = get,
             setinv = setinv,
             getinv = getinv) 
}


## cacheSolve calculates the inverse of a matrix which was created by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Gets the cached value
        i <- x$getinv()
        
        ## Returns the cached value if exists
        if(!is.null(i)) {
                message("Retreiving the cached inverse matrix...")
                return(i)
        }
        
        ## If not, get the matrix, calculate the inverse and store it in i
        m <- x$get()
        i <- solve(m, ...)
        x$setinv(i)
        
        ## Returns the inverse matrix
        i
}
