## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix is a function that will create a special "matrix" (i.e. an inversed matrix)

makeCacheMatrix <- function(x = matrix()) {
        ##Create the inverse matrix
        cm <- NULL
        
        ##Setting the matrix
        set <- function(y){
                x <<- y
                cm <<- NULL
        }
        
        ##Getting the matrix
        get <- function() x
        
        ##Solving (i.e. finding the inverse) of the matrix
        setinv <- function(inv) cm <<- inv
        getinv <- function() cm
        
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## Write a short comment describing this function

##Calculating the inverse of the matrix if not in cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        cm <- x$getinv()
        
        ##If in cache, returns inverse
        if(!is.null(cm)) {
                message("getting cached data")
                return(cm)
        }
        
        ##If not in cache, computes inverse
        
        data <- x$get()
        cm <- solve(data, ...)
        x$setinv(cm)
        cm
        
}
