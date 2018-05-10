## Put comments here that give an overall description of what your
## functions do
## The first function,makeCacheMatrix   creates a special "matrix", and perform following  function to
##set the value of the matrix
##get the value of the matrix
##set the value of the inverse
##get the value of the inverse
## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
 m <- NULL
    set <- function(y){
       x <<- y
       m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
##The following function calculates the inverse of the special "matrix" created by the above function.
##However, it first checks to see if the inverse has already been calculated.
##If so, it gets the inverse from the cache and skips the computation. 
##Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinverse function.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
          t <- x$getinverse()
    if(!is.null(t))
    {
       print("cache data")
       return(t)
    }
    data <- x$get()
    t <- solve(data)
    x$setinverse(t)
    t
}
