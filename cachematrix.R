## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## It creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
          m <- NULL
        set <- function(y){
                x <<- y
                m <<- NULL
                }
        ##get the value of the vector
        get <- function()x
        ##set the value of the inverse
        setinverse <- function(inverse)m
        ##get the value of the inverse
        getinverse <- function()m
        list(set = set,get = get,
             setinverse = setinverse,
             getinverse = getinverse)

}


## Write a short comment describing this function
## It computes the inverse of the special "matrix" returned by makeCacheMatrix above
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
