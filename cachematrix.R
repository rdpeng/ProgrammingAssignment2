## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## you should use the two functions togrther.firstly save the result of the"makecachematrix"
## in a variable like x.then call cachesolve(x) twice, at the second time u can see the message. 
makeCacheMatrix <- function(x = matrix()) {
        inv <- matrix()
        set <- function(y) {
                x <<- y
                inv <<- matrix()
        }
        get <- function() x
        setinv <- function(m) inv <<- m
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## Write a short comment describing this function
## The following function calculates the inverse of the special "matrix" created with the 
## above function. However, it first checks to see if the mean has already been 
## calculated. If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of the inverse 
## in the cache via the setinv function.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.na(inv)) {
                message("getting cached inverse of matrix x")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
