## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeVector creates a special "vector", which is really a list containing a function to
## set the value of the vector
## get the value of the vector
## set the value of the mean
## get the value of the mean

makeVector <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## Write a short comment describing this function
## The following function calculates the mean of the special "vector" 
## created with the above function. However, it first checks to see if the 
## mean has already been calculated. If so, it gets the mean from the cache 
## and skips the computation. Otherwise, it calculates the mean of the data 
## and sets the value of the mean in the cache via the setmean function.
cachemean <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}