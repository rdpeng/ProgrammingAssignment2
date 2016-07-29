## These functions will return an inverse matrix, using caches to store and retrieve the original and its inverse

## The makeCacheMatrix function generates an R object that stores a matrix and its inverse matrix 
## It stores a list of functions (set, get, setinverse, getinverse) and data objects x and m
## x is the argument matrix and m is its inverse
makeCacheMatrix <- function(x = matrix()) {
        # mackeCache Matrix creates an R object that stores a vector and its mean
        # initialize the value m to store the cached inverse value
        m <- NULL
        # set will cache the input matrix x and will reinitialize m
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        # get returns the cached matrix
        get <- function() x
        # setinverse caches m passed through cacheSolve
        setinverse <- function(inverse) m <<- inverse
        # getinverse returns cached m
        getinverse <- function() m
        #the makeCacheMatrix function finally returns a list of named functions
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The cacheSolve function takes as argument the makeCacheMatrix object in order to 
## retrieve the inverse matrix 

cacheSolve <- function(x, ...) {
        # Return the cached matrix value m and see if it stores any values
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        # store the input matrix in "data"
        data <- x$get()
        # calculate the inverse matrix using solve and assign it to "m"
        m <- solve(data, ...)
        # use setinverse to cache the value of m
        x$setinverse(m)
        #return the value of m (the inverse matrix)
        m
}
