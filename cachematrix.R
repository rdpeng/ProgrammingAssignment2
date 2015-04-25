##The below function is able to cache potentially time-consuming computations.
##For example, taking the inverse of a numeric matrix is typically a fast 
##operation. However, for a very large matrix, it may take too long to compute 
##the inverse, especially if it has to be computed repeatedly (e.g. in a loop). 
##If the contents of a matrix are not changing, it may make sense to cache the 
##value of the mean so that when we need it again, it can be looked up in the 
##cache rather than recomputed.

##The first function, makeCacheMatrix creates a special "matrix", which is a 
##list containing a function to
##1. set the value of the matrix,
##2. get the value of the matrix,
##3. set the value of the inverse and
##4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) m <<- inverse
        getInverse <- function() m
        list(set = set, 
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

##The below function calculates the inverse of the special "matrix" created from 
##the above function. However, it first checks to see if the inverse has already 
##been calculated. If so, it gets the inverse from the cache and skips the 
##computation. Otherwise, it calculates the inverse of the data and sets the 
##value of the inverse in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}

a <- makeCacheMatrix( matrix(c(1,2,12,13), nrow = 2, ncol = 2) );
a$get();
cacheSolve(a);
cacheSolve(a);
