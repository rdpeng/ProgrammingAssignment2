## In the following we cache the inverse of a matrix by a function.
## We assume that the matrix supplied is always invertible.
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inv <<- inverse
        getInverse <- function() inv
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## The following function computes the inverse of the special "matrix" created by 
## makeCacheMatrix defined as above.

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        mat <- x$get()
        inv <- solve(mat, ...)
        x$setInverse(inv)
        inv
}

##------------Testing the code--------------------

## m <- matrix(rnorm(25),5,5)
## n <- makeCacheMatrix(m)
## cacheSolve(n)

##           [,1]       [,2]       [,3]        [,4]        [,5]
##[1,]  0.37729651 -0.3336705  0.4386750  0.02228814 -0.13608972
##[2,] -0.85207336 -0.5909453 -0.6154542  0.54124955  0.74520703
##[3,]  0.06204543  0.3350884  0.6629582 -0.01445713  0.03366001
##[4,] -0.38626374 -0.1064740  0.1076266  0.52555823 -0.01029639
##[5,]  0.33023031 -0.4771565 -0.1629823 -0.84991955 -0.23875992
