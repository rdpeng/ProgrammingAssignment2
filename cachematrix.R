## The below functions are used to accomplish inversion of a matrix with caching.Caching is an efficient way of computation as it uses the data in memory
## The 2 functions below put together are an example of lexical scoping used for computing the inverse of a matrix. Lexical scoping allows for the scope of the functionality
## of a variable to be determined at runtime in the 

## The makeCacheMatrix function below allows for lexical scoping i.e. determine the value of m to be returned at runtime based on appropriate function call of get, set, getsolve or setsolve

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}


## The cacheSolve function checks if a computed value for the inversion exists and accordingly calls the getsolve() to used the exisitn value in cache
## or else inserts value of matrix into the data variable and solves for the value of the inverted matrix using setsolve()


cacheSolve <- function(x) {
## Returns a matrix that is the inverse of 'x' using the precomputed value if it exists from cache
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setsolve(m)
        m
}
