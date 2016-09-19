## makeCacheMatrix leverages R's lexical scoping and the properties of list objects
## to generate a list of the setter and getter functions that will allow the
## caching of the results of the calculation of the inverse of a matrix.

## The function makeCacheMatrix first initializes two objects: 
## x, as a function argument; and m, as NULL, both within its own environment.
## The initialization assures that no values have been left in memory from previous iterations
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        
        ## set will assign values on the right to objects in the parent environment
        ## by using the <<- operator, so that they remains available after setsolve() completes
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        ## the function now defines the getter for the matrix x, relying on R's lexical scoping
        ## to retrieve x from the parent environment, as it wasn't defined in get()
        get <- function() x
        
        ## the function now defines the setter for the inverse by using the solve() function
        ## and once again the <<- operator to do it in its parent environment
        setsolve <- function(solve) m <<- solve
        
        ## now it's time to define the getter for the inverse, again relying on R's lexical scoping
        ## to retrieve m's value from the parent environment
        getsolve <- function() m
        
        ## Finally, makeCacheMatrix creates a list with one element for each of the four functions.
        ## Each of the elements is named in order to allow the use of the $ extract operator
        ## so that the functions can be later called by name, rather than using the [[ operator.
        ## The use of a list object also allows access to all objects defined in its original environment.
        list(set = set, get = get,
                setsolve = setsolve,
                getsolve = getsolve)
}

## cacheSolve takes the list object created by makeCacheMatrix, checks if the results are already cached
## and either returns the inverse of the matrix from the cache or calculates it with the solve() function
## and stores it in the cache for later use

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getsolve()
        ## if m is not null, the result is already available in memory (cached). It doesn't need
        ## to be calculated, just displayed, with a message stating it was retrieved from the cache
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        ## otherwise, cacheSolve will use the getter function in the list to create a new variable, data,
        ## containing the matrix that needs its inverse to be calculated
        data <- x$get()
        ## it will then calculate the inverse by using the solve() function and assign the results to m
        m <- solve(data, ...)
        ## the function will then use the setsolve function to cache the result for later use
        x$setsolve(m)
        ## and finally return m, the inverse of the original matrix
        m
}
