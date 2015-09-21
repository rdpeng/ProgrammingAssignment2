## https://github.com/chockyspice/ProgrammingAssignment2.git
## 1st commit SHA-1 hash identifier: 9b4fb30af1f22f6ee7760097d46947e96f4117e2
## R-Programming-Assignment2: Lexical Scoping--caching the inverse of a matrix

## The first function, makeVector creates a special "vector", which is really a list
## containing a function to
## 1.set the value of the vector
##  2. get the value of the vector
##  3. set the value of the mean
##  4. get the value of the mean

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


## The following function calculates the mean of the special "vector" 
## created with the above function. However, it first checks to see 
## if the mean has already been calculated. If so, it gets the mean from 
## the cache and skips the computation. Otherwise, it calculates the mean 
## of the data and sets the value of the mean in the cache via the setmean 
## function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
