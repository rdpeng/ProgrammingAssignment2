##  A pair of functions that cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(matrix)
        {
          x <<- matrix
          m <<- NULL
        }
        get <- function() {x}
        setsolve <- function(inverse) {m <<- inverse}
        getsolve <<- function() {m}
        list (set = set , get = get ,
              setsolve = setsolve ,
              getsolve = getsolve)
}


## Compute the inverse of the special matrix returned by "makeCacheMatrix"
## above. If the inverse has already been calculated (and the matrix has not
## changed), then the "cachesolve" should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        
        if (!is.null(m))
        {
              message('I have reached to cached data')
              return(m)
        }
        
        my_data <- x$get()
        m <- solve(my_data , ...)
        x$setsolve(m)
        ## Return a matrix that is the inverse of 'x'
        m
        
}

### HERE SOME TESTED RANDOM MATRICES

x <- matrix(c(1,2,5,-6,0,9,12,8,3), 3, 3)
x1 <-makeCacheMatrix(x)
cacheSolve(x1)

y <- matrix(c(1,2,3,4) ,2,2)
y1 <-makeCacheMatrix(y)
cacheSolve(y1)