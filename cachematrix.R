## Calculate the inverse of a matrix using a cache to avoid
##  recalculating it everytime

## makeCacheMatrix - Creates a list that contains the fuctions to set/get the 
##   matrix value and set/get the matrix inverse
## Parameter:  x is the matrix to cache inverse evaluation for
## Return: the list of functions

makeCacheMatrix <- function(x = matrix()) {
    #initialize the cache
    cachedinverse <- NULL

    set <- function(y) {
        x <<- y
        cachedinverse <<- NULL
    }

    # define get function to get the matrix
    get <- function() x

    # define set function to set the cached inverse
    setinverse <- function(inverse) cachedinverse <<- inverse

    # define get function to get the inverse
    getinverse <- function() cachedinverse

    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve - Executes the solve funtion and catches the result for future use
## Parameter(s):  x is the matrix to calculate inverse evaluation for
## Return: the inverse of x

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'

    ## attempts to get result from cache if one is available
    cachedinverse <- x$getinverse()
    if (!is.null(cachedinverse)) {
        message("getting cached data")
        return(cachedinverse)
    }

    #If we are here, there is no cache, attempt to get x, solve it and set the cache
    data <- x$get()
    cachedinverse <- solve(data)
    x$setinverse(cachedinverse)
    cachedinverse
}

#Test
#A <- matrix(c(5, 1, 0,
#               3, -1, 2,
#               4, 0, -1), nrow = 3, byrow = TRUE)
#solve(A)

#m <- makeCacheMatrix(A)
#> m$get()
#[, 1][, 2][, 3]
#[1,] 5 1 0
#[2,] 3 - 1 2
#[3,] 4 0 - 1
#> m$getinverse()
#NULL
#> cacheSolve(m)
#[, 1][, 2][, 3]
#[1,] 0.0625 0.0625 0.125
#[2,] 0.6875 - 0.3125 - 0.625
#[3,] 0.2500 0.2500 - 0.500
#> m$getinverse()
#       [, 1][, 2][, 3]
#[1,] 0.0625 0.0625 0.125
#[2,] 0.6875 - 0.3125 - 0.625
#[3,] 0.2500 0.2500 - 0.500
#> cacheSolve(m)
#getting cached data
#       [, 1][, 2][, 3]
#[1,] 0.0625 0.0625 0.125
#[2,] 0.6875 - 0.3125 - 0.625
#[3,] 0.2500 0.2500 - 0.500