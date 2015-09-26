## Purpose of this assignment: Create two functions as follows:
## makeCacheMatrix: is a function that creates a special "matrix"
##   object that can cahche its inverse. 
## cacheSolve: is a function that computes the inverse of the 
##   special "matrix" returned by makeCacheMatrix above.
##   If the inverse has already been calculated(and the matrix is not
##   changed), then the cacheSolve should retrieve the inverse from
##   the cache.

## makeCacheMatrix function creates a special "matrix"object that can cahche 
##      its inverse. By Setting and Getting the function, then Setting and Getting
##      the inverse.

makeCacheMatrix <- function(x = matrix()) {
             m <- NULL                                    ## here matrix is NULL
             set <-function(y) {                          ## Set function
                        x <<- y
                        m <<- NULL
             }         
             get <- function() x                          ## Get function
             setInverse <- function(solve) m<<- solve     ## Set Inverse using solve fun.
             getInverse <- function () m                  ## Get Inverse
             list(set = set,
                  get = get,
                  setInverse = setInverse,
                  getInverse = getInverse)
}

## cacheSolve: is a function that computes the inverse of the special "matrix"

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <-x$getInverse()                                ##Get Inverse from x
        if (!is.null(m)) {                                ##Checkes if matrix is cached
                message("getting existing data")          ##Getting cache, since m is not NULL
                return (m)
        }
        m_data <-x$get()                                  ##Getting matrix data again
        m <- solve(m_data, ...)                           ##Creating Inverse of matrix data
        x$setInverse(m)
        m                                                 ##Returning matrix
}
