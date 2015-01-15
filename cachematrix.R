## CALCULATION OF THE INVERSE OF A MATRIX USING
## CACHE MEMORY

## Create the environment and methods for create and caching
## both matrix and inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  
        inv <- NULL
        setmat <- function(mat)  {
                x <<- mat
                inv <<- NULL
        }
        getmat <- function() x
        setinv <- function(minv)  {
                inv <<- minv
        }
        getinv <- function() inv
}


## Compare new matrix with the existing in cache. If it is the
## same and its inverse exist, get inverse matrix from cache.
## In other case, the inverse matrix is calculated and stored in
## cache

cacheSolve <- function(x, ...) {
        
        ## Compare matrix with cache. If matrix already exists and the inverse is calculated
        if(getinv() != NULL & identical(x, getmat()){
                return getinv()
        }
        ## New matrix or inverse not calculated.
        else{
                invMat <- solve(x)
                setmat(x)
                setinv(invMat)
                return(invMat)
        }
}
