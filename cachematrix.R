## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

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
