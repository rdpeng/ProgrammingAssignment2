## Put comments here that give an overall description of what your
## functions do

## The 2 functions below are used to create a square matrix to store the mean and to retrieve the inverse of the mean if it has already been cached, with a call to solve(). 
 

## Write a short comment describing this function 

## This function creates a special "matrix" object that can cache its mean.
 
makeCacheMatrix <- function(x = matrix()) {
 m <- NULL
 set <- function(y) {
             x <<- y
             m <<- NULL
 }

  get <- function() x
  setmean <- function(mean) m <<- mean
  getmean <- function() m
  matrix(set = set, get = get,
                setmean = setmean,
                 getmean = getmean)

}


## Write a short comment describing this function 

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the mean has already been calculated, and the matrix has not changed, then the cacheSolve
## will retrieve the inverve from the cache.
 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

            m <- x$getmean()
            if(!is.null(m)) {
                      message (" getting cached data" )
                      return(m)
             }
            data <- x$get()
            m <- solve(data, ...)
            x$setmean()
            m
      }
   
}
