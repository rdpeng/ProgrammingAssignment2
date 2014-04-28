##  The following functions cache the matrix inverse given a 
##  previous matrix. They allow avoid useless calculations.

##  The function, "makeCacheMatix" creates a list that contain
##  a matrix object some functions that:
##   1)set the value of the matrix
##   2)get the value of the matrix
##   3)set the value of the inverse
##   4)get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
##  ***start the inverse matrix***
       inv <- NULL
##  ***set and get functions***
       set <- function(y){
              x <<- y
              inv <<- NULL
       }
       get <- function() x
##  ***inverse functions***
       setInverse <- function(inverse) inv <<- inverse
       getInverse <- function() inv
       list(set = set,
            get = get,
            setInverse = setInverse,
            getInverse = getInverse)
}


##  The function, "cacheSolve" calculates the inverse
##  matrix created by above function. Check if inverse
##  has been calculated, if that's true, gets the inverse
##  from the cache and skips the calculations. Else,
##  calculates the inverse of the data and sets the value
##  of the inverse in the cache by the "setInverse" function.

cacheSolve <- function(x, ...) {
       inv <- x$getInverse()
##  ***check if there are a cached value***
       if(!is.null(inv)) {
              message("getting cached data")
              return(inv)
       }
       data <- x$get()
       inv <- solve(data, ...)
       x$setInverse(inv)
       inv
}
