## R function that is able to cache potentially time consuming computation of 
## inverse matrix.The functions are created with the advantage of scoping rules 
## in R to  manipulate changes in an object according to requirement.


## This function creates a special "matrix" object that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
            inv <- NULL
            set <- function(y) {
                   x <<- y
                   inv <<- NULL
                   }
            get <- function() x
            setinverse <- function(inverse) inv <<- inverse
            getinverse <- function() inv
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## This function computes the inverse of the special "matrix" returned by `makeCacheMatrix` above. 


cacheSolve <- function(x, ...) {
       inv <- x$getinverse()
             if (!is.null(inv)) {
             message("obtaining cached data")
             return(inv)
             }
      data <- x$get()
       inv <- solve(data, ...)
       x$setinverse(inv)
       inv
}
