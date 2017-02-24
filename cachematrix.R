## Writing a pair of functions that give the inverse of a matrix:

##Function creating a special "matrix" object that can cache its inverse:

makeCacheMatrix <- function(x = matrix()) {
     invrs <- NULL
     set <- function(y){
       x <- y
   invrs <- NULL
}
     get <- function()x
     setinverse <- function(inverse) invrs <<- inverse
     getinverse <- function() invrs
     list(set = set, get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}


##Function to compute the inverse of the special "matrix" returned by makeCacheMatrix above:

cacheSolve <- function(x, ...) {
     invrs <- x$getinverse()
     if(!is.null(invrs)){
       message("getting cached data")
       return(invrs)
     }
     mat <- x$get()
     invrs <- solve(mat, ...)
     x$setinverse(invrs)
     invrs
}
