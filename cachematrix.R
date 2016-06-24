makeCacheMetrix <- function(x = matrix()) {
i <- NULL
         set <- function(y) {
                 x <<- y
                 i <<- NULL
         }
         get_mat <- function() x
         setInv <- function(inv) i <<- inverse
         getInv <- function() i
         list(set = set, get = get,
              setInv = setInv,
              getInv = getInv)
}
cacheSolve <- function(x, ...) {
         i <- x$getInv()
         if(!is.null(i)) {
                message("getting cached data")
                return(i)
         }
         matrix <- x$get_mat()
         inv <- inv(matrix, ...)
         x$setinv(i)
         i
}
