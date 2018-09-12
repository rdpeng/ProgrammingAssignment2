# Create a special “matrix” object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
         i <- NULL
         set <- function(y) {
                 x <<- y
                 i <<- NULL
         }
         get <- function() x
         setinverse <- function(inverse) i <<- inverse
         getinverse <- function() i
         list(set = set, get = get,
              setinverse = setinverse,
              getinverse = getinverse)
 }

# Compute the inverse of the special “matrix” returned by makeCacheMatrix above
cachesolve <- function(x) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached matrix")
                return(i)
        }
        matrix <- x$get()
        i <- solve(matrix)
        x$setinverse(i)
        i
}
