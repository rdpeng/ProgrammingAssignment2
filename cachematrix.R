##Create Matrix
makeCacheMatrix <- function(x = matrix()) {
m <- Null
set <- function(y){
x <<- y
m <<- Null
}
     set <- function(y) {
             x <<- y
             m <<- NULL
     }
     get <- function() x
     setinverse <- function(inverse) m <<- inverse
     getinverse <- function() m
     list(set = set, get = get, 
          setinverse = setinverse, 
          getinverse = getinverse)


##Caching the Inverse of a Matrix
cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
m <- x$getinverse()
if (!is.null(m)) {
message("getting cached data")
return(m)
}
data <- x$get()
m <- solve(data, ...)
x$setinverse(m)
m
}
