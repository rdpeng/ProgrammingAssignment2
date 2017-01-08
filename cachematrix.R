## makeCacheMatrix sets value of the matrix, gets the value of the matrix, sets the inverse and gets the value of the inverse
## cacheSolve returns the cached value if it exists otherwise calculates the inverse, sets the value of the inverse and returns it


## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
m <- NULL
set <- function(y) {
x <<- y
m <<- NULL
}
get <- function() x
setInverse <- function(inverse) 
m <<- inverse
getInverse <- function() 
m
list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
if(!is.null(m)) {
message("getting cached data")
return(m)
}
data <- x$get()
m <- solve(data)
x$setInverse(m)
m

}
