> makeCacheMatrix <- function(x = matrix()) {
inv <- NULL
set <- function(y) {
x <<- y
inv <<- NULL
}
get <- function() x
setinverse <- function(inverse) inv <<- inverse
getinverse <- function() inv
list(set = set, get = get,
setinverse = setinverse,
getinverse = getinverse)
}

> cacheinverse <- function(x, ...) {
inv <- x$getinverse()
if(!is.null(inv)) {
message("getting cached data")
return(inv)
}
matrix_to_invert <- x$get()
inv <- solve(matrix_to_invert, ...)
x$setinverse(inv)
inv
}

## First example to test my code…

my_Matrix <- makeCacheMatrix(matrix(1:4, 2, 2))

my_Matrix$get()

my_Matrix$getinverse()

cacheinverse(my_Matrix)

cacheinverse(my_Matrix)

## Second example to test my code…

my_Matrix$set(matrix(c(2, 2, 1, 4), 2, 2))

my_Matrix$get()

my_Matrix$getinverse()

cacheinverse(my_Matrix)

my_Matrix$getinverse()


