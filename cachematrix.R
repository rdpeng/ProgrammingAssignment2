## the function sets value of the matrix, gets value of the matrix,and 
caches the value of matrix.

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
v <- NULL
set <- function (y) {
x <<- y
v <<- NULL

get <- function ( )x
solve <- function (x)  v <<- inverse
getinverse <- function ( ) v
list (set = set,  get =get
setinverse = setinverse
getinverse = getinverse
}

## get inverse function () x


cacheSolve <- function(x, ...) {

v <- x$getinverse ( )
if(!is.NULL(v)) {
message ("getting cache message data")
return (v)

}

data <- x$get ( )
v <- solve(x)
x$setinverse (v)
v
        ## Return a matrix that is the inverse of 'x'
}
