# makeCacheMatrix: return a list of functions to set and get value of matrix, and then to set and get value of inverse matrix
makeCacheMatrix <- function(x = matrix()) {
# inv contain cached inverse matrix
inv <- NULL

# Set function for matrix
set <- function(y) {
x <<- y
inv <<- NULL
}

# Get function for matrix
get <- function() x

# Set function for inverse matrix
setinv <- function(inverse) inv <<- inverse

# Get function for inverse matrix
getinv <- function() inv

# Return matrix based on the specified functions
list(set = set, get = get, setinv = setinv, getinv = getinv)
}

# cacheSolve: Compute the inverse of the matrix. If the inverse is already
# calculated before, it returns the cached inverse.
cacheSolve <- function(x, ...) {
inv <- x$getinv()

# Check if inverse matrix is already calculated, if true ==> Return inverse matrix
if (!is.null(inv)) {
message("getting cached data")
return(inv)
}

# if inverse matrix not yet calculated ==> Calculate inverse matrix
data <- x$get()
inv <- solve(data, ...)

# Cache inverse matrix
x$setinv(inv)

# Return inverse matrix
inv
}
