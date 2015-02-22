## This function will take a special matrix in under argument "x"
# And has 4 subfunctions defined in makeCacheMatrix


## This function returns a list with 4 sub_fuctions embedded inside
# First sub_function assigns input y to x and m as a Null value
# Second sub_function assigns x it self to get, which is the input matrix to the container function makeCacheMatrix
#Third sub_function reads the inverse as input and assings as output
#Forth sub_function assigns m as the inverse of the martix x

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
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
}

## This function utilizes makeCacheMatrix list/function container
# to check whether the inverse matrix of input matrix x in cached or not
# if it is cached, then calls the value from makeCacheMatrix's 4th list element and ends the function by returning m.
# if it not cached, since 3rd list element returns Null, then calculates the inverse and cached it.
# and prints as m


cachSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'

    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached inverse matrix")
        return(m)
    }
    inputmatrix <- x$get()
    m <- solve(inputmatrix, ...)
    x$setinverse(m)
    m
}



