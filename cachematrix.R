## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    get <- function() x
    set <- function(y = matrix()) {
        x <<- y
        inverse <<- NULL
    }
    getinverse <- function() inverse
    setinverse <- function(z) { inverse <<- z }
    list(set = set, get = get, getinverse = getinverse, setinverse = setinverse)
}


## Write a short comment describing this function

cacheSolve <- function(a) {
    cached_inverse <- a$getinverse()
    print(cached_inverse)
    if (!is.null(cached_inverse)) {
        print('getting cached data...')
        return(cached_inverse)
    }
    inverse <- solve(a$get())
    a$setinverse(inverse)
    print(inverse)
}
