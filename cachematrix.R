## The "makeCasheMatrix" function creates a special "matrix" 
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## The "cacheSolve" fuction computes the inverse of the special "matric"
## returned by "makeCacheMatrix" above. If the inverse has already been
## calculated, it gets the inverse from the cache. Otherwise, it calculates
## the inverse of the data and sets the value of the inverse in the cache.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                reture(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}

## Test
## my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
## my_matrix$get()
##      [,1] [,2]
## [1,]    1    3
## [2,]    2    4
## my_matrix$getinverse()
## NULL
## cacheSolve(my_matrix)
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## my_matrix$getinverse()
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
