## The following function computes an inverse of matrix with cache enabled

## The makeCaheMatrix function gets and sets the matrix as well as its inverse (cacheing the inverse)


makeCacheMatrix <- function(x = matrix()) {
                 m <- NULL
                 set <- function(y) {
                         x <<- y
                         m <<- NULL
                 }
                 get <- function() x
                 setinverse <- function(inverse) m <<- inverse
                 getinverse <- function() m
                 list (set= set, get = get ,
                       setinverse = setinverse,
                       getinverse = getinverse)
                 
}


## The function finds the inverse of the matrix created by the function above. But first it checks that if the inverse
##has already been calculated and that the matrix has not changed then it gets the inverse from the cache and avoids
##computation. Else, it find the inverse and sets it in cache via setinverse fucntion

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if (!is.null(m)){
                message("getting inverse from cache")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}

B <- matrix(c(1,2,3,4),2,2)
B1 <- makeCacheMatrix(B)
B1
cacheSolve(B1)
