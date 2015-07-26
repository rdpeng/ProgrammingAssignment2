Cachematrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y)
    {
        x <<- y
        mtx<<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) mtx <<- inverse
    getinverse <- function() m
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}
cacheSolve <- function(x, ...) {
    mtx<- x$getinverse()
    if(!is.null(mtx)) {
        
        return(mtx)
    }
    data <- x$get()
    mtx <- solve(data, ...)
    x$setinverse(mtx)
    mtx
}


## m1 <- matrix(data = c(100,150,200,250), nrow = 2, ncol = 2)
## m2 <- makeCacheMatrix(m1)
## cacheSolve(mat2)



