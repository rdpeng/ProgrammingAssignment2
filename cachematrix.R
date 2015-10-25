## Functions creates matrixes and inversing them depending of whether the matrix was inversed from the beginnind or changed.

## The function makeCacheMatrix creates a vector, that a) sets the matrix, b) gets the matrix, c) sets the value of the inversed  matrix and d) get the value of inversed matrix

makeCacheMatrix <- function(x = martix()) {
        m <- NULL
        #set the value of a matrix
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        #get the value of a matrix
        get <- function() x
        #set the value of the inversed matrix
        setinverse <- function(solve) m <<- solve
        #get the value of the inversed matrix
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The function calculates the inverse matrix created with the previous function.  

cacheSolve <- function(x, ...) {
        #check if the matrix is invertible
        a <- class(try(solve(x$get()),silent=T))
  if (a != "matrix"){
    stop("matrix does not have an inverse")
  }
        #get matrix from cache
        m <- x$getinverse()
        #if the cached matrix exists them returns cache
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        #if there is no cached matrix then it creates the one
        data <- x$get()
        #calculating inverse
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
