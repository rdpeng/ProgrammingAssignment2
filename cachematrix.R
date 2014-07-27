#Matrix inversion is usually a costly computation and their
#may be some benefit to caching the inverse of a matrix rather
#than compute it repeatedly  The purpose of the following two functions is to
#cache the inverse of a matrix.

#the two functions:
        
#makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

#cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), 
#then the cachesolve should retrieve the inverse from the cache.



#makeCacheMatrix creates a special "vector", which is really a list containing a function to
#set the value of the matrix
#get the value of the matrix
#set the value of the matrix inverse
#get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve calculates the inverse of the special "matrix" created with the above function - makeCacheMatrix.
#However, it first checks to see if the inverse has already been calculated. If so, 
#it gets the inverse from the cache and skips the computation. Otherwise, it calculates 
#the inverse of the data and sets the value of the inverse in the cache via the setinversea function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
