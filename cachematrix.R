## functions to cache and retrieve a square matrix

# to cache a square matrix
makeCacheMatrix <- function(x = matrix()) {
    inv_matrix <- NULL

# set the value of the matrix
    set <- function(y) {
        x <<- y
        inv_matrix <<- NULL
    }

# get the value of the matrix
    get <- function() x

# set the value of inverse of the matrix
    setinverse <- function(inverse) inv_matrix <<- inverse

# get the value of inverse of the matrix    
    getinverse <- function() inv_matrix 
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

# to retrieve a square matrix
cacheSolve <- function(x, ...) {

# the inverse of the matrix
    inv_matrix <- x$getinverse()

# if the inverse has already been computed, it gets the result
    if(!is.null(inv_matrix )) {
        message("getting cached data.")
        return(inv_matrix )
    }

# if the inverse has not been computed, it computes the inverse
    data <- x$get()
    inv_matrix <- solve(data)

# sets the value in the cache
    x$setinverse(inv_matrix )
    inv_matrix
}

# set up the square matric
x = cbind(c(1, -1/4), c(-1/4, 1))
 
# cache the square matrix
sq_matrix = makeCacheMatrix(x)

#show the square matrix
m$get()


# No cache in the first run
cacheSolve(sq_matrix)

# Retrieving from the cache in the second run
cacheSolve(sq_matrix)
