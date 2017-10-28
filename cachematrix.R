## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = numeric()) {
        
        # holds the cached value or NULL if nothing is cached
        # initially nothing is cached so set it to NULL
        cache <- NULL
        
        # store a matrix
        setMatrix <- function(newValue) {
                x <<- newValue
                # since the matrix is assigned a new value, flush the cache
                cache <<- NULL
        }

        # returns the stored matrix
        getMatrix <- function() {
                x
        }

        # cache the given argument 
        cacheInverse <- function(solve) {
                cache <<- solve
        }

        # get the cached value
        getInverse <- function() {
                cache
        }
        
        # return a list. Each named element of the list is a function
        list(setMatrix = setMatrix, getMatrix = getMatrix, cacheInverse = cacheInverse, getInverse = getInverse)
}


# The following function calculates the inverse of a "special" matrix created with 
# makeCacheMatrix
cacheSolve <- function(y, ...) {
        # get the cached value
        inverse <- y$getInverse()
        # if a cached value exists return it
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        # otherwise get the matrix, caclulate the inverse and store it in
        # the cache
        data <- y$getMatrix()
        inverse <- solve(data)
        y$cacheInverse(inverse)
        
        # return the inverse
        inverse
}

#Soluion
# Create Matrix that must be squared to obtain the inverse
# > x = matrix(sample(1:9),3,3)
# 
# Apply the cache function to the object 
# > m = makeCacheMatrix(x)

# View the object before inversion
# > m$get()
# [,1] [,2] [,3]
# [1,]    4    5    3
# [2,]    1    2    8
# [3,]    7    9    6

# Cache the Inverse of the Object
# > cacheSolve(m)
# [,1] [,2] [,3]
# [1,]   12  0.6 -6.8
# [2,]  -10 -0.6  5.8
# [3,]    1  0.2 -0.6

# If already cached, message will appear and provide the cached answer instead 
# > cacheSolve(m)
# getting cached data
# [,1] [,2] [,3]
# [1,]   12  0.6 -6.8
# [2,]  -10 -0.6  5.8
# [3,]    1  0.2 -0.6
# > 
