#The function makeCacheMatrix returns a list of functions
# It is used to store a martix and a cached value of the inverse of the 
# matrix. Following is the list of functions returned by the List 
# setMatrix      set the value of a matrix
# getMatrix      get the value of a matrix
# cacheInverse   Cache the value of the inverse of the matrix
# getInverse     get the cached value of inverse of the matrix
#
makeCacheMatrix <- function(x = numeric()) {
#initially set cache to null
        cache <- NULL  
        
        # store a matrix
        setMatrix <- function(newMatrix) {
                x <<- newMatrix
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


#Following function calculates the inverse of a special "matrix" created with the makeCacheMatrix function
#HOwever, the function first checks if the inverse already exists; if so, it returns the cached value
#else, it calculates the inverse and caches the value

cacheSolve <- function(y, ...) {
        # get the cached value of the Matrix
        inverse <- y$getInverse()
        # Return the cached value if it exists
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        # else get the matrix; caclulate the inverse and store it in the cache
        data <- y$getMatrix()
        inverse <- solve(data)
        y$cacheInverse(inverse)
        
        # return the inverse
        inverse
}