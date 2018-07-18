## Put comments here that give an overall description of what your
## functions do



## This function creates a special "matrix" object that can cache its inverse. A list containing methods to set, get, set the inverse and get the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function( y) {
        x <<- y
        i <<- NULL
        }
  
        get <- function() { x
        }
  
        setinverse <- function(inverse) { i <<- inverse
        }
  
        getinverse <- function() { i
        }
  
        list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## This function returns a matrix that is the inverse of 'x' created with the function above. It checks if it has already been calculated. If so, it gets the inverse from the cache and skips the computation. Otherwise, it calculates the inverse using solve() and sets it in the cache.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()

        if( !is.null(i) ) {
        message("getting cached data")
        return(i)
        }
  
        data <- x$get()
  
        i <- solve(data, ...)
  
        x$setinverse(i)
  
        i
}


#Let's check with a rotation matrix (the inverse of the matrix is its transpose) to check easily.
c <- matrix(c(0,0,1,1,0,0,0,1,0),3,3)
c

a <- makeCacheMatrix(c)
cacheSolve(a)

#Check if saved in cache
cacheSolve(a)
cacheSolve(a)



