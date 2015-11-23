
# Assignment: Caching the Inverse of a Matrix


# Creating "makeCacheMatrix"  function that creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

# Create function cacheSolve that computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data)
        x$setinverse(i)
        i
}

# Test Code 

a <- makeCacheMatrix(x=matrix(c(1,2,3,4),nrow=2,ncol=2))
a$get()

cacheSolve(a)

a$set(matrix(c(10,200,33,41,52,63,71,82,91),nrow=3,ncol=3))

cacheSolve(a)

# To check whether it is taking data from cache execute below command once again without changing the matrix

cacheSolve(a)
