# R PROGRAMMING - ASSIGNMENT 2- WEEK 3
# 08/25/2019

#Below is the function that creates a special matrix to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL                               
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinver <- function(inverse) i <<- inverse
        getinver <- function() i
        list(set = set,
             get = get,
             setinver = setinver,
             getinver = getinver)
}

# Below is the function to compute the inverse of special matrix returned by above function

cacheSolve <- function(x, ...) {
        i <- x$getinver()
        if (!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinver(i)
        i
}

# Below is the example. Uncomment to run the example
#m <- matrix(c(1,2,3,4,5,6,7,8,0),3,3)
#m1 <- makeCacheMatrix(m)
#cacheSolve(m1)

# Solve() function is used to verify the result from the functions
#solve (m)
