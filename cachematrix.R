## The inverse of a matrix plays the same roles in matrix algebra as the reciprocal of a number and division does in ordinary arithmetic:
## Matrix conversion can be consuming. 
## This function creates a special "matrix" object that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
         inv <- NULL                    ##declare variable inv
        set <- function(y) {            ##updates value of set to y
                x <<- y                 ##the << operator passes y to x
                inv <<- NULL
        }
        get <- function() x            ##retrieves the value of x
        setinverse <- solve(inverse) inv <<- inverse
        getinverse <- solve() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}




## This function computes the inverse of the special "matrix" returned by makeCacheMatrix. 

cacheSolve <- function(x, ...) {
        inv <- x$getinv()                ##used to check if cache is empty
        if(!is.null(inv)) {             
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)         ##find and return the inverse of input matrix
        x$setinv(inv)
        return(inv)
        ## Return a matrix that is the inverse of 'x'
}
##TESTCODE
a<-makeCacheMatrix(matrix(1:4,2,2))
> a$get()               ##verifies the contents of a
     [,1] [,2]
[1,]    1    3
[2,]    2    4
a$getinv()             ##make sure the cache is empty
NULL
cacheSolve(a)           ##calculate the inverse and store in cache
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
a$getinv()              ##checking if mean has been stored
     [,1] [,2]
[1,]   -2  1.5
[2,]    1 -0.5
a$set(matrix(3:6,2,2))     ##finished with other matrix; create a new matrix



