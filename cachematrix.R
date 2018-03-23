## David Bradford
## This assignment asks the student to write two functions, makeCacheMatrix and cacheSolve
## Details around each function are included below.
## The main objective of these functions is to compute the inverse of a square matrix using the solve function
## For example, if X is a square invertible matrix, then solve(X) returns its inverse.

## This function creates a special "matrix" object that can cache its inverse.
## Note:  I used similar naming conventions using the examples provided in the assignment

makeCacheMatrix <- function(myMatrix = matrix())
{

    m <- NULL
    
    setMatrix <- function(y)
    {
        myMatrix <<- y
        m <<- NULL
    }
    
    getMatrix <- function() myMatrix
    setinverse <- function(inv) m <<- inv
    getinverse <- function() m
    
    list(setMatrix = setMatrix,
         getMatrix = getMatrix,
         setinverse = setinverse,
         getinverse = getinverse)
}


##  This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##  If the inverse has already been calculated (and the matrix has not changed),
##  then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(myMatrix, ...)
{
    
    m <- myMatrix$getinverse()
    
    if (!is.null(m))
    {
        message("getting cached inverse matrix")
        return(m)
    }
    
    data <- myMatrix$getMatrix()
    
    x <- solve(data, ...)
    
    myMatrix$setinverse(x)
    x
}

## To test, run the following in console

## M <- matrix(c(1,2,3,4),2,2)  # create a 2x2 matrix
## M

#  M1 <- makeCacheMatrix(M)  # inverse the matrix and place in cache
#  cacheSolve(M1) #inverse returned from cache

# another test
# solve(M)

