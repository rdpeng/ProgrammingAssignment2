## Create a function to make cache matrix. Function written with the assumption that the matrix supplied is always invertible!
makeCacheMatrix <- function(mat = matrix()) {
        i <- NULL
## Setting up the set function for matrix and assigning default value of NULL for variable i (inverse matrix)
        setmat <- function(newmat) {
                mat <<- newmat
                i <<- NULL
        }
## Setting up the get function for matrix
        getmat <- function() mat
## Setting up the setter for the inverse matrix
        setinverse <- function(inverse) i <<- inverse
## Setting up the getter for the inverse matrix
        getinverse <- function() i
## The end product of makeCacheMatrix which is a list comprising of 4 elements and the assigned names to respective functions above
        list(setmat = setmat, getmat = getmat, 
             setinverse = setinverse,
             getinverse = getinverse)
}

## Function to compute inverse matrix
cacheSolve <- function(mat, ...) {
        i <- mat$getinverse()
## Check if cache matrix is already present, if so to retrieve it
        if (!is.null(i)) { 
                message("getting cached data")
                return(i)
        }
## Otherwise, calculate inverse matrix
        data <- mat$getmat()
        i <- solve(data, ...)
        mat$setinverse(i)
## Return a matrix that is the inverse of mat
        i
}
