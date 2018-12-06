
## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(solveMatrix) inv <<- solveMatrix
    getInverse <- function() inv
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if(!is.null(inv)){
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setInverse(inv)
    inv   
}

## code to test the above two functions
# function to generate a random square matrix using standard normal distribution
# the arg is the number of rows for this matrix
genMatrix=function(n_row){
  M=matrix(rnorm(n_row^2),nrow=n_row)
  return(M)
}

# generate a 10*10 matrix
mat=genMatrix(10)

# save input matrix and generate the list of functions
special_matrix=makeCacheMatrix(mat)

# at this moment the inverse has not been computed, so getInverse returns 'null'
special_matrix$getInverse()

# now compute the inverse 
cacheSolve(special_matrix)

# if I ask the function 'cacheSolve' again with the same input matrix, it will just return the cached inverse and won't 
# compute the inverse again
cacheSolve(special_matrix)
