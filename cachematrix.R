
# This function catch an object (matrix in this case) and initializing
# getters and setters. That is useful for next function cache solve

# THIS WORKS: matriz <- makeCacheMatrix(matrix( c(5, 1, 0,3,-1, 2, 4, 0,-1), nrow=3, byrow=TRUE))


makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    # With get() I retrieve parameter x (the matrix)
    # get returns the value of an object
    get <- function()x
    # setters and getters from value of inverse matrix
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
        
}


# This function takes matrix ad computes inverse matrix

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)){
        message("getting inversed matrix")
        # return inverse matrix
        return (m)
    }
    data <- x$get()
    m <- solve(data,...)
    x$setinverse(m)
    m
}


## ---------------Checking the program------------------------

# matriz <- makeCacheMatrix(matrix( c(5, 1, 0,3,-1, 2, 4, 0,-1), nrow=3, byrow=TRUE))
# > cacheSolve(matriz)
# [,1]    [,2]   [,3]
# [1,] 0.0625  0.0625  0.125
# [2,] 0.6875 -0.3125 -0.625
# [3,] 0.2500  0.2500 -0.500

# I've checked this program with a non inverse matrix just for fun:
# matriz2 <- makeCacheMatrix(matrix(c(2,4,0,4,3,0,1,8,0), nrow = 3, byrow = TRUE))
# cacheSolve(matriz2)
# Error in solve.default(data, ...) : 
#     Lapack routine dgesv: system is exactly singular: U[3,3] = 0 
# As I expected my program gives an error cause this is a special matrix that cannot inverse :D
