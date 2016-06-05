## Assignment 2: Lexical Scoping - Test Veriable Environment in R 
## makeCacheMatrix is a function create a special Matrix, is contians setMatrix, getMatrix, setInverseMatrix, and getInverseMatrix
## cacheSolve is a function to inverse a Matrix, if inverse already exixt, return the cache in makeCacheMatrix 

## function makeCacheMatrix take a matrix veriable, set Matrix in function environment. When calls return the matrix 

makeCacheMatrix <- function(x = matrix()) {

        m <<- NULL
        setMatrix <- function(y){
                x <<- y
                m <<- NULL
                }
        getMatrix <- function() x
        setInverseMatrix <- function(inverse) m <<- inverse
        getInverseMatrix <- function() m
        list(setMatrix = setMatrix, getMatrix = getMatrix, setInverseMatrix = setInverseMatrix, getInverseMatrix = getInverseMatrix)
                

}


## function cacheSolve take a "Matrix" verible test if there's matrix inverse already exist, 
## if so return cache, if not calculate the inverse and store in the matrix and return the inverse 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverseMatrix()
        if (!is.null(m)){
                message("getting cached data")
                return(m)
                }
        data <- x$getMatrix()
        m <- solve(data)
        x$setInverseMatrix(m)
        m
}
