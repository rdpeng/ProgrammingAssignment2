## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        setInputMatrix <-function (y){
                x <<- y
                m <<- NULL
        }
        getInputMatrix<-function() x
        setInverseMatrix<-function(solve) m<<- solve
        getInverseMatrix<-function() m
        list(setInputMatrix=setInputMatrix, getInputMatrix=getInputMatrix,
             setInverseMatrix=setInverseMatrix,
             getInverseMatrix=getInverseMatrix)
}

## Write a short comment describing this function
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m<-x$getInverseMatrix()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        matrix<-x$getInputMatrix()
        m<-solve(matrix, ...)
        x$setInverseMatrix(m)
        m
}
