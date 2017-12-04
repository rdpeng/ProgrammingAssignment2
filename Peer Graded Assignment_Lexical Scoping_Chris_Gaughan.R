# Peer Graded Assignment_Lexical Scoping_Chris_Gaughan
makeCacheMatrix <- function(x = matrix()) {
        invMatrix <- NULL
        setMatrix <- function(y) {
                x <<- y
                invMatrix <<- NULL
        }
        
        getMatrix <- function() x  #get the value of the Matrix
        setInverse <- function(inverse) invMatrix <<- inverse #set value of inverse matrix
        getInverse <- function() invMatrix  #get the value of the inverse
        list(setMatrix = setMatrix, getMatrix = getMatrix,
                   setInverse = setInverse, getInverse = getInverse)
         
}
# Number 2 
cacheSolve <- function(x, ...) {
        invMatrix <- x$getInverse()
          if(!is.null(invMatrix)) {                       #if inverse matrix is not NULL_which happens a lot
                   message("Getting Cached Inverse Matrix")   #Type message: Getting Cached Inverse Matrix 
                    return(invMatrix)                             #return the inverse matrix
          }
        MatrixData <- x$getMatrix()                     #get original matrix Data 
        +  invMatrix <- solve(MatrixData, ...)             #use to solve function to inverse the matrix
        +  x$setInverse(invMatrix)                         #set the inverse matrix 
        +  return(invMatrix)                               #return the invertible matrix
}
# Sometimes gives error__ Error in solve.default(MatrixData, ...) : for U[3,3]=0       