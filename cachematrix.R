
## This function makeCacheMatrix is supplied with a matrix as an input and in turn
## it has functions which set and retrive the matrix and inverse of the given matrix
makeCacheMatrix <- function(Matrix = matrix()) {
 matrixInverse <- NULL
        
    
  ##Use the supplied matrix argument to the Matrix variable and inverse to Null         
            setMatrixF <- function (setMatrix) {
                Matrix <<- setMatrix
                matrixInverse <<- NULL
        }
  ##Function to retreive the actual matrix      
        getMatrixF <- function() {
                Matrix
        }
      
  ##Function to set the inverse of the given matrix to the matrixInverse variable          
        setInverseF <- function(setInverse) {
                matrixInverse <<- setInverse
        }
   
   ##Function to retrieve the inverse of the given matrix          
        getInverseF <- function() {
                matrixInverse
        }
        
        list(set = setMatrixF, get = getMatrixF,
             setInverse = setInverseF, getInverse = getInverseF)
}


## The main aim of the function cacheSolve() is to calculate and return the inverse of a given matrix. 
## First, this function checks whether the inverse is already calculated and cached. If yes, it retrieves the
## already cached inverse matrix data and returns it. If not, then it employs the function setInverseF
## nested in the makeCacheMatrix and caches the newly calculated inverse. cacheSolve
## uses solve() to calculate the inverse of the given matrix.

cacheSolve <- function(Matrix, ...) {
           ## Return a matrix that is the inverse of 'x'
        
        Inverse <- Matrix$getInverse()
       
##Check if the inverse is already cached. If yes, notify about retrieving the
##inverse and exit the function by returning the data          
        if(!is.null(Inverse)) {
                message("retrieving cached inverse data")
                return(Inverse)
        }
##If the inverse is not cached, calculate and cache now.        
        MatrixData <- Matrix$get()
        Inverse <- solve(MatrixData, ...)
        Matrix$setInverse(Inverse)
        return(Inverse)
}
