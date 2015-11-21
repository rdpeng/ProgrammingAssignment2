## The following give an overall description of what your
## functions do at each given component

##Kevin Wunderlich

makeCacheMatrix <- function(x = matrix()) {
      
      ##In order to know that we are starting with no cached data, we set m<-NULL as follows
      m <- NULL
      
      ##x$setMatrix(y) will change the originial x matrix into an inputed y matrix 
      ##This is an important function because this will reset the cached variable, 
      ##(in this case the inverse matrix), back to m<- NULL so we can start a new "cycle" if prompted
      
      setMatrix <- function(y) {   
            x <<- y
            m <<- NULL
      }
      
      ##x$getMatrix() will return the input matrix
      getMatrix <- function() x    
      
      ##x$setInv(mInverse) takes input mInverse (inverse matrix of current set matrix)
      ##and allows "m" to cache mInverse (which will be computed in cacheSolve)
      setInv <- function(mInverse) m <<- mInverse
      
      ##x$getInv takes no input and returns the most recently cached inverse 
      ##The first time cacheSolve is run for a particular matrix, this value should return NA
      ##Once cacheSolve has been run once, this value will no longer be NA, and 
      ##cache data will result in future prompts, unless a x$setMatrix() sets m back to m=NULL
      getInv <- function() m
      
      ##The following list is actually the "special matrix" this makeCachematrix() creates, this list is
      ##very important because R has the capability of LEXICAL SCOPING which allows 
      ##us to call the functions listed below as defined above
      list(setMatrix = setMatrix, getMatrix = getMatrix,
           setInv = setInv,
           getInv = getInv)
}


## Write a short comment describing this function

cacheSolve <- function(X, ...) {
      
      ##First, set m to be the inverse from the call list in the previous function
      ##The first time this function is run for a particular set matrix, X$getInv() will return NULL 
      ##Once run a second time, X$getInv() will return an inverse matrix and therefore will no longer yield NULL
      m <- X$getInv()
      
      #This if statement is run once X$getInv() no longer yields NULL, and thus verifys that there is cache data
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      
      ##Regardless of the preceeding if statement, this function will input the special CacheMatrix and proceed
      ##to solve for the inverse of the original input matrix (_x_) of makeCacheMatrix(_x_)->CacheMatrix
      z<-X$getMatrix()
      
      ##This data __is__ our cached data once X$getInv() != NULL
      data <- X$getInv()
      
      ##The following sets !is.null(m) = TRUE and will result in cache data if prompted again before setting a new matrix
      m <- solve(z)
      X$setInv(m)
      m
        ## Return a matrix that is the inverse of 'x'
}
