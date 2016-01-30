## makeCacheMatrix will create a matrix x, and expose three methods to set/get x and its inverse

makeCacheMatrix <- function(x = matrix()) {
  Inv <- NULL ## initialize inverse
  
  ## set x in parent env with the desired value, if inverse is already set, get rid of it!
  set <- function(Value = matrix()) {
    x <<- Value 
    Inv <<- NULL
  }
  
  get <- function() x
  
  ##set inverse variable in parent env to desired value and return the value as a convenience
  setInverse <- function(Val) {
    Inv <<- Val 
    return(Inv)
  }
  
  getInverse  <- function() Inv
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## given the list variable from the first function, will first check to see if there's already a cached inverse and return
## otherwise will attempt to solve its inverse and set/return it

cacheSolve <- function(x=makeCacheMatrix(1:4, nrow=2, ncol=2), ...) { ##special matrix provided or create a test 2x2 matrix
  
  ## let's see if there's something there already
  cal_Inverse <- x$getInverse() 
  
  ##check if there's a cached value AND it's a matrix
  if(!is.null(cal_Inverse) && is.matrix(cal_Inverse)) { 
    message("We found cached data and saved valuable cpus!!!")
    return(cal_Inverse)
  }
  
  ## otherwise get the matrix
  matrixToSolve <- x$get()  
  
  ## try to solve the matrix and catch errors and warnings
  cal_Inverse <- tryCatch({ 
    solve(matrixToSolve)
  }, warning=function(w) {
    message("This may not be the result you're looking for")
    message(w)
  }, error=function(e) {
    message("Something went wrong solving your matrix")
    message(e)
    message("\n")
  })
  
  ## whatever the case, set the value of the inverse (NULL if something went wrong)
  message("Setting the value of inverse to:") 
  x$setInverse(cal_Inverse)
}
