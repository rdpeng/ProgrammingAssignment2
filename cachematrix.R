 
makeCacheMatrix <- function(x = matrix()) 
{
  cachedInv <- NULL 
  set <- function(input = matrix()) 
  {
    x <<- input
    cachedInv <<- NULL
  }
  
  get <- function() x
  setInverse <- function(invmat)
  {
    cachedInv <<- invmat 
    return(cachedInv)
  }
  getInverse  <- function() cachedInv
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}



cacheSolve <- function(x=makeCacheMatrix(1:4, nrow=2, ncol=2), ...) 
{ 
  calculatedInverse <- x$getInverse() 
  
  ##check if there's a cached solved matrix
  if(!is.null(calculatedInverse) && is.matrix(calculatedInverse)) 
  { 
    message("Cached data found")
    return(calculatedInverse)
  }
  ##if data is not cached then it jumps to solve the matrix
  matrixToSolve <- x$get()  
  calculatedInverse <-  solve(matrixToSolve)
  x$setInverse(calculatedInverse)
}
