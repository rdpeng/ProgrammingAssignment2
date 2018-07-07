#Functions allow user to create cached inverse of
#specified square matrix

#Added functionality to ensure that matrix is invertible.

makeCacheMatrix <- function(x = matrix()) 
{
  if(dim(x)[1] == dim(x)[2])
  {
    inv <- NULL
    set<-function(b)
    {
      x<<- b
      inv<<- NULL
    }
    get <- function() x
    setInv <- function(solve) inv <<- solve
    getInv <- function() inv
    list(set = set, get = get, setInv = setInv, getInv = getInv)
  }
  else
  {
    message("Matrix is not invertible. Check dimensions")
    NULL
  }
}

#Solves or fetches solved inverse matrix

cacheSolve <- function(x, ...) 
{
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInv()
  if(!is.null(inv))
  {
    message("Getting cached inverse")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data,...)
  x$setInv(inv)
  inv
}
