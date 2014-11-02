## Create Inverse Matrix with Caching 

## Creates a list out of matrix that gets fed to cacheSolve

makeCacheMatrix <- function(orig.Mx = matrix()) {
  
  # Test input
  if (!is.matrix(orig.Mx)) {
    stop("Input not a matrix")
  }
  if (nrow(orig.Mx)!=ncol(orig.Mx)) {
    stop("Input not a square matrix")
  }
  
  
  # Set stored inverse matrix to NULL
  inv.Mx <- NULL
  
  # Set value of the matrix
  setMx <- function(q) {
    orig.Mx <<- q
    inv.Mx <<- NULL # matrix has changed, change to NULL
  }
  
  # Get value of matrix
  getMx <- function() orig.Mx
  
  # Set inverse of matrix
  setInv <- function(inverse) inv.Mx <<- inverse
  
  # Get inverse of matrix
  getInv <- function() inv.Mx
  
  # Return list of above functions
  list(setMx= setMx, 
       getMx = getMx,
       setInv = setInv,
       getInv = getInv)
}



## Take a list, check for cached result and return, if not calculate

cacheSolve <- function(Mx.to.cache, ...) {
  
  # get inverse
  inv.Mx <- Mx.to.cache$getInv()
  
  # if inverse exists, check if already cached
  # if yes, return cached inverse
  if(!is.null(inv.Mx)) {
    message("Getting cached data")
    return(inv.Mx)
  }
  
  # if not, get matrix
  Mx <- Mx.to.cache$getMx()
# getElement(x, "ed")
  
  # compute inverse of matrix
  inv.Mx <- solve(Mx, ...)
  
  # cache inverse of matrix
  Mx.to.cache$setInv(inv.Mx)
  
  # return inverse
  inv.Mx
}

# testMx <- matrix(c(1,2,23, 11,12,13, 1, 2, 3), nrow = 3, ncol = 3)
# Notsq <- matrix(c(1,2,23, 11,12,13), nrow = 2, ncol = 3)
# 
# test<-makeCacheMatrix(testMx)
# cacheSolve(test)
# test2<-makeCacheMatrix(Notsq)
# cacheSolve(test2)
