## ProgrammingAssignment2
## Caching the Inverse of a Matrix
## These are a pair of functions that cache the inverse of a matrix


## Function 1: makeCacheMatrix
## This function creates a special "matrix" object that can cache its inverse
## It defines setter and getter functions of the matrix and its inverse

makeCacheMatrix <- function(m = matrix())
{
## Inizialization of matrix m and object h
  
  h <- NULL
  
## Getter (getm) and setter (setm) of the matrix
## <<- assigned variables will be available when using function cacheSolve()
  
  setm <- function(mm)
  {
    m <<- mm
    h <<- NULL
  }
  
  getm <- function()
  {
    m
    return(m)
  }
  
## Getter (getinv) and setter (setinv) of the inverse
  
  setinv <- function(inv)
  {
    h <<- inv
  }
  
  getinv <- function()
  {
    h
    return(h)
  }
  
## Assign each of these functions as an element within a list(),
## and returns it to the parent environment
  
  list(setm = setm, getm = getm,
       setinv = setinv,
       getinv = getinv)
}



## Function 2: cacheSolve
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(m, ...)
{

# Attemption to retrieve the inverse from the object m
          
  h <- m$getinv()

# If the value of inverse retrieved is not NA, the cached Inverse value is returned
   
  if(!all(is.na(h)))
  {
    message("getting cached data")
    return(h)
  }
  
# If the value of inverse retrieved is NA, the Inverse is calculated by solve() function
# and this calculated Inverse value is set in the input object

  datamatrix <- m$getm()
  h <- solve(datamatrix)
  m$setinv(h)
  h
}
