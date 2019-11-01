## MakeCacheMatrix and cacheSolve matrix are two complementary functions
## that create a matrix (set and get methods) and compute its inverse or
# retrieve its cache

## MakeCacheMatrix creates a lists of functions that sets and gets 
##the actual Matrix and its inverse
makeCacheMatrix <- function(x = matrix()) 
{
  #we guarantee we create a square matrix and initialize it to NA
  dim<-sqrt(floor(length(x)))
  m<-matrix(NA,dim,dim)
  #the set function to creates the matrix
  set <- function(y) {
    dim<-sqrt(floor(length(y)))
    x<<-matrix(y,dim,dim)
    m<<-matrix(NA,dim,dim)
  }
  #the set function to return the current value matrix
  get <- function() {
    dim<-sqrt(floor(length(x)))
    matrix(x,dim,dim)
  }
  
  #the setinverse function sets the inverse value of the matrix
  setinverse <- function(solve=matrix())
  {
    dim<-sqrt(floor(length(solve)))
    m<<-matrix(solve,dim,dim)
  }
  #getinverse returns m, the inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve calculates the inverse of a matrix or calculates it if the matrix is empty
cacheSolve <- function(x, ...)
{
  # return the inverse of x, or calculate & return if cache is empty
  m<-x$getinverse()
  #if the inverse matrix exists, we just return it
  if(!is.na(sum(m))) {
    message("getting cached matrix data")
    return(m)
  }
  #if the inverse matrix does not exits, we calculate and
  #set it as well
  data<-matrix()
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  message("calculating inverse matrix data")
  m
}
