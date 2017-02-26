## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##This function set the value of the vector, get the value of the vector, set the value of the function finversa and get the value of finversa

makeCacheMatrix <- function(x = matrix()) 
{
  inversa<-NULL
  set <- function(y){
    x<<- y
    inversa<<- NULL
    
  }
  get <-function() x
  setinversa <-function(finversa) inversa <<- finversa
  getinversa <-function() inversa
  list(set = set, get = get, setinversa=setinversa, getinversa=getinversa)
}

## Write a short comment describing this function
##This function through the function solve, calulate the inverse of x

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inversa <- x$getinversa()
  if(!is.null(inversa)){
    message("getting cached data")
    return(inversa)
  }
  
  data <- x$get()
  inversa<- solve(data)
  x$setinversa(inversa)
  inversa
}
