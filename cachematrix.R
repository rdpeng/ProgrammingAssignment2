## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## makeCacheMatrix creates a list containing a function to  set the value of the matrix, get the value of the matrix,
## set the value of de inverse matrix and get the value of this inverse matrix.
makeCacheMatrix <- function(A = matrix()) {
  IA <- NULL
  set <- function(B) {
    A <<- B
    IA <<- NULL
  }
  get <- function() A
  setinverse <- function(solve) IA <<- solve
  getinverse <- function() IA
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## Write a short comment describing this function
## This fuction calculates the inverse of the special list create with the above function. First the function checks if 
## the inverse matrix has already been calculated or it calculates the inverse matrix of the data and sets its value
## in the cache viea the setinverse.

cacheSolve <- function(A, ...) {
  IA <- A$getinverse()
  if(!is.null(IA)) {
    message("getting cached data")
    return(IA)
  }
  data <- A$get()
  IA <- solve(data, ...)
  A$setinverse(IA)
  IA
}

        ## Return a matrix that is the inverse of 'A'

   
