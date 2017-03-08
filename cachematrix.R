## Put comments here that give an overall description of what your+## Calculating an inverse matrix is usually a costly operation, and it can be beneficial
 ## to cach the inverse matrix rather than compute it several times. makeCacheMatrix and 
 ## cacheSolve functions are used to cache the inverse matrix
 ## For this assignment, we assume that the matrix supplied is always invertible.
  
 ## Write a short comment describing this function
 ## makeCacheMatrix creates a list containing the following functions:
 ## 1. set the elements of the matrix
 ## 2. get the elements of the matrix
 ## 3. set the value of the inverse matrix
 ## 4. get the value of the inverse matrix

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL #resets the inverse matrix, if previously defined
  set <- function(y) {x <<- y; inv <<- NULL} #set the elements of the matrix
  get <- function() x #get the elements of the matrix
  setInverse <- function(inverse) inv <<- inverse #set the value of the inverse matrix
  getInverse <- function() inv #get the value of the inverse matrix
  list(set = set,get = get,setInverse = setInverse,getInverse = getInverse) #special matrix that will be called later to check if Inverse exists
}


## Write a short comment describing this function
          
cacheSolve <- function(x, ...) {
  inv <- x$getInverse() #sets the inverse of the matrix as captured in makeCacheMatrix
  if (!is.null(inv)) { #checks to see if the inverse matrix is present
    message("getting cached data")  #if available, prints message
    return(inv) #returns value of the inverse matrix
  }
  mat <- x$get() #sets the inverse matrix
  inv <- solve(mat, ...) #solves for the inverse matrix using the solve() function
  x$setInverse(inv) #sets the new value of the inverse matrix
  inv #returns the inverse matrix as the output of the function
}
