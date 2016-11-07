## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

}


## Write a short comment describing this function


##Function makeCacheMatrix returns a matrix which will be used as an input to cacheSolve function for the inversion of matrix
##x is defined as a square invertible matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
makeCacheMatrix <- function(x = matrix()) {
  
  inv = NULL
  set = function(y) {
    # assign a value in an environment 
    x <<- y
    inv <<- NULL # to store inversion
  }
  get = function() x
  setinv = function(inverse) inv <<- inverse 
  getinv = function() inv
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}
## return the inverse of the original matrix 
          
          
 #This function calculates the inverse of the special "matrix" created with the above function. 
 ##Have to checksif the inverse has already been calculated. If not, calculates the inverse of the data and sets the value of the inverse in the cache via the setmatrix function.
 cacheSolve <- function(x) {
  
   inv = x$getinv()
   if (!is.null(inv)){
     message("getting cached data")
     return(inv)
   }
 
   data = x$get()
   inv = solve(data)
   x$setinv(inv)
   return(inv)
 }
