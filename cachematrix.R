## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## The below function creates the object , so that we can give input by using set function and 
## this object is passed to below function to compute.  

makeCacheMatrix <- function(x = matrix()) {
  Minverse <- matrix(data = NA)
  set <- function(y) {
    x <<- y
    Minverse <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) {
     Minverse <<- solve(x)
    }
  getinverse <- function()  Minverse
list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}
 

## Write a short comment describing this function
##It takes the returned values or object and returns inverse matrix if it is cached else it set 
##the value to  cache and  returns  the  inverse of the matrix


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i) && !is.na(i)) {
    message("getting cached data")
    return(i)
  }
  nmatrix <- x$get()
  ## Here  below iam checking whether the matrix is  invertable are not
  if(ncol(nmatrix) != nrow(nmatrix) || det(nmatrix) == 0) return(" Matrix is not invertable !!!")
  nm <- solve(nmatrix)
  x$setinverse(nm)
  nm
}
