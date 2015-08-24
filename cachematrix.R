## The functions below serve to cache the inverse of a matrix
## rather than compute it over and over.  It assumes the matrix is 
## always invertible.  

## makeCacheMatrix make a list containing a function to 
## set the value of the matrix, get the value of the matrix
## set the value of the inverse of the matrix, and get the 
## inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {

  inv = NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  setinv <- function(inverse) inv <<-inverse
  getinv <- function() inv
  list(set = set, 
       get = get,
       setinv = setinv,
       getinv = getinv)
}

## cacheSolve returns the inverse of the matrix passed 
## to it.  If the inverse has already been computed,
## it lets the user know it is retrieving the cached data
## and does not compute another inverse.  If there is no invese, 
## it calculates and caches it.  

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)){
      message("getting cached data.")
      return (inv)
  }
  data <- x$get()
  inv <- solve(data,...)
  x$setinv(inv)
  inv
  
  }


