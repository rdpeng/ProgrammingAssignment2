## Instead of compute repeadetly, we cache inverse of a matrix, and if we cached it before, we took inverse from the chache 
## if not we calculate the cache 


## makecachematrix function can take inverse of a matrix and cache it for future use

makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL ## setting inver for future value
 set <- function(y) {
    x <<- y
    inver <<- NULL
      } ## setting set as a function  assigning x to a new vektor Y and resets inver  
  get <- function() x  ## return X
  setinver <- function(inverse) inver <<- inverse ##  set inverse of inver to setinver 
  getinver <- function() inver  ##  returns inver 
  list(set = set, get = get, setinver = setinver, getinver = getinver) ##  returns list with functions
}


## cachesolve function can return inverse of a matrix. If it is been calculated and cached, it shows the inverse from the cache (returned by makeCacheMatrix). 

cacheSolve <- function(x, ...) {
  {
    inver <- x$getinver() # if it has  been calculated before this gets from getinver
    if(!is.null(inver)) {
      message("getting cached data")
      return(inver) # if
    }
    data <- x$get()
    inver <- solve(data, ...)
    x$setinver(inv)
    inver
  }	
}
